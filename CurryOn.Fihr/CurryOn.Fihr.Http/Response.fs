namespace CurryOn.Fihr.Http

open CurryOn.DependencyInjection
open CurryOn.Serialization
open Newtonsoft.Json.Linq
open System

[<Struct>] 
type HttpResponseData = 
    private 
    | ObjectHttpResponse of object: obj
    | RawHttpResponse of json: string

[<TransparentUnion>]
type HttpResponse =
    | OK of HttpResponseData
    | Created of HttpResponseData * Uri option
    | Accepted
    | NoContent
    | MultiStatus of MultiStatusResponse []
    | TemporaryRedirect of Uri
    | PermanentRedirect of HttpRedirect
    | BadRequest of IHttpError
    | Unauthorized of AuthenticationInfo
    | Forbidden 
    | NotFound
    | NotAcceptable
    | UnprocessableEntity of IHttpError
    | InternalServerError of IHttpError option
    member response.StatusCode =
        match response with
        | OK _ -> 200
        | Created (_,_) -> 201
        | Accepted -> 202
        | NoContent -> 204
        | MultiStatus _ -> 207
        | TemporaryRedirect _ -> 307
        | PermanentRedirect _ -> 308
        | BadRequest _ -> 400
        | Unauthorized _ -> 401
        | Forbidden -> 403
        | NotFound -> 404
        | NotAcceptable -> 406
        | UnprocessableEntity _ -> 422
        | InternalServerError _ -> 500
    member response.Data =
        let mapMultiStatus statuses =
            let map (multiStatus: MultiStatusResponse) =
                {
                    Href = multiStatus.Resource
                    Status = multiStatus.StatusCode
                    Data = multiStatus.Data |> Option.defaultValue (RawHttpResponse "")
                }
            statuses |> Array.map map |> fun responses -> { Responses = responses } |> box
        match response with
        | OK data -> Some data
        | Created (data,_) -> Some data
        | Accepted -> None
        | NoContent -> None
        | MultiStatus multiStatus -> multiStatus |> mapMultiStatus |> ObjectHttpResponse |> Some
        | TemporaryRedirect _ -> None
        | PermanentRedirect redirect -> redirect |> box |> ObjectHttpResponse |> Some
        | BadRequest error -> error |> box |> ObjectHttpResponse |> Some
        | Unauthorized authInfo -> authInfo |> box |> ObjectHttpResponse |> Some
        | Forbidden -> None
        | NotFound -> None
        | NotAcceptable -> None
        | UnprocessableEntity error -> error |> box |> ObjectHttpResponse |> Some
        | InternalServerError error -> error |> Option.map (box >> ObjectHttpResponse)

and MultiStatusResponse =
    {
        Resource: string
        Response: HttpResponse
    } member this.StatusCode = this.Response.StatusCode
      member this.Data = this.Response.Data

module HttpResponseData =
    open System.Xml.Serialization

    let internal create value = ObjectHttpResponse value
    let createRaw value = RawHttpResponse value

    let serializeError =
        let standardFields = typeof<IHttpError>.GetProperties()
        let camelCase str = CodeIdentifier.MakeCamel(str)
        fun (error: IHttpError) ->
            injected {
                match error with
                | :? JObject as jsonObject ->
                    return! jsonObject |> Serializer.toString
                | _ ->
                    let errorType = error.GetType()
                    let customFields = errorType.GetProperties() |> Array.filter (fun field -> standardFields |> Array.exists (fun f -> f.Name = field.Name) |> not) 
                    let jsonObject = JObject()

                    let setField (field: Reflection.PropertyInfo) =
                        injected {
                            let fieldName = camelCase field.Name
                            let fieldValue = field.GetValue(error)
                            let! token =
                                match field.PropertyType |> convertResponseData fieldValue with
                                | Ok result -> result |> Serializer.toString
                                | Error error -> error |> Serializer.toString
                        
                            jsonObject.[fieldName] <- JToken.Parse token
                        }

                    do! standardFields |> Seq.map setField |> Injected.join |> Injected.ignore
                    do! customFields |> Seq.map setField |> Injected.join |> Injected.ignore

                    return! jsonObject |> Serializer.toString
            }
           
    
    let rec toJson = function
    | ObjectHttpResponse o -> 
        match o with
        | :? IHttpError as error -> 
            error |> serializeError
        | :? HttpMultipleStatuses as statuses ->
            injected {
                let responses = JArray()
                for status in statuses.Responses do 
                    let jsonObject = JObject()
                    jsonObject.Add("href", JToken.FromObject(status.Href))
                    jsonObject.Add("status", JToken.FromObject(status.Status))
                    match status.Data with
                    | :? HttpResponseData as responseData ->
                        match responseData with
                        | ObjectHttpResponse _ ->
                            let! result = Serializer.toString responseData
                            jsonObject.Add("data", JToken.Parse(result))
                        | RawHttpResponse raw ->
                            match raw with
                            | empty when empty |> String.IsNullOrEmpty ->
                                jsonObject.Add("data", JObject())
                            | _ ->
                                jsonObject.Add("data", JToken.Parse(raw))
                    | otherData ->
                        jsonObject.Add("data", JToken.FromObject(otherData))
                    responses.Add(jsonObject)
                return! responses |> Serializer.toString
            }
        | _ -> 
            Serializer.toString o
    | RawHttpResponse json -> 
        Injected.create json

    let value = function
    | ObjectHttpResponse o -> o
    | RawHttpResponse json -> box json


