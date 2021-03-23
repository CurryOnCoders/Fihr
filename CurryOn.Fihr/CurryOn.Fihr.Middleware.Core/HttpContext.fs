namespace CurryOn.Fihr.Middleware

open CurryOn
open CurryOn.Fihr
open CurryOn.Fihr.Http
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Extensions
open Microsoft.Extensions.Primitives
open System
open System.IO
open System.Net

module HttpContext =
    let private getHeaders (headers: IHeaderDictionary) = 
        let getValue (strings: StringValues) = strings |> String.join ","
        [for header in headers -> CaseInsensitiveString.create header.Key, getValue header.Value] |> Map.ofList

    let private checkExisting<'t> (name: string) (context: HttpContext) =
        match context.Items.[name] with
        | null -> None
        | :? 't as stored -> stored |> Async.create |> Some
        | _ -> None

    let toRequest context =
        context 
        |> checkExisting<IHttpRequest> HttpRequest.Name
        |> Option.defaultWith (fun _ -> 
            async {
                let request = context.Request
                let url = UriHelper.GetEncodedUrl request
                let timestamp = DateTime.UtcNow
                let headers = request.Headers |> getHeaders

                let verb = 
                    match request.Method with
                    | Like "GET" -> HttpMethod.Get
                    | Like "PUT" -> HttpMethod.Put
                    | Like "POST" -> HttpMethod.Post
                    | Like "PATCH" -> HttpMethod.Patch
                    | Like "DELETE" -> HttpMethod.Delete
                    | Like "HEAD" -> HttpMethod.Head
                    | Like "OPTIONS" -> HttpMethod.Options
                    | other -> HttpMethod.Custom other

                let requestId =
                    match RequestId.tryParse context.TraceIdentifier with
                    | Some id -> id
                    | None -> RequestId.create()

                let client =
                    {
                        Host = 
                            try Dns.GetHostEntry(context.Connection.RemoteIpAddress).HostName
                            with _ -> context.Connection.RemoteIpAddress.ToString()
                        User =
                            context.User.Identity.Name
                    }

                let! body = 
                    async {
                        use copy = new MemoryStream()
                        do! request.Body.CopyToAsync(copy) |> Async.AwaitTask
                        let body = copy.ToArray()
                        copy.Position <- 0L
                        request.Body <- copy
                        return body
                    }

                let request =
                    {
                        Url = Uri url
                        Method = verb
                        RequestId = requestId
                        Headers = headers
                        Timestamp = timestamp
                        Server = Environment.MachineName
                        Client = client
                        Body = body
                    }

                context.Items.[HttpRequest.Name] <- request

                return request :> IHttpRequest
            })

    let toResponse (request: IHttpRequest) context =
        context 
        |> checkExisting<IHttpResponse> HttpResponse.Name
        |> Option.defaultWith (fun _ -> 
            async {
                let response = context.Response
                let timestamp = DateTime.UtcNow
                let headers = response.Headers |> getHeaders

                let! body = 
                    async {
                        use copy = new MemoryStream()
                        do! response.Body.CopyToAsync(copy) |> Async.AwaitTask
                        let body = copy.ToArray()
                        copy.Position <- 0L
                        response.Body <- copy
                        return body
                    }

                let response =
                    {
                        RequestId = request.RequestId
                        Headers = headers
                        StatusCode = response.StatusCode
                        Body = body
                        Timestamp = timestamp
                        ElapsedTime = timestamp - request.Timestamp
                    }

                context.Items.[HttpResponse.Name] <- response

                return response :> IHttpResponse
            })