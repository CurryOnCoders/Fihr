namespace CurryOn.Fihr.Client

open CurryOn.DependencyInjection
open CurryOn.Serialization
open System
open System.Net.Http

type HttpClientError =
| Unauthorized of Uri
| Forbidden of Uri
| NotFound of Uri
| NotAcceptable of Uri
| BadRequest of HttpResponseMessage
| UnprocessableEntity of HttpResponseMessage
| OtherClientError of (int * HttpResponseMessage)

type HttpClientResponse<'t> =
| HttpOk of 't
| HttpRedirect of Uri
| HttpClientError of HttpClientError
| HttpServerError of HttpResponseMessage

type HttpClientCallError<'e> =
| InvalidRequestUri
| DeserializationError of SerializationError
| DependencyInjectionError of DependencyInjectionError
| ClientProcessingError of 'e
| MultipleHttpClientCallErrors of HttpClientCallError<'e> list
| UnexpectedHttpClientError of exn

type NoClientProcessingError = NoClientProcessingError

type HttpClientCall<'t, 'e> = HttpClientCall of InjectedAsync<HttpClientResponse<'t>, HttpClientCallError<'e>>

type HttpErrorResponse =
    | ClientError of HttpClientError
    | ServerError of HttpResponseMessage
    member this.Response =
        match this with
        | ClientError error ->
            match error with
            | Unauthorized _ -> None
            | Forbidden _ -> None
            | NotFound _ -> None
            | NotAcceptable _ -> None
            | BadRequest response -> Some response
            | UnprocessableEntity response -> Some response
            | OtherClientError (_, response) -> Some response
        | ServerError response ->
            Some response
    member this.Uri =
        match this with
        | ClientError error ->
            match error with
            | Unauthorized url -> url
            | Forbidden url -> url
            | NotFound url -> url
            | NotAcceptable url -> url
            | BadRequest response -> response.RequestMessage.RequestUri
            | UnprocessableEntity response -> response.RequestMessage.RequestUri
            | OtherClientError (_, response) -> response.RequestMessage.RequestUri
        | ServerError response ->
            response.RequestMessage.RequestUri
    member this.StatusCode =
        match this with
        | ClientError error ->
            match error with
            | Unauthorized _ -> 401
            | Forbidden _ -> 403
            | NotFound _ -> 404
            | NotAcceptable _ -> 406
            | BadRequest _ -> 400
            | UnprocessableEntity _ -> 422
            | OtherClientError (status, _) -> status
        | ServerError response ->
            response.StatusCode |> int


[<AutoOpen; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpClientResponse =
    let (|HttpError|_|) (response: HttpClientResponse<_>) =
        match response with
        | HttpClientError error ->
            Some <| ClientError error
        | HttpServerError response ->
            Some <| ServerError response
        | _ -> 
            None