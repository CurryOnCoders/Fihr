namespace CurryOn.Fihr

open CurryOn
open System

[<RequireQualifiedAccess>]
type HttpMethod =
| Get 
| Put
| Post
| Patch
| Delete
| Options
| Head
| Custom of string

[<Struct>] type RequestId = private RequestId of Guid

module RequestId =
    let create () = 
        Guid.NewGuid() |> RequestId

    let tryParse (id: string) =
        tryParse<Guid> id 
        |> Option.map RequestId

    let value (RequestId id) = 
        id |> sprintf "%A"

    let guid (RequestId id) = id
        
type IHttpClientMetadata =
    abstract member Host: string
    abstract member User: string

type IHttpRequest =
    abstract member RequestId: RequestId
    abstract member Headers: Map<CaseInsensitiveString, string>
    abstract member Method: HttpMethod
    abstract member Url: Uri
    abstract member Body: byte []
    abstract member Client: IHttpClientMetadata
    abstract member Server: string
    abstract member Timestamp: DateTime

type IHttpResponse =    
    abstract member RequestId: RequestId
    abstract member Headers: Map<CaseInsensitiveString, string>
    abstract member StatusCode: int
    abstract member Body: byte []
    abstract member Timestamp: DateTime
    abstract member ElapsedTime: TimeSpan

module HttpMethod =
    let parse (httpMethod: string) =
        match httpMethod with
        | Like "GET" -> HttpMethod.Get
        | Like "PUT" -> HttpMethod.Put
        | Like "POST" -> HttpMethod.Post
        | Like "PATCH" -> HttpMethod.Patch
        | Like "DELETE" -> HttpMethod.Delete
        | Like "OPTIONS" -> HttpMethod.Options
        | Like "HEAD" -> HttpMethod.Head
        | custom -> HttpMethod.Custom custom

    let toString = function
    | HttpMethod.Get -> "GET"
    | HttpMethod.Put -> "PUT"
    | HttpMethod.Post -> "POST"
    | HttpMethod.Patch -> "PATCH"
    | HttpMethod.Delete -> "DELETE"
    | HttpMethod.Options -> "OPTIONS"
    | HttpMethod.Head -> "HEAD"
    | HttpMethod.Custom other -> other

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Constants =
    [<Literal>] 
    let TraceContext = "traceparent"

    [<Literal>]
    let NoCache = "no-cache"
