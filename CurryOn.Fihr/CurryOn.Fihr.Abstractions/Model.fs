namespace CurryOn.Fihr

open CurryOn
open System

type HttpClientMetadata =
    {
        Host: string
        User: string
    } interface IHttpClientMetadata with
        member this.Host = this.Host
        member this.User = this.User

type HttpRequestMessage =
    {
        RequestId: RequestId
        Headers: Map<CaseInsensitiveString, string>
        Method: HttpMethod
        Url: Uri
        Body: byte []
        Client: HttpClientMetadata
        Server: string
        Timestamp: DateTime
    } interface IHttpRequest with
        member this.RequestId = this.RequestId
        member this.Headers = this.Headers
        member this.Method = this.Method
        member this.Url = this.Url
        member this.Body = this.Body
        member this.Client = this.Client :> IHttpClientMetadata
        member this.Server = this.Server
        member this.Timestamp = this.Timestamp        

type HttpResponseMessage =
    {
        RequestId: RequestId
        Headers: Map<CaseInsensitiveString, string>
        StatusCode: int
        Body: byte []
        Timestamp: DateTime
        ElapsedTime: TimeSpan
    } interface IHttpResponse with
        member this.RequestId = this.RequestId
        member this.Headers = this.Headers
        member this.StatusCode = this.StatusCode
        member this.Body = this.Body
        member this.Timestamp = this.Timestamp
        member this.ElapsedTime = this.ElapsedTime
    


