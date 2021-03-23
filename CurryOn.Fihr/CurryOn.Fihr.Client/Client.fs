namespace CurryOn.Fihr.Client

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Serialization
open CurryOn.Tracing
open CurryOn.Validation

open OpenTracing
open OpenTracing.Propagation

open System
open System.Net
open System.Net.Http

module HttpClient =
    type HttpRequest =
        {
            Message: HttpRequestMessage
            TracingScope: IScope option
        } interface IDisposable with
            member this.Dispose () = 
                this.Message.Dispose()
                this.TracingScope |> Option.iter (fun scope -> scope.Dispose())

    let internal isRedirect (status: HttpStatusCode) =
        let statusCode = status |> int
        statusCode = 301 || (statusCode > 306 && statusCode < 310)

    let Default = id

    let create fSetup handler =
        let client = 
            match handler with
            | Some handler ->
                new HttpClient(handler)
            | None ->
                new HttpClient()

        fSetup client
        client

    let private getRequest method (uri: Uri) content =
        let request = new HttpRequestMessage(method, uri)
        
        match content with
        | Some data ->
            request.Content <- data
        | None ->
            ()

        let tracedRequest = 
            injected {            
                let! tracing = inject<IDistributedTracing>()
                if tracing.Enabled then
                    let tracer = tracing.Tracer
                    let scope = 
                        tracer 
                        |> TraceEvent.create (sprintf "HttpClient.%s" method.Method)
                        |> TraceEvent.start
                        |> TraceEvent.log (sprintf "%s %s" method.Method uri.AbsoluteUri)

                    let headers = 
                        {new ITextMap with
                            member __.Set (key, value) = request.Headers.Add(key, value)
                            member __.GetEnumerator () = request.Headers.GetEnumerator() :> System.Collections.IEnumerator
                            member __.GetEnumerator () : System.Collections.Generic.IEnumerator<_> = 
                                (seq {
                                    for header in request.Headers do
                                        yield System.Collections.Generic.KeyValuePair(header.Key, header.Value |> String.join ",")
                                }).GetEnumerator()
                        }

                    tracer.Inject(scope.Span.Context, BuiltinFormats.HttpHeaders, headers)
                    return { Message = request; TracingScope = Some scope }
                else
                    return { Message = request; TracingScope = None }
            }
        
        tracedRequest |> Injected.orElseWith (fun _ -> { Message = request; TracingScope = None })


    let rec sendRequest<'dto, 'error> (f: Uri -> Injected<HttpRequest,_>) (uri: Uri) (client: HttpClient) : HttpClientCall<'dto, 'error> =
        injectedAsync {
            use! request = f uri  |> Injected.mapError DependencyInjectionError
            let! response = client.SendAsync(request.Message) |> Async.AwaitTask
            try
                match response with
                | success when response.IsSuccessStatusCode ->
                    if typeof<'dto> = typeof<unit> then
                        let dto = () |> unbox<'dto>
                        return HttpOk dto
                    else
                        let! dto = success.Content |> Serializer.parseContent<'dto> |> InjectedAsync.mapError DeserializationError
                        return HttpOk dto
                | error ->
                    match int error.StatusCode with
                    | 302 | 303 | 307 | 308 ->
                        let location = response.Headers.Location
                        return HttpRedirect location
                    | 400 ->
                        return HttpClientError (BadRequest error)
                    | 401 | 403 ->
                        return HttpClientError <| Unauthorized uri
                    | 404 ->
                        return HttpClientError <| NotFound uri
                    | 406 ->
                        return HttpClientError <| NotAcceptable uri
                    | 422 ->
                        return HttpClientError (UnprocessableEntity error)
                    | serverError when serverError >= 500 ->
                        return HttpServerError error
                    | other ->
                        return HttpClientError (OtherClientError (other, error))
            finally
                request.TracingScope |> Option.iter (fun scope -> scope |> TraceEvent.finish)
        } |> HttpClientCall

    let get<'dto, 'error> uri client : HttpClientCall<'dto, 'error> =
        client |> sendRequest<'dto, 'error> (fun url -> getRequest HttpMethod.Get url None) uri

    let post<'request, 'response, 'error> uri (request: 'request) client : HttpClientCall<'response, 'error> =
        injectedAsync {
            let! requestContent = request |> Serializer.toContent |> Injected.mapError DeserializationError
            let (HttpClientCall result) = client |> sendRequest<'response, 'error> (fun url -> getRequest HttpMethod.Post url (Some requestContent)) uri
            return! result
        } |> HttpClientCall

    let put<'request, 'error> uri (request: 'request) client : HttpClientCall<unit, 'error> =
        injectedAsync {
            let! requestContent = request |> Serializer.toContent |> Injected.mapError DeserializationError
            let (HttpClientCall result) = client |> sendRequest<unit, 'error> (fun url -> getRequest HttpMethod.Put url (Some requestContent)) uri
            return! result
        } |> HttpClientCall

    let patch<'request, 'error> uri (request: 'request) client : HttpClientCall<unit, 'error> =
        injectedAsync {
            let! requestContent = request |> Serializer.toContent |> Injected.mapError DeserializationError            
            let (HttpClientCall result) = client |> sendRequest<unit, 'error> (fun url -> getRequest (HttpMethod "PATCH") url (Some requestContent)) uri
            return! result
        } |> HttpClientCall

    let delete<'error> uri client : HttpClientCall<unit, 'error> =
        client |> sendRequest<unit, 'error> (fun url -> getRequest HttpMethod.Delete url None) uri

    let map<'dto, 'model, 'validationError, 'clientError> (mapError: 'validationError list -> 'clientError) (create: 'dto -> ValidatedResult<'model, 'validationError>) ((HttpClientCall call): HttpClientCall<'dto, 'clientError>) =
        injectedAsync {
            let! response = call
            match response with
            | HttpOk dto ->
                let! model = dto |> create |> ValidatedResult.toResult |> Result.mapError mapError |> Result.mapError ClientProcessingError
                return HttpOk model
            | HttpRedirect location ->
                return HttpRedirect location
            | HttpClientError error ->
                return HttpClientError error
            | HttpServerError error ->
                return HttpServerError error
        } |> HttpClientCall

module Url =
    let parse (url: string) =
        match url |> Url.tryParse with
        | Some uri -> Ok uri
        | None -> Error InvalidRequestUri

    let makeRelative (fragment: string) baseUrl =
        match Uri.TryCreate(baseUrl, fragment) with
        | (true, uri) -> Ok uri
        | _ -> Error InvalidRequestUri

    let makeRelativef formatFragment =
        Printf.kprintf makeRelative formatFragment