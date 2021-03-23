namespace CurryOn.Fihr.Client

open CurryOn
open CurryOn.DependencyInjection

module HttpClientCall =
    let create x = 
        HttpOk x |> InjectedAsync.create |> HttpClientCall

    let bind<'a, 'b, 'e> (f: 'a -> HttpClientCall<'b, 'e>) (HttpClientCall x) =
        injectedAsync {
            let! response = x
            match response with
            | HttpOk value -> 
                let (HttpClientCall result) = f value
                return! result
            | HttpRedirect url ->
                return HttpRedirect url
            | HttpClientError error ->
                return HttpClientError error
            | HttpServerError error ->
                return HttpServerError error
        } |> HttpClientCall

    let bindResult f result =
        match result with
        | Ok value -> 
            f value
        | Error error -> 
            Error error |> InjectedAsync.ofResult |> HttpClientCall

    let bindInjectedAsync f injected =
        injected |> InjectedAsync.map HttpOk |> HttpClientCall |> bind f

    let bindAsyncResult f result =
        result |> InjectedAsync.ofAsyncResult |> bindInjectedAsync f

    let bindAsync f asyncValue =
        asyncValue |> InjectedAsync.ofAsync |> bindInjectedAsync f

    let bindInjected f injected =
        injected |> InjectedAsync.ofInjected |> bindInjectedAsync f

    let bindResponse f (response: HttpClientResponse<_>) =
        response |> InjectedAsync.create |> HttpClientCall |> bind f

    let map f x = x |> bind (f >> create)
    
    let combine<'a, 'e> (acc: HttpClientCall<'a list, 'e>) cur =
        acc |> bind (fun values -> cur |> map (fun value -> value :: values))
    
    let join results =
        results |> Seq.fold (combine) (create [])

    let ofAsyncResult (result: AsyncResult<_,_>) : HttpClientCall<_,_> =
        result |> AsyncResult.map HttpOk |> Reader.create |> HttpClientCall

    let ofAsync (result: Async<_>) : HttpClientCall<_,_> =
        result |> Async.map Ok |> AsyncResult |> ofAsyncResult

    let ofInjected (injected: Injected<_,_>) : HttpClientCall<_,_> =
        let future state =
            async {
                let result = injected |> Reader.run state
                return result |> Result.map HttpOk
            } |> AsyncResult
        Reader future |> HttpClientCall

    let ofInjectedAsync (injected: InjectedAsync<_,_>) : HttpClientCall<_,_> =
        let future state =
            async {
                let result = injected |> Reader.run state
                return! result |> AsyncResult.map HttpOk |> AsyncResult.toAsync
            } |> AsyncResult
        Reader future |> HttpClientCall

    let ofResult (result: Result<_,_>) : HttpClientCall<_,_> =
        result |> Async.create |> AsyncResult |> ofAsyncResult

    let ofResponse (response: HttpClientResponse<_>) : HttpClientCall<_,_> =
        response |> InjectedAsync.create |> HttpClientCall

    let value (HttpClientCall x) = x

    let run (container: System.IServiceProvider) (HttpClientCall x) =
        x |> DependencyInjection.resolveAsync container

    let Parallel calls =
        calls 
        |> Seq.map value 
        |> InjectedAsync.Parallel 
        |> InjectedAsync.mapError MultipleHttpClientCallErrors 
        |> InjectedAsync.map HttpClientResponse.join 
        |> HttpClientCall

type HttpClientBuilder () =
    member __.Bind (x, f) = HttpClientCall.bind f x
    member __.Bind (x, f) = HttpClientCall.bindResult f x
    member __.Bind (x, f) = HttpClientCall.bindAsyncResult f x
    member __.Bind (x, f) = HttpClientCall.bindAsync f x
    member __.Bind (x, f) = HttpClientCall.bindInjectedAsync f x
    member __.Bind (x, f) = HttpClientCall.bindInjected f x
    member __.Bind (x, f) = HttpClientCall.bindResponse f x
    member __.Return x = HttpClientCall.create x
    member __.ReturnFrom (x: HttpClientCall<_,_>) : HttpClientCall<_,_> = x
    member __.ReturnFrom (x: AsyncResult<_,_>) : HttpClientCall<_,_> = HttpClientCall.ofAsyncResult x
    member __.ReturnFrom (x: Async<_>) : HttpClientCall<_,_> = HttpClientCall.ofAsync x
    member __.ReturnFrom (x: Result<_,_>) : HttpClientCall<_,_> = HttpClientCall.ofResult x
    member __.ReturnFrom (x: Injected<_,_>) : HttpClientCall<_,_> = HttpClientCall.ofInjected x
    member __.ReturnFrom (x: InjectedAsync<'a,HttpClientCallError<'e>>) : HttpClientCall<'a,'e> = HttpClientCall.ofInjectedAsync x
    member __.ReturnFrom (x: HttpClientResponse<_>) : HttpClientCall<_,_> = HttpClientCall.ofResponse x
    member __.Yield x = HttpClientCall.create x
    member __.YieldFrom x = x
    member __.Zero () = HttpClientCall.create ()
    member __.Delay (f) : HttpClientCall<_,_> = f()
    member __.Combine (a, b) : HttpClientCall<_,_> =
        a |> HttpClientCall.bind (fun () -> b)
    member this.TryFinally<'a,'e>(body: unit -> HttpClientCall<'a,'e>, compensation) : HttpClientCall<'a,'e> =
        try 
            this.ReturnFrom(body())
        finally 
            compensation()
    member this.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> HttpClientCall<_,_>) : HttpClientCall<_,_> = 
        let body' = fun () -> binder resource
        this.TryFinally(body', fun () -> 
            if resource |> isNotNull
            then resource.Dispose())

    member this.While (guard, body: unit -> HttpClientCall<_,_>) : HttpClientCall<_,_> =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) : HttpClientCall<_,_> =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))

[<AutoOpen; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpClientBuidler =
    let http = HttpClientBuilder()