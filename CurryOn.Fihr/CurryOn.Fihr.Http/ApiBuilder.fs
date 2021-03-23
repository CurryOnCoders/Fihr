namespace CurryOn.Fihr.Http

open CurryOn
open CurryOn.Fihr
open CurryOn.Fihr.Http
open CurryOn.DependencyInjection
open System

type HttpApi<'response, 'error> = InjectedAsync<'response, 'error>

type IHttpErrorHandler<'error> =
    abstract member GetHttpResponse: RequestId -> 'error -> HttpResponse

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpApi =
    let run<'response, 'error> (container: IServiceProvider) requestId (api: HttpApi<'response, 'error>) =
        async {
            let! apiResult = 
                api 
                |> DependencyInjection.resolveAsync container
                |> AsyncResult.toAsync

            match apiResult with
            | Ok data ->
                match box data with
                | :? HttpResponse as response ->
                    return response
                | _ ->
                    return Http.ok data
            | Error error ->
                let injectedErrorHandler = injectAsync<IHttpErrorHandler<'error>>()

                let! errorHandlerResult =
                    injectedErrorHandler
                    |> DependencyInjection.resolveAsync container
                    |> AsyncResult.toAsync

                match errorHandlerResult with
                | Ok handler ->
                    return handler.GetHttpResponse requestId error
                | Error e ->
                    let httpError =
                        {
                            DebugId = requestId |> RequestId.value
                            ErrorName = "Dependency Injection Error"
                            Message = sprintf "No Error Handler found for Error Type %s:  %A. \r\n Root Cause: %A" (error.GetType().Name) e error
                            Details = []
                            Links = []
                        } :> IHttpError
                    return Http.internalServerError (Some httpError)
        }

type HttpApiBuilder () =
    member __.Return value : HttpApi<'a, 'b> = 
        InjectedAsync.create value

    member __.ReturnFrom(api : HttpApi<'a, 'b>) : HttpApi<'a, 'b> = 
        api

    member __.ReturnFrom(a : Async<'a>) : HttpApi<'a, 'b> = 
        a |> Async.map Ok |> AsyncResult |> Reader.create

    member __.ReturnFrom(r: AsyncResult<'a, 'b>) : HttpApi<'a, 'b> =
        r |> Reader.create

    member __.ReturnFrom(r: Result<'a, 'b>) : HttpApi<'a, 'b> =
        r |> Async.create |> AsyncResult |> Reader.create

    member __.ReturnFrom(i: Injected<'a, 'b>) : HttpApi<'a, 'b> =
        injectedAsync {
            let! injectedValue = i
            return injectedValue
        }

    member this.Zero() : HttpApi<unit, 'b> = 
        this.Return()

    member __.Delay(generator : unit -> HttpApi<'a, 'b>) : HttpApi<'a, 'b> = 
        generator ()
    
    member __.Bind(api : HttpApi<'a, 'c>, binder : 'a -> HttpApi<'b, 'c>) : HttpApi<'b, 'c> = 
        InjectedAsync.bind binder api

    member __.Bind(injection : Injected<'a, 'c>, binder : 'a -> HttpApi<'b, 'c>) : HttpApi<'b, 'c> = 
        injection |> InjectedAsync.bindInjected binder
    
    member __.Bind(result : AsyncResult<'a, 'c>, binder : 'a -> HttpApi<'b, 'c>) : HttpApi<'b, 'c> = 
        InjectedAsync.bindAsyncResult binder result

    member __.Bind(result : Result<'a, 'c>, binder : 'a -> HttpApi<'b, 'c>) : HttpApi<'b, 'c> = 
        InjectedAsync.bindResult binder result
    
    member __.Bind(async : Async<'a>, binder : 'a -> HttpApi<'b, 'c>) : HttpApi<'b, 'c> = 
        InjectedAsync.bindAsyncResult binder (async |> Async.map Ok |> AsyncResult)

    member __.Combine (a, b) : HttpApi<_,_> =
        a |> InjectedAsync.bind (fun () -> b)

    member this.TryWith(body: unit -> HttpApi<'a, 'b>, catchHandler : exn -> HttpApi<'a, 'b>) : HttpApi<'a, 'b> = 
        try
            this.ReturnFrom(body ())
        with ex ->
            catchHandler ex

    member this.TryFinally(body: unit -> HttpApi<_,_>, compensation) : HttpApi<_,_> =
        try 
            this.ReturnFrom(body ())
        finally 
            compensation()

    member this.Using(resource : 'T when 'T :> System.IDisposable, binder : 'T -> HttpApi<'a, 'e>) : HttpApi<'a, 'e> = 
        let body' = fun () -> binder resource
        this.TryFinally(body', fun () -> 
            if resource |> isNotNull
            then resource.Dispose())

    member this.While (guard, body: unit -> HttpApi<_,_>) : HttpApi<_,_> =
        if not (guard()) then 
            this.Zero()
        else
            this.Bind(body(), fun () -> this.While(guard, body))

    member this.For (sequence: seq<_>, body) : HttpApi<_,_> =
        this.Using(sequence.GetEnumerator(), fun enum ->
            this.While(enum.MoveNext, fun () -> body enum.Current))


[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HttpApiBuilder =
    let api = HttpApiBuilder()