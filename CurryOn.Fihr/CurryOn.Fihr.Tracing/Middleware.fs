namespace CurryOn.Fihr.Tracing

open CurryOn
open CurryOn.Tracing
open Microsoft.AspNetCore.Http
open OpenTracing.Propagation

type TracingMiddleware (next: RequestDelegate, tracing: IDistributedTracing) =
    let tracer = tracing.Tracer

    let traceRequest (context: HttpContext) =
        asyncResult {
            let path = context.Request.Path.ToUriComponent()
            let operation = sprintf "%s %s" context.Request.Method path
            let traceEvent = tracer |> TraceEvent.create operation

            use scope =
                try
                    let headers = TextMapExtractAdapter(context.Request.Headers |> Seq.map (fun h -> h.Key, h.Value |> String.join ",") |> Map.ofSeq)            
                    let context = tracer.Extract(BuiltinFormats.HttpHeaders, headers :> ITextMap)                    
                    if context |> isNull then
                        traceEvent |> TraceEvent.start
                    else
                        traceEvent |> TraceEvent.withParentContext context |> TraceEvent.start
                with _ ->
                    traceEvent |> TraceEvent.start

            try
                do! context |> next.Invoke |> Async.AwaitTask
            finally
                scope |> TraceEvent.finish
            
        } |> AsyncResult.toTask :> System.Threading.Tasks.Task

    member __.InvokeAsync (context: HttpContext) =
        if tracing.Enabled
        then context |> traceRequest
        else context |> next.Invoke
        
        

