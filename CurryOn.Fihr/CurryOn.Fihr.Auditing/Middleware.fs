namespace CurryOn.Fihr.Auditing

open CurryOn
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging

type AuditingMiddleware (next: RequestDelegate, auditor: IHttpAuditor, log: ILogger) =
    let agent = auditor |> AuditingAgent.start auditor.NumberOfAgents log

    member __.InvokeAsync (context: HttpContext) =
        asyncResult {
            let! request = agent.PostAndAsyncReply (fun channel -> Request (context, channel))
            do! context |> next.Invoke |> Async.AwaitTask
            return! agent.PostAndAsyncReply (fun channel -> Response (request, context, channel))
        } |> AsyncResult.toTask :> System.Threading.Tasks.Task
        
        

