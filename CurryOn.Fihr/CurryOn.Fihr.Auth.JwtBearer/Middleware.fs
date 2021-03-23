namespace CurryOn.Fihr.Auth.JwtBearer

open CurryOn
open Microsoft.AspNetCore.Http

type JwtBearerTokenMiddleware (next: RequestDelegate) =
    member __.InvokeAsync (context: HttpContext) =
        asyncResult {            
            // TODO: Validate that the request is authorized
            ()
        } |> AsyncResult.toTask :> System.Threading.Tasks.Task