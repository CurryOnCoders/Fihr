namespace CurryOn.Fihr.Caching

open CurryOn
open CurryOn.DependencyInjection
open Microsoft.AspNetCore.Http

type CachingMiddleware (next: RequestDelegate, cacheManager: ICachePolicyManager<HttpContext>) =

    member __.InvokeAsync (context: HttpContext) =
        asyncResult {            
            let! response = 
                injectedAsync {
                    let! policy = cacheManager.GetPolicy context
                    let! key = context.Request |> CacheKey.generate
                    return! 
                        Cache.getOrAdd policy key (fun _ -> async {
                            do! next.Invoke(context) |> Async.AwaitTask
                            return context.Response
                        }) 
                } |> DependencyInjection.resolveAsync context.RequestServices
                
            context.Response.StatusCode <- response.StatusCode
            context.Response.Body <- response.Body
            
            for header in response.Headers do
                context.Response.Headers.[header.Key] <- header.Value

        } |> AsyncResult.toTask :> System.Threading.Tasks.Task
        
        
