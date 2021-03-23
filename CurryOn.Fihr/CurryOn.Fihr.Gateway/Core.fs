namespace CurryOn.Fihr.Gateway

open CurryOn.Fihr.Auditing
open CurryOn.Fihr.Caching
open CurryOn.Fihr.Middleware
open CurryOn.Fihr.Tracing
open Microsoft.AspNetCore.Builder
open System.Runtime.CompilerServices

module Fihr =
    let useApiGateway (app: IApplicationBuilder) =
        app.UseMiddleware<TracingMiddleware>()
           .UseMiddleware<AuditingMiddleware>()
           .UseMiddleware<CachingMiddleware>()
           .UseMiddleware<HttpMiddleware>()


[<Extension>]
type AspNetExtensions =
    [<Extension>]
    static member UseFihrGateway (app: IApplicationBuilder) = 
        Fihr.useApiGateway app