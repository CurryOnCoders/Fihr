namespace CurryOn.Fihr.Tracing

open Microsoft.AspNetCore.Builder
open System.Runtime.CompilerServices
 
module CurryOn =
    let useTracingMiddleware (app: IApplicationBuilder) =
        app.UseMiddleware<TracingMiddleware>()

[<Extension>]
type AspNetExtensions =
    [<Extension>]
    static member UseCurryOnTracing (app: IApplicationBuilder) = 
        CurryOn.useTracingMiddleware app