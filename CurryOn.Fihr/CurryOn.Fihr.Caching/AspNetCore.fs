namespace CurryOn.Fihr.Caching

open Microsoft.AspNetCore.Builder

open System.Runtime.CompilerServices
 
module CurryOn =
    let useCaching (app: IApplicationBuilder) =
        app.UseMiddleware<CachingMiddleware>()

[<Extension>]
type AspNetExtensions =
    [<Extension>]
    static member UseCurryOnCaching (app: IApplicationBuilder) = 
        CurryOn.useCaching app