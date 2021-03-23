namespace CurryOn.Fihr.Auditing

open Microsoft.AspNetCore.Builder
open System.Runtime.CompilerServices
 
module CurryOn =
    let useAuditingMiddleware (app: IApplicationBuilder) =
        app.UseMiddleware<AuditingMiddleware>()

[<Extension>]
type AspNetExtensions =
    [<Extension>]
    static member UseCurryOnAuditing (app: IApplicationBuilder) = 
        CurryOn.useAuditingMiddleware app