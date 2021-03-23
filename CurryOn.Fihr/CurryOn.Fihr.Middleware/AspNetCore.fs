namespace CurryOn.Fihr.Middleware

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open System.Runtime.CompilerServices
 
module CurryOn =
    let getConfig (env: IHostEnvironment) = 
        ConfigurationBuilder()
            .SetBasePath(env.ContentRootPath)
            .AddJsonFile("appsettings.json", optional = true, reloadOnChange = true)
            .AddJsonFile(sprintf "appsettings.%s.json" env.EnvironmentName, optional = true)
            .AddEnvironmentVariables()
            .Build()

    let useHttpMiddleware (app: IApplicationBuilder) =
        app.UseMiddleware<HttpMiddleware>()

[<Extension>]
type AspNetExtensions =
    [<Extension>]
    static member UseCurryOnHttp (app: IApplicationBuilder) = 
        CurryOn.useHttpMiddleware app