namespace CurryOn.Fihr.Http

open CurryOn
open System

[<AttributeUsage(AttributeTargets.Module ||| AttributeTargets.Class ||| AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = true)>]
type ApiVersionAttribute(version: string, deprecated: bool) =
    inherit Attribute()
    let versions = version.Split([| ','; ';' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim())
    new (version) = ApiVersionAttribute(version, false)
    member __.Versions = versions
    member __.Deprecated = deprecated

type ApiVersionMismatch =
    {
        VersionRequired: string
        VersionsHandled: string []
    }

type ApiVersionError =
| UrlIsNotAbsolute
| InvalidUrlFormat of string
| VersionMismatch of ApiVersionMismatch

module ApiVersion =
    let checkVerison (url: Uri) (attribute: ApiVersionAttribute) =
        try
            match url.AbsolutePath.Split('/') with
            | segments when segments.Length > 1 ->
                let versionSegment = segments.[1]
                match attribute.Versions |> Array.tryFind (String.like versionSegment) with
                | Some version -> Ok version
                | None -> Error <| VersionMismatch { VersionRequired = versionSegment; VersionsHandled = attribute.Versions }
            | _ -> 
                Error <| InvalidUrlFormat (sprintf "%O" url)
        with | :? InvalidOperationException ->
            Error UrlIsNotAbsolute

