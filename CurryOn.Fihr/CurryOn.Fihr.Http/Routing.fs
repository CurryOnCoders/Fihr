namespace CurryOn.Fihr.Http

open CurryOn
open CurryOn.Fihr
open System
open System.Web

type HttpRouteTemplateSegment =
| Path of string
| UserToken of string
| SystemToken of string

type HttpRouteTemplate =
    {
        Template: string
        Segments: HttpRouteTemplateSegment list
    }

type HttpRouteSegment =
| RoutePath of string
| RouteToken of string * string
| RouteParameter of string * string

module private RouteParameter =
    let convert (clrType: Type) (value: string) =
        try
            if clrType.IsGenericType && clrType.GetGenericTypeDefinition() = typedefof<Nullable<_>> then
                let targetType = clrType.GetGenericArguments().[0]
                let result = Convert.ChangeType(value, targetType)
                Activator.CreateInstance(clrType, [|result|])
            else
                Convert.ChangeType(value, clrType)
            |> Some
        with _ ->
            None

module UrlEncoding =
    let inline encode (s: string) =
        HttpUtility.UrlEncode(s)

    let inline decode (s: string) =
        HttpUtility.UrlDecode(s)

[<AllowNullLiteral>]
type ICacheableAttribute =
    abstract member CacheExpiration: TimeSpan option

type HttpRoute =
    {
        Path: string
        Tokens: Map<CaseInsensitiveString, string>
        Parameters: Map<CaseInsensitiveString, string>
        Query: Map<CaseInsensitiveString, string>
    } member route.GetToken name = 
        route.Tokens |> Map.tryFind (CaseInsensitiveString.create name)
      member route.GetToken (name, clrType: Type) =
        route.GetToken(name) |> Option.bind (RouteParameter.convert clrType)
      member route.GetToken<'a when 'a :> IConvertible> name = 
        route.GetToken(name, typeof<'a>) |> Option.map unbox<'a> 
      member route.GetParameter name = 
        route.Parameters |> Map.tryFind (CaseInsensitiveString.create name) |> Option.map UrlEncoding.decode
      member route.GetParameter (name, clrType: Type) =
        route.GetParameter(name) |> Option.bind (RouteParameter.convert clrType)
      member route.GetParameter<'a when 'a :> IConvertible> name = 
        route.GetParameter(name, typeof<'a>) |> Option.map unbox<'a> 
      member route.GetQueryParameter name = 
        route.Query |> Map.tryFind (CaseInsensitiveString.create name) |> Option.map UrlEncoding.decode
      member route.GetQueryParameter (name, clrType: Type) =
        route.GetQueryParameter(name) |> Option.bind (RouteParameter.convert clrType)
      member route.GetQueryParameter<'a when 'a :> IConvertible> name = 
        route.GetQueryParameter(name, typeof<'a>) |> Option.map unbox<'a> 

[<AllowNullLiteral>]
type IHttpRequestTemplate =
    abstract member Method: HttpMethod
    abstract member RouteTemplate: HttpRouteTemplate

module HttpRouteTemplate =
    let private getToken (segment: string) = 
        segment |> Seq.skip 1 |> Seq.take (segment.Length - 2) |> Seq.toArray |> String

    let parse (template: string) =
        match template.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> s.Trim()) with
        | segments when segments.Length > 0 ->
            let routeSegments = 
                [for segment in segments do 
                    match segment with
                    | Like "{*}" ->
                        yield UserToken (getToken segment)
                    | Like "[*]" ->
                        yield SystemToken (getToken segment)
                    | path -> 
                        yield Path path
                ]

            { Template = template; Segments = routeSegments}
        | _ -> 
            { Template = template; Segments = [] }

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Module)>]
type HttpServiceAttribute (boundedContext: string option) =
    inherit Attribute()
    new () = HttpServiceAttribute(None)
    new (boundedContext: string) = HttpServiceAttribute(Some boundedContext)
    member __.BoundedContext = boundedContext    

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = true)>]
type HttpAttributeBase(method: HttpMethod, route: string, cacheExpiry: TimeSpan option) =
    inherit Attribute()
    let routeTemplate = HttpRouteTemplate.parse route
    new (method, route) = HttpAttributeBase(method, route, None)
    new () = HttpAttributeBase(HttpMethod.Get, "/", None)
    member __.Method = method
    member __.Route = route
    member __.RouteTemplate = routeTemplate
    member __.CacheTime = cacheExpiry
    interface IHttpRequestTemplate with
        member this.Method = this.Method
        member this.RouteTemplate = this.RouteTemplate
    interface ICacheableAttribute with
        member this.CacheExpiration = this.CacheTime

type HttpGetAttribute(route, cacheExpiry) =
    inherit HttpAttributeBase(HttpMethod.Get, route, cacheExpiry)
    new (route, cacheExpiry) = HttpGetAttribute(route, if cacheExpiry = NoCache then None else tryParse<TimeSpan>(cacheExpiry))
    new (route) = HttpGetAttribute(route, None)    
    new () = HttpGetAttribute("/", None)

type HttpPutAttribute(route) =
    inherit HttpAttributeBase(HttpMethod.Put, route)
    new () = HttpPutAttribute("/")

type HttpPostAttribute(route) =
    inherit HttpAttributeBase(HttpMethod.Post, route)
    new () = HttpPostAttribute("/")

type HttpPatchAttribute(route) =
    inherit HttpAttributeBase(HttpMethod.Patch, route)
    new () = HttpPatchAttribute("/")

type HttpDeleteAttribute(route) =
    inherit HttpAttributeBase(HttpMethod.Delete, route)
    new () = HttpDeleteAttribute("/")

type HttpOptionsAttribute(route) =
    inherit HttpAttributeBase(HttpMethod.Options, route)
    new () = HttpOptionsAttribute("/")

type HttpHeadAttribute(route) =
    inherit HttpAttributeBase(HttpMethod.Head, route)
    new () = HttpHeadAttribute("/")

type HttpCustomVerbAttribute(verb, route) =
    inherit HttpAttributeBase(HttpMethod.Custom verb, route)
    new () = HttpCustomVerbAttribute("JSONP", "/")

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Parameter)>]
type HttpBodyAttribute () =
    inherit Attribute()

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Parameter)>]
type HttpRawBodyAttribute () =
    inherit Attribute()

type HttpHeaderAttributeName =
| UseParameterName
| UseGivenName of string

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Parameter)>]
type HttpHeaderAttribute (name: HttpHeaderAttributeName) =
    inherit Attribute()
    new () = HttpHeaderAttribute(UseParameterName)
    new (name: string) = HttpHeaderAttribute(UseGivenName name)
    member __.Name = name

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Parameter)>]
[<AbstractClass>]
type HttpRequestMetadataAttribute () =
    inherit Attribute()

type RequestUrlAttribute () =
    inherit HttpRequestMetadataAttribute()

type RequestIdAttribute () =
    inherit HttpRequestMetadataAttribute()

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Parameter)>]
type ClaimAttribute (name: HttpHeaderAttributeName) =
    inherit Attribute()
    new () = ClaimAttribute(UseParameterName)
    new (claim: string) = ClaimAttribute(UseGivenName claim)
    member __.Claim = name

/// Attribute used to populate a service parameter based on the specific authorized role.
[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Parameter)>]
type AuthorizedRoleAttribute(targetRole: string option) =
    inherit HttpRequestMetadataAttribute()
    new (targetRole: string) =
        AuthorizedRoleAttribute(Option.ofObj targetRole)
    new () = 
        AuthorizedRoleAttribute(None)

    member __.TargetRole = targetRole

[<AllowNullLiteral>]
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property, AllowMultiple = false)>]
type NullipotentAttribute (cacheExpiration) =
    inherit Attribute ()
    static let defaultExpiration = TimeSpan.FromDays(1.0)
    new () = NullipotentAttribute(defaultExpiration)
    new (cacheExpiration: string) =  NullipotentAttribute(tryParse<TimeSpan>(cacheExpiration) |> Option.defaultValue defaultExpiration)
    member __.CacheExpiration = cacheExpiration
    interface ICacheableAttribute with
        member __.CacheExpiration = Some cacheExpiration