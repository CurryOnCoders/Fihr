namespace CurryOn.Fihr.Auth.JwtBearer

open CurryOn
open CurryOn.Authorization
open CurryOn.DependencyInjection
open CurryOn.Fihr
open CurryOn.Fihr.Auth
open Microsoft.AspNetCore.Http
open System.Security.Claims

type JwtClientPrincipal =
    {
       Principal: ClaimsPrincipal
    } interface IClientPrincipal with
        member this.ClientId = this.Principal.Identity.Name

[<AbstractClass>]
type JwtHttpContextBase internal () =
    let contextAccessor = Container.instanceOf<IHttpContextAccessor>() |> Async.RunSynchronously
    
    let getContext () =
        match contextAccessor with
        | Some accessor ->
            Ok accessor.HttpContext
        | None ->
            Error NoAuthorizationHandlerFound

    member __.GetContext () =
        getContext()

    member __.GetUser() =
        getContext() |> Result.map (fun context -> context.User)

type JwtIdentityProvider () =
    inherit JwtHttpContextBase()
    let config = lazy(
        Container.instanceOf<ICustomJwtTokenConfiguration>()
        |> Async.RunSynchronously
        |> Option.orElseWith (fun _ -> 
            Container.instanceOf<IJwtTokenValidationConfiguration>()
            |> Async.RunSynchronously
            |> Option.map (fun c -> {new ICustomJwtTokenConfiguration with
                                        member __.Issuer = c.Issuer
                                        member __.Audience = c.Audience
                                        member __.PublicKey = c.PublicKey
                                        member __.CustomHeader = "Authorization"
                                    }))
        )

    member this.GetClaimsPrincipal (request: IHttpRequest) =
        match this.GetUser() with
        | Ok user when user.Identity.IsAuthenticated ->
            Ok { Principal = user }
        | _ ->
            !config 
            |> Result.ofOption NoAuthenticationProviderFound
            |> Result.bind (fun customConfig -> 
                request.Headers 
                |> Map.tryFind (CaseInsensitiveString.create customConfig.CustomHeader)
                |> Result.ofOption NoAuthorizationHeader
                |> Result.bind (fun header -> header |> JwtToken.validateToken customConfig.Issuer customConfig.Audience customConfig.PublicKey)
                |> Result.map (fun token -> { Principal = token.Principal }))
                 
    member this.GetClaims (claim:string) (request: IHttpRequest) =
        match this.GetClaimsPrincipal(request) with
        | Ok jwt ->
            jwt.Principal.FindAll(claim) |> Seq.map(fun c -> c.Value)
        | _ ->
            Seq.empty<string>

    interface IIdentityProvider with
        member this.IsLoggedIn (request) =
            this.GetClaimsPrincipal(request)
            |> Result.toOption
            |> Option.map (fun principal -> principal.Principal.Identity.IsAuthenticated)
            |> Option.defaultValue false

        member this.GetClientPrincipal (request) =
            this.GetClaimsPrincipal(request)
            |> Result.map (fun principal -> principal :> IClientPrincipal)

    interface IClaimProvider with
        member this.GetClaims claim request =
            [ for value in (this.GetClaims claim request) ->
                { new IClaim with
                    member __.Type = claim
                    member __.Value = value
                }
            ]

type JwtAuthorizationHandler () =    
    inherit JwtHttpContextBase()

    let getJwtRoles (principal: IClientPrincipal) =
        match principal with
        | :? JwtClientPrincipal as jwt ->
            let jwtRoles = 
                jwt.Principal.Claims 
                |> Seq.filter (fun claim -> claim.Type = ClaimTypes.Role) 
                |> Seq.map(fun claim -> claim.Value)
            Ok jwtRoles
        | _ ->
            Error (UnableToVerifyClient "Incorrect Principal Type")

    member __.CheckAuthorizedRole (allowed: RoleList) (principal: IClientPrincipal) =
        asyncResult {
            let! jwtRoles = getJwtRoles principal
            return allowed.Roles |> Seq.tryFind (fun role -> jwtRoles |> Seq.exists ((=) role))
        }

    member __.HasRole role (principal: IClientPrincipal) =
        asyncResult {
            let! jwtRoles = getJwtRoles principal
            return jwtRoles |> Seq.exists ((=) role)
        }

    interface IAuthorizationHandler with
        member __.UseHostAuthorization = true

        member this.CheckAuthorizedRole roles principal = 
            this.CheckAuthorizedRole roles principal

        member this.HasRole role principal =
            this.HasRole role principal
