namespace CurryOn.Fihr.Auth

open CurryOn
open CurryOn.Authorization
open CurryOn.Fihr
open Microsoft.Extensions.DependencyInjection

type AuthenticationError =
    | NoAuthenticationProviderFound
    | UserNotAuthenticated
    | NoAuthorizationHeader
    | NoTokenFound
    | TokenExpired
    | TokenIssuerInvalid
    | TokenAudienceInvalid
    | TokenSignatureInvalid of string
    | TokenInvalid of string
    | TokenValidationException of exn
    | UnsupportedIdentityClaimsPrincipal
    | UnhandledHttpAuthenticationError of exn
    member this.Message =
        match this with
        | NoAuthenticationProviderFound -> "No configured authentication provider"
        | UserNotAuthenticated -> "User could not be authenticated"
        | NoAuthorizationHeader -> "No authoriztation header was present in the request"
        | NoTokenFound -> "Bearer error='invalid_token', error_description='The token was not provided'"
        | TokenExpired -> "Bearer error='invalid_token', error_description='The token expired is expired'"
        | TokenIssuerInvalid -> "Bearer error='invalid_token', error_description='The token issuer is invalid'"
        | TokenAudienceInvalid -> "Bearer error='invalid_token', error_description='The token audience is invalid'"
        | TokenSignatureInvalid reason -> sprintf "Bearer error='invalid_token', error_description='The token signature is invalid: %s'" reason
        | TokenInvalid error -> sprintf "Bearer error='invalid_token', error_description='%s'" error
        | TokenValidationException ex -> sprintf "Bearer error='invalid_token', error_description='%s'" ex.Message
        | UnsupportedIdentityClaimsPrincipal -> "Unsupported principal type"
        | UnhandledHttpAuthenticationError ex -> ex.Message


type IIdentityProvider =    
    abstract member IsLoggedIn: IHttpRequest -> bool
    abstract member GetClientPrincipal: IHttpRequest -> Result<IClientPrincipal, AuthenticationError>

type IAuthenticationProvider =
    abstract member Enable: IServiceScope -> unit
    abstract member Configure: IServiceCollection -> unit

type IAuthenticatedUser =
    abstract member UserName: string
    abstract member DisplayName: string
    abstract member EmailAddress: string
    abstract member Identity: System.Security.Principal.IIdentity option
    
type IAuthenticationContext =
    inherit IIdentityProvider
    abstract member GetCurrentUser: unit -> AsyncResult<IAuthenticatedUser, AuthenticationError>