namespace CurryOn.Fihr.Auth.JwtBearer

open CurryOn.Fihr.Auth
open Microsoft.IdentityModel.Logging
open Microsoft.IdentityModel.Tokens
open System
open System.IdentityModel.Tokens.Jwt
open System.Security.Cryptography
open System.Security.Claims

type IJwtTokenValidationConfiguration =
    abstract member Issuer: string
    abstract member Audience: string
    abstract member PublicKey: string

type ICustomJwtTokenConfiguration =
    inherit IJwtTokenValidationConfiguration
    abstract member CustomHeader: string

type JwtToken =
    {
        Principal: ClaimsPrincipal
        Token: SecurityToken
    }

module JwtToken =
    let internal getPublicKey (pem: string) =
        let publicKey = ReadOnlySpan<byte>(Convert.FromBase64String(pem))
        let rsa = RSA.Create()
        let mutable read = 0
        rsa.ImportSubjectPublicKeyInfo(publicKey, &read)
        new RsaSecurityKey(rsa)

    let internal getValidationParmaeters issuer audience publicKey =
        let signingKey = getPublicKey publicKey
        new TokenValidationParameters(
            ValidateIssuerSigningKey = true,
            IssuerSigningKey = signingKey,
            ValidateIssuer = true,
            ValidIssuer = issuer,
            ValidAudience = audience,
            ValidateLifetime = true,
            ClockSkew = TimeSpan.FromMinutes(5.0)
        )
        
    let validateToken issuer audience publicKey (jwtToken: string) =
        try            
            IdentityModelEventSource.ShowPII <- true
            let tokenValidationParameters = getValidationParmaeters issuer audience publicKey 

            let mutable validatedToken : SecurityToken = null
            let handler = new JwtSecurityTokenHandler()
            let token = 
                if jwtToken.StartsWith("Bearer")
                then jwtToken.Replace("Bearer", "").Trim()
                else jwtToken

            let claimsPrincipal = handler.ValidateToken(token, tokenValidationParameters, &validatedToken)

            if validatedToken |> isNull
            then Error NoTokenFound
            else Ok { Principal = claimsPrincipal; Token = validatedToken }
        with
        | :? SecurityTokenExpiredException ->
            Error TokenExpired
        | :? SecurityTokenInvalidIssuerException ->
            Error TokenIssuerInvalid
        | :? SecurityTokenInvalidAudienceException ->
            Error TokenAudienceInvalid
        | :? SecurityTokenSignatureKeyNotFoundException as ex ->
            Error (TokenSignatureInvalid ex.Message)
        | :? SecurityTokenInvalidSignatureException as ex ->
            Error (TokenSignatureInvalid ex.Message)
        | :? SecurityTokenInvalidSigningKeyException as ex ->
            Error (TokenSignatureInvalid ex.Message)
        | :? SecurityTokenValidationException as ex ->
            Error (TokenInvalid ex.Message)
        | ex ->
            Error (TokenValidationException ex)
