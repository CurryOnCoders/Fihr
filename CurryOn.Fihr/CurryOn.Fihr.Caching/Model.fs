namespace CurryOn.Fihr.Caching

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Fihr
open CurryOn.Serialization
open System
open System.Security.Cryptography

type HttpCacheError =
| DependencyInjectionError of DependencyInjectionError
| SerializationError of SerializationError
| CacheKeyCannotBeEmpty
| KeyGeneratorNotFound of System.Type
| PolicyManagerNotFound of System.Type
| UnexpectedCacheError of exn

[<Struct>]
type CacheKey = private CacheKey of string

type IKeyGenerator<'model> =
    abstract member GetKey : 'model -> Result<CacheKey, HttpCacheError>

type CacheExpirationPolicy =
| AbsoluteExpiration of TimeSpan
| SlidingExpiration of TimeSpan
| NoExpiration

type ICachePolicyManager<'model> =
    abstract member GetPolicy : 'model -> Result<CacheExpirationPolicy, HttpCacheError>

type ICachableHttpRequest =
    inherit IHttpRequest
    abstract member CachePolicy: CacheExpirationPolicy
    abstract member CacheKey: CacheKey

module CacheKey =   
    let create (key: string) =
        if key |> isNullOrEmpty then
            Error CacheKeyCannotBeEmpty
        else
            Ok <| CacheKey key

    let generate<'t> (value: 't) =
        injected {
            let! generator = inject<IKeyGenerator<'t>>() |> Injected.mapError (fun _ -> KeyGeneratorNotFound typeof<'t>)
            return! generator.GetKey value
        } 

    let value (CacheKey key) = key


type HttpRequestCacheKeyGenerator () =
    let filterHeaders = Seq.filter (fun (key: string, _) -> key.StartsWith("x-coref-"))
    
    member __.Generate (request: IHttpRequest) =
        let url = request.Url.AbsoluteUri |> String.lowercase
        let headers = request.Headers |> Map.toSeq |> Seq.map (fun (key, value) -> key |> CaseInsensitiveString.value, value) |> filterHeaders |> Seq.map (fun (key, value) -> sprintf "%s=%s" key value) |> String.join ";"
        let method = request.Method |> Union.getName |> String.lowercase
        use sha = new SHA256CryptoServiceProvider() 
        let requestKey = headers |> Utf8.toBytes |> Array.append request.Body |> sha.ComputeHash |> Base64.toString
        sprintf "%s-%s-%s" method url requestKey |> CacheKey.create

    interface IKeyGenerator<IHttpRequest> with
        member this.GetKey request = 
            this.Generate request