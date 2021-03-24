namespace CurryOn.Fihr.Caching

open CurryOn.DependencyInjection
open CurryOn.Fihr
open CurryOn.Serialization
open Microsoft.Extensions.Caching.Distributed
open System

module Cache =
    let private getCacheEntryOptions policy =
        match policy with
        | SlidingExpiration window ->
             DistributedCacheEntryOptions(SlidingExpiration = Nullable window)
        | AbsoluteExpiration time ->
             DistributedCacheEntryOptions(AbsoluteExpirationRelativeToNow = Nullable time)
        | NoExpiration ->
            DistributedCacheEntryOptions()

    let tryFind<'value> policy (key: CacheKey) =
        injectedAsync {
            let! cache = inject<IDistributedCache>() |> Injected.mapError DependencyInjectionError
            let! serializer = inject<ISerializer>() |> Injected.mapError DependencyInjectionError
            let! cacheData = key |> CacheKey.value |> cache.GetAsync |> Async.AwaitTask
            match cacheData |> Option.ofObj with
            | Some [||] -> 
                return None
            | Some bytes ->
                match policy with
                | SlidingExpiration _ ->
                    do! key |> CacheKey.value |> cache.RefreshAsync |> Async.AwaitTask
                | _ ->
                    ()
                let! value = bytes |> serializer.Deserialize<'value> |> Result.mapError SerializationError
                return Some value
            | None ->
                return None
        }

    let add<'value> policy (key: CacheKey) (value: 'value) =
        injectedAsync {
            let! cache = inject<IDistributedCache>() |> Injected.mapError DependencyInjectionError
            let! serializer = inject<ISerializer>() |> Injected.mapError DependencyInjectionError
            let! data = serializer.Serialize(value) |> Result.mapError SerializationError
            let cacheKey = key |> CacheKey.value
            let options = policy |> getCacheEntryOptions
            return! cache.SetAsync(cacheKey, data, options) |> Async.AwaitTask
        }

    let getOrAdd<'value> policy (key: CacheKey) (f: unit -> Async<'value>) =
        injectedAsync {
            match! key |> tryFind<'value> policy with
            | Some value ->
                return value
            | None ->
                let! value = f ()
                do! value |> add<'value> policy key
                return value
        }

    let getCachableRequest (request: IHttpRequest) =
        injectedAsync {
            let! policyManager = inject<ICachePolicyManager<IHttpRequest>>() |> Injected.mapError (fun _ -> PolicyManagerNotFound typeof<IHttpRequest>)
            let! policy = policyManager.GetPolicy(request)
            let! key = request |> CacheKey.generate
            
            return 
                {new ICachableHttpRequest with
                    member __.CacheKey = key
                    member __.CachePolicy = policy
                    member __.RequestId = request.RequestId
                    member __.Body = request.Body
                    member __.Client = request.Client
                    member __.Principal = request.Principal
                    member __.Headers = request.Headers
                    member __.Method = request.Method
                    member __.Server = request.Server
                    member __.Timestamp = request.Timestamp
                    member __.Url = request.Url
                }
        }