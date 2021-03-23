namespace CurryOn.Fihr.Http

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Fihr
open FSharp.Reflection
open System

module HttpRequest =
    let parseMethod httpMethod =
        HttpMethod.parse httpMethod

    let parseRoute (url: Uri) (template: HttpRouteTemplate) =
        try
            match url.AbsolutePath.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries) with
            | segments when segments.Length = template.Segments.Length ->
                let urlSegments =
                    [for index in 0..(template.Segments.Length - 1) do
                        let segment = segments.[index]
                        let templateSegment = template.Segments.[index]
                        match templateSegment with
                        | Path path -> 
                            yield
                                if segment |> String.like path
                                then Some <| RoutePath segment
                                else None
                        | SystemToken token ->
                            yield Some <| RouteToken (token, segment)
                        | UserToken token ->
                            yield Some <| RouteParameter (token, segment)                            
                    ]
                match urlSegments with
                | all when all |> List.forall Option.isSome ->
                    let getSubset f = 
                        all 
                        |> List.choose (Option.bind f)
                        |> Map.ofList

                    let tokens = getSubset (fun segment ->
                        match segment with
                        | RouteToken (name, value) -> Some (CaseInsensitiveString.create name, value)
                        | _ -> None)

                    let parameters = getSubset (fun segment ->
                        match segment with
                        | RouteParameter (name, value) -> Some (CaseInsensitiveString.create name, value)
                        | _ -> None)

                    let query = 
                        url.Query.Split([| '?'; '&' |], StringSplitOptions.RemoveEmptyEntries)
                        |> Array.choose (fun parameter -> 
                            match parameter.Split([| '=' |], StringSplitOptions.RemoveEmptyEntries) with
                            | flag when flag.Length = 1 -> Some (CaseInsensitiveString.create flag.[0], "true")
                            | keyValue when keyValue.Length = 2 -> Some (CaseInsensitiveString.create keyValue.[0], keyValue.[1])
                            | _ -> None) // Invalid query parameter
                        |> Map.ofArray

                    Ok { Path = url.AbsolutePath; Tokens = tokens; Parameters = parameters; Query = query }
                | _ ->
                    Error RouteDoesNotMatch
            | _ -> 
                Error WrongNumberOfSegments
        with _ ->
            Error InvalidUrl

    let checkTemplate verb url (template: IHttpRequestTemplate) =
        if verb <> template.Method
        then Error WrongVerb
        else parseRoute url template.RouteTemplate

    let mapParameter (request: IHttpRequest) (route: HttpRoute) (httpParameter: HttpRequestParameter) =
        injected {
            match httpParameter with
            | BodyParameter parameter ->
                if parameter.IsRaw then
                    match parameter with
                    | arrayParam when arrayParam.Type = typeof<byte []> ->
                        return! request.Body |> box |> Ok
                    | stringParam when stringParam.Type = typeof<string> -> 
                        return! request.Body |> Utf8.toString |> box |> Ok
                    | _ ->
                        return! Error RequestBodyMustBeByteArrayOrString
                else
                   return! request |> deserialize parameter.Type
            | HeaderParameter (name, parameter) ->                
                return! parameter.Type |> deserializeParameter
                    (fun _ ->
                        let parameterName = CaseInsensitiveString.create name
                        match request.Headers |> Map.tryFind parameterName with
                        | Some parameterValue -> Ok (box parameterValue)
                        | None -> Error <| ParameterNotFound name)
            | MetadataParameter metaParameter ->
                match metaParameter with
                | RequestIdParameter parameter ->
                    match parameter.Type with
                    | t when t = typeof<RequestId> -> return! request.RequestId |> box |> Ok
                    | t when t = typeof<Guid> -> return! request.RequestId |> RequestId.guid |> box |> Ok
                    | t when t = typeof<string> -> return! request.RequestId |> RequestId.value |> box |> Ok
                    | other -> return! Error <| UnsupportedMetadataParameterType other
                | RequestUrlParameter parameter ->
                    match parameter.Type with
                    | t when t = typeof<Uri> -> return! request.Url |> box |> Ok
                    | t when t = typeof<string> -> return! request.Url.AbsoluteUri |> box |> Ok
                    | other -> return! Error <| UnsupportedMetadataParameterType other
            | UrlParameter parameter ->
                return! parameter.Type |> deserializeParameter
                    (fun parameterType ->
                        match route.GetParameter(parameter.Name, parameterType) with
                        | Some routeParameter -> 
                            Ok routeParameter
                        | None ->
                            match route.GetQueryParameter(parameter.Name, parameterType) with
                            | Some queryParameter -> Ok queryParameter
                            | None -> Error <| ParameterNotFound parameter.Name)
        }
        |> fun result -> (httpParameter.Parameter, result)

    let checkVersion (entryPoint: IEntryPoint) (route: HttpRoute) =
        match route.Tokens |> Map.tryFind (CaseInsensitiveString.create "version") with
        | Some version ->
            let supportsVersion = Array.exists (fun (attribute: ApiVersionAttribute) -> attribute.Versions |> Array.exists (String.like version))
            let entryPointAttributes = entryPoint.GetAttributes<ApiVersionAttribute>()
            if entryPointAttributes |> supportsVersion ||
               (entryPointAttributes |> Array.isEmpty && entryPoint.DeclaringType.GetAttributes<ApiVersionAttribute>() |> supportsVersion)
            then Ok route
            else Error <| WrongApiVersion version
        | None ->
            Ok route

    let checkContext (entryPoint: IEntryPoint) (route: HttpRoute) =
        match route.Tokens |> Map.tryFind (CaseInsensitiveString.create "context") with
        | Some context ->
            match entryPoint.DeclaringType.GetAttributes<HttpServiceAttribute>() |> Seq.tryHead with
            | Some service ->
                match service.BoundedContext with
                | Some requiredContext -> 
                    if context |> String.like requiredContext
                    then Ok route
                    else Error <| WrongBoundedContext context
                | None -> 
                    Error <| WrongBoundedContext context
            | None ->
                Ok route
        | None ->
            Ok route

    let checkEntryPoint (request: IHttpRequest) (entryPoint: IEntryPoint) (route: HttpRoute) =
        fun provider ->
            let methodParameters = entryPoint.Parameters |> Array.Parallel.map (mapParameter request route)
            let injectedParameters = 
                methodParameters 
                |> Seq.map (fun (parameter, injectedResult) -> parameter, Injected.run provider injectedResult)
                |> Seq.toList

            let hasAllRequiredParameters =
                injectedParameters |> Seq.forall (fun (parameter, result) -> 
                    match result with
                    | Ok _ -> true
                    | _ -> parameter.IsOptional)

            let hasMatchingBodyParameter = 
                injectedParameters
                |> List.filter (fun (parameter, _) -> parameter.IsBody)
                |> List.map snd
                |> List.forall Result.isOk

            if not hasAllRequiredParameters then
                let validationErrors = injectedParameters |> List.choose (fun (_, value) ->
                    match value with
                    | Ok _ -> None
                    | Error error -> Some error)
                validationErrors |> MissingRequiredParameters |> EntryPointParametersDoNotMatch |> Error
            elif hasMatchingBodyParameter then
                Ok injectedParameters
            else 
                Error (EntryPointParametersDoNotMatch InvalidRequestBodyType)
        |> Reader
        

    let getEntryPoints =
        let entryPoints = lazy(
            AppDomain.CurrentDomain.GetAssemblies()
            |> Seq.collect (fun asm -> try asm.GetTypes() with _ -> [||])
            |> Seq.filter Attribute.hasAttribute<HttpServiceAttribute>
            |> Seq.collect (fun clrType -> clrType.GetMethods())
            |> Seq.choose (fun m -> m.GetAttributes<HttpAttributeBase>(true) |> Seq.tryHead |> Option.map (fun a -> m, a))
            |> Seq.map MethodEntryPoint
            |> Seq.cast<IEntryPoint>
            |> Seq.toList)
        fun () -> !entryPoints

    let getMatchingRoutes = memoize <| fun (verb, url) ->
        getEntryPoints()
        |> List.map (fun entryPoint -> 
            entryPoint,
            checkTemplate verb url entryPoint.Attribute
            |> Result.mapError HttpRequestTemplateDoesNotMatch
            |> Result.bind (checkContext entryPoint >> Result.mapError WrongRequestContext)
            |> Result.bind (checkVersion entryPoint >> Result.mapError WrongRequestContext))

    let checkEntryPoints (request: IHttpRequest) : Injected<CheckEntryPointResult, DependencyInjectionError> list =
        getMatchingRoutes (request.Method, request.Url)
        |> List.map (fun (entryPoint, routeResult) -> entryPoint, routeResult |> Injected.bindResult (checkEntryPoint request entryPoint))
        |> List.map (fun (entryPoint, injectedResult) ->
            fun provider ->
                let result = injectedResult |> Reader.run provider
                match result with
                | Ok parameters -> MatchesRequest {EntryPoint = entryPoint; Request = request; ParameterResults = parameters}
                | Error (HttpRequestTemplateDoesNotMatch error) -> RequestDoesNotMatchRouteTemplate {EntryPoint = entryPoint; Error = error}
                | Error (EntryPointParametersDoNotMatch error) -> RequestParametersDoNotMatch {EntryPoint = entryPoint; Error = error}
                | Error (WrongRequestContext error) -> RequestDoesNotMatchContext {EntryPoint = entryPoint; Error = error}
                | Error (InvalidEntryPoint error) -> EntryPointIsNotValid  {EntryPoint = entryPoint; Error = error}
                |> Ok
            |> Reader)

    let findMatchingEntryPoints (request: IHttpRequest) =
        injected {
            let! results = checkEntryPoints request |> Injected.join
            
            return 
                results
                |> List.choose (fun result -> 
                    match result with 
                    | MatchesRequest entryPoint -> Some (entryPoint.EntryPoint, entryPoint.ParameterResults)
                    | _ -> None) 
                |> List.sortByDescending (snd >> List.map snd >> List.filter Result.isOk >> List.length)
        }

    let tryFindEntryPoint request =
        request |> findMatchingEntryPoints |> Injected.map List.tryHead

    let findEntryPoint request =
        injected {
            let! results = checkEntryPoints request |> Injected.join

            return
                results
                |> List.fold (fun acc cur -> 
                    match acc with
                    | FoundMatches candidates ->
                        match cur with
                        | MatchesRequest entryPoint -> FoundMatches {candidates with AllMatches = entryPoint :: candidates.AllMatches}
                        | _ -> acc
                    | NoMatchesFound nonMatches ->
                        match cur with
                        | MatchesRequest entryPoint -> FoundMatches {AllMatches = [entryPoint]}
                        | nonMatching -> NoMatchesFound (nonMatching :: nonMatches)
                ) (NoMatchesFound [])
        }

    let rec private toValidatedParameter (result: obj) = 
        try 
            let getErrors list =
                [| for error in list -> sprintf "%A" error |]
            match checkCollection <| result.GetType() with
            | Some collectionType ->
                let collection = result |> unbox<System.Collections.IEnumerable>
                let allResults = 
                    [| 
                        for element in collection do 
                            if element?Tag = 0 then  
                                yield Some element?ResultValue
                            else 
                                yield None
                    |]

                let validResults = allResults |> Array.choose id |> Array.map (fun r -> r?Value)


                if validResults.Length = allResults.Length then
                    let typedResults =
                        match validResults |> Array.tryHead with
                        | Some element ->
                            let arrayType = element.GetType()
                            let array = Array.CreateInstance(arrayType, validResults.Length)
                            Array.Copy(validResults, array, validResults.Length)
                            array |> box
                        | None ->
                            validResults |> box
                    {new IValidatedParameter with
                        member __.Result = Some typedResults
                        member __.IsValid = true
                        member __.Errors = [||]
                        member __.Warnings = [||]
                    }
                else
                    let errors = 
                        [| 
                            for element in collection do 
                                if element?Tag = 1 then  
                                    yield element?ErrorValue |> getErrors
                        |] |> Array.collect id
                    {new IValidatedParameter with
                        member __.Result = None
                        member __.IsValid = false
                        member __.Errors = errors
                        member __.Warnings = [||]
                    }
                |> Ok         
            | None ->
                if result?Tag = 0 then  
                    let resultValue = result?ResultValue
                    {new IValidatedParameter with
                        member __.Result = Some resultValue?Value
                        member __.IsValid = true
                        member __.Errors = [||]
                        member __.Warnings = resultValue?Warnings |> getErrors
                    }
                else 
                    {new IValidatedParameter with
                        member __.Result = None
                        member __.IsValid = false
                        member __.Errors = result?ErrorValue |> getErrors
                        member __.Warnings = [||]
                    }
                |> Ok
        with ex ->
            Error <| UnhandledErrorInspectingParameterValidationResult (ex, result)

    let validateParameters (parameters: (Parameter*Result<obj, HttpParameterValidationError>) list) =
        seq {
            for (parameter, result) in parameters do 
                match result with
                | Ok value -> 
                    if parameter.IsValidated then
                        match value |> toValidatedParameter with
                        | Ok validatedParameter ->
                            if validatedParameter.IsValid
                            then yield Ok (parameter, validatedParameter.Result)
                            else yield Error <| ParameterDomainValidationError validatedParameter.Errors
                        | Error error ->
                            yield Error error
                    else yield Ok (parameter, Some value)
                | Error error ->
                    match error with
                    | ParameterNotFound name ->
                        if parameter.IsOptional
                        then yield Ok (parameter, None)
                        else yield Error (ParameterNotFound name)
                    | other -> 
                        yield Error other
        } |> Result.join

    let private evaluateEntryPoint (entryPoint: IEntryPoint) (parameters: (Parameter*Result<obj, HttpParameterValidationError>) list) =
        async {
            match validateParameters parameters with
            | Ok validatedParameters ->
                let callParameters = 
                    [| for (parameter, value) in validatedParameters do
                            if parameter.IsOptional
                            then match value with
                                 | Some v -> 
                                     if parameter.Type |> FSharpType.IsUnion then
                                         let unionCases = FSharpType.GetUnionCases(parameter.Type)
                                         yield FSharpValue.MakeUnion(unionCases.[1], [| v |])
                                     else 
                                         yield v
                                 | None -> 
                                     match parameter.DefaultValue with
                                     | Some value -> yield value
                                     | None -> yield null |> box
                            else match value with
                                 | Some v -> yield v
                                 | None -> failwithf "Missing Requried Parameter %s" parameter.Name
                    |]

                match entryPoint with
                | :? MethodEntryPoint as methodEntryPoint ->
                    try 
                        match methodEntryPoint.Method.Invoke(null, callParameters) with
                        | :? InjectedAsync<HttpResponse, HttpRequestError> as response ->
                            return response |> Ok
                        | other ->
                            return Error <| UnsupportedReturnType (other.GetType())
                    with ex -> 
                        return Error <| UnhandledExceptionExecutingRequest ex
                | _ ->
                    return Error <| EntryPointTypeNotSupported (entryPoint.GetType())
            | Error error ->
                return Error (RuntimeParameterValidationFailed error)
        } |> AsyncResult

    let callEntryPointAsync (entryPoint: IEntryPoint) (request: IHttpRequest) parameters =
        asyncResult {
            let! response = evaluateEntryPoint entryPoint parameters                
            return response               
        }

    let callEntryPoint entryPoint request parameters =
        callEntryPointAsync entryPoint request parameters
        |> AsyncResult.toAsync
        |> Async.RunSynchronously

