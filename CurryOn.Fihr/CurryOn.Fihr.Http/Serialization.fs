namespace CurryOn.Fihr.Http

open CurryOn
open CurryOn.DependencyInjection
open CurryOn.Fihr
open CurryOn.Mapping
open CurryOn.Serialization
open CurryOn.Validation
open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System
open System.IO

[<AutoOpen>]
module internal Serialization =
    let (?) (this: 'a) (prop: string) : 'r =
        let p = this.GetType().GetProperty(prop)
        p.GetValue(this, null) :?> 'r

    let private isValidationResult (parameter: Type) (result: Type) =
        result.IsGenericType
            && result.GetGenericTypeDefinition() = typedefof<Result<_,_>>
            && match result.GetGenericArguments() with
               | [| ok; _ |] -> 
                    ok.IsGenericType
                        && ok.GetGenericTypeDefinition() = typedefof<Validated<_,_>>
                        && match ok.GetGenericArguments() with
                           | [| success; _ |] -> success.IsAssignableFrom(parameter)
                           | _ -> false
               | _ -> 
                    false

    let private makeOk, private makeError =        
        let resultType = typedefof<Result<_,_>>.MakeGenericType [| typeof<obj>; typeof<HttpParameterValidationError> |]
        let resultCases = FSharpType.GetUnionCases resultType
        let toResult = unbox<Result<obj, HttpParameterValidationError>>
        (fun value -> FSharpValue.MakeUnion(resultCases.[0], [| value |]) |> toResult),
        (fun (error: HttpParameterValidationError) -> FSharpValue.MakeUnion(resultCases.[1], [| box error |]) |> toResult)

    let private findMapping = memoize (fun (dtoType, parameterType) -> Mapper.find (fun map -> dtoType |> map.SourceType.IsAssignableFrom && map.DestinationType |> isValidationResult parameterType))
    let private getMappingFunction  = memoize (fun (sourceType, destinationType) -> Mapper.getGenericMappingFunction sourceType destinationType)

    let mapDto dtoType parameterType (dtoParameter: obj) =        
        match findMapping (dtoType, parameterType) with
        | Ok mapping ->
            match getMappingFunction (mapping.SourceType, mapping.DestinationType) with
            | Some mapper -> 
                let result = mapper.Invoke(null, [| dtoParameter |])
                try 
                    if result?Tag = 0
                    then result?ResultValue |> makeOk
                    else result?ErrorValue |> ErrorMappingValidatedObject |> makeError
                with ex ->
                    Error <| ErrorDeserializingRequest (UnexpectedSerializationError ex)
            | None -> 
                Error <| NoDtoMappingFunctionFound (dtoType, parameterType)
        | Error mappingError -> 
            Error <| ErrorFindingParameterMapping ((dtoType, parameterType), mappingError)

    let private convertParameter tryGetValue primitiveType parameterType =
        let parseUnion () =
            tryGetValue typeof<string>
            |> Result.bind (fun value ->
                match value |> unbox<string> |> Union.tryParseType parameterType with
                | Some unionValue -> unionValue |> ValidatedResult.success |> box |> Ok
                | None -> Error <| UnsupportedValidationType parameterType)
        if FSharpType.IsUnion(parameterType, true) then 
            match FSharpType.GetUnionCases(parameterType, true) with
            | [| singleCase |] -> 
                if singleCase.HasFields() then
                    match singleCase.GetFields() with
                    | [| field |] -> // TODO: Can we support multiple fields?
                        match primitiveType with
                        | Some t -> 
                           tryGetValue t
                           |> Result.bind (fun parameterValue -> parameterValue |> mapDto t parameterType)
                        | None -> 
                           tryGetValue field.PropertyType
                           |> Result.bind (fun parameterValue -> parameterValue |> mapDto field.PropertyType parameterType)
                    | _ -> 
                        Error <| UnsupportedValidationType parameterType
                else
                    parseUnion ()
            | _ -> 
               parseUnion()
        elif parameterType.IsEnum then            
            match primitiveType with
            | Some t -> 
               tryGetValue t
               |> Result.bind (fun parameterValue -> parameterValue |> mapDto t parameterType)
            | None -> 
               tryGetValue typeof<string>
               |> Result.bind (fun parameterValue -> parameterValue |> mapDto typeof<string> parameterType)
        else 
            Error <| UnsupportedParameterType parameterType

    let checkCollection =
        let collection = typedefof<System.Collections.IEnumerable>
        fun (dataType: Type) -> 
            if dataType.IsArray
            then Some <| dataType.GetElementType()
            elif dataType.IsGenericType && collection.IsAssignableFrom(dataType.GetGenericTypeDefinition())
            then dataType.GetGenericArguments() |> Seq.tryHead
            else None

    let private tryDeserialize whenValidatedDto whenValidated whenUnvalidated (parameterType: Type) =        
        match parameterType.GetAttributes<ValidatedAttribute>(true) with
        | [| validated |] ->
            match validated.DTO with
            | Some dtoType -> whenValidatedDto dtoType                    
            | None -> whenValidated ()
        | _ -> 
            whenUnvalidated ()
                    
    let rec private deserializeBody (parser: Type -> byte [] -> Result<obj, SerializationError>) (parameterType: Type) parameter =
        let deserialize () =
            parameterType |> tryDeserialize 
                (fun dtoType -> parameter |> parser dtoType |> Result.mapError ErrorDeserializingRequest |> mapDto dtoType parameterType) 
                (fun () -> parameterType |> convertParameter (fun fieldType -> parameter |> parser fieldType |> Result.mapError ErrorDeserializingRequest) None)
                (fun () -> parameter |> parser parameterType |> Result.mapError ErrorDeserializingRequest)

        match parameterType |> checkCollection with
        | Some innerType ->
            match innerType.GetAttributes<ValidatedAttribute>(true) with
            | [| validated |] ->
                match validated.DTO with
                | Some dtoType -> 
                    use stream = new MemoryStream(parameter)
                    use reader = new StreamReader(stream)
                    use jsonReader = new JsonTextReader(reader)
                    let jArray = JArray.Load(jsonReader)
                    let elements = jArray |> Seq.map (fun j -> j.ToString()) |> Seq.toArray                   
                    let models = elements |> Array.Parallel.map (Utf8.toBytes >> parser dtoType >> mapDto dtoType innerType)
                    models |> Result.join |> Result.map (Seq.toArray >> box) |> Result.mapError ArrayDeserializationError
                | None -> 
                    // TODO: Refactor this to use a solution based on Convert Parameter and the DTO solution above
                    innerType.MakeArrayType() |> convertParameter (fun fieldType -> try parameter |> parser fieldType |> Ok with ex -> Error <| ErrorDeserializingRequest (UnexpectedSerializationError ex)) None
            | _ -> 
                deserialize()            
        | None ->
            deserialize()

    let deserializeParameter tryGetValue (parameterType: Type) =
        let underlyingType =
            if parameterType.IsGenericType && parameterType.GetGenericTypeDefinition() = typedefof<Option<_>>
            then parameterType.GetGenericArguments().[0]
            else parameterType

        underlyingType |> tryDeserialize
            (fun dtoType -> 
                match Type.GetTypeCode(dtoType) with
                | TypeCode.Object -> Error <| UrlParametersMustNotBeDTOs underlyingType
                | _ -> underlyingType |> convertParameter tryGetValue (Some dtoType))
            (fun () -> underlyingType |> convertParameter tryGetValue None)
            (fun () -> tryGetValue underlyingType)

    let deserialize (parameterType: Type) (request: IHttpRequest) = 
        let deserializer = 
            injected {
                match request.Headers |> Map.tryFind (CaseInsensitiveString.create "Content-Type") with
                | Some contentType ->
                    match contentType with
                    | Like "*/json*" -> 
                        let! jsonSerializer = inject<IJsonSerializer>()
                        return! parameterType |> deserializeBody jsonSerializer.DeserializeAsType |> Ok
                    | Like "*/xml*" -> 
                        let! xmlSerialiezr = inject<IXmlSerializer>()
                        return! parameterType |> deserializeBody xmlSerialiezr.DeserializeAsType |> Ok
                    | other -> 
                        let! serializer = inject<ISerializer>()
                        return! parameterType |> deserializeBody serializer.DeserializeAsType |> Ok
                | None ->
                    let! serializer = inject<ISerializer>()
                    return! parameterType |> deserializeBody serializer.DeserializeAsType |> Ok
            } |> Injected.mapError (SerializerNotFound >> ErrorDeserializingRequest)
            
        
        deserializer |> Reader.map (Result.bind (fun f -> 
            match request.Body with
            | null | [||] -> Error MissingRequestBody
            | bytes -> f bytes))

    let exceptionToError (ex: exn) =
        { DebugId = Guid.NewGuid() |> sprintf "%A"
          ErrorName = ex.GetType().Name
          Message = ex.Message
          Links = []
          Details = []
        } :> IHttpError

    let mappingError error = 
        match error with
        | AutoMapperInitializationFailed ex -> 
            { DebugId = sprintf "%A" <| Guid.NewGuid()
              ErrorName = "AutoMapper Initialization Failed"
              Message = ex.Message
              Links = []
              Details = []
            }
        | ErrorMappingTypes (source, destination, error) ->
            { DebugId = sprintf "%A" <| Guid.NewGuid()
              ErrorName = sprintf "Error Mapping %s to %s" source.FullName destination.FullName
              Message = error.Message
              Links = []
              Details = []
            }
        | NoTypeMappingFound predicate ->
            { DebugId = sprintf "%A" <| Guid.NewGuid()
              ErrorName = sprintf "No Mapping Found to satisfy %A" predicate
              Message = "No Type Mapping was Found that could be used to convert between the required types"
              Links = []
              Details = []
            }
        :> IHttpError

    let tryMap dataType dtoType data =
        match Mapper.getGenericMappingFunction dataType dtoType with
        | Some mappingMethod ->
            try
                let result = mappingMethod.Invoke(null, [| data |])
                if result?Tag = 0
                then result?ResultValue |> Ok // Mapped to DTO successfully
                else result?ErrorValue |> mappingError |> Error // Error when mapping to DTO
            with ex ->
                exceptionToError ex |> Error // Unhandled exception
        | None -> 
            Ok data // The mapper was unable to construct a mapping    

    let identity x = x |> id |> Result.Ok

    let private responseConverter (dataType: Type) =
        let attributes = dataType.GetAttributes<ValidatedAttribute>(true)
        match attributes with
        | [||] -> identity // This is not a validated type, return it as-is
        | validations ->
            match validations |> Array.choose (fun attr -> attr.DTO) |> Array.toList with
            | dtoType :: _ -> tryMap dataType dtoType // Map the validated object to its DTO type
            | [] -> 
                if dataType |> FSharpType.IsUnion then
                    match dataType |> FSharpType.GetUnionCases with
                    | [| singleCase |] ->
                        match singleCase.GetFields() with
                        | [||] -> tryMap dataType typeof<string> // If the union has no fields, map to string
                        | [| field |] -> tryMap dataType field.PropertyType // If the union has one field, map to the type of the field
                        | _ -> identity // If the union has multiple fields, just return it as-is
                    | _ -> 
                        tryMap dataType typeof<string> // If it's not a single-case union, try to map the union value to a string
                else 
                    identity // If this is not a union or a validated type with a DTO, return it as-is

    let rec convertResponseData data (dataType: Type) =
        match dataType |> checkCollection with
        | Some _ ->
            let collection = data |> unbox<System.Collections.IEnumerable>
            seq { for element in collection -> prepareResponseBody element }
            |> Result.join
            |> Result.map (List.toArray >> box)
            |> Result.mapError Seq.head
        | None ->
            data |> responseConverter dataType

    and prepareResponseBody data =
        data.GetType() |> convertResponseData data


