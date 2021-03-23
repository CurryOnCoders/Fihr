namespace CurryOn.Fihr.Http

open CurryOn
open CurryOn.Fihr
open CurryOn.Validation
open FSharp.Reflection
open System
open System.Reflection

type Parameter =
    {
        Name: string
        Type: Type
        Position: int
        DefaultValue: obj option
        IsOptional: bool
        IsBody: bool
        IsHeader: bool
        IsRaw: bool
    } member parameter.IsValidated =
        let underlyingType =
            if parameter.IsOptional && parameter.Type.IsGenericType && parameter.Type.GetGenericTypeDefinition() = typedefof<Option<_>>
            then parameter.Type.GetGenericArguments().[0]
            elif parameter.Type.IsArray
            then parameter.Type.GetElementType()
            else parameter.Type
        underlyingType.GetAttributes<ValidatedAttribute>() |> Array.isEmpty |> not

type HttpMetadataParameter =
    | RequestUrlParameter of Parameter
    | RequestIdParameter of Parameter

type HttpRequestParameter =
    | BodyParameter of Parameter
    | UrlParameter of Parameter
    | HeaderParameter of string * Parameter    
    | MetadataParameter of HttpMetadataParameter
    member this.Parameter =
        match this with
        | BodyParameter parameter -> parameter
        | UrlParameter parameter -> parameter
        | HeaderParameter (_, parameter) -> parameter
        | MetadataParameter metaParameter ->
            match metaParameter with
            | RequestUrlParameter parameter -> parameter
            | RequestIdParameter parameter -> parameter

type IEntryPoint =
    inherit ICustomAttributeProvider
    abstract member Attribute: HttpAttributeBase
    abstract member Caching: ICacheableAttribute option
    abstract member Name: string
    abstract member DeclaringType: Type
    abstract member ReturnType: Type        
    abstract member Parameters: HttpRequestParameter []
    abstract member IsStatic: bool

type private IValidatedParameter =
    abstract member Result: obj option
    abstract member IsValid: bool
    abstract member Errors: string []
    abstract member Warnings: string []

type MethodEntryPoint (methodInfo: MethodInfo, attribute: HttpAttributeBase) =
    static let isOption (parameter: ParameterInfo) =
        parameter.IsOptional ||
        (parameter.ParameterType.IsGenericType &&
            parameter.ParameterType.GetGenericTypeDefinition() = typedefof<Option<_>>)
    static let getParameter index (parameterInfo: ParameterInfo) =
        let defaultValue = 
            if parameterInfo.HasDefaultValue
            then Some parameterInfo.DefaultValue
            else None
        let parameter = {Name = parameterInfo.Name; Type = parameterInfo.ParameterType; Position = index; DefaultValue = defaultValue; IsOptional = isOption parameterInfo; IsBody = false; IsHeader = false; IsRaw = false}
        match parameterInfo with
        | p when p |> Attribute.hasAttribute<HttpRawBodyAttribute> ->
            BodyParameter {parameter with IsBody = true; IsRaw = true}
        | p when p |> Attribute.hasAttribute<HttpBodyAttribute> -> 
            BodyParameter {parameter with IsBody = true}
        | p when p |> Attribute.hasAttribute<HttpHeaderAttribute> ->
            match p.GetAttributes<HttpHeaderAttribute>(true) |> Seq.tryHead with
            | Some attribute -> 
                match attribute.Name with
                | UseParameterName -> HeaderParameter (parameter.Name, {parameter with IsHeader = true})
                | UseGivenName name -> HeaderParameter (name, {parameter with IsHeader = true})
            | None -> 
                HeaderParameter (parameter.Name, parameter)
        | p when p |> Attribute.hasAttribute<HttpRequestMetadataAttribute> ->
            if p |> Attribute.hasAttribute<RequestIdAttribute>
            then MetadataParameter (RequestIdParameter parameter)
            elif p |> Attribute.hasAttribute<RequestUrlAttribute>
            then MetadataParameter (RequestUrlParameter parameter)
            else UrlParameter parameter
        | _ ->
            UrlParameter parameter
    let parameters = 
        methodInfo.GetParameters()
        |> Array.mapi (fun index p -> getParameter index p)
    let caching =
        methodInfo.GetCustomAttributes()
        |> Seq.filter (fun attribute -> attribute.GetType().Implements<ICacheableAttribute>())
        |> Seq.map unbox<ICacheableAttribute>
        |> Seq.filter (fun cache -> cache.CacheExpiration |> Option.isSome)
        |> Seq.tryHead

    member __.Method = methodInfo

    interface IEntryPoint with 
        member __.Attribute = attribute
        member __.Name = methodInfo.Name
        member __.DeclaringType = methodInfo.DeclaringType
        member __.ReturnType = methodInfo.ReturnType
        member __.Caching = caching
        member __.Parameters = parameters
        member __.IsStatic = methodInfo.IsStatic
            
    interface ICustomAttributeProvider with
        member __.GetCustomAttributes i = methodInfo.GetCustomAttributes(i)
        member __.GetCustomAttributes (t, i) = methodInfo.GetCustomAttributes(t, i)
        member __.IsDefined (t, i) = methodInfo.IsDefined(t, i)

type private FunctionEntryPoint (propertyInfo: PropertyInfo) =
    do if propertyInfo.PropertyType |> FSharpType.IsFunction |> not 
        then failwith "PropertyInfo for FunctionEntryPoint must be an F# Function Type"

type MatchingEntryPoint =
    {
        EntryPoint: IEntryPoint
        Request: IHttpRequest
        ParameterResults: (Parameter * Result<obj, HttpParameterValidationError>) list
    }

type NonMatchingEntryPoint<'error> =
    {
        EntryPoint: IEntryPoint
        Error: 'error
    }

type HttpRequestHandlerCandidates =
    {
        AllMatches: MatchingEntryPoint list
    } member this.BestMatch = 
        this.AllMatches 
        |> List.sortByDescending (fun m -> m.ParameterResults |> List.map snd |> List.filter Result.isOk |> List.length)
        |> List.head


type CheckEntryPointResult =
| MatchesRequest of MatchingEntryPoint
| RequestDoesNotMatchRouteTemplate of NonMatchingEntryPoint<HttpRequestTemplateError>
| RequestParametersDoNotMatch of NonMatchingEntryPoint<HttpRequestParameterError>
| RequestDoesNotMatchContext of NonMatchingEntryPoint<HttpRequestContextError>
| EntryPointIsNotValid of NonMatchingEntryPoint<HttpRequestEntryPointError>

type MatchHttpRequestResult =
| FoundMatches of HttpRequestHandlerCandidates
| NoMatchesFound of CheckEntryPointResult list

