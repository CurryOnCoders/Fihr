namespace CurryOn.Fihr.Http

open CurryOn.DependencyInjection
open CurryOn.Mapping
open CurryOn.Serialization
open System

type HttpRequestTemplateError =
| WrongVerb
| WrongNumberOfSegments
| RouteDoesNotMatch
| InvalidUrl

type HttpParameterValidationError =
| MissingRequestBody
| RequestBodyMustBeByteArrayOrString
| ParameterNotFound of string
| ErrorDeserializingRequest of SerializationError
| MissingParameterSerializationDependency of DependencyInjectionError
| NoDtoMappingFunctionFound of Type * Type
| ErrorMappingValidatedObject of MappingError
| ErrorFindingParameterMapping of (Type * Type) * MappingError
| UrlParametersMustNotBeDTOs of Type
| UnsupportedContentType of string
| UnsupportedParameterType of Type
| UnsupportedValidationType of Type
| UnsupportedMetadataParameterType of Type
| ParameterDomainValidationError of string []
| ArrayDeserializationError of HttpParameterValidationError list
| UnhandledErrorInspectingParameterValidationResult of exn * obj

type HttpRequestParameterError =
| MissingRequiredParameters of HttpParameterValidationError list
| InvalidRequestBodyType
| ParameterValidationFailed of HttpParameterValidationError list

type HttpRequestEntryPointError =
| MissingHttpAttribute
| UnsupportedEntryPointType of Type

type HttpRequestContextError = 
| WrongApiVersion of string
| WrongBoundedContext of string

type MatchHttpRequestError =
| HttpRequestTemplateDoesNotMatch of HttpRequestTemplateError
| EntryPointParametersDoNotMatch of HttpRequestParameterError
| WrongRequestContext of HttpRequestContextError
| InvalidEntryPoint of HttpRequestEntryPointError

type HttpRequestError =
| HttpHandlerMethodMustBeStaticOrClassMustHaveDefaultConstructor
| ErrorMatchingAvailableEntryPoints of MatchHttpRequestError
| RuntimeParameterValidationFailed of HttpParameterValidationError list
| NoMatchingEntryPointsFound
| UnsupportedReturnType of Type
| EntryPointTypeNotSupported of Type
| UnresolvedDependencies of DependencyInjectionError
| HttpResponseSerializationError of SerializationError
| InvalidAttributeError of string
| UnhandledExceptionExecutingRequest of exn

