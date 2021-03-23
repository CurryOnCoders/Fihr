namespace CurryOn.Fihr.Http

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Http =
    /// Convert an exception to an HTTP Error
    let propagateException (ex: exn) =
        Serialization.exceptionToError ex

    /// Get the integer status code for an HTTP Response
    let getStatusCode (response: HttpResponse) = response.StatusCode

    let private respond create fOk fError result =
        match result with
        | Ok response -> fOk (create response)
        | Error error -> fError error

    let private serverError = Some >> InternalServerError

    /// Construct an HTTP 200 response with the given object, serialized as JSON
    let ok  data = data |> prepareResponseBody |> respond HttpResponseData.create OK serverError

    /// Construct an HTTP 200 response with the given response raw data
    let okRaw (data: string) = data |> Ok |> respond (HttpResponseData.createRaw) OK serverError
            
    /// Construct an HTTP 201 response with the given data and optional Location header
    let created location = prepareResponseBody >> respond HttpResponseData.create (fun data -> Created (data, location)) serverError

    /// Construct an HTTP 202 response
    let accepted () = Accepted

    /// Construct an HTTP 204 response
    let noContent () = NoContent

    /// Construct an HTTP 207 response
    let multiStatus responses = MultiStatus responses

    /// Construct an HTTP 307 response
    let temporaryRedirect uri = TemporaryRedirect uri

    /// Construct an HTTP 308 response
    let permanentRedirect redirect = PermanentRedirect redirect

    /// Construct an HTTP 400 response
    let badRequest error = BadRequest error

    /// Construct an HTTP 401 response
    let unauthorized authInfo = Unauthorized authInfo

    /// Construct an HTTP 403 response
    let forbidden () = Forbidden

    /// Construct an HTTP 404 response
    let notFound () = NotFound

    /// Construct an HTTP 406 response
    let notAcceptable () = NotAcceptable

    /// Construct an HTTP 422 response
    let unprocessableEntity error = UnprocessableEntity error

    /// Construct an HTTP 500 response
    let internalServerError error = InternalServerError error
