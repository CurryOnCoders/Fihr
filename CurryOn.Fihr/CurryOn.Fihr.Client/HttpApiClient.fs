namespace CurryOn.Fihr.Client

module HttpApi =
    let get<'dto, 'model, 'validationError, 'clientError> mapError create uri client = 
        client 
        |> HttpClient.get<'dto, 'clientError> uri
        |> HttpClient.map<'dto, 'model, 'validationError, 'clientError> mapError create

    let post<'requestDto, 'requestModel, 'responseDto, 'responseModel, 'validationError, 'clientError> mapError create toDto uri (request: 'requestModel) client =
        http {
            let requestDto = request |> toDto
            return! 
                client 
                |> HttpClient.post<'requestDto, 'responseDto, 'clientError> uri requestDto
                |> HttpClient.map<'responseDto, 'responseModel, 'validationError, 'clientError> mapError create
        }        

    let put<'requestDto, 'requestModel, 'clientError> toDto uri (request: 'requestModel) client =
        http {
            let requestDto = request |> toDto
            return! client |> HttpClient.put<'requestDto, 'clientError> uri requestDto
        }

    let patch<'requestDto, 'requestModel, 'clientError> toDto uri (request: 'requestModel) client =
        http {
            let requestDto = request |> toDto
            return! client |> HttpClient.patch<'requestDto, 'clientError> uri requestDto
        }

    let delete<'error> uri client =
        client |> HttpClient.delete<'error> uri