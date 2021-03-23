namespace CurryOn.Fihr.Client

module HttpClientResponse =
    let create x = HttpOk x 

    let bind f x =
        match x with
        | HttpOk value -> 
            f value
        | HttpRedirect url ->
            HttpRedirect url
        | HttpClientError error ->
            HttpClientError error
        | HttpServerError error ->
            HttpServerError error

    let map f x = x |> bind (f >> create)
    
    let combine<'a> (acc: HttpClientResponse<'a list>) cur =
        acc |> bind (fun values -> cur |> map (fun value -> value :: values))
    
    let join results =
        results |> Seq.fold (combine) (create [])