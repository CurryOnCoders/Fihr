namespace CurryOn.Fihr.Http

open System

type HttpRedirect =
    {
        Location: Uri
        EntityId: int64
    }

type AuthenticationInfo =
    {
        Authenticate: string
    }

type HttpVerb =
    | Get = 0
    | Post = 1
    | Put = 2
    | Patch = 3
    | Delete = 4
    | Options = 5
    | Head = 6

type Location =
    | Body = 0
    | Path = 1
    | Query = 2
    | Header = 3

type ILink = 
    abstract member Href: string
    abstract member Rel: string
    abstract member Method: HttpVerb

[<CLIMutable>]
type Link = 
    {
        Href: string
        Rel: string
        Method: HttpVerb
    } interface ILink with
        member this.Href = this.Href
        member this.Rel = this.Rel
        member this.Method = this.Method

type IErrorDetail =
    abstract member Field: string
    abstract member Value: string
    abstract member Location: Location
    abstract member Issue: string

[<CLIMutable>]
type ErrorDetail = 
    {
        Field: string
        Value: string
        Location: Location
        Issue: string
    } interface IErrorDetail with
        member this.Field = this.Field
        member this.Value = this.Value
        member this.Location = this.Location
        member this.Issue = this.Issue

type IHttpError =
    abstract member ErrorName: string
    abstract member DebugId: string
    abstract member Message: string
    abstract member Links: ILink []
    abstract member Details: IErrorDetail []

[<CLIMutable>]
type HttpError = 
    {
        ErrorName: string
        DebugId: string
        Message: string
        Links: Link list
        Details: ErrorDetail list
    } interface IHttpError with
        member this.ErrorName = this.ErrorName
        member this.DebugId = this.DebugId
        member this.Message = this.Message
        member this.Links = this.Links |> List.map (fun l -> l :> ILink) |> List.toArray
        member this.Details = this.Details |> List.map (fun d -> d :> IErrorDetail) |> List.toArray

[<CLIMutable>]
type HttpMultiStatus =
    {
        Href: string
        Status: int
        Data: obj
    }

[<CLIMutable>]
type HttpMultipleStatuses = 
    {
        Responses: HttpMultiStatus []
    }



