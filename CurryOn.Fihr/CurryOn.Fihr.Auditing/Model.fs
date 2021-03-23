namespace CurryOn.Fihr.Auditing

open CurryOn
open CurryOn.Fihr
open System

type AuditingError =
| AuditingConfigurationNotFound
| ErrorCollectingAuditData of IHttpRequest * exn
| UnhandledAuditingError of exn

type IHttpAuditor =
    inherit IDisposable
    abstract member Enabled: bool
    abstract member NumberOfAgents: int
    abstract member AuditRequest: IHttpRequest -> AsyncResult<unit, AuditingError> 
    abstract member AuditResponse: IHttpResponse -> AsyncResult<unit, AuditingError>