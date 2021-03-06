#' Google Service Control API
#' Google Service Control provides control plane functionality to managed services, such as logging, monitoring, and status checks.
#' 
#' Auto-generated code by googleAuthR::gar_create_api_skeleton
#'  at 2017-03-05 20:12:13
#' filename: /Users/mark/dev/R/autoGoogleAPI/googleservicecontrolv1.auto/R/servicecontrol_functions.R
#' api_json: api_json
#' 
#' @details 
#' Authentication scopes used are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/servicecontrol
#' \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' @docType package 
#' @name servicecontrol_googleAuthR
#' 
NULL
## NULL

#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))
#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
    x <- Filter(Negate(is.NullOb), x)
    lapply(x, function(x) if (is.list(x)) 
        rmNullObs(x) else x)
}

#' Reports operation results to Google Service Control, such as logs andmetrics. It should be called after an operation is completed.If feasible, the client should aggregate reporting data for up to 5seconds to reduce API traffic. Limiting aggregation to 5 seconds is toreduce data loss during client crashes. Clients should carefully choosethe aggregation time window to avoid data loss risk more than 0.01%for business and compliance reasons.NOTE: the `ReportRequest` has the size limit of 1MB.This method requires the `servicemanagement.services.report` permissionon the specified service. For more information, see[Google Cloud IAM](https://cloud.google.com/iam).
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/service-control/}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/servicecontrol
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/servicecontrol)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param ReportRequest The \link{ReportRequest} object to pass to this method
#' @param serviceName The service name as specified in its service configuration
#' @importFrom googleAuthR gar_api_generator
#' @family ReportRequest functions
#' @export
services.report <- function(ReportRequest, serviceName) {
    url <- sprintf("https://servicecontrol.googleapis.com/v1/services/%s:report", 
        serviceName)
    # servicecontrol.services.report
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(ReportRequest, "gar_ReportRequest"))
    
    f(the_body = ReportRequest)
    
}

#' Attempts to allocate quota for the specified consumer. It should be calledbefore the operation is executed.This method requires the `servicemanagement.services.quota`permission on the specified service. For more information, see[Google Cloud IAM](https://cloud.google.com/iam).
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/service-control/}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/servicecontrol
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/servicecontrol)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param AllocateQuotaRequest The \link{AllocateQuotaRequest} object to pass to this method
#' @param serviceName Name of the service as specified in the service configuration
#' @importFrom googleAuthR gar_api_generator
#' @family AllocateQuotaRequest functions
#' @export
services.allocateQuota <- function(AllocateQuotaRequest, serviceName) {
    url <- sprintf("https://servicecontrol.googleapis.com/v1/services/%s:allocateQuota", 
        serviceName)
    # servicecontrol.services.allocateQuota
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(AllocateQuotaRequest, "gar_AllocateQuotaRequest"))
    
    f(the_body = AllocateQuotaRequest)
    
}

#' Unlike rate quota, allocation quota does not get refilled periodically.So, it is possible that the quota usage as seen by the service differs fromwhat the One Platform considers the usage is. This is expected to happenonly rarely, but over time this can accumulate. Services can invokeStartReconciliation and EndReconciliation to correct this usage drift, asdescribed below:1. Service sends StartReconciliation with a timestamp in future for each   metric that needs to be reconciled. The timestamp being in future allows   to account for in-flight AllocateQuota and ReleaseQuota requests for the   same metric.2. One Platform records this timestamp and starts tracking subsequent   AllocateQuota and ReleaseQuota requests until EndReconciliation is   called.3. At or after the time specified in the StartReconciliation, service   sends EndReconciliation with the usage that needs to be reconciled to.4. One Platform adjusts its own record of usage for that metric to the   value specified in EndReconciliation by taking in to account any   allocation or release between StartReconciliation and EndReconciliation.Signals the quota controller that the service wants to perform a usagereconciliation as specified in the request.This method requires the `servicemanagement.services.quota`permission on the specified service. For more information, see[Google Cloud IAM](https://cloud.google.com/iam).
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/service-control/}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/servicecontrol
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/servicecontrol)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param StartReconciliationRequest The \link{StartReconciliationRequest} object to pass to this method
#' @param serviceName Name of the service as specified in the service configuration
#' @importFrom googleAuthR gar_api_generator
#' @family StartReconciliationRequest functions
#' @export
services.startReconciliation <- function(StartReconciliationRequest, serviceName) {
    url <- sprintf("https://servicecontrol.googleapis.com/v1/services/%s:startReconciliation", 
        serviceName)
    # servicecontrol.services.startReconciliation
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(StartReconciliationRequest, "gar_StartReconciliationRequest"))
    
    f(the_body = StartReconciliationRequest)
    
}

#' Checks an operation with Google Service Control to decide whetherthe given operation should proceed. It should be called before theoperation is executed.If feasible, the client should cache the check results and reuse them for60 seconds. In case of server errors, the client can rely on the cachedresults for longer time.NOTE: the `CheckRequest` has the size limit of 64KB.This method requires the `servicemanagement.services.check` permissionon the specified service. For more information, see[Google Cloud IAM](https://cloud.google.com/iam).
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/service-control/}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/servicecontrol
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/servicecontrol)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param CheckRequest The \link{CheckRequest} object to pass to this method
#' @param serviceName The service name as specified in its service configuration
#' @importFrom googleAuthR gar_api_generator
#' @family CheckRequest functions
#' @export
services.check <- function(CheckRequest, serviceName) {
    url <- sprintf("https://servicecontrol.googleapis.com/v1/services/%s:check", 
        serviceName)
    # servicecontrol.services.check
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(CheckRequest, "gar_CheckRequest"))
    
    f(the_body = CheckRequest)
    
}

#' Releases previously allocated quota done through AllocateQuota method.This method requires the `servicemanagement.services.quota`permission on the specified service. For more information, see[Google Cloud IAM](https://cloud.google.com/iam).
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/service-control/}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/servicecontrol
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/servicecontrol)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param ReleaseQuotaRequest The \link{ReleaseQuotaRequest} object to pass to this method
#' @param serviceName Name of the service as specified in the service configuration
#' @importFrom googleAuthR gar_api_generator
#' @family ReleaseQuotaRequest functions
#' @export
services.releaseQuota <- function(ReleaseQuotaRequest, serviceName) {
    url <- sprintf("https://servicecontrol.googleapis.com/v1/services/%s:releaseQuota", 
        serviceName)
    # servicecontrol.services.releaseQuota
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(ReleaseQuotaRequest, "gar_ReleaseQuotaRequest"))
    
    f(the_body = ReleaseQuotaRequest)
    
}


#' Signals the quota controller that service ends the ongoing usagereconciliation.This method requires the `servicemanagement.services.quota`permission on the specified service. For more information, see[Google Cloud IAM](https://cloud.google.com/iam).
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/service-control/}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/servicecontrol
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/servicecontrol)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param EndReconciliationRequest The \link{EndReconciliationRequest} object to pass to this method
#' @param serviceName Name of the service as specified in the service configuration
#' @importFrom googleAuthR gar_api_generator
#' @family EndReconciliationRequest functions
#' @export


services.endReconciliation <- function(EndReconciliationRequest, serviceName) {
    
    
    url <- sprintf("https://servicecontrol.googleapis.com/v1/services/%s:endReconciliation", 
        serviceName)
    # servicecontrol.services.endReconciliation
    
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    
    stopifnot(inherits(EndReconciliationRequest, "gar_EndReconciliationRequest"))
    
    f(the_body = EndReconciliationRequest)
    
    
}



