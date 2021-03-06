#' Google Dataflow API
#' Manages Google Cloud Dataflow projects on Google Cloud Platform.
#' 
#' Auto-generated code by googleAuthR::gar_create_api_skeleton
#'  at 2017-03-05 19:41:42
#' filename: /Users/mark/dev/R/autoGoogleAPI/googledataflowv1b3.auto/R/dataflow_functions.R
#' api_json: api_json
#' 
#' @details 
#' Authentication scopes used are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/userinfo.email
#' }
#' 
#' @docType package 
#' @name dataflow_googleAuthR
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


#' Send a worker_message to the service.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/dataflow}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/userinfo.email
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/userinfo.email)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param SendWorkerMessagesRequest The \link{SendWorkerMessagesRequest} object to pass to this method
#' @param projectId The project to send the WorkerMessages to
#' @importFrom googleAuthR gar_api_generator
#' @family SendWorkerMessagesRequest functions
#' @export


projects.workerMessages <- function(SendWorkerMessagesRequest, projectId) {
    
    
    url <- sprintf("https://dataflow.googleapis.com/v1b3/projects/%s/WorkerMessages", 
        projectId)
    # dataflow.projects.workerMessages
    
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    
    stopifnot(inherits(SendWorkerMessagesRequest, "gar_SendWorkerMessagesRequest"))
    
    f(the_body = SendWorkerMessagesRequest)
    
    
}



