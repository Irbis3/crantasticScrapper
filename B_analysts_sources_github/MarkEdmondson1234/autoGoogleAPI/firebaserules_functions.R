#' Firebase Rules API
#' Creates and manages rules that determine when a Firebase Rules-enabled service should permit a request.
#' 
#' Auto-generated code by googleAuthR::gar_create_api_skeleton
#'  at 2017-03-05 19:50:31
#' filename: /Users/mark/dev/R/autoGoogleAPI/googlefirebaserulesv1.auto/R/firebaserules_functions.R
#' api_json: api_json
#' 
#' @details 
#' Authentication scopes used are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/firebase
#' \item https://www.googleapis.com/auth/firebase.readonly
#' }
#' 
#' @docType package 
#' @name firebaserules_googleAuthR
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


#' Test `Source` for syntactic and semantic correctness. Issues present in therules, if any, will be returned to the caller with a description, severity,and source location.The test method will typically be executed with a developer provided`Source`, but if regression testing is desired, this method may beexecuted against a `Ruleset` resource name and the `Source` will beretrieved from the persisted `Ruleset`.The following is an example of `Source` that permits users to upload imagesto a bucket bearing their user id and matching the correct metadata:_*Example*_    // Users are allowed to subscribe and unsubscribe to the blog.    service firebase.storage {      match /users/{userId}/images/{imageName} {          allow write: if userId == request.userId              && (imageName.endsWith('.png') || imageName.endsWith('.jpg'))              && resource.mimeType.startsWith('image/')      }    }
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://firebase.google.com/docs/storage/security}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' \item https://www.googleapis.com/auth/firebase
#' \item https://www.googleapis.com/auth/firebase.readonly
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform, https://www.googleapis.com/auth/firebase, https://www.googleapis.com/auth/firebase.readonly)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param TestRulesetRequest The \link{TestRulesetRequest} object to pass to this method
#' @param name Name of the project
#' @importFrom googleAuthR gar_api_generator
#' @family TestRulesetRequest functions
#' @export


projects.test <- function(TestRulesetRequest, name) {
    
    
    url <- sprintf("https://firebaserules.googleapis.com/v1/{+name}:test", name)
    # firebaserules.projects.test
    
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    
    stopifnot(inherits(TestRulesetRequest, "gar_TestRulesetRequest"))
    
    f(the_body = TestRulesetRequest)
    
    
}



