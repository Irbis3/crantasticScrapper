#' Cloud Source Repositories API
#' Access source code repositories hosted by Google.
#' 
#' Auto-generated code by googleAuthR::gar_create_api_skeleton
#'  at 2017-03-05 20:14:45
#' filename: /Users/mark/dev/R/autoGoogleAPI/googlesourcerepov1.auto/R/sourcerepo_functions.R
#' api_json: api_json
#' 
#' @details 
#' Authentication scopes used are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' @docType package 
#' @name sourcerepo_googleAuthR
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

#' Gets the access control policy for a resource.Returns an empty policy if the resource exists and does not have a policyset.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/eap/cloud-repositories/cloud-sourcerepo-api}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param resource REQUIRED: The resource for which the policy is being requested
#' @importFrom googleAuthR gar_api_generator
#' @export
projects.repos.getIamPolicy <- function(resource) {
    url <- sprintf("https://sourcerepo.googleapis.com/v1/{+resource}:getIamPolicy", 
        resource)
    # sourcerepo.projects.repos.getIamPolicy
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()
    
}

#' Returns information about a repo.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/eap/cloud-repositories/cloud-sourcerepo-api}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param name The name of the requested repository
#' @importFrom googleAuthR gar_api_generator
#' @export
projects.repos.get <- function(name) {
    url <- sprintf("https://sourcerepo.googleapis.com/v1/{+name}", name)
    # sourcerepo.projects.repos.get
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()
    
}

#' Returns permissions that a caller has on the specified resource.If the resource does not exist, this will return an empty set ofpermissions, not a NOT_FOUND error.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/eap/cloud-repositories/cloud-sourcerepo-api}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param TestIamPermissionsRequest The \link{TestIamPermissionsRequest} object to pass to this method
#' @param resource REQUIRED: The resource for which the policy detail is being requested
#' @importFrom googleAuthR gar_api_generator
#' @family TestIamPermissionsRequest functions
#' @export
projects.repos.testIamPermissions <- function(TestIamPermissionsRequest, resource) {
    url <- sprintf("https://sourcerepo.googleapis.com/v1/{+resource}:testIamPermissions", 
        resource)
    # sourcerepo.projects.repos.testIamPermissions
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(TestIamPermissionsRequest, "gar_TestIamPermissionsRequest"))
    
    f(the_body = TestIamPermissionsRequest)
    
}

#' Deletes a repo.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/eap/cloud-repositories/cloud-sourcerepo-api}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param name The name of the repo to delete
#' @importFrom googleAuthR gar_api_generator
#' @export
projects.repos.delete <- function(name) {
    url <- sprintf("https://sourcerepo.googleapis.com/v1/{+name}", name)
    # sourcerepo.projects.repos.delete
    f <- googleAuthR::gar_api_generator(url, "DELETE", data_parse_function = function(x) x)
    f()
    
}

#' Returns all repos belonging to a project.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/eap/cloud-repositories/cloud-sourcerepo-api}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param name The project ID whose repos should be listed
#' @importFrom googleAuthR gar_api_generator
#' @export
projects.repos.list <- function(name) {
    url <- sprintf("https://sourcerepo.googleapis.com/v1/{+name}/repos", name)
    # sourcerepo.projects.repos.list
    f <- googleAuthR::gar_api_generator(url, "GET", data_parse_function = function(x) x)
    f()
    
}

#' Creates a repo in the given project with the given name..If the named repository already exists, `CreateRepo` returns`ALREADY_EXISTS`.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/eap/cloud-repositories/cloud-sourcerepo-api}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param Repo The \link{Repo} object to pass to this method
#' @param parent The project in which to create the repo
#' @importFrom googleAuthR gar_api_generator
#' @family Repo functions
#' @export
projects.repos.create <- function(Repo, parent) {
    url <- sprintf("https://sourcerepo.googleapis.com/v1/{+parent}/repos", parent)
    # sourcerepo.projects.repos.create
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    stopifnot(inherits(Repo, "gar_Repo"))
    
    f(the_body = Repo)
    
}


#' Sets the access control policy on the specified resource. Replaces anyexisting policy.
#' 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_skeleton}}
#' 
#' @seealso \href{https://cloud.google.com/eap/cloud-repositories/cloud-sourcerepo-api}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/cloud-platform
#' }
#' 
#' Set \code{options(googleAuthR.scopes.selected = c(https://www.googleapis.com/auth/cloud-platform)}
#' Then run \code{googleAuthR::gar_auth()} to authenticate.
#' See \code{\link[googleAuthR]{gar_auth}} for details. 
#' 
#' @param SetIamPolicyRequest The \link{SetIamPolicyRequest} object to pass to this method
#' @param resource REQUIRED: The resource for which the policy is being specified
#' @importFrom googleAuthR gar_api_generator
#' @family SetIamPolicyRequest functions
#' @export


projects.repos.setIamPolicy <- function(SetIamPolicyRequest, resource) {
    
    
    url <- sprintf("https://sourcerepo.googleapis.com/v1/{+resource}:setIamPolicy", 
        resource)
    # sourcerepo.projects.repos.setIamPolicy
    
    f <- googleAuthR::gar_api_generator(url, "POST", data_parse_function = function(x) x)
    
    stopifnot(inherits(SetIamPolicyRequest, "gar_SetIamPolicyRequest"))
    
    f(the_body = SetIamPolicyRequest)
    
    
}



