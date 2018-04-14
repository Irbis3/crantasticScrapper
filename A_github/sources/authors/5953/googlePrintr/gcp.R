#' List printers associated with Google account
#'
#' @param name Query printer names
#' @param type Query printer type, see notes
#' @param connection_status Query connection status, see notes
#' @param use_cdd Indicate whether to include printer info
#' @param extra_fields Comma separated list of extra fields to include
#' @importFrom googleAuthR gar_api_generator
#' @note For info about parameters see \url{https://developers.google.com/cloud-print/docs/appInterfaces#search}
#' @export
#' @examples
#' \dontrun{
#' # get all printers
#' all_printers <- gcp_search()
#' } 
#'

gcp_search <- function(name = NULL,
                       type = NULL,
                       connection_status = NULL,
                       use_cdd = NULL,
                       extra_fields = NULL) {
  if (!is.null(type)) {
    type <- match.arg(
      type,
      c(
        "GOOGLE",
        "HP",
        "DOCS",
        "DRIVE",
        "FEDEX",
        "ANDROID_CHROME_SNAPSHOT",
        "IOS_CHROME_SNAPSHOT"
      )
    )
  }
  
  if (!is.null(connection_status)) {
    connection_status <- match.arg(connection_status,
                                   c("ALL", "ONLINE", "UNKNOWN", "OFFLINE", "DORMANT"))
  }
  
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/search",
    data_parse_function = function(x) {
      x$printers
    },
    pars_args = list(
      q = '',
      type = '',
      connection_status = '',
      use_cdd = '',
      extra_fields = ''
    ),
    checkTrailingSlash = FALSE
  )
  
  f(
    pars_arguments = list(
      q = name,
      type = type,
      connection_status = connection_status,
      use_cdd = use_cdd,
      extra_fields = extra_fields
    )
  )
}

#' Get details about printer
#'
#' @param printerid The ID of the printer
#' @param client See notes
#' @inheritParams gcp_search
#' @note Client field lets user augment printer capabilities see \url{https://developers.google.com/cloud-print/docs/appInterfaces#printer}
#' @export
#' @examples
#' \dontrun{
#' # get all printers
#' printers <- gcp_search()
#' # select info about first printer
#' gcp_printer(printer$id[1])
#' }
#'

gcp_printer <-
  function(printerid,
           client = NULL,
           extra_fields = NULL) {
    f <- googleAuthR::gar_api_generator(
      "https://www.google.com/cloudprint/printer",
      data_parse_function = function(x) {
        x$printers
      },
      pars_args = list(
        printerid = '',
        client = '',
        extra_fields = ''
      ),
      checkTrailingSlash = FALSE
    )
    
    f(pars_arguments = list(
      printerid = printerid,
      client = client,
      extra_fields = extra_fields
    ))
  }

#' Accept printer invitation
#'
#' @inheritParams gcp_printer
#' @param accept Boolean indicating whether to accept request from printer
#' @export
#' @note Undocumented API discovered at \url{https://stackoverflow.com/a/36366114/4564432}
#'

gcp_processinvite <- function(printerid, accept = TRUE) {
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/processinvite",
    http_header = "POST",
    data_parse_function = function(x) {
      x$success
    },
    pars_args = list(printerid = '',
                     accept = TRUE),
    checkTrailingSlash = FALSE
  )
  
  f(pars_arguments = list(printerid = printerid,
                          accept = accept))
  
}

#' Submit print job to printer
#'
#' @importFrom jsonlite toJSON unbox
#' @importFrom httr upload_file
#' @inheritParams gcp_printer
#' @param title Title of print job
#' @param ticket Print ticket, see notes
#' @param content Document to print, see notes
#' @param contentType MIME type of document to print
#' @param tag Tags to add to print job
#' @note For info about parameters see \url{https://developers.google.com/cloud-print/docs/appInterfaces#submit}
#' @note If content is a path to a file it will use \code{httr::upload_file} otherwise it well send raw content
#'
#' @export
#' @examples
#' \dontrun{
#' printer <- gcp_search("myPrinter")
#' 
#' gcp_submit(printer$id[1], 
#'            "New Title",
#'            content = 
#'              "<h1>Hello World</h1>",
#'            contentType = "text/html"
#' )
#' }
#'

gcp_submit <- function(printerid,
                       title,
                       ticket = jsonlite::toJSON(list(version = jsonlite::unbox("1.0"), print = c()), auto_unbox = FALSE),
                       content,
                       contentType,
                       tag = NULL) {
  if (file.exists(content)) {
    content <- httr::upload_file(content)
  }
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/submit",
    http_header = "POST",
    data_parse_function = function(x) {
      x$success
    },
    # https://developers.google.com/cloud-print/docs/pythonCode#multipart-form-data
    customConfig = list(
      encode = "multipart"),
    checkTrailingSlash = FALSE
  )
  
  f(
    the_body = list(
      printerid = printerid,
      title = title,
      ticket = ticket,
      content = content,
      contentType = contentType
    )
  )
}

#' Query submitted jobs
#'
#' @inheritParams gcp_printer
#' @param owner Query job owner
#' @param status Query job status
#' @param query Query title and tags
#' @param offset Start search at given offset
#' @param limit Restrict number of jobs returned
#' @param sortorder Choose paramter to sort by
#' @note For info about parameters see \url{https://developers.google.com/cloud-print/docs/appInterfaces#jobs}
#' @export
#'

gcp_jobs <-
  function(printerid = NULL,
           owner = NULL,
           status = NULL,
           query = NULL,
           offset = NULL,
           limit = NULL,
           sortorder = NULL) {
    f <- googleAuthR::gar_api_generator(
      "https://www.google.com/cloudprint/jobs",
      http_header = "GET",
      data_parse_function = function(x) {
        x$jobs
      },
      pars_args = list(
        printerid = '',
        owner = '',
        status = '',
        query = '',
        offset = '',
        limit = '',
        sortorder = ''
      ),
      checkTrailingSlash = FALSE
    )
    
    f(
      pars_arguments = list(
        printerid = printerid,
        owner = owner,
        status = status,
        query = query,
        offset = offset,
        limit = limit,
        sortorder = sortorder
      )
    )
  }

#' Delete job
#'
#' @param jobid ID of job to delete
#' @note For info about parameters see \url{https://developers.google.com/cloud-print/docs/appInterfaces#deletejob}
#' @export
#'

gcp_deletejob <- function(jobid) {
  f <- googleAuthR::gar_api_generator(
    "https://www.google.com/cloudprint/deletejob",
    http_header = "POST",
    data_parse_function = function(x) {
      x$success
    },
    # https://developers.google.com/cloud-print/docs/pythonCode#multipart-form-data
    customConfig = list(
      encode = "multipart"),
    checkTrailingSlash = FALSE
  )
  
  f(the_body = list(jobid = jobid))
}