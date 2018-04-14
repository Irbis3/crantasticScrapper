# Some miscellaneous tests:
# ab -n 400 -c 10 http://127.0.0.1:5000/header
# ab -n 400 -c 10 http://127.0.0.1:5000/
# ab -n 400 -c 10 http://127.0.0.1:5000/async
# ab -n 100 -c 1 http://127.0.0.1:5000/sync-error

library(httpuv)
library(promises)

content <- list(
  status = 200L,
  headers = list(
    'Content-Type' = 'text/html'
  ),
  body = "some content here"
)

# app_handle <- startPipeServer("mypipe", 777,
app_handle <- startServer("0.0.0.0", 5000,
  list(
    onHeaders = function(req) {
      if (req$PATH_INFO == "/header") {
        content$body <- "this is a response from onHeaders()\n"
        return(content)

      } else if (req$PATH_INFO == "/header-delay") {
        print(capture.output(print(str(as.list(req)))))
        Sys.sleep(5)
        return(content)

      } else if (req$PATH_INFO == "/header-print") {
        print(capture.output(print(str(as.list(req)))))
        return(content)

      } else if (req$PATH_INFO == "/header-error") {
        stop("Error in app (header)")

      } else {
        return(NULL)
      }
    },
    call = function(req) {      
      if (req$PATH_INFO %in% c("/", "/sync")) {
        return(content)

      } else if (req$PATH_INFO == "/body-error") {
        stop("Error in app (body)")
        
      } else if (req$PATH_INFO == "/sync-delay") {
        Sys.sleep(5)
        return(content)

      } else if (req$PATH_INFO == "/sync-error") {
        stop("Error in app (sync)")
        
      } else if (req$PATH_INFO == "/async") {
        promise(function(resolve, reject) {
          resolve(content)
        })
        
      } else if (req$PATH_INFO == "/async-delay") {
        promise(function(resolve, reject) {
          Sys.sleep(5)
          resolve(content)
        })

      } else if (req$PATH_INFO == "/async-error") {
        promise(function(resolve, reject) {
          stop("Error in app (async)")
        })

      } else {
        stop("Unknown request path:", req$PATH_INFO)
      }
    }
  )
)
