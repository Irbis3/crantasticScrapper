# Copyright Duncan Temple Lang 2014
# License: BSD_3_Clause license.
#
# Based on a query from Dirk Eddelbuettel
# and taken from the pushbullet.com API examples
# https://www.pushbullet.com/api

library(RCurl) 
library(RJSONIO) # or json or JSONlite

devices =
function(key = getOption("PushBulletKey", stop("Need an API key for pushbullet")),
         ...,
         u = "https://api.pushbullet.com/api/devices",
         curl = getPushBulletHandle(key))
{
  ans = getURLContent(u, curl = curl, ...)
  fromJSON(ans)
}

getPushBulletHandle =
function(key = getOption("PushBulletKey", stop("Need an API key for pushbullet")),
         curl = getCurlHandle(), ...)
{
  curlSetOpt(userpwd = sprintf("%s:", key),
             httpauth = AUTH_BASIC,
             followlocation = TRUE,
             cainfo = system.file("CurlSSL", "certs.pem", package = "RCurl"),
             ...,
             curl = curl)
  curl
}



push =
function(device, body, title, type = "note",
         key = getOption("PushBulletKey", stop("Need an API key for pushbullet")),
         ...,
         u = "https://api.pushbullet.com/api/pushes",
         curl = getPushBulletHandle(key))
{
  params = list(device_iden = device, type = type, title = title)
  if(type == "file") {
    params$file = fileUpload(body) 
    .opts = list(...)
    ans = postForm(u, .params = params, style = "HTTPPOST", .opts = .opts, curl = curl)
  } else {
    params$body = body
    ans = postForm(u, .params = params, style = "POST", .opts = list(...), curl = curl)
  }
  fromJSON(ans)
}

#curl https://api.pushbullet.com/api/pushes -v -u key:  -d device_iden=u1qSJddxeKwOGuGW  -d type=note  -d title=Title  -d body=Body  -X POST

# curl https://api.pushbullet.com/api/pushes -u API_KEY:  -F device_iden=u1qSJddxeKwOGuGW -F type=file -F file=@pushbullet.R
