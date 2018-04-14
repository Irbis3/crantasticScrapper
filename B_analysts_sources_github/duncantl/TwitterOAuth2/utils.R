is.directory =
function(name)
   file.exists(name) && file.info(name)[1, 'isdir']


getMaxID =
function(x, tweets = getTweetIDs(x))
{
   if(length(tweets))
     max(tweets)
   else
     -1
}

getTweetIDs =
function(x)
{
   if(length(names(x)) == 0) 
      unlist(lapply(x, getTweetIDs))
   else {
     
      sapply(x$statuses, `[[`, "id")
  }
}


