# We want a URL to which to write the individual provenance objects
# The task callback can write/store the objects wherever it wants.
# 

getProvURL <-
  #
  #
  #
  #
function()
{
  getOption("SessionProvenanceURL", NA)
}
