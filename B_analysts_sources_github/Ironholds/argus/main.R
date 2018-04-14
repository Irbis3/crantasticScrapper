library(plumber)

router <- plumb("core.R")
router$run(port=8000)
