function(package, name, qpath, path, github.user, ...){
 
    ## add .Rprofile
    rprofile <- "https://raw.githubusercontent.com/trinker/pax_tweak/master/materials/.Rprofile"
    rprofile <- try(readLines(curl::curl(rprofile)))
    if (!inherits("try-error", rprofile)){
        message("  -> Adding:............  .Rprofile") 
        cat(paste(rprofile, collapse="\n"), file = qpath(".Rprofile."))
    }

    ## Update Readme
    readme_rmd <- suppressWarnings(readLines(qpath("README.Rmd")))
    locrmd <- grep("^## Installation", readme_rmd)
    rdmimg <- sprintf("![](\"tools/%s_logo/r_%s.png\")  \n\n", 
        package, package)
    readme_rmd[locrmd] <- paste0(rdmimg, readme_rmd[locrmd])
    message("  -> Upgrading:.........  README.Rmd") 
    cat(paste(readme_rmd, collapse="\n"), file=qpath("README.Rmd"))

    readme_md <- suppressWarnings(readLines(qpath("README.md")))
    locmd <- grep("^## Installation", readme_md)
    readme_md[locmd] <- paste0(rdmimg, readme_md[locmd])
    cat(paste(readme_md, collapse="\n"), file=qpath("README.md")) 

    readme_md <- suppressWarnings(readLines(qpath("README.md")))
    inds <- 1:(which(!grepl("^\\s*-", readme_md))[1] - 1)
    temp <- gsub("(^[ -]+)(.+)", "\\1", readme_md[inds])
    content <- gsub("^[ -]+", "", readme_md[inds])
    toc <- paste(c("\nTable of Contents\n============\n",
        sprintf("%s[%s](#%s)", temp, content, gsub("[;/?:@&=+$,]", "",
            gsub("\\s", "-", tolower(content)))),
        "\nInstallation\n============\n"),
        collapse = "\n"
    )

    readme_md <- readme_md[(max(inds) + 1):length(readme_md)]

    inst_loc <- which(grepl("^Installation$", readme_md))[1]
    readme_md[inst_loc] <- toc
    readme_md <- readme_md[-c(1 + inst_loc)]

    message("  -> Upgrading:.........  README.md") 
    cat(paste(c(sprintf("%s\n============\n", package), readme_md), 
        collapse = "\n"), file = qpath("README.md"))

    ## Add extra file header for the static docs index that is usually taken from README
    extra_staticdoc <- c(
        "<p><img src=\"https://raw.githubusercontent.com/%s/%s/master/inst/%s_logo/r_%s.png\" width=\"300\"/><br/>", 
        "<p><a href=\"http://%s.github.com/%s_dev\">%s</a> is a...</p>", 
        "<p>Download the development version of %s <a href=\"https://github.com/%s/%s/\">here</a>\n"
    )
    
    
    extstat <- do.call(sprintf, c(list(paste(extra_staticdoc, collapse="\n")), 
        c(github.user, rep(package, 3), github.user, rep(package, 3), github.user, package)))

#    message("    -> Creating:..........  inst/extra_statdoc") 
#    suppressWarnings(dir.create(qpath("inst/extra_statdoc")))
#    message("      -> Adding:............  readme.R") 
#    cat(extstat, file = qpath("inst/extra_statdoc/readme.R"))
    
    ## Function to get url files
    grab_url <- function(x, url) {
        bin <- RCurl::getBinaryURL(url, ssl.verifypeer = FALSE)
        con <- file(x, open = "wb")
        writeBin(bin, con)
        close(con)
    }

    ## grab pptx from net and put in logo directory
    message(sprintf("    -> Creating:..........  tools/%s_logo", package)) 
    suppressWarnings(dir.create(qpath(sprintf("tools/%s_logo", package))))
    message(sprintf("      -> Adding:............  r_%s.pptx", package)) 
    grab_url(qpath(sprintf("tools/%s_logo/r_%s.pptx", package, package)), 
        "https://dl.dropboxusercontent.com/u/61803503/packages/r_PACKAGE.pptx")

#    ## Create static docs directory/files
#    statdocs <- "library(staticdocs)\n\nsd_section(\"\",\n  \"Function for...\",\n  c(\n      \"myfun\"\n  )\n)"
#    message("    -> Creating:..........  inst/staticdocs") 
#    suppressWarnings(dir.create(qpath("inst/staticdocs")))
#    message("      -> Adding:............  index.html") 
#    cat(statdocs, file = qpath("inst/staticdocs/index.R"))

    ## add build
    build <- "https://raw.githubusercontent.com/trinker/pax_tweak/master/materials/build.R"
    build <- try(readLines(curl::curl(build)))
    if (!inherits("try-error", build)){
        message("      -> Adding:............  build.R") 
        cat(paste(build, collapse="\n"), file = qpath("inst/build.R"))
    }

    ## Maintenance upgrade
    main <- "https://raw.githubusercontent.com/trinker/pax_tweak/master/materials/maintenance.R"
    main <- try(readLines(curl::curl(main)))
    if (!inherits("try-error", main)){
        message("      -> Adding:............  maintenance.R") 
        cat(paste(main, collapse="\n"), file = qpath("inst/maintenance.R"))
    }

    ## Update .Rbuildignore
    rbuild <- suppressWarnings(readLines(qpath(".Rbuildignore")))
    message("  -> Updating:..........  .Rbuildignore") 
    cat(paste(unique(c(rbuild, "inst/build.R", sprintf("tools/%s_logo/r_%s.pptx", package, package), 
        "inst/staticdocs", "inst/extra_statdoc", "inst/maintenance.R", "\n")), 
        collapse="\n"), file = qpath(".Rbuildignore."))
 
    ## Add CITATION to inst directory
    citation <- "https://raw.githubusercontent.com/trinker/pax_tweak/master/materials/CITATION"
    citation <- suppressWarnings(try(readLines(curl::curl(citation))))
    yr <- format(Sys.Date(), format="%Y")
    vec <- c(package, package, yr, github.user, package, yr, package, github.user, package)
    message(sprintf("    -> Creating:..........  inst/CITATION", package))
    cat(do.call(sprintf, c(list(paste(citation, collapse = "\n")), vec)), 
        file = qpath("inst/CITATION"))

    message(sprintf("%s has been tweaked!", package))
}
