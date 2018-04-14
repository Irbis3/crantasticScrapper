library(magrittr)
library(jsonlite)
library(parallel)
library(logging)
basicConfig()

library(data.table)

companies <- 
  local({
    dst <- "company-info.Rds"
    parse_company <- function(path) {
      dst <- file.path(dirname(path), sub(pattern = "json", replacement = "Rds", basename(path)))
      if (file.exists(dst)) {
        loginfo(sprintf("Loading RDS of %s...", path))
        return(readRDS(dst))
      } 
      loginfo(sprintf("Parsing %s ...", path))
      tmp <- readLines(path)
      tmp2 <- regmatches(tmp, regexec("^(\\d+),(.*)$", tmp))
      tmp3 <-
        lapply(tmp2, `[`, 3) %>%
        lapply(fromJSON)
      tmp4 <-
        lapply(tmp3, `[[`, "董監事名單") %>% 
        sapply(function(df) {
          if (is.null(nrow(df))) {
            ""
          } else {
            paste(df[["姓名"]], collapse = ",")
          }
        })
      loginfo(sprintf("Generating data.frame ..."))
      .id <- sapply(tmp2, `[`, 2)
      .data <- lapply(tmp3, function(obj) {
        is_subcompany <- "總(本)公司統一編號" %in% names(obj)
        if (is_subcompany) {
          name <- obj[["分公司名稱"]]
          parent <- obj[["總(本)公司統一編號"]]
        } else {
          name <- obj[["公司名稱"]]
          parent <- NA
        }
        birthday <- obj[["核准設立日期"]]
        birthday <- if (is.null(birthday)) {
          NA
        } else {
          ISOdate(birthday$year, birthday$month, birthday$day) %>% as.Date
        }
        list(name = paste(name, collapse = ";"), parent = parent, birthday = birthday)
      })
      stopifnot(length(.id) == length(.data))
      .magnate <- tmp4
      stopifnot(length(.id) == length(.magnate))
      retval <- data.frame(stringsAsFactors = FALSE,
                 id = .id,
                 name = sapply(.data, `[[`, "name"),
                 parent = sapply(.data, `[[`, "parent"),
                 birthday = sapply(.data, `[[`, "birthday"),
                 magnate = .magnate)
      class(retval$birthday) <- "Date"
      (sapply(retval, class) == c("character", "character", "character", "Date", "character")) %>%
        all %>% stopifnot
      loginfo(sprintf("Writing Rds..."))
      saveRDS(object = retval, file = dst)
      retval
    }
    if (file.exists(dst)) {
      loginfo(sprintf("Loading company-info..."))
      readRDS(dst)
    } else {
      companies <-
        dir("company-info", "^\\d*.json", full.names = TRUE)
      loginfo(sprintf("Create %d clusters to parse data", length(companies)))
      cl <- makePSOCKcluster(length(companies))
      clusterEvalQ(cl, {
        library(magrittr)
        library(jsonlite)
        library(parallel)
        library(logging)
        basicConfig()
        
        library(data.table)
      })
      companies <- parLapply(cl, companies, parse_company)
      stopCluster(cl)
      loginfo("Combining data.frame...")
      companies <- rbindlist(companies) %>%
        as.data.frame
      loginfo("Writing result...")
      saveRDS(companies, dst)
      companies
    }
  })
# object.size(companies)
