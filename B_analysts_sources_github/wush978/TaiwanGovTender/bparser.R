library(magrittr)
library(httr)
library(XML)
library(stringr)
library(tools)
library(stringi)
library(R.utils)
library(logging)
#library(rlist)
#library(parallel)

set_root <- function(file_name){
    switch(tools::file_ext(file_name),
           "gz" = {
                dstname <- tools::file_path_sans_ext(file_name)
                dsttmp <- sprintf("%stmp", dstname)
                if (!file.exists(dstname)) {
                    gunzip(file_name, destname = dsttmp, remove = FALSE, overwrite = TRUE)
                    file.rename(dsttmp, dstname)
                }
           },
           "html" = {
              dstname <- file_name
           },
           "htmltmp" = {
              dstname <- file_name
           },
           stop("Unknown file extension"))
    htmlTreeParse(dstname) %>% xmlRoot
}

node_value_list <- function(root, path){
    getNodeSet(root, path)%>%
        lapply(xmlValue) %>% 
        lapply(convert_empty_str) %>%
        lapply(remove_space) %>%
        Filter(f = function(x) !grepl(pattern = "紅色字體", x, fixed = TRUE))
}

get_prefix <- function(root, path, indx){
    tmp <- getNodeSet(root, path) %>%
        lapply(xmlValue) %>% 
        lapply(convert_empty_str) %>%
        lapply(remove_space) %>%
        Filter(f = function(x) !grepl(pattern = "紅色字體", x, fixed = TRUE))
    
    tmp[[indx]]
}

convert_empty_str <- function(instr){
    outstr <- if(identical(instr, character(0))){ "" }else{ instr }
}

remove_space <- function(instr){
    stri_replace_all_charclass(instr, "\\p{WHITE_SPACE}", "")
}

remove_html <- function(file_name, rm_html){
    if(rm_html) {
        file_path_sans_ext(file_name) %>%
        file.remove
    }
}

remove_by_name <- function(inlist, pattern){
    inlist[!grepl(pattern = pattern, names(inlist))]  
}

pattern_match <- function(instr, pattern_list){
    TRUE %in% lapply(pattern_list, grepl, x=instr, fixed = TRUE)
}

get_subtable <- function(root, path1, path2, prefix, add_prefix = FALSE, shift = 0, check_tender = FALSE, keep_all = FALSE, pattern_list=NULL){
    # get XMLnode set 
    key <- node_value_list(root, path1)
    value <- node_value_list(root, path2)


    subtable <- list()
    tender_tag <- ""
    prefix_tag <- if(add_prefix) paste0(prefix, ".")  else ""
    # retrive all tuples of subtable
    for(i in c(1:length(key))){
        if(keep_all || pattern_match(key[i], pattern_list)){

            attr_name <- paste0(prefix_tag, key[i]) 
            if(check_tender)
                if(grepl(pattern = "投標廠商[0-9]+", key[i]))
                    tender_tag <- paste0(key[i], ".")
                else
                    attr_name <- paste0(prefix_tag, tender_tag, key[i])

            subtable[attr_name] <- value[i+shift]
        }
    }
    subtable
}

get_subtable2 <- function(root, path1) {
    dum.fun <- function(x) { 
        if (xmlName(x)=="br") { "<br/>" } else { xmlValue(x) }
    }
    node_set <- getNodeSet(root, path1)
    count <- node_set[[2]] %>% xmlValue %>% as.integer
    tryCatch({
      tmp <- xmlChildren(node_set[[3]])
    }, error = function(e) {
      browser()
      stop(conditionMessage(e))
    })
    # pick the img which might be "rare word"
    index.rare_word <- which(names(tmp) == "img") %>%
      Filter(f = function(n) n > 1 & n < length(tmp)) %>%
      Filter(f = function(n) all(names(tmp)[c(n-1, n+1)] == "text"))
    if (length(index.rare_word) == 0) {
      index.text <- which(names(tmp) == "text")
      tmp2 <- tmp[index.text] %>% lapply(xmlValue)
    } else {
      index.text.merged <- lapply(index.rare_word, function(n) c(n-1, n+1))
      text.merged <- lapply(index.text.merged, function(index) paste(sapply(tmp[index], xmlValue), collapse=""))
      index.text.merged.head <- sapply(index.text.merged, head, 1)
      index.text <- which(names(tmp) == "text")
      index.text.non_merged <- setdiff(index.text, index.text.merged %>% unlist)
      index.text <- sort(c(index.text.non_merged, index.text.merged.head))
      info.to.replacement <- match(index.text, index.text.merged.head)
      tmp2.to.replacement <- which(info.to.replacement %>% is.na %>% `!`)
      tmp2 <- tmp[index.text] %>% lapply(xmlValue)
      tmp2[tmp2.to.replacement] <- text.merged
    }
    tmp2 <- tmp2 %>%
      lapply(gsub, pattern = "\n|\t", replacement = "") %>%
      lapply(function(s) {
        tmp <- regmatches(s, regexec(pattern = "(\\d+)(未?[得]標)(\\d+)([^\\d]*)$", s))[[1]]
        list(id = tmp[4], name = tmp[5], is_win = (tmp[3] == "得標"))
      })
    names(tmp2) <- NULL
    stopifnot(count == length(tmp2))
    tmp2
}


export_to_string_list <- function(file_name = "temp.csv", ...){
    arg <- list(...)
    attr_name_str <- ""
    attr_value_str <- ""
    for(a in arg[]){
        a_names <- paste(names(a), collapse = '\", \"', sep='')
        attr_name_str <- {
            if(attr_name_str != "") 
                paste0(attr_name_str, '\", \"', a_names) 
            else
                paste0(attr_name_str, a_names)
        }
        
        a_values <- paste(a, collapse = '\", \"', sep='')
        attr_value_str <- {
            if(attr_value_str != "")
                paste0(attr_value_str, '\", \"', a_values)
            else
                paste0(attr_value_str, a_values)
        }
    }
    
    attr_name_str <- paste0('\"', attr_name_str, '\"')
    attr_value_str<- paste0('\"', attr_value_str, '\"')
    list(attr_name_str, attr_value_str)
    #write(paste0(attr_name_str, '\n', attr_value_str), paste0(file_path_sans_ext(file_name), ".csv"))
}



export_to_csv <- function(file_name = "temp.csv", ...){
    arg <- list(...)
    attr_name_str <- ""
    attr_value_str <- ""
    for(a in arg[]){
        a_names <- paste(names(a), collapse = '\", \"', sep='')
        attr_name_str <- {
            if(attr_name_str != "") 
                paste0(attr_name_str, '\", \"', a_names) 
            else
                paste0(attr_name_str, a_names)
        }
        
        a_values <- paste(a, collapse = '\", \"', sep='')
        attr_value_str <- {
            if(attr_value_str != "")
                paste0(attr_value_str, '\", \"', a_values)
            else
                paste0(attr_value_str, a_values)
        }
    }
    
    attr_name_str <- paste0('\"', attr_name_str, '\"')
    attr_value_str<- paste0('\"', attr_value_str, '\"')
    
    write(paste0(attr_name_str, '\n', attr_value_str), paste0(file_path_sans_ext(file_name), ".csv"))
}

brute_force_parse <- function(file_name, rm_html = TRUE, save_a_csv_for_each = FALSE){
    #0 initiate
    root <- set_root(file_name)
    
    #1 機關資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_1']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_1']/td"
    prefix <- get_prefix(root, path2, 1)
    org_info <- get_subtable(root, path1, path2, prefix, add_prefix = TRUE, shift = 1, keep_all = TRUE)
    
    #2 採購資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/td"
    prefix <- get_prefix(root, path2, 1)
    purchase_info <- get_subtable(root, path1, path2, prefix, add_prefix = TRUE, shift = 1, keep_all = TRUE)
    
    #3 投標廠商
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/td"
    pathp <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td"
    prefix <- get_prefix(root, pathp, 1)
    tender_company_info <- get_subtable(root, path1, path2, add_prefix = TRUE, prefix, shift = 0, check_tender = TRUE, keep_all = TRUE)
    
    #4 決標品項
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_4']/td/table/tr/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_4']/td/table/tr/td"
    pathp <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_4']/td"
    prefix <- get_prefix(root, pathp, 1)
    tender_award_item_info <- get_subtable(root, path1, path2, add_prefix = TRUE, prefix, shift = 0, keep_all = TRUE)
    
    #6 決標資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/td"
    prefix <- get_prefix(root, path2, 1)
    tender_award_info <- get_subtable(root, path1, path2, add_prefix = TRUE, prefix, shift = 1, keep_all = TRUE)
    
    remove_html(file_name, rm_html)   
    if(save_a_csv_for_each) 
        export_to_csv(file_name, purchase_info, tender_award_info, tender_company_info)
    else
        export_to_string_list(file_name, purchase_info, tender_award_info, tender_company_info)
}

minimum_parse <-function(file_name, rm_html = TRUE, save_a_csv_for_each = FALSE, simplified_bidder_list = FALSE){
    if (interactive()) browser()
    # 標案案號
    # 標案名稱
    # 決標日期
    # 總決標金額
    # 投標廠商家數
    # 廠商代碼
    # 廠商名稱
    # 是否得標

    #0 initiate
    root <- set_root(file_name)
    retval <- list()
    #1 機關資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_1']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_1']/td"
    pattern_list <- list("機關代碼")
    retval[["org_info"]] <- get_subtable(root, path1, path2, prefix = "", add_prefix = FALSE, shift = 1, keep_all = FALSE, pattern_list = pattern_list)
    
    #2 採購資料
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_2']/td"
    pattern_list <- list("標案案號", "招標方式", "決標方式")
    retval[["purchase_info"]] <- get_subtable(root, path1, path2, prefix = "", add_prefix = FALSE, shift = 1, keep_all = FALSE, pattern_list = pattern_list)  
    
    #3 投標廠商
    if (simplified_bidder_list) {
        path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td"
        retval[["tender_company"]] <- get_subtable2(root, path1)
    } else {
        path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/th"
        path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_3']/td/table/tr/td"
        pattern_list <- list("投標廠商","投標廠商家數", "廠商代碼", "廠商名稱", "是否得標")
        tender_company_info <- get_subtable(root, path1, path2, prefix = "", add_prefix = FALSE, shift = 0, check_tender = TRUE, keep_all = FALSE, pattern_list = pattern_list)
        tender_company_info <- remove_by_name(tender_company_info, "投標廠商[0-9]+$")
        tender_company <- vector("list", as.integer(tender_company_info[["投標廠商家數"]]))
        for(.i in seq_along(tender_company)) {
          element <- list(
              id = tender_company_info[[sprintf("投標廠商%d.廠商代碼", .i)]],
              name = tender_company_info[[sprintf("投標廠商%d.廠商名稱", .i)]],
              is_win = tender_company_info[[sprintf("投標廠商%d.是否得標", .i)]] == "是"
          )
          tender_company[[.i]] <- element
        }
        retval[["tender_company"]] <- tender_company
    }
        
    #4 決標品項

    #6 決標資料
    if (interactive()) browser()
    path1 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/th"
    path2 <- "//form[@id='mainForm']/table/tr/td/div[@id='printArea']/table/tbody/tr[@class='award_table_tr_6']/td"
    pattern_list <- list("決標日期", "總決標金額") 
    tender_award_info <- get_subtable(root, path1, path2,prefix = "", add_prefix = FALSE, shift = 1, keep_all = FALSE, pattern_list = pattern_list)    
    tender_award_info <- remove_by_name(tender_award_info, "總決標金額.+")
    retval[["tender_award"]] <- list(
#'      Date parsing has bug
#'      Example: tenders/112/617/0/931116-2.html
#       date = local({
#         twdate <- strsplit(tender_award_info[["決標日期"]], "/")[[1]] %>%
#             sapply(as.integer)
#         twdate[1] <- twdate[1] + 1911
#       }),
      award = local({
        if (is.null(tender_award_info[["總決標金額"]])) {
          NA
        } else {
          tmp <- regmatches(tender_award_info[["總決標金額"]], regexec("^(-?[0-9,]+)", tender_award_info[["總決標金額"]]))[[1]][2]
          award <- gsub(",", "", tmp) %>%
            as.numeric
          stopifnot(!is.na(award))
          award
        }
      })
    )
    stopifnot(!is.null(retval[["tender_award"]]$award))
    remove_html(file_name, rm_html)   
#     if(save_a_csv_for_each) {
#         export_to_csv(file_name, purchase_info, tender_award_info, tender_company_info)
#     } else {
#         export_to_string_list(file_name, purchase_info, tender_award_info, tender_company_info)
#     }
    retval
}


gen_log_name <- function(instr){
    tmp <- str_replace_all(instr, pattern="[:]", replacement="") %>%  str_replace(pattern=" ", replacement="-")
    paste0(tmp, ".log")
}

get_content <- function(d, simplified_bidder_list = FALSE) {
    bname <- basename(d)
    dname <- dirname(d)
    minimum_parse(paste0(d), rm_html = FALSE, save_a_csv_for_each = FALSE, simplified_bidder_list = simplified_bidder_list)
}

roaming_dir <- function(start_dir= "./", use_minimum_parse = TRUE, dont_save_each_csv = TRUE){
    loginfo(paste("start roaming_dir, timestamp = ", as.character(start_time)))

    all_dir <- dir(start_dir, "gz$", recursive = TRUE)
    
    all_column_names <- ""
    column_number <- 0
        
    iter_cnt <- 1

    for(d in all_dir){
    }
    
    invisible(all_column_names)
}

file_add_header <- function(start_dir= "./", all_column_names){
    start_time <- Sys.time()
    d <- dir(start_dir)
    tender_csv <- d[grepl(pattern = "tender[0-9]+", basename(d))]
    write(all_column_names, file="tender_total.csv", append = TRUE)
    for( t in tender_csv){
        rl <- readLines(t,-1)
        write(rl, file="tender_total.csv", append = TRUE)        
    }
    end_time <- Sys.time()
    paste("all tender*.csv concated, elapse time: ", as.character(end_time - start_time)) %>% print    
}
