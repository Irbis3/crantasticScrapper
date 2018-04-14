rm(list = ls())
library(rpart)
library(partykit)
library(RWeka)
library(jsonlite)
library(magrittr)
library(purrr)
library(broom)
library(stringr)
library(dplyr)

# http://stackoverflow.com/questions/34196611/converting-rpart-output-into-json-format-in-r

# json_prsr <- function(tree, node = 1, node_stats = NULL){
#   
#   # Checking the decision tree object
#   if(!is(tree, c("constparty","party")))
#     tree <- partykit::as.party(tree)
#   
#   # Parsing into json format
#   str  <- ""
#   rule <- partykit:::.list.rules.party(tree, node)
#   
#   if(is.null(node_stats))
#     node_stats <- table(tree$fitted[1])
#   children <- partykit::nodeids(tree, node)
#   
#   if (length(children) == 1) {
#     ct  <- node_stats[as.character(children)]
#     str <- paste("{","'name': '",children,"','size':",ct,",'rule':'",rule,"'}", sep='')
#   } else {
#     str <- paste("{","'name': '", node,"', 'rule': '", rule, "', 'children': [", sep='')
#     for(child in children){
#       check <- paste("{'name': '", child, "'", sep='')
#       if(child != node & ( !grepl(check, str, fixed=TRUE) ) ) {
#         print(child)
#         child_str <- json_prsr(tree, child, node_stats)
#         str <- paste(str, child_str, ',', sep='')
#       }
#     }
#     str <- substr(str, 1, nchar(str)-1) #Remove the comma
#     str <- paste(str,"]}", sep='')
#   }
#   return(str)
# }


clnrule <- function(x) {
  
  x %>% 
    str_replace("%in%", "in") %>% 
    str_replace("c\\(", "(") %>% 
    str_replace_all("\"", "")
  
}

summ <- function(x) {
  
  if(class(x) %in% c("numeric", "integer")) {
    res <- tidy(summary(x))
  } else {
    res <- count(data_frame(class = x), class)
  }
  res
}

tree_list <- function(tree, node = 1){
  
  # Checking the decision tree object
  if(!is(tree, c("constparty","party")))
    tree <- partykit::as.party(tree)
  
  completerule <- partykit:::.list.rules.party(tree, node)
  children <- partykit::nodeids(tree, node)
  size <- sum(table(tree$fitted[1])[as.character(children)], na.rm = TRUE)
  depth <-  depth(tree[[node]])
  summary <- summ(tree[[node]]$fitted[["(response)"]])
  rule <- clnrule(last(unlist(str_split(completerule, "\\s+&\\s+"))))

  isterminal <- length(children) == 1
  
  str <- list(
    name = node,
    size = size,
    depth = depth,
    rule = rule,
    completerule = completerule,
    terminal = isterminal,
    nchildren = length(children) - 1,
    summary = summary
  )
  
  if (!isterminal) {

    children2 <- setdiff(children, node)
    children22 <- map(children2, partykit::nodeids, obj = tree)
    
    nochildren <- map2(children22, children2, setdiff) %>% 
      unlist() %>% 
      unique()
    
    childrenf <- setdiff(children2, nochildren)
    
    str$childrensid <- childrenf
    
    str$children <- map(childrenf, tree_list, tree = tree)
    
    # more elegant plz!
    str$children[[1]]$side <- "left"
    str$children[[2]]$side <- "right"
    
  }
  
  return(str)
  
}


# EXAMPLE 1 ---------------------------------------------------------------
tree <- rpart(hp ~ ., data = mtcars, control = rpart.control(minsplit = 4))
tree <- as.party(tree)

plot(tree)

tree %>% 
  tree_list() %>% 
  toJSON(auto_unbox = TRUE, pretty = TRUE) %>% 
  writeLines(con = "data1.json")

# EXAMPLE 2 ---------------------------------------------------------------
tree <- ctree(Species ~ .,data = iris)
plot(tree)

tree %>% 
  tree_list() %>% 
  toJSON(auto_unbox = TRUE, pretty = TRUE) %>% 
  writeLines(con = "data2.json")

# EXAMPLE 3 ---------------------------------------------------------------
tree <- ctree(Ozone ~ ., data = subset(airquality, !is.na(Ozone)))
plot(tree)

tree %>%
  tree_list() %>%
  toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
  writeLines(con = "data3.json")

# tree %>% 
#   json_prsr() %>% 
#   as.character() %>% 
#   writeLines(con = "data3.json")

# EXAMPLE 4 ---------------------------------------------------------------
# devtools::install_github("jbkunst/riskr")
data(credit, package = "riskr")
set.seed(1234)

credit2 <- credit %>%
  tbl_df() %>% 
  filter(complete.cases(credit)) %>%
  select(-id_client) %>% 
  sample_n(5000) %>% 
  mutate(bad = ifelse(bad == 0, "bad", "good"),
         bad = factor(bad)) %>% 
  map_if(is.character, as.factor) %>% 
  as_data_frame()

tree <- ctree(bad ~ ., data = credit2, control = ctree_control(mincriterion = 0.80))
plot(tree)

tree %>%
  tree_list() %>%
  toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
  writeLines(con = "data4.json")


# EXAMPLE 5 ---------------------------------------------------------------
data("Titanic", package = "datasets")
ttnc <- as.data.frame(Titanic)
ttnc <- ttnc[rep(1:nrow(ttnc), ttnc$Freq), 1:4]
names(ttnc)[2] <- "Gender"

tree <- ctree(Survived ~ ., data = ttnc)
plot(tree)

tree %>%
  tree_list() %>%
  toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
  writeLines(con = "data5.json")

# EXAMPLE 6 ---------------------------------------------------------------
j48 <- J48(Survived ~ ., data = ttnc)
plot(j48)

tree <- as.party(j48)
plot(tree)

tree %>%
  tree_list() %>%
  toJSON(auto_unbox = TRUE, pretty = TRUE) %>%
  writeLines(con = "data6.json")


