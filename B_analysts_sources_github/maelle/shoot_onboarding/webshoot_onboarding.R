library("magrittr")

# helper to shoot a review
webshoot_review <- function(review_url, package_name, path){
  comment_id <- stringr::str_replace(review_url, ".*issuecomment", "issuecomment")
  webshot::webshot(review_url,
                   selector = ".js-comment-container",
                   file = paste0(package, ".png"))
}

# helper to shoot the whole thread
webshoot_thread <- function(issue_url, package_name, path){
  webshot::webshot(issue_url,
                   selector = "#show_issue",
                   file = paste0(path, package, "_whole_issue.png"))
}

# helper to shoot the upper info
webshoot_upper <- function(issue_url, package_name, path){
  webshot::webshot(issue_url,
                   selector = "#partial-discussion-header",
                   file = paste0(path, package, "_upper.png"),
                   expand = c(10, 10, 600, 10))
}

# helper to shoot the first answer

webshoot_first_response <- function(issue_url, package_name, path){
  webshot::webshot(issue_url,
                   selector = ".js-comment-container:nth-of-type(2)",
                   file = paste0(path, package, "_first_response.png"),
                   expand = c(10, 10, 10, 10))
}

shoot_package_onboarding <- function(package_name, path = "screenshots/"){

  # airtable data
  airtable <- airtabler::airtable("appZIB8hgtvjoV99D", "Reviews")
  airtable <- airtable$Reviews$select_all()
  review_info <- dplyr::filter(airtable, package == package_name)
  issue_url <- dplyr::pull(review_info, onboarding_url) %>% unique()
  review_urls <- dplyr::pull(review_info, review_url)

  webshoot_thread(issue_url, package_name, path)
  webshoot_meta(issue_url, package_name, path)
  purrr::walk(review_urls, webshoot_review, package_name, path)
}

