# all available packages
pkgs <- as.data.frame(available.packages())
# their names
package_names <- pkgs$Package

# which ones have exactly one dot?
dotted <- as.character(package_names[stringr::str_count(package_names, "\\.") == 1])
length(dotted)
length(dotted)/length(package_names)

# for each of these dotted packages find the potential origin name
# in my theory it's the first thing before the dot
get_origin <- function(name){
  stringr::str_split(name, "\\.", simplify = TRUE)[1]
}
origin <- purrr::map_chr(dotted, get_origin)

# the dotted packages is linked to an origin
# if the origin is a package
# if one of them is a dependency of the other
find_whether_linked <- function(dotted, origin){
  # is origin a package?
  is_a_package <- origin %in% package_names 
  
  dotted_info <- dplyr::filter_(pkgs, ~Package == dotted)
  dotted_dependencies <- toString(c(as.character(dotted_info$Imports),
                               as.character(dotted_info$Suggests),
                               as.character(dotted_info$LinkingTo),
                               as.character(dotted_info$Enhances),
                               as.character(dotted_info$Depends)))
  dotted_dependencies <- stringr::str_split(dotted_dependencies, ",", simplify = TRUE)
  dotted_dependencies <- trimws(dotted_dependencies)
  origin_imported_by_dotted <- origin %in% dotted_dependencies
  
  origin_info <- dplyr::filter_(pkgs, ~Package == origin)
  origin_dependencies <- toString(c(as.character(origin_info$Imports),
                               as.character(origin_info$Suggests),
                               as.character(origin_info$LinkingTo),
                               as.character(origin_info$Enhances),
                               as.character(origin_info$Depends)))
  origin_dependencies <- stringr::str_split(origin_dependencies, ",", simplify = TRUE)
  origin_dependencies <- trimws(origin_dependencies)
  origin_dependencies <- stringr::str_replace(origin_dependencies, "\\(.*", "")
  origin_dependencies <- trimws(origin_dependencies)
  dotted_imported_by_origin <- dotted %in% origin_dependencies
  
  is_a_package & (origin_imported_by_dotted|dotted_imported_by_origin)
}

which_origins <- purrr::map2_lgl(dotted, origin, find_whether_linked)

data.frame(origin = origin, dotted = dotted)[which_origins,]

length(which_origins) / length(dotted)