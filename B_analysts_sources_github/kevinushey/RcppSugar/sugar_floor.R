source( "./sugarizer/sugarizer.R" )

sugarize(
  sugar_function="floor",
  num_args=1,
  possible_types=c("double", "integer"),
  package_dir=getwd(),
  include_dots=FALSE, ## include dot args in the call?
  export=TRUE, ## export the function?
  fallback="base::floor(x)", ## function to fall back to if no match to Rcpp supported types
  ask=FALSE
) 