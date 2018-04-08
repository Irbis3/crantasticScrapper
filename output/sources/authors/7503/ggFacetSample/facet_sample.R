#' Sample facets when ggplotting.
#'
#' @description \code{facet_sample(...)} does layout as in \code{\link[ggplot2]{facet_grid}(...)},
#'   but samples facet(s) vice showing everything.
#' @param ... \code{facets}, \code{margins}, etc - see \code{\link[ggplot2]{facet_grid}(...)}.
#'   The only eliminated parameter is \code{drop} - \code{facet_sample(...)} forces \code{drop = TRUE}
#'
#'   future: changed default for \code{margin} to show margins
#'   in the sampled dimension, though \code{margin} may be manually set to \code{FALSE}
#'
#' @param sample_n a length 2 integer array, optionally with one value \code{NA_integer_};
#'   these values are the maximum number of rows (index 1) and columns (index 2) to show
#'   in \code{\link[ggplot2]{facet_grid}(...)} layout (excluding margins).  If \code{sample_n} exceeds the number of unique
#'   options in a dimension, \code{facet_sample(...)} will produce a warning.
#'
#'   a \code{NA_integer_} entry (the default) will provide all the values in that facet, though
#'   \code{facet_sample(...)} will issue a warning if \code{sample_n} is left as default
#'
#'   future: a number between 0 and 1 will be treated as the fraction
#'   of options to be shown (converted to integer by ceiling)
#'
#' @param seed the random seed for drawing which rows & columns will be shown;
#'   by default, this is \code{NA_integer_} and no seed will be set.
#'
#'   The random number generator is reset for both rows and columns (with an offset) so,
#'   with \code{seed} provided, changing the number of one dimension to display will not change
#'   which of the other dimensions are displayed.
#'
#'   The seed for each dimension may be independently specified with a 2 element array.
#'
#' @export
#'
#' @examples
#'   # construct some data:
#'   sampn <- 10000
#'
#'   testdata <- data.frame(
#'     rowcat = paste0("1", sample(LETTERS[1:10], sampn, replace = TRUE)),
#'     rowcat2 = paste0("2", sample(LETTERS[11:15], sampn, replace = TRUE)),
#'     colcat = paste0("2", sample(letters[1:10], sampn, replace = TRUE)),
#'     x = runif(sampn),
#'     y = runif(sampn)
#'   )
#'
#'   p <- ggplot2::ggplot(testdata) + ggplot2::aes(x=x, y=y, color=rowcat2) +
#'     ggplot2::geom_point()
#'
#'   # plot the works (not actually recommended)
#'   \dontrun{p + ggplot2::facet_grid(rowcat ~ colcat)}
#'
#'   # plot a sample of the row-facets:
#'   p + facet_sample(rowcat ~ colcat, sample_n = c(3, NA))
#'
#'   # plot a sample of the column-facets:
#'   p + facet_sample(rowcat ~ colcat, sample_n = c(NA, 3))
#'
#'   # plot a sample of all facets:
#'   p + facet_sample(rowcat ~ colcat, sample_n = c(3, 3))
#'
facet_sample <- function(
  ...,
  sample_n=c(NA_integer_, NA_integer_), seed=NA_integer_
) {
  args <- as.list(substitute(list(...)))[-1L]
  args$drop <- NULL
  shrink <- if (is.null(args$shrink)) TRUE else args$shrink
  #browser()
  facet <- do.call(ggplot2::facet_grid, args)
  if (all(is.na(sample_n))) {
    warning("facet_sample(...): no sampling requested; falling back facet_grid(...).", call. = FALSE)
    return(facet)
  }
  if (any(sample_n <= 0, na.rm = TRUE))
    stop(sprintf(
      "facet_sample(...): requested 0 or negative samples in a facet dimension: sample=c(%s)", paste(sample_n, collapse = ", ")
    ), call. = FALSE)
  ## TODO check sample is non-negative integer
  ## TODO future functionality: allow sample rate (i.e., allow sample a double between 0 and 1)
  facet$params$sample_n <- sample_n
  facet$params$seed <- if(length(seed) > 1) seed[1:2] else c(seed, seed + 1L)
  ggplot2::ggproto(NULL, FacetSample,
          shrink = shrink,
          params = facet$params
  )
}

#' The FacetSample object.
#'
#' @description refer to \code{\link[ggplot2]{Facet}}
#' @usage NULL
#' @export
#' @note The general idea is to generate a data-frame for each facet dimension,
#' with a unique set of keys (initially all possible combinations) for each dimension,
#' then sample without replacement from that unique-key list,
#' then left-join that key list to the original data, leaving only the relevant data to display.
FacetSample <- ggplot2::ggproto("FacetSample", ggplot2::FacetGrid,
  rowfilter = NULL,
  colfilter = NULL,
  map_data = function(self, data, layout, params) {
    reduced_data <- self$merge_helper(data)
    return(ggplot2::ggproto_parent(ggplot2::FacetGrid, self)$map_data(reduced_data, layout, params))
  },
  compute_layout = function(self, data, params) {
    ref <- data[[1]]
    self$rowfilter <- self$filter_helper(
      ref, params$rows, params$sample_n[1], params$seed[1]
    )
    self$colfilter <- self$filter_helper(
      ref, params$cols, params$sample_n[2], params$seed[2]
    )
    # browser()
    # self$filter <- substitute(rowfilter & colfilter)
    reduced_data <- lapply(X=data, FUN=self$merge_helper)
    ## TODO reduced data shouldn't touch overall data, just what's in panels?
    ## effects margins, stats that use all data pre-faceting, etc?
    return(ggplot2::ggproto_parent(ggplot2::FacetGrid, self)$compute_layout(
      reduced_data,
      params
    ))
  },
  filter_helper = function(self, refdata, dims, nreq, seed) {
    if (length(dims)) { ## if there is anything to this dimension
      srcs <- plyr::as.quoted(dims)
      nms <- names(srcs)
      unord <- unique(refdata[,nms,drop=F])
      ord <- with(unord,{
        do.call(order, args=as.list(parse(text=nms)), quote=F)
      })
      src <- unord[ord,nms,drop=F]
      maxn <- self$n_helper(nreq, src, nms)
      return(self$sample_helper(src, maxn, seed))
    } else {
      if (!is.na(nreq)) warning(sprintf("facet_sample(...): requested sampling (n=%d) on non-faceted dimension.", nreq), call.=FALSE)
      return(NULL)
    }
  },
  n_helper = function(n, src, dim) {
    if (!is.na(n)) {
      srclen <- dim(src)[1]
      if (n >= srclen) {
        warning(sprintf(
          "facet_sample(...): requested number of values, n=%d, is >= number of unique values in %s (%d).  Setting n=NA_integer_ (i.e., show all values).",
          n, dim, srclen
        ), call. = FALSE) # call. doesn't provide useful info to user
        return(NA_integer_)
      } else return(n)
    } else return(NA_integer_)
  },
  sample_helper = function(src, n, seed) {
    if (is.na(n)) return(NULL)
    if (!is.na(seed)) set.seed(seed)
    slice <- if (!is.na(n)) sample(dim(src)[1], size = n, replace = FALSE) else 1:dim(src)[1]
    src[slice,,drop=F]
  },
  merge_helper = function(df, self) {
    if(!is.null(self$colfilter)) df <- merge(df, self$colfilter)
    if(!is.null(self$rowfilter)) df <- merge(df, self$rowfilter)
    return(df)
  }
)
