# Read in a CSV file, calculate winner, and write to file
process_file <- function(filename, outfile = NULL) {
  if (is.null(outfile)) {
    outfile <- sub("\\.csv$", "-results.txt", filename)
  }

  # Save results to file
  if (outfile != stdout()) {
    sink(outfile)
    on.exit(sink())
  }

  # Read data: first column is first-rank vote, second is second, etc.
  dat <- suppressWarnings(read.csv(filename, header = FALSE,
    fill = TRUE, stringsAsFactors = FALSE))

  # Convert to matrix and replace blanks with NA
  dat <- as.matrix(dat)
  dat[dat == ""] <- NA

  calculate_ranked_choice(dat)
}


# Takes a matrix of vote data and finds the winner recursively. Each row of the
# matrix represents one voter's vote, and each column represents the
# first-ranked vote, the second-ranked vote, and so on.
calculate_ranked_choice <- function(dat, round = 1) {
  cat("==== Round", round, "====\n")

  # Shift the ranked votes, removing gaps in 1st, 2nd, 3rd, etc.
  cat("Promoting votes to fill gaps in rankings...\n")
  dat <- shift_left_na(dat)

  # Get names of all candidates
  all_names <- unique(as.vector(dat))
  all_names <- all_names[!is.na(all_names)]

  # Get counts of first-rank votes for each candidate -------------------------
  # Turn first col of dat into a factor, with all_names as levels. This is so
  # table() will report entries with zero count.
  rank1 <- factor(dat[, 1], levels = all_names)
  counts <- as.data.frame(table(rank1))
  names(counts) <- c("name", "votes")
  # Sort by votes
  counts <- counts[order(counts$votes, decreasing = TRUE), ]

  # Print the table -----------------------------------------------------------
  cat("Counts of rank-1 votes:\n")
  print(counts, row.names = FALSE)

  # Do we have a winner? ------------------------------------------------------
  if (max(counts$votes) > sum(counts$votes)/2) {
    maxrow <- counts$votes == max(counts$votes)
    cat(wrap(
      "Winner is ", counts$name[maxrow], ", with ",
      round(counts$votes[maxrow] / sum(counts$votes) * 100, 2),
      "% of the votes in the last round.", sep = ""
    ))
    cat("\n")
    return(invisible(counts$name[maxrow]))
  }

  # ---------------------------------------------------------------------------
  # Drop those who got the lowest number of first-rank votes
  # (can be more than one)
  lowest_votes <- min(counts$votes)

  if (nrow(counts) > 1 && all(counts$votes == lowest_votes)) {
    cat(wrap("Tie between ", paste(counts$name, collapse = ", "), "."))
    cat("\n")
    return(invisible(counts$name))
  }

  drop_names <- counts$name[counts$votes == lowest_votes]
  cat(wrap("Dropping candidate(s) with ", lowest_votes, " votes:"))
  cat("\n")
  cat(wrap(paste(drop_names, collapse = ", ")))
  cat("\n\n")

  # Remove all instances of the lowest vote-getters
  dat[dat %in% drop_names] <- NA

  # Recurse
  calculate_ranked_choice(dat, round + 1)
}

# For any NA's in a given column, shift names over from next column. For
# example, if a voter's first-rank vote was removed, their second-rank vote is
# now treated as their first-rank vote.
shift_left_na <- function(dat) {
  ncols <- ncol(dat)

  for (i in 1:(ncols-1)) {
    # Find NA's in current column
    na_idx <- is.na(dat[, i])

    # For any NA's, shift over values from all subsequent columns and fill last
    # with NA.
    dat[na_idx, i:(ncols-1)] <- dat[na_idx, (i+1):ncols]
    dat[na_idx, ncols] <- NA
  }

  dat
}

# Wrap text to specified column
wrap <- function(..., sep = "", width = 80) {
  paste(strwrap(paste(..., sep = sep), width = width), collapse = "\n")
}


process_file("SampleData.csv")
process_file("SampleData2.csv")
process_file("SampleData3.csv")
process_file("SampleData4.csv")
process_file("SampleData5.csv")
process_file("SampleData6.csv")
process_file("SampleDataLarger.csv")
