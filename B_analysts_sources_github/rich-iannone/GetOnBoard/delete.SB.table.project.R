delete.SB.table.project <- function(delete,
                                    file_location,
                                    file_name,
                                    interactive = TRUE){
  
  # Read all of the data currently in the HTML file
  html_lines <- readLines(con = paste("file://", file_location, file_name, sep = ''),
                          encoding = "UTF-8", warn = FALSE)
  
  html_table_begin <- min(grep("<table ", html_lines))
  html_table_end <- max(grep("</table>", html_lines))
  
  legend_table_begin <- grep("<table class=\"legend\" ", html_lines)
  legend_table_end <- min(grep("</table>", html_lines))
  
  if (html_table_begin == legend_table_begin &
        html_table_end == legend_table_end){
    stop("There are no entries to delete")
  }
  
  line_table_begin <- grep("<table class=\"line\" ", html_lines)
  line_table_end <- setdiff(grep("</table>", html_lines), legend_table_end)
  current_number_of_lines <- (length(line_table_end) + length(line_table_begin))/2
  
  if (interactive == FALSE){
    
    # If deletion requested is set to the 'first', set to numeric value 1
    if (delete == "first") delete <- 1
    
    # If deletion requested is greater than the 9th line, set it to the last line
    if (delete > 9) delete <- current_number_of_lines
    
    # If deletion requested is for a line greater than the current number of lines,
    # set it to the last line
    if (delete > current_number_of_lines) delete <- current_number_of_lines
    
    # If the deletion request is set to 'last', set it to the last line
    if (delete == "last") delete <- current_number_of_lines
    
  }
  
  # Read current inner HTML elements from inside td tags into a data frame
  # and present this to the user
  if (interactive == TRUE){
    
    # Read current inner HTML elements from inside td tags into a data frame
    for (i in 1:current_number_of_lines){
      if (i == 1) td_char <- as.data.frame(mat.or.vec(nc = 6, nr = current_number_of_lines))
      for (j in 1:6){
        td_char[i, j] <- gsub("^.*\">([a-zA-Z0-9 -,/.]*)</td>$", "\\1",
                              html_lines[line_table_begin[i] + 1 + j], perl = TRUE)
      }
      colnames(td_char) <- c("projectName", "projectPeople",
                             "nextTasks", "nextDeliverables",
                             "percentComplete", "status")
    }
    
    # Present this table to the user and ask which table item should be deleted
    print(td_char)
    delete <- readline("Which table item would you like to delete? ")
    if (delete %in% as.character(seq(from = 1, to = current_number_of_lines, by = 1))){
      delete <- as.numeric(delete)
    } else {
      stop("Invalid entry provided.")
    }
  }
  
  # Delete the requested table row
  deletion_begin_line <- line_table_begin[delete]
  deletion_begin_end <- as.integer(deletion_begin_line + 9)
  
  # Define the beginning and ending lines of the first HTML chunk
  file_1_begin <- 1
  file_1_end <- deletion_begin_line - 1
  
  # Define the beginning and ending lines of the final HTML chunk
  file_2_begin <- deletion_begin_end + 1
  file_2_end <- length(html_lines)
  
  # Create objects containing the first and final HTML chunks
  file_1 <- html_lines[file_1_begin:file_1_end]
  file_2 <- html_lines[file_2_begin:file_2_end]
  
  # Write out beginning part of file
  cat(file_1,
      file = paste(file_location, file_name, sep = ''),
      append = FALSE, sep = "\n")
  
  # Write out last part of file
  cat(file_2,
      file = paste(file_location, file_name, sep = ''),
      append = TRUE, sep = "\n")  
  
  # End function  
}
