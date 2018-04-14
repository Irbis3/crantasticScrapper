add.SB.table.project <- function(position = "last",
                                 projectName,
                                 projectPeople = NULL,
                                 nextTasks = NULL,
                                 nextDeliverables = NULL,
                                 percentComplete = NULL,
                                 status = NULL,
                                 file_location,
                                 file_name,
                                 interactive = TRUE){
  
  # Read all of the data currently in the html file
  html_lines <- readLines(con = paste("file://", file_location, file_name, sep = ''),
                          encoding = "UTF-8", warn = FALSE)
  
  # Detemine beginning and ending line numbers for HTML table
  html_table_begin <- min(grep("<table ", html_lines))
  html_table_end <- max(grep("</table>", html_lines))
  
  # Detemine beginning and ending line numbers for HTML table header row
  legend_table_begin <- grep("<table class=\"legend\" ", html_lines)
  legend_table_end <- min(grep("</table>", html_lines))
  
  if (html_table_begin == legend_table_begin &
        html_table_end == legend_table_end){
    no_entries <- TRUE
  }
  
  if (html_table_begin == legend_table_begin &
        html_table_end != legend_table_end){
    no_entries <- FALSE
  }
  
  if (no_entries == TRUE){
    line_table_begin <- 47
    line_table_end <- 47
    position <- 1
  }
  
  if (no_entries == FALSE){
    line_table_begin <- grep("<table class=\"line\" ", html_lines)
    line_table_end <- setdiff(grep("</table>", html_lines), legend_table_end)
    current_number_of_lines <- (length(line_table_end) + length(line_table_begin))/2
    
    # If the current number of lines is 9 (or greater), stop function
    if (current_number_of_lines >= 9){
      stop("The maximum number of entries has been reached. Remove or replace existing.")
    }
    
    # If position requested is set to 'first', set to numeric value 1
    if (position == "first") position <- 1
    
    # If position requested is greater than 9, set to "last"
    if (position > 9) position <- current_number_of_lines
    
    # If position is greater than the current number of lines + 1, set to "last"
    if (position >= current_number_of_lines + 1) position <- "last"
    
    # If position is set to 'last', define a number for the last position
    if (position == "last") position <- current_number_of_lines + 1
    
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
      
      # Present this table to the user and ask for the position of insertion
      print(td_char)
      position <- readline(paste(cat("At which position do you want the new table row? ", "\n",
                                     "Enter number in the range of 1-", current_number_of_lines,
                                     ".", "\n",
                                     sep = ''), sep = ''))
      
      # Check input and determine the intended position of insertion
      if (position %in% as.character(seq(from = 1, to = current_number_of_lines, by = 1))){
        position <- as.numeric(position)
      } else if (position == "first"){
        position <- 1 
      } else if (position == "last"){
        position <- current_number_of_lines + 1
      } else {
        stop("Invalid entry provided.")
      }
      
    }
    
  }
  
  # Process the project people
  for (i in 1:length(projectPeople)){
    if (i == 1){
      projectPeoplelist <- vector(mode = "character", length = 1)
    }
    projectPeoplelist <- paste(projectPeoplelist, projectPeople[i], sep = '')
    
    if (i != length(projectPeople)){
      projectPeoplelist <- paste(projectPeoplelist, ", ", sep = '')  
    }
  }
  
  # Insert the table row into the appropriate part of the file
  if (no_entries == FALSE){
    position_to_line <- line_table_begin[position]
    
    if (position >= current_number_of_lines | position == "last"){
      position_to_line <- grep("</div>", html_lines)[1]
    }
    
    file_1_begin <- 1
    file_1_end <- position_to_line - 1
    
    file_2_begin <- position_to_line
    file_2_end <- length(html_lines)
    
    file_1 <- html_lines[file_1_begin:file_1_end]
    file_2 <- html_lines[file_2_begin:file_2_end]
    
    # Write out beginning part of file
    cat(file_1,
        file = paste(file_location, file_name, sep = ''),
        append = FALSE, sep = "\n")
    
    # Add table row
    table_row_added <- cat(
      paste(""), "\t", "\t", "\t",
      paste("<table class=\"line\" style=\"padding-top: 8px; padding-bottom: 5px;\">", sep = ''), "\n",
      "\t", "\t", "\t", "\t",
      paste("<tr>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectName\" style=\"text-align: left; width: 180px; font-size: 15px; color: rgb(256, 256, 256)\">", projectName, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectPeople\" style=\"text-align: left; width: 170px; font-size: 15px; color: rgb(256, 256, 256)\">", projectPeoplelist, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectNextTasks\" style=\"text-align: left; width: 220px; font-size: 15px; color: rgb(256, 256, 256)\">", nextTasks, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectNextDeliverables\" style=\"text-align: left; width: 240px; font-size: 15px; color: rgb(256, 256, 256)\">", nextDeliverables, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectPercentComplete\" style=\"text-align: center; width: 80px; font-size: 15px; color: rgb(256, 256, 256)\">", percentComplete, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectStatus\" style=\"text-align: right; width: 80px; font-size: 15px; color: rgb(256, 256, 256)\">", status, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t",
      paste("</tr>", sep = ''), "\n",
      "\t", "\t", "\t",
      paste("</table>", sep = ''), "\n",
      sep = '',
      file = paste(file_location, file_name, sep = ''),
      append = TRUE)
    
    # Write out last part of file
    cat(file_2,
        file = paste(file_location, file_name, sep = ''),
        append = TRUE, sep = "\n")  
    
  }
  
  if (no_entries == TRUE){
    
    # If there are no entries, the default position is known
    position_to_line <- 47
    
    # Define the beginning and ending lines of the first HTML chunk
    file_1_begin <- 1
    file_1_end <- 46
    
    # Define the beginning and ending lines of the final HTML chunk
    file_2_begin <- 47
    file_2_end <- length(html_lines)
    
    # Create objects containing the first and final HTML chunks
    file_1 <- html_lines[file_1_begin:file_1_end]
    file_2 <- html_lines[file_2_begin:file_2_end]
    
    # Write out beginning part of file
    cat(file_1,
        file = paste(file_location, file_name, sep = ''),
        append = FALSE, sep = "\n")
    
    # Add table row
    table_row_added <- cat(
      paste(""), "\t", "\t", "\t",
      paste("<table class=\"line\" style=\"padding-top: 8px; padding-bottom: 5px;\">", sep = ''), "\n",
      "\t", "\t", "\t", "\t",
      paste("<tr>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectName\" style=\"text-align: left; width: 180px; font-size: 15px; color: rgb(256, 256, 256)\">", projectName, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectPeople\" style=\"text-align: left; width: 170px; font-size: 15px; color: rgb(256, 256, 256)\">", projectPeoplelist, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectNextTasks\" style=\"text-align: left; width: 220px; font-size: 15px; color: rgb(256, 256, 256)\">", nextTasks, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectNextDeliverables\" style=\"text-align: left; width: 240px; font-size: 15px; color: rgb(256, 256, 256)\">", nextDeliverables, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectPercentComplete\" style=\"text-align: center; width: 80px; font-size: 15px; color: rgb(256, 256, 256)\">", percentComplete, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t", "\t",
      paste("<td class=\"projectStatus\" style=\"text-align: right; width: 80px; font-size: 15px; color: rgb(256, 256, 256)\">", status, "</td>", sep = ''), "\n",
      "\t", "\t", "\t", "\t",
      paste("</tr>", sep = ''), "\n",
      "\t", "\t", "\t",
      paste("</table>", sep = ''), "\n",
      sep = '',
      file = paste(file_location, file_name, sep = ''),
      append = TRUE)
    
    # Write out last part of file
    cat(file_2,
        file = paste(file_location, file_name, sep = ''),
        append = TRUE, sep = "\n")
    
  }
  
  # End function  
}
