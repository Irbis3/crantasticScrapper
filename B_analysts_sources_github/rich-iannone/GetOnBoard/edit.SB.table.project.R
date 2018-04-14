edit.SB.table.project <- function(edit,
                                  projectName = NULL,
                                  projectPeople = NULL,
                                  nextTasks = NULL,
                                  nextDeliverables = NULL,
                                  percentComplete = NULL,
                                  status = NULL,
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
    stop("There are no entries to edit")
  }
  
  line_table_begin <- grep("<table class=\"line\" ", html_lines)
  line_table_end <- setdiff(grep("</table>", html_lines), legend_table_end)
  current_number_of_lines <- (length(line_table_end) + length(line_table_begin))/2
  
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
  
  if (interactive == TRUE){
    
    # Remove any supplied values for the table
    projectName <- NULL
    projectPeople <- NULL
    nextTasks <- NULL
    nextDeliverables <- NULL
    percentComplete <- NULL
    status <- NULL
    
    # Present the table of data in the table to the user and ask for the position of insertion
    print(td_char)
    
    if (current_number_of_lines == 1){
      edit <- 1
    }
    
    if (current_number_of_lines > 1){
      edit <- readline(paste(cat("Which table item would you like to edit? ", "\n",
                                 "Choose from 1 to ", current_number_of_lines, ".", "\n",
                                 sep = ''),
                             sep = ''))
    }
    
    if (edit %in% as.character(seq(from = 1, to = current_number_of_lines, by = 1))){
      edit <- as.numeric(edit)
    } else {
      stop("Invalid entry provided.")
    }
    
    # Get the current values from the line to edit
    projectName_old <- td_char[edit, 1]
    projectPeople_old <- td_char[edit, 2]
    nextTasks_old <- td_char[edit, 3]
    nextDeliverables_old <- td_char[edit, 4]
    percentComplete_old <- td_char[edit, 5]
    status_old <- td_char[edit, 6]
    
    # Interactively ask user for the project name
    projectName <-
      readline(paste(cat("Edit the project name.", "\n",
                         "Press <ENTER> for no change, \".\" for empty field.", "\n",
                         "Current project name is: ", projectName_old, "\n", sep = ''),
                     sep = ''))
    
    if (projectName == ""){
      projectName <- projectName_old
    } else if (projectName == "."){
      projectName <- ""
    }
    
    # Interactively ask user for the people involved in the project
    projectPeople <-
      readline(paste(cat("Edit the names of people in the project.", "\n",
                         "Press <ENTER> for no change, \".\" for empty field.", "\n",
                         "Current people include: ", projectPeople_old, "\n", sep = ''),
                     sep = ''))
    
    if (projectPeople == ""){
      projectPeople <- projectPeople_old
    } else if (projectPeople == "."){
      projectPeople <- ""
    }
    
    # Interactively ask user for the project's next tasks
    nextTasks <-
      readline(paste(cat("Edit the next tasks for the project.", "\n",
                         "Press <ENTER> for no change, \".\" for empty field.", "\n",
                         "Current tasks are: ", nextTasks_old, "\n", sep = ''),
                     sep = ''))
    
    if (nextTasks == ""){
      nextTasks <- nextTasks_old
    } else if (nextTasks == "."){
      nextTasks <- ""
    }
    
    # Interactively ask user for the project's next deliverables
    nextDeliverables <-
      readline(paste(cat("Edit the next deliverables for the project.", "\n",
                         "Press <ENTER> for no change, \".\" for empty field.", "\n",
                         "Current deliverables are: ", nextDeliverables_old, "\n", sep = ''),
                     sep = ''))
    
    if (nextDeliverables == ""){
      nextDeliverables <- nextDeliverables_old
    } else if (nextDeliverables == "."){
      nextDeliverables <- ""
    }
    
    # Interactively ask user for the percentage completion for the project
    percentComplete <-
      readline(paste(cat("Edit the project's percent completion.", "\n",
                         "Press <ENTER> for no change, \".\" for empty field.", "\n",
                         "Current figure is: ", percentComplete_old, "\n", sep = ''),
                     sep = ''))
    
    if (percentComplete == ""){
      percentComplete <- percentComplete_old
    } else if (percentComplete == "."){
      percentComplete <- ""
    }
    
    # Interactively ask user for the project's status
    status <-
      readline(paste(cat("Edit the project's status.", "\n",
                         "Press <ENTER> for no change, \".\" for empty field.", "\n",
                         "Current status is: ", status_old, "\n", sep = ''),
                     sep = ''))
    
    if (status == ""){
      status <- status_old
    } else if (status == "."){
      status <- ""
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
  
  # Delete the table row to be edited
  deletion_begin_line <- line_table_begin[edit]
  deletion_begin_end <- as.integer(deletion_begin_line + 9)
  
  # Define the beginning and ending lines of the first HTML chunk
  file_1_begin <- 1
  file_1_end <- deletion_begin_line - 1
  
  # Define the beginning and ending lines of the final HTML chunk
  file_2_begin <- file_1_end + 11
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
  
  # End function
}
