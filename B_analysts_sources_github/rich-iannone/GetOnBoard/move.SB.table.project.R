move.SB.table.project <- function(move,
                                  where,
                                  projectName = NULL,
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
    stop("There are no entries to move")
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
    
  if (interactive == FALSE){
    
    # If a project name is provided as a search for the line to move, find the
    # match and identify the line number
    if (!is.null(projectName)){
      
      # First remove any value assigned to 'move'
      if (exists("move")) move <- NULL
      
      # Determine which line number contains a match for the project name, then
      # set the matched line to the object 'move'
      for (i in 1:nrow(td_char)){
        if (tolower(projectName) == tolower(td_char[i, 1])) move <- i
      }
      
      # If there is no match, stop function with message
      if (!exists("move")){
        stop(paste("There is no match for '", projectName, "'.", sep = ''))
      }
      
    }
    
    # Validate inputs 
    if (where %in% setdiff(seq(from = 1, to = current_number_of_lines, by = 1), move)){
      as.numeric(where)
    } else if (move == "up"){
      if (move > 1){
        where <- move - 1
      } else {
        stop("The line is at the top")
      }
    } else if (position == "down"){
      if (move < current_number_of_lines){
        where <- move + 1
      } else {
        stop("The line is at the bottom")
      }
    } else {
      stop("Invalid entry provided.")
    }
    
  }
  
  
  if (interactive == TRUE){
    
    # Present the table of data in the table to the user and ask for the position of insertion
    print(td_char)
    
    if (current_number_of_lines == 1){
      stop("There is only a single project entry.")
    }
    
    if (current_number_of_lines > 1){
      move <- readline(paste(cat("Which table item would you like to move? ", "\n",
                                 "Choose from 1 to ", current_number_of_lines, ".", "\n",
                                 sep = ''),
                             sep = ''))
      
      where <- readline(paste(cat("Where would you like to move this item? ", "\n",
                                  "Provide a line number or the words \"up\" or \"down\".", "\n",
                                  sep = ''),
                              sep = ''))
      
      if (where %in% setdiff(seq(from = 1, to = current_number_of_lines, by = 1), move)){
        as.numeric(where)
      } else if (move == "up"){
        if (move > 1){
          where <- move - 1
        } else {
          stop("The line is at the top")
        }
      } else if (position == "down"){
        if (move < current_number_of_lines){
          where <- move + 1
        } else {
          stop("The line is at the bottom")
        }
      } else {
        stop("Invalid entry provided.")
      }
      
    }
    
    # End interactive block
  }

  
  delete.SB.table.project(delete = move,
                          file_location = file_location,
                          file_name = file_name,
                          interactive = FALSE)
  
  add.SB.table.project(position = where,
                       projectName = td_char[move, 1],
                       projectPeople = td_char[move, 2],
                       nextTasks = td_char[move, 3],
                       nextDeliverables = td_char[move, 4],
                       percentComplete = td_char[move, 5],
                       status = td_char[move, 6],
                       file_location = file_location,
                       file_name = file_name,
                       interactive = FALSE)
  
}