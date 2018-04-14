library('markdown')
library('knitr')

# actionButton with dark color
col.rev.actionButton = function (inputId, label, icon = NULL) {
  if (!is.null(icon)) 
    buttonContent <- list(icon, label)
  else buttonContent <- label
  tags$button(id = inputId, type = "button", class = "btn btn-primary action-button", 
              buttonContent)
}
