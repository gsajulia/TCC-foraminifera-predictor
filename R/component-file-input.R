styledFileInput <- function(inputId, label = NULL, labelIcon = NULL, multiple = FALSE, 
                       accept = NULL, width = NULL, progress = TRUE, btnStyle = "file-btn", ...) {
  # add class fileinput_2 defined in UI to hide the inputTag
  inputTag <- tags$input(id = inputId, name = inputId, type = "file", 
                         class = "fileinput_2")
  if (multiple) 
      inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0) 
      inputTag$attribs$accept <- paste(accept, collapse = ",")

  div(..., style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"), 
    inputTag,
    # label customized with an action button
    tags$label(`for` = inputId, div(icon(labelIcon), label, 
                class = paste("btn", btnStyle, "action-button", sep=" "))),
    # optionally display a progress bar
    if(progress)
      tags$div(id = paste(inputId, "_progress", sep = ""), 
        class = "progress shiny-file-input-progress", 
        tags$div(class = "progress-bar")
      )
  )
}          
