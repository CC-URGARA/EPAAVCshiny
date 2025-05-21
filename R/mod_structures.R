#' structures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_structures_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Questionnaire structures")
    )
  )
}

#' structures Server Functions
#'
#' @noRd
mod_structures_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_structures_ui("structures_1")

## To be copied in the server
# mod_structures_server("structures_1")
