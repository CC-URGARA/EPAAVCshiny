#' patients UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_patients_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Questionnaire patient")
    )
  )
}

#' patients Server Functions
#'
#' @noRd
mod_patients_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_patients_ui("patients_1")

## To be copied in the server
# mod_patients_server("patients_1")
