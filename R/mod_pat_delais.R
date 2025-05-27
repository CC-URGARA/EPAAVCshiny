#' pat_delais UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pat_delais_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Questionnaire patient - D\u00e9lais")
    ),
    mod_selectPop_ui(ns("selectPop"))
  )
}

#' pat_delais Server Functions
#'
#' @noRd
mod_pat_delais_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mod_selectPop_server("selectPop", r_global = r_global)
  })
}

## To be copied in the UI
# mod_pat_delais_ui("pat_delais_1")

## To be copied in the server
# mod_pat_delais_server("pat_delais_1")
