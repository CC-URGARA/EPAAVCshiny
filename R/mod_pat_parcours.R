#' pat_parcours UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pat_parcours_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Questionnaire patient - Parcours")
    ),
    mod_selectPop_ui(ns("selectPop"))
  )
}

#' pat_parcours Server Functions
#'
#' @noRd
mod_pat_parcours_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mod_selectPop_server("selectPop", r_global = r_global)
  })
}

## To be copied in the UI
# mod_pat_parcours_ui("pat_parcours_1")

## To be copied in the server
# mod_pat_parcours_server("pat_parcours_1")
