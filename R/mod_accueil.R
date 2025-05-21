#' accueil UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_accueil_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Accueil")
    ),
    box(id = ns("box1"), title = "Lorem",
        width = 12, collapsible = T,
        "Lorem Ipsum"),
    box(id = ns("box2"), title = "Ipsum",
        width = 12, collapsible = T,
        "Lorem Ipsum")
  )
}

#' accueil Server Functions
#'
#' @noRd
mod_accueil_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_accueil_ui("accueil_1")

## To be copied in the server
# mod_accueil_server("accueil_1")
