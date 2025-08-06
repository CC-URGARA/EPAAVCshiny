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
    mod_selectPop_ui(ns("selectPop")),
    box(id = ns("boxplotDelaisOutput"), title = "Graphique",
        width = 12, collapsible = TRUE,
        jqui_resizable(plotOutput(ns("plotDelais")))
    )
  )
}

#' pat_delais Server Functions
#'
#' @noRd
mod_pat_delais_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mod_selectPop_server("selectPop", r_global = r_global)


    #Outputs
    output$plotDelais <- renderPlot(expr = {
      tryCatch({
        plot = fct_delaisPlot(tab = r_global$dataPatientSelected)
        plot_logoed = plot_add_logo(plot)#Ajout en dehors de la fonction car sinon les tests unitaires sur le rendu du plot dynamique sont plus compliqués
        plot_logoed
      }, error = function(e) {
        validate(need(FALSE, "Une erreur s\'est produite lors de la g\u00e9n\u00e9ration du graphique. Merci de nous contacter."))
      })
    })
  })
}

## To be copied in the UI
# mod_pat_delais_ui("pat_delais_1")

## To be copied in the server
# mod_pat_delais_server("pat_delais_1")
