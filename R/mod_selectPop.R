#' selectPop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput virtualSelectInput
mod_selectPop_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(id = ns("boxInput"), title = "S\u00e9lection de la population",
        width = 12, collapsible = T, collapsed = TRUE,
        fluidRow(
          column(width = 4,
                 # dateRangeInput(inputId = ns("period"), label = "P\u00e9riode d\'\u00e9tude", separator = " \u00e0 "),
                 virtualSelectInput(inputId = ns("period"), label = "P\u00e9riode d\'\u00e9tude",
                                    choices = list(
                                      "2024" = c("A", "B"),
                                      "2025" = c("A", "B")),
                                    showValueAsTags = TRUE,
                                    search = TRUE,
                                    multiple = TRUE),
                 pickerInput(inputId = ns("etabSelect"), label = "Etablissement(s)",
                             choices = c("A", "B"), multiple = TRUE, options = pickerInputOptions_custom()),
                 checkboxInput(inputId = ns("includNonIsch"), label = "Inclure les AVC non isch\u00e9miques ?", value = TRUE),
                 sliderInput(inputId = ns("age"), label = "\u00c2ge",
                             min = 0, max = 120, value = c(0, 120), step = 1),
                 checkboxInput(inputId = ns("includAgeNA"), label = "Inclure les \u00e2ges manquants ?", value = TRUE)
          ),
          column(width = 4,
                 checkboxGroupInput(inputId = ns("sexe"), label = "Sexe",
                                    choices = c("H", "F", "Manquant"), selected = c("H", "F", "Manquant"),
                                    inline = TRUE),
                 sliderInput(inputId = ns("nihss"), label = "NIHSS",
                             min = 0, max = 42, value = c(0, 42), step = 1),
                 checkboxInput(inputId = ns("includNihssNA"), label = "Inclure les NIHSS manquants ?", value = TRUE),
                 pickerInput(inputId = ns("modeArrive"), label = "Mode d\'arriv\u00e9e",
                             choices = c("A", "B"), multiple = TRUE, options = pickerInputOptions_custom())

          ),
          column(width = 4,
                 checkboxGroupInput(inputId = ns("regul"), label = "Patient r\u00e9gul\u00e9",
                                    choices = c("Oui", "Non", "Manquant"), selected = c("Oui", "Non", "Manquant"),
                                    inline = TRUE),
                 checkboxGroupInput(inputId = ns("imagerie"), label = "Imagerie",
                                    choices = c("IRM", "TDM", "Manquant"), selected = c("IRM", "TDM", "Manquant"),
                                    inline = TRUE),
                 checkboxGroupInput(inputId = ns("stratReperf"), label = "Strat\u00e9gie de reperfusion",
                                    choices = c("Thrombolyse", "Thrombectomie", "Aucune", "Manquant"),
                                    selected = c("Thrombolyse", "Thrombectomie", "Aucune", "Manquant"),
                                    inline = TRUE)

          )
        )
    )
  )
}

#' selectPop Server Functions
#'
#' @noRd
mod_selectPop_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_selectPop_ui("selectPop")

## To be copied in the server
# mod_selectPop_server("selectPop")
