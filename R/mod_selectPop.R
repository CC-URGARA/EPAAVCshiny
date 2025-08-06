#' selectPop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput virtualSelectInput materialSwitch
mod_selectPop_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(id = ns("boxInput"), title = "S\u00e9lection de la population",
        width = 12, collapsible = T, collapsed = TRUE,
        fluidRow(
          column(width = 4,
                 virtualSelectInput(inputId = ns("periodSelect"), label = "P\u00e9riode d\'\u00e9tude",
                                    choices = list(
                                      "2024" = c("Avril" = "Avril 2024", "Octobre" = "Octobre 2024"),
                                      "2025" = c("Avril" = "Avril 2025")),
                                    selected = c("Avril 2024", "Octobre 2024", "Avril 2025"),
                                    showValueAsTags = TRUE,
                                    search = TRUE,
                                    multiple = TRUE),
                 pickerInput(inputId = ns("etabSelect"), label = "Etablissement(s)",
                             choices = NULL, multiple = TRUE, options = pickerInputOptions_custom()),
                 materialSwitch(inputId = ns("includNonIsch"),
                                label = "Inclure les AVC non isch\u00e9miques ?", value = TRUE,
                                status = "primary"),
                 sliderInput(inputId = ns("ageSelect"), label = "\u00c2ge",
                             min = 0, max = 120, value = c(0, 120), step = 1),
                 materialSwitch(inputId = ns("includAgeNA"),
                                label = "Inclure les \u00e2ges manquants ?", value = TRUE,
                                status = "primary")
          ),
          column(width = 4,
                 checkboxGroupInput(inputId = ns("sexeSelect"), label = "Sexe",
                                    choices = c("H" = "Masculin", "F" = "F\u00e9minin", "Manquant"), selected = c("Masculin", "F\u00e9minin", "Manquant"),
                                    inline = TRUE),
                 sliderInput(inputId = ns("nihssSelect"), label = "NIHSS",
                             min = 0, max = 42, value = c(0, 42), step = 1),
                 materialSwitch(inputId = ns("includNihssNA"),
                                label = "Inclure les NIHSS manquants ?", value = TRUE,
                                status = "primary"),
                 pickerInput(inputId = ns("modeArriveSelect"), label = "Mode d\'arriv\u00e9e",
                             choices = NULL, multiple = TRUE, options = pickerInputOptions_custom())

          ),
          column(width = 4,
                 checkboxGroupInput(inputId = ns("regulSelect"), label = "Patient r\u00e9gul\u00e9",
                                    choices = c("Oui", "Non", "Manquant"), selected = c("Oui", "Non", "Manquant"),
                                    inline = TRUE),
                 checkboxGroupInput(inputId = ns("imagerieSelect"), label = "Premi\u00e8re imagerie",
                                    choices = c("IRM", "TDM", "Manquant"), selected = c("IRM", "TDM", "Manquant"),
                                    inline = TRUE),
                 checkboxGroupInput(inputId = ns("stratReperfSelect"), label = "Strat\u00e9gie de reperfusion",
                                    choices = c("Thrombolyse", "Thrombectomie", "Les deux", "Aucune", "Manquant"),
                                    selected = c("Thrombolyse", "Thrombectomie", "Les deux", "Aucune", "Manquant"),
                                    inline = TRUE),
                 actionButton(inputId = ns("bttn_submit"), label = "Valider la selection"),
                 tags$b(textOutput(outputId = ns("Nb_pat"))),

          )
        )
    )
  )
}

#' selectPop Server Functions
#' @importFrom shinyWidgets updateVirtualSelect updatePickerInput updateMaterialSwitch sendSweetAlert
#' @importFrom dplyr if_else
#' @importFrom stats na.omit
#'
#' @noRd
mod_selectPop_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #Update des input déterminés automatiquement sur les données dispo
    observeEvent(r_global$dataPatient, once = TRUE, handlerExpr = {
      updateSliderInput(inputId = "ageSelect",
                        min = min(r_global$dataPatient$Age, na.rm = TRUE),
                        max = max(r_global$dataPatient$Age, na.rm = TRUE))
      updatePickerInput(inputId = "etabSelect", choices = unique(r_global$dataPatient$NOM_ETAB),
                        selected = unique(r_global$dataPatient$NOM_ETAB))
      updatePickerInput(inputId = "modeArriveSelect", choices = na.omit(c(unique(r_global$dataPatient$mode_arrivee), "Manquant")),
                        selected = na.omit(c(unique(r_global$dataPatient$mode_arrivee), "Manquant")))
    })

    #Maj des input on changement sur une autre page
    #=> Se déclanche si une de ces valeurs dans r_global change
    observeEvent(eventExpr = {
      vals = reactiveValuesToList(r_global)
      vals[c("periodSelect", "etabSelect", "includNonIsch", "ageSelect", "includAgeNA",
             "sexeSelect",  "nihssSelect", "includNihssNA", "modeArriveSelect",
             "regulSelect", "imagerieSelect", "stratReperfSelect")]
    }, handlerExpr = {
      if(!is.null(r_global$periodSelect)) updateVirtualSelect(inputId = "periodSelect", selected = r_global$periodSelect)
      if(!is.null(r_global$etabSelect)) updatePickerInput(inputId = "etabSelect", selected = r_global$etabSelect)
      if(!is.null(r_global$includNonIsch)) updateMaterialSwitch(session = getDefaultReactiveDomain(), inputId = "includNonIsch", value = r_global$includNonIsch)
      if(!is.null(r_global$ageSelect)) updateSliderInput(inputId = "ageSelect", value = r_global$ageSelect)
      if(!is.null(r_global$includAgeNA)) updateMaterialSwitch(session = getDefaultReactiveDomain(), inputId = "includAgeNA", value = r_global$includAgeNA)
      if(!is.null(r_global$sexeSelect)) updateCheckboxGroupInput(inputId = "sexeSelect", selected = r_global$sexeSelect)
      if(!is.null(r_global$nihssSelect)) updateSliderInput(inputId = "nihssSelect", value = r_global$nihssSelect)
      if(!is.null(r_global$includNihssNA)) updateMaterialSwitch(session = getDefaultReactiveDomain(), inputId = "includNihssNA", value = r_global$includNihssNA)
      if(!is.null(r_global$modeArriveSelect)) updatePickerInput(inputId = "modeArriveSelect", selected = r_global$modeArriveSelect)
      if(!is.null(r_global$regulSelect)) updateCheckboxGroupInput(inputId = "regulSelect", selected = r_global$regulSelect)
      if(!is.null(r_global$imagerieSelect)) updateCheckboxGroupInput(inputId = "imagerieSelect", selected = r_global$imagerieSelect)
      if(!is.null(r_global$stratReperfSelect)) updateCheckboxGroupInput(inputId = "stratReperfSelect", selected = r_global$stratReperfSelect)
    })

    #on submit maj des valeurs dans le r_global pour diffusion d'un module \u00e0 l'autre
    observeEvent(eventExpr = input$bttn_submit, ignoreInit = TRUE,
                 handlerExpr = {
                   #Maj du setting des input pour communication entre modules
                   r_global$periodSelect <- input$periodSelect
                   r_global$etabSelect <- input$etabSelect
                   r_global$includNonIsch <- input$includNonIsch
                   r_global$ageSelect <- input$ageSelect
                   r_global$includAgeNA <- input$includAgeNA
                   r_global$sexeSelect <- input$sexeSelect
                   r_global$nihssSelect <- input$nihssSelect
                   r_global$includNihssNA <- input$includNihssNA
                   r_global$modeArriveSelect <- input$modeArriveSelect
                   r_global$regulSelect <- input$regulSelect
                   r_global$imagerieSelect <- input$imagerieSelect
                   r_global$stratReperfSelect <- input$stratReperfSelect

                   #Maj des données et nb_pat dans r_global
                   r_global$dataPatientSelected <- selectPopPatient(tabPatient = r_global$dataPatient,
                                                                    periodSelect = input$periodSelect,
                                                                    etabSelect = input$etabSelect,
                                                                    includNonIsch = input$includNonIsch,
                                                                    ageSelect = input$ageSelect,
                                                                    includAgeNA = input$includAgeNA,
                                                                    sexeSelect = input$sexeSelect,
                                                                    nihssSelect = input$nihssSelect,
                                                                    includNihssNA = input$includNihssNA,
                                                                    modeArriveSelect = input$modeArriveSelect,
                                                                    regulSelect = input$regulSelect,
                                                                    imagerieSelect = input$imagerieSelect,
                                                                    stratReperfSelect = input$stratReperfSelect)
                   r_global$Nb_pat = nrow(r_global$dataPatientSelected)
                   #Popup indiquant le nb de patients
                   sendSweetAlert(title = "Population s\u00e9lectionn\u00e9e",
                                  type = if_else(r_global$Nb_pat > 0, "success", "warning"),
                                  text = paste0("La population s\u00e9lectionn\u00e9e contient ", r_global$Nb_pat, " patients")
                                  )

                 })

    #Outputs
    output$Nb_pat <- renderText({
      paste0("Nombre de patients selectionn\u00e9s : ", format(r_global$Nb_pat, big.mark = " "))
      })

  })
}

## To be copied in the UI
# mod_selectPop_ui("selectPop")

## To be copied in the server
# mod_selectPop_server("selectPop")

