#' pat_graph_cust UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyjqui jqui_resizable
#' @importFrom DT DTOutput
mod_pat_graph_cust_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "header",
        h1("Questionnaire patient - Graphique personnalis\u00e9")
    ),
    mod_selectPop_ui(ns("selectPop")),
    box(id = ns("boxCrossPlot"), title = "Graphique personnalis\u00e9",
        width = 12, collapsible = FALSE,
        box(id = ns("boxCrossPlotInput"), title = "Param\u00e8tres du graphique",
            width = 12, collapsible = TRUE, collapsed = FALSE,
            fluidRow(
              column(width = 4,
                     h3("Axes :"),
                     pickerInput(ns("x_var"), label = "Variable en abscisse",
                                 choices = NULL, options = pickerInputOptions_custom()),
                     pickerInput(ns("y_var"), label = "Variable en ordonn\u00e9e",
                                 choices = NULL, options = pickerInputOptions_custom()),
              ),
              column(width = 4,
                     h3("Regroupements :"),
                     pickerInput(ns("group_var"), label = "Variable de regroupement",
                                 choices = NULL, options = pickerInputOptions_custom()),
                     pickerInput(ns("facet_x"), label = "Variable facette en abscisse",
                                 choices = NULL, options = pickerInputOptions_custom()),
                     pickerInput(ns("facet_y"), label = "Variable facette en ordonn\u00e9e",
                                 choices = NULL, options = pickerInputOptions_custom()),
                     sliderInput(ns("n_categ"), label = "Discr\u00e9tisation, nombre de cat\u00e9gories",
                                 min = 2, max = 6, value = 4, step = 1),

              ),
              column(width = 4,
                     h3("Param\u00e8tres suppl\u00e9mentaires :"),
                     checkboxInput(ns("includ_x0"), label = "Inclure le z\u00e9ro sur l\'axe des abscisses ?",
                                   value = FALSE),
                     checkboxInput(ns("includ_y0"), label = "Inclure le z\u00e9ro sur l\'axe des ordonn\u00e9es ?",
                                   value = FALSE),
                     checkboxInput(ns("show_NA"), label = "Afficher les donn\u00e9es manquantes ?",
                                   value = FALSE)
              )
            ),
            actionButton(ns("bttn_generate"), label = "G\u00e9n\u00e9rer le graphique"),
        ),
        box(id = ns("boxCrossPlotOutput"), title = "Graphique",
            width = 12, collapsible = TRUE,
            jqui_resizable(plotOutput(ns("crossPlot")))
        ),
        box(id = ns("boxCrossTableOutput"), title = "Tableau",
            width = 12, collapsible = TRUE,
            DTOutput(ns("crossTable")))
    )
  )
}

#' pat_graph_cust Server Functions
#'
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom labelled var_label
#' @importFrom purrr keep
#' @importFrom stats setNames
#' @importFrom DT renderDT
#' @noRd
mod_pat_graph_cust_server <- function(id, r_global){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    #Initilisation du r_local
    r_local = reactiveValues(
      data = NULL,
      x_var = NULL,
      y_var = NULL,
      group_var = NULL,
      facet_x = NULL,
      facet_y = NULL,
      includ_x0 = NULL,
      includ_y0 = NULL,
      n_categ = NULL,
      show_NA = NULL
    )

    #Serveurs
    mod_selectPop_server("selectPop", r_global = r_global)

    #Update des input à l'aide des données lorsque les données sont crées
    observeEvent(eventExpr = r_global$dataPatient, handlerExpr = {
      listvar_labellized <- var_label(r_global$dataPatient)#extraction des labels
      listvar_labellized <- keep(listvar_labellized, function(x){
        if(!is.null(x)){
          if(!is.na(x)){
            if(nchar(x > 0)) return(TRUE)
          }
        }
        return(FALSE)
      })
      listvar_labellized = setNames(as.list(names(listvar_labellized)), unlist(listvar_labellized))#inversion nom valeur pour correspondre aux attente de pickerinput

      updatePickerInput(inputId = "x_var",
                        choices = listvar_labellized)
      updatePickerInput(inputId = "y_var",
                        choices = c("Aucun" = "", listvar_labellized))
      updatePickerInput(inputId = "group_var",
                        choices = c("Aucun" = "", listvar_labellized))
      updatePickerInput(inputId = "facet_x",
                        choices = c("Aucun" = "", listvar_labellized))
      updatePickerInput(inputId = "facet_y",
                        choices = c("Aucun" = "", listvar_labellized))
    })

    #Mise à jour du r_local sur bttn_generate
    observeEvent(eventExpr = input$bttn_generate, handlerExpr = {
      r_local$data <- r_global$dataPatientSelected
      r_local$x_var <- input$x_var
      r_local$y_var <- input$y_var
      r_local$group_var <- input$group_var
      r_local$facet_x <- input$facet_x
      r_local$facet_y <- input$facet_y
      r_local$includ_x0 <- input$includ_x0
      r_local$includ_y0 <- input$includ_y0
      r_local$n_categ <- input$n_categ
      r_local$show_NA <- input$show_NA
    })

    #Outputs
    output$crossPlot <- renderPlot(expr = {
      validate(
        need(r_local$x_var, message = 'Choisissez vos param\u00e8tres puis cliquez sur \"g\u00e9n\u00e9rer le graphique\"')
      )
      tryCatch({
        plot = fct_crossDynamicPlot(tab = r_local$data, x_var = r_local$x_var, y_var = r_local$y_var,
                                    group_var = r_local$group_var, facet_x = r_local$facet_x, facet_y = r_local$facet_y,
                                    includ_x0 = r_local$includ_x0, includ_y0 = r_local$includ_y0, n_categ = r_local$n_categ,
                                    show_NA = r_local$show_NA)
        plot_logoed = plot_add_logo(plot)#Ajout en dehors de la fonction car sinon les tests unitaires sur le rendu du plot dynamique sont plus compliqués
        plot_logoed
      }, error = function(e) {
        validate(need(FALSE, "Une erreur s\'est produite lors de la g\u00e9n\u00e9ration du graphique. Merci de r\u00e9essayer ou de changer les param\u00e8tres"))
      })
    })

    output$crossTable <- renderDT(expr = {
      req(r_local$x_var)
      tryCatch({
        fct_crossDynamicTable(tab = r_local$data, x_var = r_local$x_var, y_var = r_local$y_var,
                              group_var = r_local$group_var, facet_x = r_local$facet_x, facet_y = r_local$facet_y,
                              n_categ = r_local$n_categ, show_NA = r_local$show_NA)
      }, error = function(e) {
        validate(need(FALSE, "Une erreur s\'est produite lors de la g\u00e9n\u00e9ration du tableau. Merci de r\u00e9essayer ou de changer les param\u00e8tres"))
      })
    })
  })
}

## To be copied in the UI
# mod_pat_graph_cust_ui("pat_graph_cust_1")

## To be copied in the server
# mod_pat_graph_cust_server("pat_graph_cust_1")
