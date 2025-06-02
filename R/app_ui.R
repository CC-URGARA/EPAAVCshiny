#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      header = dashboardHeader(
        title = "EPA'AVC", disable = FALSE,
        tags$li(class = "dropdown", tags$a(href="mailto:supportsi@urgences-ara.fr",
                                           icon("envelope"), target="_blank")),
        tags$li(class = "dropdown", tags$img(src="www/img/Logo_UrgAra_Long.png",
                                             height='50',width='175')),
        tags$li(class = "dropdown", tags$img(src="www/img/logo_EPA_AVC.png",
                                             height='50',width='150'))
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Accueil", icon = NULL, tabName  = "accueil"),
          menuItem("Structures", icon = NULL, tabName  = "structures"),
          menuItem("Patients", icon = NULL, tabName  = "patients",
                   menuSubItem("D\u00e9lais", icon = NULL, tabName = "pat_delais"),
                   menuSubItem("Parcours", icon = NULL, tabName = "pat_parcours"),
                   menuSubItem("Graphique personnalis\u00e9", icon = NULL, tabName = "pat_graph_cust")
          )
        )),
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "accueil",
                  mod_accueil_ui("accueil")),
          tabItem(tabName = "structures",
                  mod_structures_ui("structures")),
          tabItem(tabName = "pat_delais",
                  mod_pat_delais_ui("pat_delais")),
          tabItem(tabName = "pat_parcours",
                  mod_pat_parcours_ui("pat_parcours")),
          tabItem(tabName = "pat_graph_cust",
                  mod_pat_graph_cust_ui("pat_graph_cust"))
        ))
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "EPAAVCshiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
