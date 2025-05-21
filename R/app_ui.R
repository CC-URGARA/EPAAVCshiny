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
        title = "EPA'AVC", disable = F,
        tags$li(class = "dropdown", tags$a(href="mailto:supportsi@urgences-ara.fr",
                                           icon("envelope"), target="_blank")),
        tags$li(class = "dropdown", style = "padding: 6px;",
                actionButton(inputId = "logout_bttn", label = "D\u00e9connexion", style = "display: none;")),
        tags$li(class = "dropdown", tags$img(src="www/img/logo_EPA_AVC.png",
                                             height='50',width='150'))
      ),
      sidebar = dashboardSidebar(
        sidebarMenu(
          menuItem("Login", icon = NULL, tabName  = "login_page") |>
            tagAppendAttributes(id = "login_tab"),#Id utilisée pour faire disparaître le menu après login
          menuItem("Accueil", icon = NULL, tabName  = "accueil"),
          menuItem("Structures", icon = NULL, tabName  = "structures"),
          menuItem("Patients", icon = NULL, tabName  = "patients")
        ) |> tagAppendAttributes(hidden = "true", id = "hidden_menu")),
      body = dashboardBody(
        tabItems(
          tabItem(tabName = "login_page",
                  mod_login_page_ui("login_page")),
          tabItem(tabName = "accueil",
                  mod_accueil_ui("accueil")),
          tabItem(tabName = "structures",
                  mod_structures_ui("structures")),
          tabItem(tabName = "patients",
                  mod_patients_ui("patients"))

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
