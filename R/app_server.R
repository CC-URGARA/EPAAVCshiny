#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom golem invoke_js
#' @importFrom RSQLite dbConnect dbDisconnect
#' @noRd
app_server <- function(input, output, session) {
  #Authentification
  passphrase <- keyring::key_get(service = "shinymanager", username = "EPAAVCshiny")
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = system.file("DB", "credentials.sqlite", package = "EPAAVCshiny"),
      passphrase = passphrase
    )
  )

  # Your application server logic
  # Initialisation du r_global
  r_global = reactiveValues(
    periodSelect = NULL,
    etabSelect = NULL,
    includNonIsch = NULL,
    ageSelect = NULL,
    includAgeNA = NULL,
    sexeSelect = NULL,
    nihssSelect = NULL,
    includNihssNA = NULL,
    modeArriveSelect = NULL,
    regulSelect = NULL,
    imagerieSelect = NULL,
    stratReperfSelect = NULL,
    dataPatient = readRDS("Y:/EPA_AVC/datastep application shiny/data application/patients.rds")#données totales
  )
  observeEvent(eventExpr = r_global$dataPatient, once = TRUE, handlerExpr = {#val initiales
    r_global$Nb_pat = nrow(r_global$dataPatient)
    r_global$dataPatientSelected = r_global$dataPatient#Données sélectionnée dans les analyses
  })


  #Appel serveurs
  mod_accueil_server("accueil", r_global = r_global)
  mod_structures_server("structures", r_global = r_global)
  mod_pat_delais_server("pat_delais", r_global = r_global)
  # mod_pat_parcours_server("pat_parcours", r_global = r_global)
  mod_pat_graph_cust_server("pat_graph_cust", r_global = r_global)
}
