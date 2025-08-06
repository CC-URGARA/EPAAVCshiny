#' selectPopPatient
#'
#' @description Selection des patients correspondant aux input selection de la pop
#' @importFrom dplyr filter between
#'
#' @return tibble
#'
#' @noRd
selectPopPatient <- function(tabPatient, periodSelect, etabSelect, includNonIsch,
                             ageSelect, includAgeNA, sexeSelect, nihssSelect,
                             includNihssNA, modeArriveSelect, regulSelect,
                             imagerieSelect, stratReperfSelect) {
  #Sélection de la période
  tabPatient <- tabPatient %>% filter(period %in% periodSelect)

  #Sélection de l'établissement
  tabPatient <- tabPatient %>% filter(NOM_ETAB %in% etabSelect)

  #Inclusion des non ischémiques
  if(!includNonIsch) tabPatient <- tabPatient %>% filter(AVCType %in% "AVC Isch\u00e9mique")

  #selection sur l'age
  if(includAgeNA) {#si include
    tabPatient <- tabPatient %>%
      filter(is.na(Age) | between(Age, ageSelect[1], ageSelect[2]))
  } else {#si exclude
    tabPatient <- tabPatient %>%
      filter(between(Age, ageSelect[1], ageSelect[2]))
  }

  #Selection sur le sexe
  if(any(sexeSelect %in% "Manquant")) {
    tabPatient <- tabPatient %>%
      filter(is.na(Sexe) | Sexe %in% sexeSelect)
  } else {
    tabPatient <- tabPatient %>%
      filter(Sexe %in% sexeSelect)
  }

  #Selection sur le NIHSS
  if(includNihssNA) {#si include
    tabPatient <- tabPatient %>%
      filter(is.na(NIHSS) | between(NIHSS, nihssSelect[1], nihssSelect[2]))
  } else {#si exclude
    tabPatient <- tabPatient %>%
      filter(between(NIHSS, nihssSelect[1], nihssSelect[2]))
  }

  #Selection sur le mode d'arrivée
  if(any(modeArriveSelect %in% "Manquant")) {
    tabPatient <- tabPatient %>%
      filter(is.na(mode_arrivee) | mode_arrivee %in% modeArriveSelect)
  } else{
    tabPatient <- tabPatient %>%
      filter(mode_arrivee %in% modeArriveSelect)
  }

  #Selection sur la régulation
  if(any(regulSelect %in% "Manquant")) {
    tabPatient <- tabPatient %>%
      filter(is.na(Regule) | Regule %in% regulSelect)}
  else{
    tabPatient <- tabPatient %>%
      filter(Regule %in% regulSelect)
  }

  #Selection sur l'imagerie
  if(any(imagerieSelect %in% "Manquant")) {
    tabPatient <- tabPatient %>%
      filter(is.na(TypeImagerie1) | TypeImagerie1 %in% imagerieSelect)}
  else{
    tabPatient <- tabPatient %>%
      filter(TypeImagerie1 %in% imagerieSelect)
  }

  #Selection sur la stratégie de reperfusion
  tabPatient = tabPatient %>%
    filter(stratReperf %in% stratReperfSelect)

  return(tabPatient)
}
