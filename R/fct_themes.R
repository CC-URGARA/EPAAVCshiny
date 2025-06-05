# Contient les fonctions de theme (ggplot, datatable, palettes) ---------------------


#' DT_theme
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom DT formatStyle datatable
#'
#' @noRd
DT_theme <- function(tab, theme = "minimal", col_disable_filter = NULL, clickable = FALSE,
                     fixedColumns = NULL){
  #Paramètres par défaut
  searching = FALSE
  paging = TRUE
  lengthChange = FALSE
  pageLength = 50
  bInfo = TRUE
  autoWidth = TRUE
  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')#a tester en ligne
  rownames = FALSE
  escape = TRUE
  selection = "none"
  columnDefs = list()
  filter = "none"
  dom = "lftipr"
  ordering = TRUE
  extensions = list()
  scroller = FALSE
  scrollY = FALSE
  scrollX = FALSE

  #theme "top_filter"
  if(theme[1] == "top_filter"){
    filter = "top"
    dom = "t"
    searching = TRUE
    ordering = FALSE
    columnDefs = list(
      list(targets = col_disable_filter, searchable = FALSE)
    )
  }

  # theme "scrollTable"
  if(theme[1] == "scrollTable"){
    extensions = append(extensions, list("Scroller"))
    scroller = TRUE
    scrollX = TRUE
    pageLength = 10
  }

  #Création de la table
  tab_clean <- datatable(tab,
                         filter = filter,
                         extensions = extensions,
                         options = list(searching = searching, paging = paging, lengthChange = lengthChange,
                                        pageLength = pageLength, bInfo = bInfo, autoWidth = autoWidth,
                                        language = language, columnDefs = columnDefs, dom = dom,
                                        ordering = ordering, scroller = scroller, scrollX = scrollX, scrollY = scrollY),
                         rownames = rownames, escape = escape,
                         selection = selection)

  #Ajout de "formatStyle" si le thème le nécessite
  if(clickable){
    tab_clean = tab_clean %>%
      formatStyle(columns = names(tab), cursor = "pointer")
  }

  return(tab_clean)
}


#' cust_palette
#'
#' @description color palette used in plots and tables
#'
#' @param n number of levels (max = 8)
#' @param alpha Opacity of the color. Must be declared in hexadecimal (FF = 100%, 00 = 0%)
#'
#' @return a color vector
#' @importFrom scales hue_pal
#'
#' @noRd
cust_palette = function(n, alpha = "FF"){
  #definition of the palette
  # list_col = c("#1c6fad", "#e34041", "#aecf38", "#93CDDD",
  #              "#eb9846", "#FFFF33", "#d084db", "#F781BF")
  list_col = hue_pal()(n)

  #Applying the alpha value
  list_col_alpha = paste0(list_col, alpha)

  #Selection of n first colors
  col_vec = list_col_alpha[seq_len(n)]

  return(col_vec)
}


