#' Get hyperlink function
#'
#' Creates function to format hyperlinks in reactable
#'
#' @param opts Options from dsvizopts
#'
#' @return Function passed to reactable cells
get_hyperlink_function <- function(opts){

  link_style <- create_link_style(opts_theme = opts$theme)

  linksAsHyperlinks <- opts$table$linksAsHyperlinks
  if(is.null(linksAsHyperlinks)) linksAsHyperlinks <- TRUE

  function(value, index, name) {
    NULL
    if(!is.null(data[index, name])){
      if (linksAsHyperlinks & grepl("^www\\.|^http(s|)://", data[index, name])) {
        list(link_style,
             shiny::tags$a(href = value, target = "_blank", value))
      } else {
        value
      }
    }
  }
}
