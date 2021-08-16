#' Get html logo without styles
#'
#' @param opts_theme Theme options from dsvizopts
#'
#' @return HTML object for logo
get_html_logo <- function(opts_theme){

  if (!opts_theme$branding_include) return("")

  logo_path <- url_logo(logo = opts_theme$logo,
                        background_color = opts_theme$background_color)
  htmltools::img(src = logo_path)
}

#' Get css style for html logo
#'
#' @param opts_theme Theme options from dsvizopts
#'
#' @return String of CSS styles for logo
get_logo_style <- function(opts_theme){
  logo_width <- opts_theme$logo_width
  logo_height <- opts_theme$logo_height

  style <- "float: right;margin-right:10px;"
  if(!is.null(logo_width)){
    style <- paste0(style, 'width:', logo_width, 'px;')
  }
  if(!is.null(logo_height)){
    style <- paste0(style, 'height:', logo_height, 'px;')
  }
  style
}


url_logo <- function(logo, background_color) {
  isUrl <- grepl("http", logo)
  if (isUrl) logo_url <- logo
  if (grepl("/", logo) & !isUrl) {
    logo_path <- logo
  } else {
    logo_path <- dsvizopts::local_logo_path(logo, background_color)
  }
  logo_url <- knitr::image_uri(f = logo_path)
  logo_url
}
