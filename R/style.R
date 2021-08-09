#' Create rctbtheme
#'
#' Create extra styling for fonts and colors of reactable.
#'
#' @param opts_theme Theme options from dsvizopts
#'
#' @return reactableTheme
create_rctbtheme <- function(opts_theme = NULL){

  if(is.null(opts_theme)){
    opts <- dsvizopts::dsviz_default_opts()
    opts_theme <- opts$theme
  }

  headerStyle <- list(fontFamily = opts_theme$header_text_family,
                      fontSize = opts_theme$header_text_size,
                      fontWeight = opts_theme$header_text_weight,
                      color = opts_theme$header_text_color,
                      backgroundColor = opts_theme$header_background)

  style <- list(fontFamily = opts_theme$cell_text_family,
                fontSize = opts_theme$cell_text_size,
                fontWeight = opts_theme$cell_text_weight,
                color = opts_theme$cell_text_color)

  reactable::reactableTheme(stripedColor = opts_theme$zebra_stripe_color,
                            backgroundColor = opts_theme$table_background,
                            style = style,
                            headerStyle = headerStyle)

}


#' Create link style
#'
#' Create css style of hyperlinks in table.
#'
#' @param opts_theme Theme options from dsvizopts
#'
#' @return String of css style
create_link_style <- function(opts_theme = NULL){
  # font_import <- get_font_import(opts_theme = theme)
  #
  # shiny::tags$head(shiny::tags$style(font_import,
                                     # shiny::HTML(link_color_css)))

  if(is.null(opts_theme)){
    opts <- dsvizopts::dsviz_default_opts()
    opts_theme <- opts$theme
  }

  link_color_css <- paste0("a {color: ",opts_theme$link_color,"}")
  shiny::tags$head(shiny::tags$style(shiny::HTML(link_color_css)))
}


#' Create element styles
#'
#' Create css styles for extra elements title, subtitle, caption and logo.
#'
#' @param opts_theme Theme options from dsvizopts
#'
#' @return Named list of strings of css styles
create_element_styles <- function(opts_theme = NULL){

  if(is.null(opts_theme)){
    opts <- dsvizopts::dsviz_default_opts()
    opts_theme <- opts$theme
  }

  titleStyle <- paste0("color:", opts_theme$table_title_color, ";",
                       "font-family:", opts_theme$table_title_family, ";",
                       "font-size:", opts_theme$table_title_size, "px;",
                       "font-weight:", opts_theme$table_title_weight, ";",
                       "margin: 2px;",
                       "text-align: left;")

  subtitleStyle <- paste0("color:", opts_theme$table_subtitle_color, ";",
                          "font-family:", opts_theme$table_subtitle_family, ";",
                          "font-size:", opts_theme$table_subtitle_size, "px;",
                          "font-weight:", opts_theme$table_subtitle_weight, ";",
                          "margin-left: 2px;",
                          "margin-top: 4px;",
                          "margin-bottom: 25px;",
                          "text-align: left;")

  captionStyle <- paste0("color:", opts_theme$table_caption_color, ";",
                         "font-family:", opts_theme$table_caption_family, ";",
                         "font-size:", opts_theme$table_caption_size, "px;",
                         "font-weight:", opts_theme$table_caption_weight, ";",
                         "margin-left: 2px;",
                         "margin-top: 10px;",
                         "text-align: left;")

  logoStyle <- get_logo_style(opts_theme = opts_theme)

  list(titleStyle = titleStyle,
       subtitleStyle = subtitleStyle,
       captionStyle = captionStyle,
       logoStyle = logoStyle)
}


#' Get font names
#'
#' Get names of google fonts in theme options. Searches for all
#' parameters in theme options with term `family`
#'
#' @param opts_theme Theme options from dsvizopts
#'
#' @return Vector of strings of font names
get_font_names <- function(opts_theme = NULL){

  if(is.null(opts_theme)){
    opts <- dsvizopts::dsviz_default_opts()
    opts_theme <- opts$theme
  }

  opts_fonts <- opts_theme[grepl("family", names(opts_theme))]
  opts_fonts <- Filter(Negate(is.null), opts_fonts)
  unique(as.character(opts_fonts))
}
