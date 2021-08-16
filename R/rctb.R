#' Create rctb table
#'
#' Wrapper function for reactable tables from the reactable package.
#'
#' @param data Data frame or tibble.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' org_name <- "public"
#' data <- data.frame(a = c('b', 'c'), b = c('https://google.com', 'https://google.co.uk'))
#'
#' opts <- dsthemer::dsthemer_get(org_name)
#' opts$logo <- org_name
#' opts$logo_width <- 200
#' opts$title <- "a title"
#' opts$subtitle <- "a subtitle"
#' opts$caption <- "a caption"
#'
#' do.call(rctb, list(data, opts))
rctb <- function(data, ...){

  opts <- dsvizopts::merge_dsviz_options(...)
  opts_table <- opts$table
  opts_theme <- opts$theme

  # format hyperlinks
  hyperlinks <- get_hyperlink_function(opts = opts, data = data)
  opts_table$defaultColDef <- reactable::colDef(cell = hyperlinks)

  # create extra styles for reactable
  rctbtheme <- create_rctbtheme(opts_theme = opts_theme)

  # create css styles for extra elements (title, subtitle, caption, logo)
  elementStyles <- create_element_styles(opts_theme = opts_theme)

  # get logo as html img

  logo <- NULL
  if(opts_theme$branding_include){
    if(grepl("dsthemer", opts_theme$logo)) opts_theme$logo <- gsub("/.*", "", gsub(".*logos/","",opts_theme$logo))
    logo <- get_html_logo(opts_theme = opts_theme)
    logo <- list(logo)
  }

  # get google fonts needed for import
  fonts <- get_font_names(opts_theme = opts_theme)

  # remove params not accepted by reactable
  opts_table <- opts_table[!names(opts_table) %in% c("linksAsHyperlinks")]

  # create all params (with data)
  params <- c(data = list(data),
              title = opts$title$title,
              subtitle = opts$title$subtitle,
              caption = opts$title$caption,
              logo = logo,
              titleStyle = elementStyles$titleStyle,
              subtitleStyle = elementStyles$subtitleStyle,
              captionStyle = elementStyles$captionStyle,
              logoStyle = elementStyles$logoStyle,
              backgroundStyle = elementStyles$backgroundStyle,
              googlefonts = list(fonts),
              opts_table,
              theme = list(rctbtheme))

  do.call(reactable::reactable, params)
}

