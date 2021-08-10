test_that("rctb works without opts", {

  data <- data.frame(x = 1, y = "b", stringsAsFactors = FALSE)
  tbl <- rctb(data)
  name <- tbl$x$tag$children[[1]]$name
  attribs <- tbl$x$tag$children[[1]]$attribs

  data <- jsonlite::toJSON(data, dataframe = "columns", rownames = FALSE)
  columns <- list(
    list(accessor = "x", name = "x", type = "numeric", cell = list("1")),
    list(accessor = "y", name = "y", type = "character", cell = list("b"))
  )

  expected_opts <- dsvizopts::merge_dsviz_options()
  expected_theme <- list(backgroundColor = expected_opts$theme$table_background,
                         stripedColor = expected_opts$theme$zebra_stripe_color,
                         style = list(fontFamily = expected_opts$theme$cell_text_family,
                                      fontSize = expected_opts$theme$cell_text_size,
                                      fontWeight = expected_opts$theme$cell_text_weight,
                                      color = expected_opts$theme$cell_text_color),
                         headerStyle = list(fontFamily = expected_opts$theme$header_text_family,
                                            fontSize = expected_opts$theme$header_text_size,
                                            fontWeight = expected_opts$theme$header_text_weight,
                                            color = expected_opts$theme$header_text_color,
                                            backgroundColor = expected_opts$theme$header_background))

  class(expected_theme) <- "reactableTheme"

  expected <- list(
    data = data,
    columns = columns,
    defaultPageSize = 10,
    paginationType = "numbers",
    showPageInfo = TRUE,
    minRows = 1,
    borderless = TRUE,
    striped = TRUE,
    showSortable = TRUE,
    theme = expected_theme,
    dataKey = digest::digest(list(data, columns))
  )
  expect_equal(name, "Reactable")
  expect_equal(attribs, expected)
  expect_equal(tbl$width, "auto")
  expect_equal(tbl$height, "auto")
  expect_null(tbl$elementId)

})

test_that("rctb works with opts", {

  org_name <- "public"
  data <- data.frame(x = 1, y = "b", stringsAsFactors = FALSE)
  opts <- dsthemer::dsthemer_get(org_name)
  opts$zebra_stripe_color <- "#ffffff"
  opts$striped <- FALSE

  tbl <- do.call(rctb, list(data, opts))
  attribs <- tbl$x$tag$children[[1]]$attribs
  name <- tbl$x$tag$children[[1]]$name

  data <- jsonlite::toJSON(data, dataframe = "columns", rownames = FALSE)
  columns <- list(
    list(accessor = "x", name = "x", type = "numeric", cell = list("1")),
    list(accessor = "y", name = "y", type = "character", cell = list("b"))
  )

  expected_opts <- dsvizopts::merge_dsviz_options()
  expected_theme <- list(backgroundColor = expected_opts$theme$table_background,
                         stripedColor = "#ffffff",
                         style = list(fontFamily = expected_opts$theme$cell_text_family,
                                      fontSize = expected_opts$theme$cell_text_size,
                                      fontWeight = expected_opts$theme$cell_text_weight,
                                      color = expected_opts$theme$cell_text_color),
                         headerStyle = list(fontFamily = expected_opts$theme$header_text_family,
                                            fontSize = expected_opts$theme$header_text_size,
                                            fontWeight = expected_opts$theme$header_text_weight,
                                            color = expected_opts$theme$header_text_color,
                                            backgroundColor = expected_opts$theme$header_background))

  class(expected_theme) <- "reactableTheme"

  expected <- list(
    data = data,
    columns = columns,
    defaultPageSize = 10,
    paginationType = "numbers",
    showPageInfo = TRUE,
    minRows = 1,
    borderless = TRUE,
    showSortable = TRUE,
    theme = expected_theme,
    dataKey = digest::digest(list(data, columns))
  )
  expect_equal(name, "Reactable")
  expect_equal(attribs, expected)
  expect_equal(tbl$width, "auto")
  expect_equal(tbl$height, "auto")
  expect_null(tbl$elementId)

})

test_that("rctb works with extra elements", {

  org_name <- "public"
  data <- data.frame(x = 1, y = "b", stringsAsFactors = FALSE)
  # data <- sample_data("Cat-Num")
  opts <- dsthemer::dsthemer_get(org_name)
  opts$title <- 'a title'
  opts$subtitle <- 'a subtitle'
  opts$caption <- 'a caption'
  opts$logo <- org_name

  tbl <- do.call(rctb, list(data, opts))
  attribs <- tbl$x$tag$children[[3]]$attribs
  name <- tbl$x$tag$children[[3]]$name

  data <- jsonlite::toJSON(data, dataframe = "columns", rownames = FALSE)
  columns <- list(
    list(accessor = "x", name = "x", type = "numeric", cell = list("1")),
    list(accessor = "y", name = "y", type = "character", cell = list("b"))
  )

  expected_opts <- dsvizopts::merge_dsviz_options()
  expected_theme <- list(backgroundColor = expected_opts$theme$table_background,
                         stripedColor = expected_opts$theme$zebra_stripe_color,
                         style = list(fontFamily = expected_opts$theme$cell_text_family,
                                      fontSize = expected_opts$theme$cell_text_size,
                                      fontWeight = expected_opts$theme$cell_text_weight,
                                      color = expected_opts$theme$cell_text_color),
                         headerStyle = list(fontFamily = expected_opts$theme$header_text_family,
                                            fontSize = expected_opts$theme$header_text_size,
                                            fontWeight = expected_opts$theme$header_text_weight,
                                            color = expected_opts$theme$header_text_color,
                                            backgroundColor = expected_opts$theme$header_background))

  class(expected_theme) <- "reactableTheme"

  expected <- list(
    data = data,
    columns = columns,
    defaultPageSize = 10,
    paginationType = "numbers",
    showPageInfo = TRUE,
    minRows = 1,
    borderless = TRUE,
    striped = TRUE,
    showSortable = TRUE,
    theme = expected_theme,
    dataKey = digest::digest(list(data, columns))
  )
  expect_equal(name, "Reactable")
  expect_equal(attribs, expected)
  expect_equal(tbl$width, "auto")
  expect_equal(tbl$height, "auto")
  expect_null(tbl$elementId)

})

test_that("extra elements created correctly", {

  org_name <- "public"
  data <- data.frame(x = 1, y = "b", stringsAsFactors = FALSE)
  opts <- dsthemer::dsthemer_get(org_name)
  opts$title <- 'a title'
  opts$subtitle <- 'a subtitle'
  opts$caption <- 'a caption'
  opts$logo <- org_name

  tbl <- do.call(rctb, list(data, opts))

  # test if correct id
  expected_title_id <- "reactable-title"
  title_id <- tbl$x$tag$children[[1]]$attribs$id

  expected_subtitle_id <- "reactable-subtitle"
  subtitle_id <- tbl$x$tag$children[[2]]$attribs$id

  expected_caption_id <- "reactable-caption"
  caption_id <- tbl$x$tag$children[[4]]$attribs$id

  expected_logo_id <- "reactable-logo"
  logo_id <- tbl$x$tag$children[[5]]$attribs$id

  expect_equal(title_id, expected_title_id)
  expect_equal(subtitle_id, expected_subtitle_id)
  expect_equal(caption_id, expected_caption_id)
  expect_equal(logo_id, expected_logo_id)

  # test if correct text
  expected_title_text <- "a title"
  title_text <- tbl$x$tag$children[[1]]$children[[1]]

  expected_subtitle_text <- "a subtitle"
  subtitle_text <- tbl$x$tag$children[[2]]$children[[1]]

  expected_caption_text <- "a caption"
  caption_text <- tbl$x$tag$children[[4]]$children[[1]]

  expect_equal(title_text, expected_title_text)
  expect_equal(subtitle_text, expected_subtitle_text)
  expect_equal(caption_text, expected_caption_text)

  # test logo
  expected_logo_name <- "img"
  logo_name <- tbl$x$tag$children[[5]]$children[[1]]$name

  expected_src_class <- "character"
  src_class <- class(tbl$x$tag$children[[5]]$children[[1]]$attribs$src)

  expect_equal(logo_name, expected_logo_name)
  expect_equal(src_class, expected_src_class)

})

test_that("import google fonts", {

  data <- data.frame(x = 1, y = "b", stringsAsFactors = FALSE)
  tbl <- rctb(data)

  expected_import <- "@import url('https://fonts.googleapis.com/css?family=IBM+Plex+Sans');"
  import <- tbl$x$tag$children[[2]]$children[[1]]

  expect_equal(import, expected_import)

})

test_that("style tags", {

  data <- data.frame(x = 1, y = "b", stringsAsFactors = FALSE)
  tbl <- rctb(data)

  elementStyles <- create_element_styles()

  expected_styles <- paste0(
    "body {background-color: #ffffff;} #reactable-title {",elementStyles$titleStyle,"} #reactable-subtitle {",elementStyles$subtitleStyle,"} #reactable-caption {",elementStyles$captionStyle,"} #reactable-logo img {",elementStyles$logoStyle,"}")

  styles <- tbl$x$tag$children[[3]]$children[[1]]

  expect_equal(styles, expected_styles)

})
