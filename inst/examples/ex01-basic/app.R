library(shinypanels)
library(dsmodules)

ui <- panelsPage(
  panel(
    title = "First Panel",
    width = 300,
    body =  h3("First panel"),
    collapsed = TRUE,
  ),
  panel(
    title = "Viz",
    title_plugin = downloadHtmlwidgetUI("download_html", dropdownLabel = "Download", formats = "html", display = "dropdown"),
    body = reactable::reactableOutput("rctbl")
  )
)

org_name <- "public"
data <- data.frame(a = c('b', 'c'), b = c('https://google.com', 'https://google.co.uk'))

opts <- dsthemer::dsthemer_get(org_name)
opts$logo <- org_name
opts$logo_width <- 200
opts$title <- "some title"
opts$subtitle <- "and a subtitle"
opts$caption <- "blahblah"

server <-  function(input, output, session) {

  table <- reactive({do.call(rctb, list(data, opts))})

  output$rctbl <- reactable::renderReactable(table())

  downloadHtmlwidgetServer("download_html", element = table(), formats = "html")
}


shinyApp(ui, server)
