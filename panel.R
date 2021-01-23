library("dplyr")
library("glue")
library("purrr")
library("readr")
library("shiny")
library("shinyjs")
library("shinythemes")

options(shiny.host = "192.168.100.7") # As shown by `ifconfig`.
options(shiny.port = 4000)

products <- read_csv("productos.csv") # TODO: give as param
attributes <- read_csv("atributos.csv") # TODO: give as param



ui <- fluidPage(
  theme = shinytheme("cyborg"),
  useShinyjs(),
  extendShinyjs(
    text = "shinyjs.scrolltop = function() {window.scrollTo(0, 0)};",
    functions = "scrolltop"
  ),
  selectInput("product", "", choices = NULL),
  uiOutput("attributes"),
  actionButton("submit", "Enviar"),
  align = "center"
)

username_modal <- function(session) {
  showModal(
    modalDialog(
      textInput("username", "Tu nombre (es tu identificador)"),
      actionButton("submitName", "Enviar"),
      align = "center",
      title = "Bienvenida/o",
      footer = NULL,
      size = "s"
    ),
    session
  )
}

create_ui <- function(attribute) {
  type <- trimws(strsplit(attribute$Valores, ":|,")[[1]])
  switch (
    type[[1]],
    Numeric = sliderInput(
      make.names(as.character(attribute$Nombre)),
      label = as.character(attribute$Nombre),
      min = as.numeric(type[[2]]), max = as.numeric(type[[3]]),
      value = (as.numeric(type[[2]]) + as.numeric(type[[3]])) / 2, step = .5
    ),
    Text = selectizeInput(
      make.names(as.character(attribute$Nombre)),
      label = as.character(attribute$Nombre),
      choices = NULL, multiple = TRUE, options = list(create = TRUE)
    )
  )
}

reset_inputs <- function(attributes, session) {
  types <- strsplit(attributes$Valores, ":|,") %>% 
    map(trimws)
  map(seq_along(types), function(i) {
    type <- types[[i]]
    switch (
      type[[1]],
      Numeric = updateSliderInput(
        session, make.names(as.character(attributes$Nombre[[i]])),
        value = (as.numeric(type[[2]]) + as.numeric(type[[3]])) / 2
      ),
      Text = updateSelectizeInput(
        session, make.names(as.character(attributes$Nombre[[i]])), selected = ""
      )
    )
  })
}

set_inputs <- function(values, session) {
  values <- data.frame(values)
  map(seq_along(values), function(i) {
    switch (
      class(values[, i]),
      numeric = updateSliderInput(session, colnames(values)[[i]], value = values[, i]),
      character = updateSelectizeInput(
        session, colnames(values)[[i]], selected = strsplit(values[, i], ", ")[[1]],
        choices = strsplit(values[, i], ", ")[[1]]
      )
    )
  })
}

panel <- c()
server <- function(input, output, session) {
  # Get username.
  username_modal(session)
  observeEvent(input$submitName, {
    if (nchar(input$username) == 0) {
      showNotification("Ingrese su nombre.", type = "error")
      username_modal(session)
      return()
    }
    if (input$username %in% panel) {
      showNotification(
        "El nombre ya había sido seleccionado, asegúrese que no esté repetido.",
        type = "warning"
      )
    }
    panel <<- c(panel, input$username)
    dir.create(glue("Answers/{input$username}"), showWarnings = FALSE, recursive = TRUE)
    removeModal()
  })

  # Prepare products selector.
  updateSelectInput(session, "product", label = colnames(products)[[1]], choices = products[, 1])

  # Prepare attributes inputs.
  output$attributes <- renderUI({
    map(seq_len(nrow(attributes)), function(i) {
      create_ui(attributes[i, ])
    })
  })

  # If product selection changed, then restore attributes inputs.
  observeEvent(c(input$product, input$username), {
    req(input$username, input$product)
    # reset_inputs(attributes, session)
    if (file.exists(glue("Answers/{input$username}/{input$product}.csv"))) {
      values <- read_csv(glue("Answers/{input$username}/{input$product}.csv"))
      set_inputs(values, session)
    }
  })

  # Submit a result.
  observeEvent(input$submit, {
    reactiveValuesToList(input)[make.names(as.character(attributes$Nombre))] %>%
      setNames(as.character(attributes$Nombre)) %>%
      map(~ifelse(!is.numeric(.x), paste(.x, collapse = ", "), .x)) %>% 
      bind_rows() %>%
      write_csv(glue("Answers/{input$username}/{input$product}.csv"))
    js$scrolltop() # Scroll to top.
    showNotification("Valuación guardada", type = "message")
  })
}

shinyApp(ui, server)
