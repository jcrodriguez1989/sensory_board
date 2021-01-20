library("dplyr")
library("glue")
library("purrr")
library("readr")
library("shiny")
library("shinythemes")

options(shiny.host = "192.168.100.7") # As shown by `ifconfig`.
options(shiny.port = 4000)

products <- read_csv("productos.csv") # TODO: give as param
attributes <- read_csv("atributos.csv") # TODO: give as param

ui <- fluidPage(
  theme = shinytheme("cyborg"),
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
    dir.create(glue("Answers/{input$username}"), showWarnings = FALSE)
    removeModal()
  })
  
  # Prepare products selector.
  updateSelectInput(session, "product", label = colnames(products)[[1]], choices = products[, 1])
  
  # Prepare attributes inputs.
  output$attributes <- renderUI(
    map(seq_len(nrow(attributes)), function(i) {
      attribute <- attributes[i, ]
      sliderInput(
        make.names(as.character(attribute$Nombre)),
        label = as.character(attribute$Nombre),
        min = 0, max = 10, value = 5, step = .5
      )
    })
  )
  
  # If product selection changed, then restore attributes inputs.
  observeEvent(input$product, {
    req(input$username, input$product)
    map(make.names(as.character(attributes$Nombre)), ~updateSliderInput(session, .x, value = 5))
    if (file.exists(glue("Answers/{input$username}/{input$product}.csv"))) {
      values <- read_csv(glue("Answers/{input$username}/{input$product}.csv"))
      map(
        colnames(values),
        ~updateSliderInput(session, make.names(.x), value = as.numeric(values[, .x]))
      )
    }
  })
  
  # Submit a result.
  observeEvent(input$submit, {
    reactiveValuesToList(input)[make.names(as.character(attributes$Nombre))] %>%
      setNames(as.character(attributes$Nombre)) %>% 
      bind_rows() %>% 
      write_csv(glue("Answers/{input$username}/{input$product}.csv"))
    showNotification("Valuación guardada", type = "message")
  })
}

shinyApp(ui, server)