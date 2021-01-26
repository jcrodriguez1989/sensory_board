#' Sensory Board Panel
#'
#' @param products_file A character path to the csv file containing the products to evaluate.
#' @param attributes_file A character path to the csv file containing the attributes to evaluate.
#' @param answers_dir A character path to the folder in which to save user responses (if it does not
#'   exist, it will create it).
#' @param dest_url An optional character including the URL to use as destination host and port.
#'   For example: 192.168.100.7:4000 .
#' @param numeric_range A numeric vector indicating the range for numeric inputs.
#'
#' @importFrom dplyr `%>%` bind_rows
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom readr cols read_csv write_csv
#' @importFrom shiny actionButton fluidPage getQueryString modalDialog reactiveValuesToList
#' @importFrom shiny removeModal renderUI selectInput selectizeInput shinyApp showModal
#' @importFrom shiny showNotification sliderInput textInput
#' @importFrom shiny uiOutput updateQueryString updateSelectInput updateSelectizeInput
#' @importFrom shiny updateSliderInput updateTextInput
#' @importFrom shinyjs extendShinyjs js useShinyjs
#' @importFrom shinythemes shinytheme
#' @importFrom stats setNames
#'
#' @export
#'
run_panel <- function(
                      products_file, attributes_file, answers_dir = "Answers", dest_url = NULL,
                      numeric_range = c(0, 10)) {
  # Set default host/port, if not provided as `dest_url`.
  host <- getOption("shiny.host", "127.0.0.1")
  port <- getOption("shiny.port")
  if (!is.null(dest_url)) {
    dest_url <- strsplit(dest_url, ":")[[1]]
    if (length(dest_url) != 2) {
      stop("`dest_url` should follow the format HOST:PORT , for example, 192.168.100.7:4000 .")
    }
    host <- dest_url[[1]]
    port <- as.numeric(dest_url[[2]])
  }

  # Load configuration files.
  products <- read_csv(products_file, col_types = cols())
  attributes <- read_csv(attributes_file, col_types = cols())

  ui <- fluidPage(
    # Set a dark theme.
    theme = shinytheme("cyborg"),
    # Use shinyjs, to add the function `shinyjs.scrolltop` - scrolls to the top of the window).
    useShinyjs(),
    extendShinyjs(
      text = "shinyjs.scrolltop = function() {window.scrollTo(0, 0)};",
      functions = "scrolltop"
    ),
    # Selector of the product to evaluate.
    selectInput("product", "", choices = NULL),
    # UI for the different attributes inputs.
    uiOutput("attributes"),
    actionButton("submit", "Enviar"),
    align = "center"
  )

  # Shows the modal in which to enter the "username".
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

  # Creates the UI for each attribute.
  create_ui <- function(attribute, numeric_range) {
    type <- trimws(strsplit(attribute$Valores, ":|,")[[1]])
    switch(
      type[[1]],
      Numeric = sliderInput(
        make.names(as.character(attribute$Nombre)),
        label = as.character(attribute$Nombre),
        min = numeric_range[[1]], max = numeric_range[[2]],
        value = mean(numeric_range), step = .5
      ),
      Text = selectizeInput(
        make.names(as.character(attribute$Nombre)),
        label = as.character(attribute$Nombre),
        choices = NULL, multiple = TRUE, options = list(create = TRUE)
      )
    )
  }

  # Sets the values for attributes UI.
  set_inputs <- function(values, session) {
    values <- data.frame(values)
    map(seq_along(values), function(i) {
      switch(
        class(values[, i]),
        numeric = updateSliderInput(session, colnames(values)[[i]], value = values[, i]),
        character = updateSelectizeInput(
          session, colnames(values)[[i]],
          selected = strsplit(values[, i], ", ")[[1]],
          choices = strsplit(values[, i], ", ")[[1]]
        )
      )
    })
  }

  # Logged panelists.
  panel <- c()
  # Set server side functionality.
  server <- function(input, output, session) {
    # Get username.
    observeEvent(
      getQueryString()$user,
      # Try to get the username from the query string.
      updateTextInput(session, "username", value = getQueryString()$user)
    )
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
      panel <<- unique(c(panel, input$username))
      dir.create(glue("{answers_dir}/{input$username}"), showWarnings = FALSE, recursive = TRUE)
      # Add username to the query string, so if they update, it will remember it.
      updateQueryString(glue("?user={input$username}"), mode = "replace")
      removeModal()
    })

    # Prepare products selector.
    updateSelectInput(session, "product", label = colnames(products)[[1]], choices = products[, 1])

    # Prepare attributes inputs.
    output$attributes <- renderUI({
      map(seq_len(nrow(attributes)), function(i) {
        create_ui(attributes[i, ], numeric_range)
      })
    })

    # If product selection changed, then restore attributes inputs (if previously saved).
    observeEvent(c(input$product, input$username), {
      req(input$username, input$product)
      if (file.exists(glue("{answers_dir}/{input$username}/{input$product}.csv"))) {
        glue("{answers_dir}/{input$username}/{input$product}.csv") %>%
          read_csv(col_types = cols()) %>%
          set_inputs(session)
      }
    })

    # Submit a result.
    observeEvent(input$submit, {
      # Get the attributes inputs.
      reactiveValuesToList(input)[make.names(as.character(attributes$Nombre))] %>%
        setNames(as.character(attributes$Nombre)) %>%
        # If they are text, paste them with commas.
        map(~ ifelse(!is.numeric(.x), paste(.x, collapse = ", "), .x)) %>%
        bind_rows() %>%
        write_csv(glue("{answers_dir}/{input$username}/{input$product}.csv"))
      js$scrolltop() # Scroll to top.
      showNotification("Valuación guardada", type = "message")
    })
  }

  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}
