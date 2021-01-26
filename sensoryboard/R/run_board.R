#' Sensory Board
#'
#' @param answers_dir A character path to the folder from which to load user responses.
#' @param dest_url An optional character including the URL to use as destination host and port.
#'   For example: 192.168.100.7:4001 .
#' @param panel_url An optional character including the panel's URL to show it as a QR code.
#'   For example: 192.168.100.7:4000 .
#' @param numeric_range A numeric vector indicating the range for numeric inputs.
#'
#' @importFrom dplyr `%>%` arrange as_tibble bind_rows filter group_by group_map if_else mutate n
#' @importFrom dplyr pull select slice_max summarise_if tibble
#' @importFrom DT datatable dataTableOutput renderDataTable
#' @importFrom ggplot2 aes coord_cartesian element_text facet_wrap geom_boxplot geom_text geom_tile
#' @importFrom ggplot2 ggplot scale_color_gradient scale_size_area scale_y_continuous theme theme_bw
#' @importFrom ggplot2 theme_minimal theme_void xlab ylab
#' @importFrom ggradar ggradar
#' @importFrom ggwordcloud geom_text_wordcloud
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette
#' @importFrom purrr map negate
#' @importFrom qrcode qrcode_gen
#' @importFrom readr cols read_csv
#' @importFrom shiny br checkboxInput column conditionalPanel fluidPage fluidRow h1 hr observeEvent
#' @importFrom shiny plotOutput reactiveTimer reactiveVal renderPlot req selectInput shinyApp
#' @importFrom shiny tabPanel tabsetPanel updateSelectInput
#' @importFrom stats heatmap median setNames
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @export
#'
run_board <- function(
                      answers_dir = "Answers", dest_url = NULL, panel_url = NULL, numeric_range = c(0, 10)) {
  # Set default host/port, if not provided as `dest_url`.
  host <- getOption("shiny.host", "127.0.0.1")
  port <- getOption("shiny.port")
  if (!is.null(dest_url)) {
    dest_url <- strsplit(dest_url, ":")[[1]]
    if (length(dest_url) != 2) {
      stop("`dest_url` should follow the format HOST:PORT , for example, 192.168.100.7:4001 .")
    }
    host <- dest_url[[1]]
    port <- as.numeric(dest_url[[2]])
  }

  ui <- fluidPage(
    tabsetPanel(
      tabPanel(
        "Participantes",
        h1("Participantes"),
        br(),
        fluidRow(
          column(6, DT::dataTableOutput("panel")),
          column(6, plotOutput("qrcode"))
        ),
        hr(),
        checkboxInput("show_dist_plot", "Mostrar resultados panel"),
        conditionalPanel(
          "input.show_dist_plot == true",
          fluidRow(
            column(2, selectInput("experts", "Expertos", NULL, multiple = TRUE)),
            column(10, plotOutput("panel_dist_plot"))
          )
        )
      ),
      tabPanel(
        "Respuestas",
        h1("Respuestas"),
        br(),
        DT::dataTableOutput("answers_summary_table"),
        plotOutput("answers_radar"),
        selectInput("answer_selector", "Individuales", NULL),
        plotOutput("answer_box"),
        plotOutput("answer_wordcloud")
      ),
      tabPanel(
        "Datos",
        h1("Datos"),
        br(),
        DT::dataTableOutput("data")
      )
    )
  )

  # Load panel members.
  get_panel <- function() {
    names <- dir(glue("{answers_dir}/"))
    req(length(names) > 0)
    map(
      names, ~ c(
        Nombre = .x, Valuaciones = length(dir(glue("{answers_dir}/{.x}"), pattern = ".csv"))
      )
    ) %>%
      bind_rows() %>%
      arrange(Nombre)
  }

  # Load panelists answers.
  get_answers <- function() {
    names <- dir(glue("{answers_dir}/"))
    req(length(names) > 0)
    answers <- map(names, function(user) {
      map(
        dir(glue("{answers_dir}/{user}")),
        ~ tibble(
          Producto = sub("\\.csv$", "", .x),
          Valuador = user,
          read_csv(glue("{answers_dir}/{user}/{.x}"), col_types = cols())
        )
      )
    }) %>%
      bind_rows()
    req(nrow(answers) > 0)
    arrange(answers, Producto, Valuador)
  }

  # Get column types from a data frame.
  col_types <- function(dataset) sapply(dataset, class)

  # Render table as datatable, with default options.
  render_dtable <- function(table) {
    DT::datatable(
      table,
      options = list(paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE
    )
  }

  # Generates the QR code plot from the `text`.
  qr_gen <- function(text) {
    if (!require("qrcode") | is.null(text)) {
      return(invisible(text))
    }
    qr_matrix <- qrcode_gen(text, dataOutput = TRUE, plotQRcode = FALSE)
    qr_matrix <- as.data.frame.table(qr_matrix)
    qr_matrix[1:2] <- lapply(qr_matrix[1:2], as.numeric)
    qr_matrix <- qr_matrix[qr_matrix$Freq == 1, ]
    ggplot(qr_matrix, aes(Var1, Var2)) +
      geom_tile() +
      theme_void() +
      theme(aspect.ratio = 1)
  }

  server <- function(input, output, session) {
    panel <- reactiveVal()
    answers <- reactiveVal()

    # Refresh answers every 5 seconds.
    timer <- reactiveTimer(1000 * 5)
    observeEvent(timer(), {
      panel(get_panel())
      answers(get_answers())
    })

    # Show QR code.
    output$qrcode <- renderPlot(qr_gen(panel_url))

    # Show panel table.
    output$panel <- DT::renderDataTable(render_dtable(panel()))
    output$data <- DT::renderDataTable(render_dtable(answers()))

    # Panel distances plot.
    output$panel_dist_plot <- renderPlot({
      req(input$show_dist_plot)
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      ans <- filter(ans, Valuador %in% slice_max(panel(), Valuaciones)$Nombre) %>%
        arrange(Valuador, Producto)
      experts <- intersect(input$experts, ans$Valuador)
      num_res <- select(ans, Valuador, Producto, where(is.numeric)) %>%
        pivot_wider(
          names_from = Producto, values_from = where(is.numeric),
          names_glue = "{Producto} - {.value}"
        ) %>%
        as.data.frame()
      rownames(num_res) <- num_res$Valuador
      num_res <- select(num_res, -Valuador) %>%
        as.matrix()
      num_res <- rbind(
        General = apply(num_res, 2, median),
        num_res
      )
      colors <- c("black", rep("white", nrow(num_res) - 1))
      if (length(experts) > 0) {
        num_res <- rbind(
          Expertos = apply(num_res[experts, , drop = FALSE], 2, median),
          num_res
        )
        colors <- c("gold", colors)
      }
      heatmap(
        num_res,
        RowSideColors = colors,
        col = colorRampPalette(c("white", "darkred"))(diff(numeric_range) * 2),
        scale = "none",
        breaks = seq(numeric_range[[1]], numeric_range[[2]], 0.5)
      )
    })

    # Update panel at the experts selector.
    observeEvent(
      panel(),
      updateSelectInput(session, "experts", choices = panel()$Nombre, selected = input$experts)
    )

    # Create answers summary table.
    output$answers_summary_table <- DT::renderDataTable(render_dtable({
      req(nrow(answers()) > 0)
      answers() %>%
        select(-Valuador) %>%
        group_by(Producto) %>%
        mutate(Valuaciones = n()) %>%
        summarise_if(is.numeric, median) %>%
        arrange(Producto)
    }))

    # Create answers radar plot.
    output$answers_radar <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      select(ans, -Valuador) %>%
        group_by(Producto) %>%
        summarise_if(is.numeric, median) %>%
        ggradar(
          grid.min = numeric_range[[1]], grid.max = numeric_range[[2]], values.radar = c("", "", "")
        )
    })

    # Update answers selector.
    observeEvent(answers(), {
      req(nrow(answers()) > 0)
      updateSelectInput(
        session, "answer_selector",
        choices = answers()$Producto,
        selected = if_else(
          nchar(input$answer_selector) > 0, input$answer_selector, answers()$Producto[[1]]
        )
      )
    })

    # Selected answer boxplot.
    output$answer_box <- renderPlot({
      ans <- answers()
      req(nrow(ans) > 0 && sum(col_types(ans) == "numeric") > 0)
      ans %>%
        filter(Producto == input$answer_selector) %>%
        select(-Producto) %>%
        pivot_longer(where(is.numeric), names_to = "Atributo", values_to = "Valor") %>%
        mutate(Atributo = factor(Atributo, levels = colnames(answers()[-(1:2)]))) %>%
        ggplot(aes(x = Atributo, y = Valor, label = Valuador)) +
        geom_boxplot() +
        geom_text(aes(color = Valuador)) +
        theme_bw() +
        xlab("") +
        ylab("") +
        coord_cartesian(ylim = numeric_range) +
        scale_y_continuous(breaks = seq(numeric_range[[1]], numeric_range[[2]])) +
        theme(legend.position = "none")
    })

    # Selected answer wordclouds.
    output$answer_wordcloud <- renderPlot({
      ans <- answers()
      req(
        nrow(ans) > 0 && sum(col_types(ans) == "character") > 2 && nchar(input$answer_selector) > 0
      )
      ans %>%
        filter(Producto == input$answer_selector) %>%
        select(negate(is.numeric)) %>%
        select(-Producto) %>%
        pivot_longer(-Valuador, names_to = "Atributo", values_to = "Valor") %>%
        group_by(Atributo) %>%
        group_map(function(val, group) {
          if (all(is.na(val$Valor))) {
            return(tibble(Valor = "", Freq = 0, Atributo = as.character(group)))
          }
          resps <- val %>%
            pull(Valor) %>%
            strsplit(", ") %>%
            map(~ unique(tolower(trimws(.x)))) %>%
            unlist() %>%
            table() %>%
            as_tibble()
          colnames(resps) <- c("Valor", "Freq")
          mutate(resps, Atributo = as.character(group))
        }) %>%
        bind_rows() %>%
        ggplot(aes(label = Valor, size = Freq, color = Freq)) +
        geom_text_wordcloud(area_corr_power = 1) +
        facet_wrap(~Atributo) +
        scale_size_area(max_size = 24) +
        theme_minimal() +
        scale_color_gradient(low = "red", high = "darkred") +
        theme(strip.text = element_text(size = 20))
    })
  }

  # Run the app.
  shinyApp(ui, server, options = list(host = host, port = port))
}
