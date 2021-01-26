library("dplyr")
library("DT")
library("ggplot2")
library("ggradar")
library("ggwordcloud")
library("glue")
library("grDevices")
library("purrr")
library("readr")
library("shiny")
library("tidyr")

options(shiny.host = "192.168.100.7") # As shown by `ifconfig`.
options(shiny.port = 4001)

ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Participantes",
      h1("Participantes"),
      br(),
      DT::dataTableOutput("panel"),
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

get_panel <- function() {
  names <- dir("Answers/")
  req(length(names) > 0)
  map(
    names, ~ c(Nombre = .x, Valuaciones = length(dir(glue("Answers/{.x}"), pattern = ".csv")))
  ) %>%
    bind_rows() %>%
    arrange(Nombre)
}

get_answers <- function() {
  names <- dir("Answers/")
  req(length(names) > 0)
  answers <- map(names, function(user) {
    map(
      dir(glue("Answers/{user}")),
      ~ tibble(
        Producto = sub("\\.csv$", "", .x),
        Valuador = user,
        read_csv(glue("Answers/{user}/{.x}"), col_types = cols())
      )
    )
  }) %>%
    bind_rows()
  req(nrow(answers) > 0)
  arrange(answers, Producto, Valuador)
}

col_types <- function(dataset) sapply(dataset, class)

render_dtable <- function(table) {
  DT::datatable(
    table, options = list(paging = FALSE, searching = FALSE, info = FALSE), rownames = FALSE
  )
}

server <- function(input, output, session) {
  panel <- reactiveVal()
  answers <- reactiveVal()

  # Refrescar cada 5 segundos.
  timer <- reactiveTimer(1000 * 5)
  observeEvent(timer(), {
    panel(get_panel())
    answers(get_answers())
  })

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
    num_res <- ans %>%
      select(Valuador, Producto, where(is.numeric)) %>%
      pivot_wider(
        names_from = Producto, values_from = where(is.numeric), names_glue = "{Producto} - {.value}"
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
        Expertos = apply(num_res[experts,, drop = FALSE], 2, median),
        num_res
      )
      colors <- c("gold", colors)
    }
    heatmap(
      num_res, RowSideColors = colors, col = colorRampPalette(c("white", "darkred"))(10),
      scale = "none", breaks = seq(0, 5, 0.5) # TODO: get this from input formats.
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
    ans %>%
      select(-Valuador) %>%
      group_by(Producto) %>%
      summarise_if(is.numeric, median) %>%
      ggradar(grid.min = 0, grid.max = 5, values.radar = c("", "", ""))
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
      coord_cartesian(ylim = c(0, 5)) +
      scale_y_continuous(breaks = seq(0, 5)) +
      theme(legend.position = "none")
  })
  
  # Selected answer wordclouds.
  output$answer_wordcloud <- renderPlot({
    ans <- answers()
    req(nrow(ans) > 0 && sum(col_types(ans) == "character") > 2 && nchar(input$answer_selector) > 0)
    ans %>%
      filter(Producto == input$answer_selector) %>% 
      select(negate(is.numeric)) %>% 
      select(-Producto) %>%
      pivot_longer(-Valuador, names_to = "Atributo", values_to = "Valor") %>%
      group_by(Atributo) %>% 
      group_map(function(val, group) {
        if (all(is.na(val$Valor)))
          return(tibble(Valor = "", Freq = 0, Atributo = as.character(group)))
        resps <- val %>%
          pull(Valor) %>% 
          strsplit(", ") %>% 
          map(~unique(tolower(trimws(.x)))) %>% 
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

shinyApp(ui, server)
