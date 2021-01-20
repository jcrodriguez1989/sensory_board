library("dplyr")
library("ggplot2")
library("ggradar")
library("glue")
library("purrr")
library("readr")
library("shiny")
library("tidyr")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Participantes", dataTableOutput("participants")),
    tabPanel(
      "Respuestas",
      dataTableOutput("answers_summary_table"),
      plotOutput("answers_radar"),
      selectInput("answer_selector", "", NULL),
      plotOutput("answer_box")
    ),
    tabPanel("Datos", dataTableOutput("data")),
    tabPanel("Settings", selectInput("experts", "Expertos", NULL, multiple = TRUE))
  )
)

get_participants <- function() {
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
        read_csv(glue("Answers/{user}/{.x}"))
      )
    )
  }) %>%
    bind_rows()
  req(nrow(answers) > 0)
  arrange(answers, Producto, Valuador)
}

server <- function(input, output, session) {
  participants <- reactiveVal()
  answers <- reactiveVal()

  # Refrescar cada 5 segundos.
  timer <- reactiveTimer(1000 * 5)
  observeEvent(timer(), {
    participants(get_participants())
    answers(get_answers())
  })

  # Show participants table.
  output$participants <- renderDataTable(participants())
  output$data <- renderDataTable(answers())

  # Update participants at the experts selector.
  observeEvent(
    participants(),
    updateSelectInput(session, "experts", choices = participants()$Nombre, selected = input$experts)
  )

  # Create answers summary table.
  output$answers_summary_table <- renderDataTable({
    req(nrow(answers()) > 0)
    answers() %>%
      select(-Valuador) %>%
      group_by(Producto) %>%
      mutate(Valuaciones = n()) %>%
      summarise_all(median) %>%
      arrange(Producto)
  })

  # Create answers radar plot.
  output$answers_radar <- renderPlot({
    req(nrow(answers()) > 0)
    answers() %>%
      select(-Valuador) %>%
      group_by(Producto) %>%
      summarise_all(median) %>%
      ggradar(grid.min = 0, grid.max = 10, values.radar = c("", "", ""))
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
    req(nrow(answers()) > 0)
    answers() %>%
      filter(Producto == input$answer_selector) %>%
      select(-Producto) %>%
      pivot_longer(-Valuador, names_to = "Atributo", values_to = "Valor") %>%
      mutate(Atributo = factor(Atributo, levels = colnames(answers()[-(1:2)]))) %>%
      ggplot(aes(x = Atributo, y = Valor, label = Valuador)) +
      geom_boxplot() +
      geom_text(aes(color = Valuador)) +
      theme_bw() +
      xlab("") +
      ylab("") +
      coord_cartesian(ylim = c(0, 10)) +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
