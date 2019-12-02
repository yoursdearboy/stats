library(shiny)
library(rhandsontable)
library(tidyverse)

source('../misc.R')
source('../setup.R')

ui <- fluidPage(
  includeCSS('../styles.css'),
  fluidRow(
    column(4,
           div(class = 'toolbar',
               div(p("Enter data below or paste using",
                     HTML("<kbd><kbd>Ctrl</kbd>+<kbd>V</kbd></kbd>")))
           ),
           rHandsontableOutput('hot', height = '88vh')),
    column(8, htmlOutput('result'))
  )
)

server <- function(input, output) {
  initialData <- tibble(values = rep(NA_real_, 1000))

  output$hot <- renderRHandsontable({
    rhandsontable(initialData, rowHeaders = NULL) %>%
      hot_cols(colWidths = c(100))
  })

  data <- reactive({
    data <- if (!is.null(input$hot)) {
      hot_to_r(input$hot)
    } else {
      initialData
    }
    data %>%
      drop_na()
  })

  fit <- reactive({
    if (nrow(data()) < 1) return()
    t.test(data()$values)
  })

  mean_est <- reactive({
    if (nrow(data()) < 1) return()
    mean(data()$values)
  })

  sd_est <- reactive({
    if (nrow(data()) < 1) return()
    sd(data()$values)
  })

  output$result <- renderUI({
    if (is.null(fit())) return()
    result <- sprintf("<br/>
                      <p>Mean = %s, SD = %s</p>
                      <p>p-value = %.3f</p>",
                      format(mean_est(), digits = 3),
                      format(sd_est(), digits = 3),
                      fit()$p.value)
    HTML(result)
  })
}

shinyApp(ui = ui, server = server)
