library(shiny)
library(rhandsontable)
library(tidyverse)
library(ggfortify)
library(survival)

source('../misc.R')
source('../setup.R')

ui <- fluidPage(
    includeCSS('../styles.css'),
    fluidRow(
        column(4,
            div(class = 'toolbar',
                p("Enter data below or paste using",
                  HTML("<kbd><kbd>Ctrl</kbd>+<kbd>V</kbd></kbd>")),
                p("Status is 0 for censored, 1 for event.")),
            rHandsontableOutput('hot', height="88vh")),
        column(8,
            div(class = 'toolbar'),
            plotOutput('plot'))
    )
)

server <- function(input, output) {
    initialData <- tibble(value = rep(NA_real_, 1000),
                          group = rep(NA_character_, 1000))

    output$hot <- renderRHandsontable({
        rhandsontable(initialData, rowHeaders = NULL) %>%
            hot_cols(colWidths = c(70, 100))
    })

    data <- reactive({
        data <- if (!is.null(input$hot)) {
            hot_to_r(input$hot)
        } else {
            initialData
        }
        data %>%
            filter_all(any_vars(!is.na(.)))
    })

    plot_data <- reactive({
        result <- data()
        if (nrow(result) < 1) return(result)
        if (all(is.na(result$group))) {
            result$group <- "All"
        }
        result
    })

    output$plot <- renderPlot({
        if (nrow(plot_data()) < 1) return()
        ggplot(plot_data(), aes(group, value)) +
            geom_boxplot() +
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_text(color = 'black'))
    }, res = 80)
}

shinyApp(ui = ui, server = server)
