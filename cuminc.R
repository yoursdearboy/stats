library(shiny)
library(rhandsontable)
library(ggfortify)
library(survival)

theme_set(theme_bw(base_size = 14))

ui <- fluidPage(
    fluidRow(
        column(4, (
            rHandsontableOutput('hot'))),
        column(8,
            plotOutput('plot'))
    )
)

server <- function(input, output) {
    initialData <- tibble(time = rep(NA_real_, 100),
                          status = rep(NA_integer_, 100),
                          group = rep(NA_character_, 100))

    output$hot <- renderRHandsontable({
        rhandsontable(initialData, rowHeaders = NULL)
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

    fit_data <- reactive({
        data() %>%
            mutate(group = if_else(is.na(group), "All", group))
    })

    fit <- reactive({
        if (nrow(fit_data()) < 1) return()
        survfit(Surv(time, status, type = 'mstate') ~ group, fit_data())
    })

    output$plot <- renderPlot({
        if (is.null(fit())) return()
        autoplot(fit()[,1], conf.int.alpha = .2) +
            labs(y = "Cumulative Incidence",
                 x = "Time",
                 color = "Group",
                 fill = "Group")
    })
}

shinyApp(ui = ui, server = server)
