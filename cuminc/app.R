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
                div(p("Enter data below or paste using",
                      HTML("<kbd><kbd>Ctrl</kbd>+<kbd>V</kbd></kbd>")),
                    p("Status is 0 for censored, 1 for event and 2 for competing."))
            ),
            rHandsontableOutput('hot', height = '88vh')),
        column(8,
            div(class = 'toolbar',
                div(numericInput('confint_level', "Confidence level", .95, min = .01, max = .99, step = .01, width = 130)),
                div(div(class = 'form-group',
                        tags$label("Display CI?"),
                        checkboxInput('display_confint', "Yes", T)))
            ),
            plotOutput('plot'),
            htmlOutput('table'))
    )
)

server <- function(input, output) {
    initialData <- tibble(time = rep(NA_real_, 1000),
                          status = rep(NA_integer_, 1000),
                          group = rep(NA_character_, 1000))

    output$hot <- renderRHandsontable({
        rhandsontable(initialData, rowHeaders = NULL) %>%
            hot_cols(colWidths = c(70, 50, 100))
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
        survfit(Surv(time, status, type = 'mstate') ~ group,
                fit_data(),
                conf.int = input$confint_level)
    })

    output$plot <- renderPlot({
        if (is.null(fit())) return()
        autoplot(fit()[,1], conf.int = input$display_confint, conf.int.alpha = .2) +
            labs(y = "Probability",
                 x = "Time",
                 color = "Group",
                 fill = "Group")
    }, res = 80)

    table <- function(data) {
        data %>%
            select(time, pstate, std.err, lower, upper) %>%
            mutate(time = format(time, digits = 2)) %>%
            mutate_at(vars(pstate, std.err, lower, upper),
                      ~ format(round(., 3), nsmall = 3)) %>%
            rename(`Time` = time,
                   `CumInc` = pstate,
                   `Std. Error` = std.err,
                   `Lower CI` = lower,
                   `Upper CI` = upper) %>%
            knitr::kable(format = 'html') %>%
            kableExtra::kable_styling(full_width = F, position = 'left', bootstrap_options = 'condensed')
    }

    table_header <- function(group) {
        h3(sprintf("Group: %s", group))
    }

    table_group <- function(group, data) {
        paste(table_header(group),
              table(data),
              collapse = '\n')
    }

    output$table <- renderUI({
        if (is.null(fit())) return()
        result <- fortify(fit()[,1])
        if ('strata' %nin% colnames(result)) {
            result <- mutate(result, strata = "All")
        }
        result <- result %>%
            group_by(strata) %>%
            nest() %>%
            mutate(table = pmap_chr(list(strata, data), table_group))
        result <- paste(result$table, collapse = '\n')
        HTML(result)
    })
}

shinyApp(ui = ui, server = server)
