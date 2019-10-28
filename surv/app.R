library(shiny)
library(rhandsontable)
library(tidyverse)
library(ggfortify)
library(survival)

source('../misc.R')
source('../setup.R')

APP_NAME <- "Survival Function (Kaplan-Meier)"

ui <- fluidPage(
    includeCSS('../styles.css'),
    tags$head(
        tags$title(page_title(APP_NAME))
    ),
    fluidRow(
        column(4,
            div(class = 'toolbar',
                div(class = 'toolbar-text',
                    p("Enter data below or paste using",
                      HTML("<kbd><kbd>Ctrl</kbd>+<kbd>V</kbd></kbd>")),
                    p("Status is 0 for censored, 1 for event."))
            ),
            rHandsontableOutput('hot', height = '89vh')),
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
        survfit(Surv(time, status, type = 'right') ~ group,
                fit_data(),
                conf.int = input$confint_level)
    })

    output$plot <- renderPlot({
        if (is.null(fit())) return()
        autoplot(fit(), conf.int = input$display_confint, conf.int.alpha = .2) +
            labs(y = "Survival Probability",
                 x = "Time",
                 color = "Group",
                 fill = "Group")
    }, res = 80)

    table <- function(data) {
        data %>%
            select(time, surv, std.err, lower, upper) %>%
            mutate(time = format(time, digits = 2)) %>%
            mutate_at(vars(surv, std.err, lower, upper),
                      ~ format(round(., 3), nsmall = 3)) %>%
            rename(`Time` = time,
                   `Surv` = surv,
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
        result <- fortify(fit())
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
