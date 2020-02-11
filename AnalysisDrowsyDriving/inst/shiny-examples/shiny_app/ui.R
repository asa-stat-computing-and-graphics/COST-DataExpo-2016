library(survey)
library(WeightedROC)
library(ggplot2)
library(wesanderson)
library(DT)
library(shiny)
library(lattice)
library(GGally)

load("GES2013.drivers.rda")

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}

shinyUI(fluidPage(
  tags$h1("GES Plotter"),
  br(),
  sidebarPanel(
      # dropdownButton(
      #   label = "Single Response", status = "default", width = 80,
      #   checkboxGroupInput(inputId = "check1", label = "Choose", choices = sort(colnames(GES2013.drivers)))
      # ),
      
      selectInput(inputId = "check1", 
                  label = "Single Response", 
                  choices = sort(colnames(GES2013.drivers))),
      
      # verbatimTextOutput(outputId = "res1"),
      
      dropdownButton(
        label = "Set of Predictors", status = "default", width = 80,
        # actionButton(inputId = "a2z", label = "Sort A to Z", icon = icon("sort-alpha-asc")),
        # actionButton(inputId = "z2a", label = "Sort Z to A", icon = icon("sort-alpha-desc")),
        br(),
        actionButton(inputId = "all", label = "(Un)select all"),
        checkboxGroupInput(inputId = "check2", label = "Choose", choices = sort(colnames(GES2013.drivers)))
      ),
      
      verbatimTextOutput(outputId = "res2"),
      
      selectInput(inputId = "plot_type", 
                  label = "Plot Type", 
                  choices = c("ROC", "Scatterplot Matrix", "Mosaic Matrix")),
      
      actionButton(inputId = "input_action", label = "Create Plot")
    ),
  
  mainPanel(plotOutput("plot1"))
))