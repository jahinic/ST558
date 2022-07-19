###############################################################################
## Author:  John Hinic
## Date:    07Jul2022
## Purpose: ST 558 HW 10 ui script
###############################################################################

library(shiny)
library(caret)
library(tidyverse)
library(DT)
data("GermanCredit")


shinyUI(fluidPage(
  titlePanel("Summaries for German Credit Data"),
  sidebarLayout(
    sidebarPanel(
      tags$div(
        h3("This data set comes from the ",
        a("caret package", href = "https://topepo.github.io/caret/"),
        " - originally from the UCI machine learning repository")
      ),
      br(),
      h4("You can create a few bar plots using the radio buttons below."),
      radioButtons(
        "plotType", 
        "Select the Plot Type", 
        choices = c("Just Classification", "Classification and Unemployed", "Classification and Foreign")
      ),
      br(),
      tags$div(
        h4("You can find the ",
        strong("sample mean"),
        " for a few variables below:")
      ),
      selectInput(
        "var", 
        "Variables to Summarize", 
        choices = c("Duration", "Amount", "Age"),
        selected = "Age"
      ),
      numericInput(
        "nRound",
        "Select the number of digits for rounding",
        value = 2, min = 0, max = 5, step = 1
      )
    ),
    
    mainPanel(
      plotOutput("plot"),
      dataTableOutput("summary")
    )
  )
))

