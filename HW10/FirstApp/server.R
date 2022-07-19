library(shiny)
library(caret)
library(tidyverse)
library(DT)
data("GermanCredit")


shinyServer(function(input, output) {
  output$plot <- renderPlot({
    if(input$plotType == 'Just Classification'){
      GermanCredit %>% ggplot(aes(Class)) +
        geom_bar()
    } else if(input$plotType == 'Classification and Unemployed'){
      GermanCredit %>% 
        ggplot(aes(Class, fill = as.factor(EmploymentDuration.Unemployed))) +
        geom_bar(position = "dodge") +
        scale_fill_discrete(name = "Unemployment status", labels = c("Employed", "Unemployed"))
    } else if(input$plotType == 'Classification and Foreign') {
      GermanCredit %>% 
        ggplot(aes(Class, fill = as.factor(ForeignWorker))) +
        geom_bar(position = "dodge") +
        scale_fill_discrete(name = "Status", labels = c("German", "Foreign"))
    }
  })
  output$summary <- renderDataTable({
    var <- input$var
    dataSubset <- GermanCredit[, c("Class", "InstallmentRatePercentage", var), drop = FALSE]
    tab <- aggregate(
      dataSubset[[var]] ~ Class + InstallmentRatePercentage, 
      data = dataSubset, 
      FUN = mean
    )
    colnames(tab)[3] <- paste0("Average ", input$var)
    tab %>%
      datatable() %>%
      formatRound(columns = colnames(tab)[3], digits = input$nRound)
  })
  
  
})