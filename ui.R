#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bolão da Copa @ DCX"),
  
  navlistPanel("Menu",
    tabPanel("Ranking da rodada",
             DT::dataTableOutput("pontuacao_table")
    ),
    tabPanel("Palpites de hoje",
             DT::dataTableOutput("palpites_hoje_table")
    ),
    tabPanel("Palpites da rodada",
             DT::dataTableOutput("palpites_table")
    ),
    tabPanel("Placares da rodada",
             DT::dataTableOutput("resultados_copa_table")
    )
  )
  
))
