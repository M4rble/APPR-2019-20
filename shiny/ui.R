library(shiny)

shinyUI(fluidPage(
  titlePanel("Dostop do interneta"), 
  DT::dataTableOutput("dostop_do_interneta"))
)