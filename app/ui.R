library(shiny)
library(ggplot2)
library(shinysky)
library(shinyIncubator)

shinyUI(fluidPage(
 title = "asdf",
  
 # sidebarPanel(  
  fluidRow(
    column(2,
  numericInput("acres", label = h4("Coffee Acres"), value = "1.67" ),
  numericInput("proj_cherry", label = h4("Projected Cherry/Acre"), value = "7500"),
  numericInput("spraycost", label = h4("Spray Cost/month"), value = "179.30"),
  numericInput("harvestcost", label = h4("Harvest Cost"), value = "0.50"),
  numericInput("price", label = h4("Price"), value = "2"),
  numericInput("maxinf", label = h4("Max Infestation"), value = "25", )
  ),
  
  column(5, offset = 1,
  p(h4("No Spray T-Matrix")),  htable("tbl1"), 
  p(h4("Spray T-Matrix")),  htable("tbl2"),
  p(h4("Initial Values")),  htable("tbl3"),
    actionButton("optimize", "Optimize NB") 
  )),
  #mainPanel(
    #dataTableOutput("table"))
    tableOutput("table")
    
    #hr(),
  #fluidRow(column(3, verbatimTextOutput("value")))
      
))

