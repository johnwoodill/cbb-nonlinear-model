library(shiny)
library(ggplot2)
library(shinysky)
library(shinyIncubator)
library(shinyTable)
library(shinythemes)

shinyUI(fluidPage(
 tags$script(HTML('
          Shiny.addCustomMessageHandler("jsCode",
            function(message) {
              eval(message.value);
            }
          );'
      )),
  fluidRow(
    column(3,
  numericInput("acres", label = h4("Coffee Acres"), value = "1.67" ),
  numericInput("proj_cherry", label = h4("Projected Cherry/Acre"), value = "7500"),
  numericInput("spraycost", label = h4("Spray Cost/month"), value = "179.30"),
  numericInput("harvestcost", label = h4("Harvest Cost"), value = "0.50"),
  numericInput("price", label = h4("Price"), value = "2"),
  numericInput("maxinf", label = h4("Max Infestation"), value = "25", ),
  hr(),
  actionButton("optimize", "Optimize NB", styleclass="primary", icon = "check")),#, icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  
  
  column(4, offset = 1,
  p(h4("Harvest")),  htable("tbl4")
  ),
  column(4,
  p(h4("No Spray T-Matrix")),  htable("tbl1"), 
  p(h4("Spray T-Matrix")),  htable("tbl2"),
  p(h4("Initial Values")),  htable("tbl3")
  )),
  fluidRow(
    column(3, 
  
  #mainPanel(
    #dataTableOutput("table"))
  hr(width="60%"),
  p(h4("Max Net Benefit")), verbatimTextOutput("maxnetrevenue"),  
  p(h4("CBB Growth Rates")),tableOutput("markov")
    ),
  column(4, offset = 1,
    plotOutput("markovplot"))),
    tableOutput("table")
    
    #hr(),
  #fluidRow(column(3, verbatimTextOutput("value")))
      
))

