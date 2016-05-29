library(shiny)
library(ggplot2)
library(shinysky)
library(shinyTable)

shinyUI(fluidPage(

# Message when no optimization routing round
 tags$script(HTML('
          Shiny.addCustomMessageHandler("jsCode",
            function(message) {
              eval(message.value);
            }
          );'
      )),
  fluidRow(
    
# Farm level variables
    column(2,
  numericInput("acres", label = h4("Coffee Acres"), value = "1.67" ),
  numericInput("proj_cherry", label = h4("Projected Cherry/Acre"), value = "7500"),
  numericInput("spraycost", label = h4("Spray Cost/month"), value = "179.30"),
  numericInput("harvestcost", label = h4("Harvest Cost"), value = "0.50"),
  numericInput("price", label = h4("Price"), value = "2"),
  numericInput("maxinf", label = h4("Max Infestation"), value = "25"),
  hr(),
  actionButton("optimize", "Optimize Net Benefit", styleclass="primary", icon = "check")),#, icon("paper-plane"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  
# Harvest Table  
  column(2,
  p(h4("Harvest")),  htable("tbl4")
  ),
  column(3,

# Markov Tables
  p(h4("No Spray T-Matrix")),  htable("tbl1"), 
  p(h4("Spray T-Matrix")),  htable("tbl2"),
  p(h4("Initial Values")),  htable("tbl3")
  )),

# Max Net Benefit
 fluidRow(
   column(3,
    p(h4("Max Net Benefit")), verbatimTextOutput("maxnetrevenue")
 )),

# Plots and final table
  fluidRow(
    column(4, 
  p(h4("CBB Growth Rates")), tableOutput("markov")
    ),
  column(3,
    plotOutput("markovplot"))),
    tableOutput("table")

))

