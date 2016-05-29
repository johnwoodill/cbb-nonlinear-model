library(shiny)
#library(shinyIncubator)
library(shinysky)
library(shinyTable)
library(ggplot2)
library(dplyr)
library(tidyr)


#----------------------------------------
# Start of Optimization Routing
#----------------------------------------


# Empty data frame
dat <- data.frame(month = 1:12,
                  spraystrat = rep(0,12),
                  inflevel = rep(0,12),
                  harvest = rep(0,12),
                  cherry_harvest = rep(0,12),
                  damage = rep(0,12),
                  cherry_ending = rep(0,12),
                  revenue = rep(0,12),
                  spray_cost = rep(0,12),
                  harvest_cost = rep(0,12),
                  net_revenue = rep(0,12))


# Setup Harvest rates
#harvest <- c(0, 0, 0, 0, 0, 0, 0, 0, .32, .48, .12, .07)
harvest <- data.frame(month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                      rate = c(0, 0, 0, 0, 0, 0, 0, 0, .32, .48, .12, .07), stringsAsFactors = FALSE)

# All possible combinations (brute force)
zeroone <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)

# Markov Transition Matrices

# No Spray
nsp <- matrix(c(0.9, 0.1 , 0  , 0,
                0  , 0.6 , 0.2, 0.2,
                0  , 0   , 1  , 0,
                0  , 0   , 0  , 1), 
              nrow = 4, ncol = 4, byrow = TRUE)

# Spray
sp <- matrix(c(0.9 , 0.1 , 0   , 0,
               0    , 0.4  , 0.5 , 0.1,
               0    , 0    , 1   , 0,
               0    , 0    , 0   , 1), 
             nrow = 4, ncol = 4, byrow = TRUE)

# Initial Values
#ii <- c(0.80,0.10,0,.10)
ii <- data.frame(type = c("NI", "AB_live", "AB_dead", "CD"), value = c(0.80,0.10,0,.10), stringsAsFactors = FALSE)

# Markov matrix setup
mat <- matrix(nrow=12, ncol = 4)

# Markov Process Function
markov <- function(spraystrat){
    if(ncol(ii) == 2){
    ii <- ii[,2]
    }
    for (i in 1:12){
    if(spraystrat[i] == 1){
      output <- ii %*% sp
      } else {
        output <- ii %*% nsp
      }
    ii <- output
    mat[i,] <- output
    }
  mat <- as.data.frame(mat)
  names(mat) <- c("ni", "ab_live", "ab_dead", "cd")
  mat$month <- c(1:12)
  mat <- select(mat, month, ni, ab_live, ab_dead, cd)
  return(mat)
}

optimal <- function(x){
  newdat <- dat
  newdat$spraystrat <- t(x)
  markov_matrix <- markov(x)
  newdat$inflevel <- markov_matrix$cd
  newdat$harvest <- harvest
  newdat$cherry_harvest = acres*proj_cherry*newdat$harvest
  newdat$damage = newdat$cherry_harvest*newdat$inflevel
  newdat$cherry_ending = newdat$cherry_harvest - newdat$damage  
  newdat$revenue = price*newdat$cherry_ending
  newdat$spray_cost = spraycost*newdat$spraystrat
  newdat$harvest_cost = newdat$cherry_harvest*harvest_cost
  newdat$net_revenue = newdat$revenue - newdat$spray_cost - newdat$harvest_cost
  
  # Check net revenue > spray cost
  newdat$cherry_harvest <- ifelse(newdat$net_revenue < spraycost, 0, newdat$cherry_harvest) 
  newdat$cherry_harvest <- ifelse(newdat$inflevel > max_inf, 0, newdat$cherry_harvest)
  
  # Recalculate
  newdat$damage = newdat$cherry_harvest*newdat$inflevel
  newdat$cherry_ending = newdat$cherry_harvest - newdat$damage  
  newdat$revenue = price*newdat$cherry_ending
  newdat$spray_cost = spraycost*newdat$spraystrat
  newdat$harvest_cost = newdat$cherry_harvest*harvest_cost
  newdat$net_revenue = newdat$revenue - newdat$spray_cost - newdat$harvest_cost
  return(newdat)
  }

comb <- function(var = 0, zeroone = zeroone){
  # pb <- txtProgressBar(min = 0, max = nrow(zeroone), initial = 0)
  # stepi <- 0
  withProgress(message = 'Running Optimization ...', value = 0,  {
  for (i in 1:nrow(zeroone)){
    x <- zeroone[i,]
    newmat <- optimal(x)
    x$net_revenue <- sum(newmat$net_revenue)
    if(x$net_revenue > var){
      var <- x$net_revenue
      maxmat <- x
    }
    incProgress(1/nrow(zeroone))
  }})
  
  if (is.data.frame(maxmat) == TRUE){
  # Find maximum value
  optimal_scenario <- optimal(maxmat[1:12])
  return(optimal_scenario)
  } else {
    return(0)
  }
  
}


shinyServer(function(input, output, session) {

#----------------------------------------
# Start of Optimization Routing
#----------------------------------------

# Empty data frame
dat <- data.frame(month = 1:12,
                  spraystrat = rep(0,12),
                  inflevel = rep(0,12),
                  harvest = rep(0,12),
                  cherry_harvest = rep(0,12),
                  damage = rep(0,12),
                  cherry_ending = rep(0,12),
                  revenue = rep(0,12),
                  spray_cost = rep(0,12),
                  harvest_cost = rep(0,12),
                  net_revenue = rep(0,12))


# Setup Harvest rates
#harvest <- c(0, 0, 0, 0, 0, 0, 0, 0, .32, .48, .12, .07)
harvest <- data.frame(month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"), 
                      rate = c(0, 0, 0, 0, 0, 0, 0, 0, .32, .48, .12, .07), stringsAsFactors = FALSE)

# All possible combinations (brute force)
zeroone <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)

# Markov Transition Matrices

# No Spray
nsp <- matrix(c(0.9, 0.1 , 0  , 0,
                0  , 0.6 , 0.2, 0.2,
                0  , 0   , 1  , 0,
                0  , 0   , 0  , 1), 
              nrow = 4, ncol = 4, byrow = TRUE)

# Spray
sp <- matrix(c(0.9 , 0.1 , 0   , 0,
               0    , 0.4  , 0.5 , 0.1,
               0    , 0    , 1   , 0,
               0    , 0    , 0   , 1), 
             nrow = 4, ncol = 4, byrow = TRUE)

# Initial Values
#ii <- c(0.80,0.10,0,.10)
#ii <- data.frame(type = c("NI", "AB_live", "AB_dead", "CD"), value = c(0.80,0.10,0,.1), stringsAsFactors = FALSE)

# Markov matrix setup
mat <- matrix(nrow=12, ncol = 4)

# Markov Process Function
markov <- function(spraystrat){
    if(ncol(ii) == 2){
    ii <- ii[,2]
    }
    for (i in 1:12){
    if(spraystrat[i] == 1){
      output <- ii %*% sp
      } else {
        output <- ii %*% nsp
      }
    ii <- output
    mat[i,] <- output
    }
  mat <- as.data.frame(mat)
  names(mat) <- c("ni", "ab_live", "ab_dead", "cd")
  mat$month <- c(1:12)
  mat <- select(mat, month, ni, ab_live, ab_dead, cd)
  return(mat)
}

optimal <- function(x){
  newdat <- dat
  newdat$spraystrat <- t(x)
  markov_matrix <- markov(x)
  newdat$inflevel <- markov_matrix$cd
  newdat$harvest <- harvest
  newdat$cherry_harvest = acres*proj_cherry*newdat$harvest
  newdat$damage = newdat$cherry_harvest*newdat$inflevel
  newdat$cherry_ending = newdat$cherry_harvest - newdat$damage  
  newdat$revenue = price*newdat$cherry_ending
  newdat$spray_cost = spraycost*newdat$spraystrat
  newdat$harvest_cost = newdat$cherry_harvest*harvest_cost
  newdat$net_revenue = newdat$revenue - newdat$spray_cost - newdat$harvest_cost
  
  # Check net revenue > spray cost
  newdat$cherry_harvest <- ifelse(newdat$net_revenue < spraycost, 0, newdat$cherry_harvest) 
  newdat$cherry_harvest <- ifelse(newdat$inflevel > max_inf, 0, newdat$cherry_harvest)
  
  # Recalculate
  newdat$damage = newdat$cherry_harvest*newdat$inflevel
  newdat$cherry_ending = newdat$cherry_harvest - newdat$damage  
  newdat$revenue = price*newdat$cherry_ending
  newdat$spray_cost = spraycost*newdat$spraystrat
  newdat$harvest_cost = newdat$cherry_harvest*harvest_cost
  newdat$net_revenue = newdat$revenue - newdat$spray_cost - newdat$harvest_cost
  return(newdat)
  }

comb <- function(zeroone = zeroone){
  # pb <- txtProgressBar(min = 0, max = nrow(zeroone), initial = 0)
  # stepi <- 0
  var = 0
  maxmat <- 0
  withProgress(message = 'Running Optimization ...', value = 0,  {
  for (i in 1:nrow(zeroone)){
    x <- zeroone[i,]
    newmat <- optimal(x)
    x$net_revenue <- sum(newmat$net_revenue)
    if(x$net_revenue > var){
      var <- x$net_revenue
      maxmat <- x
    }
    incProgress(1/nrow(zeroone))
  }
  }
  )
  
  if (is.data.frame(maxmat) == TRUE){
  # Find maximum value
  optimal_scenario <- optimal(maxmat[1:12])
  return(optimal_scenario)
  } else {
    return(0)
  }
  
}
  
  
#----------------------------------------
# Shiny Server Setup
#----------------------------------------

# Initial values setup
values <- reactiveValues()          # Optimal Scenario Table
markov <- reactiveValues()          # Markov Values
maxnetrevenue <- reactiveValues()   # Optimal Net Revenue
nsp_t <- reactiveValues(cachedTbl = NULL)
sp_t <- reactiveValues(cachedTbl = NULL)
ii_t <- reactiveValues(cachedTbl = NULL)
harvest_t <- reactiveValues(cachedTbl = NULL)

# No Spray Markov output  
output$tbl1 <- renderHtable({
  if (is.null(input$tbl1)){
    tbl1 <- nsp
    nsp_t$cachedTbl <<- tbl1
    return(tbl1)
  } else{
    nsp_t$cachedTbl <<- input$tbl1
    return(input$tbl1)
  }
})  

# Spray Markov output
output$tbl2 <- renderHtable({
  if (is.null(input$tbl2)){
    tbl2 <- sp
    sp_t$cachedTbl <<- tbl2
    return(tbl2)
  } else{
    sp_t$cachedTbl <<- input$tbl2
    return(input$tbl2)
  }
})  
    
# Initial Values output
output$tbl3 <- renderHtable({
  if (is.null(input$tbl3)){
    tbl3 <- ii
    ii_t$cachedTbl <<- tbl3
    return(tbl3)
  } else{
    ii_t$cachedTbl <<- input$tbl3
    return(input$tbl3)
  }
}) 

# Harvest output        
output$tbl4 <- renderHtable({
  if (is.null(input$tbl4)){
    tbl4 <- harvest
    harvest_t$cachedTbl <<- tbl4
    return(tbl4)
  } else{
    harvest_t$cachedTbl <<- input$tbl4
    return(input$tbl4)
  }
}) 

  
values$df <- dat                    # Initial optimal scenario dummy set
markov$df <- markov(zeroone[100,])  # Markov set from scenario at 100
markov_df <- markov(zeroone[100,])

# Order Markov data for plot
markov_df <- gather(markov_df, "month", "value")    
names(markov_df) <- c("month", "inf", "value") 
markov_df[,2] <- factor(markov_df[,2], levels = c("ni", "ab_live", "ab_dead", "cd"), labels = c("Not Inf", "AB Live", "AB Dead", "CD"))
maxnetrevenue$df <- sum(dat[,11])
  
# Action button Events
ntext <- observeEvent(input$optimize, {
  acres <<- input$acres
  proj_cherry <<- input$proj_cherry
  spraycost <<- input$spraycost
  harvest_cost <<- input$harvestcost
  price <<- input$price
  max_inf <<- input$maxinf
  nsp <<- data.matrix(as.data.frame(reactiveValuesToList(nsp_t)))
  sp <<- data.matrix(as.data.frame(reactiveValuesToList(sp_t)))
    
  # Initial Values
  ii <<- as.data.frame(reactiveValuesToList(ii_t))
  ii <<- ii[2]
  ii <<- t(data.matrix(ii))
    
  # Harvest Values
  harvest <<- as.data.frame(reactiveValuesToList(harvest_t))
  harvest <<- harvest[2]
  harvest <<- data.matrix(harvest)
      
  # Run optimization and get markov
  values$df <<- comb(zeroone)
  # if (values$df == 0){
  #     js_string <- 'alert("No Optimal Solution Found");'
  #     session$sendCustomMessage(type='jsCode', list(value = js_string))
  #   } else {
    spraystrat <<- as.data.frame(reactiveValuesToList(values))
    spraystrat <<- t(spraystrat[2])
    markov$df <<- markov(spraystrat)
    markov_df <<- markov(spraystrat)
    markov_df <<- gather(markov_df, "month", "value")
    names(markov_df) <<- c("month", "inf", "value") 
    markov_df[,2] <<- factor(markov_df[,2])
    maxnr_df <<- as.data.frame(reactiveValuesToList(values))
    maxnetrevenue$df <<- sum(maxnr_df[,11])
  #}
    
    },priority = 1)

# Setup output variables
output$maxnetrevenue = renderText(maxnetrevenue$df)
output$markov = renderTable({markov$df})
output$table = renderTable({values$df})
output$markovplot = renderPlot(ggplot(markov_df, aes(markov_df[,1], markov_df[,3], color = markov_df[,2]), group = markov_df[,2]) + geom_line() + xlab("Month") + ylab("Infestation level (%)") + ggtitle("CBB Growth Rates") + scale_x_continuous(breaks=1:12) + theme(legend.title = element_blank(), legend.text = element_text(c("nii", "ab_live", "ab_dead", "cd"))), 
                               width = 600, height = 400)
  
})
