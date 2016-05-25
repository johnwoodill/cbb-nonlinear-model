########################################################################################################
#-------------------------------------------------------------------------------------------------------
# Authors    : A. John Woodill 
# Date      : 5/25/2016
# Filename  : cbb-nonlinear-model.R
# Sections  :  Nonlinear programming model for optimal harvesting and spraying
#            
#-------------------------------------------------------------------------------------------------------
########################################################################################################

library(dplyr)

#------------------------
# Parameters
#------------------------

acres <- 1.67
proj_cherry <- 7500
spraycost <- 179.30
harvest_cost <- .5
price <- 2
max_inf <- .25
harvest <- c(0, 0, 0, 0, 0, 0, 0, 0, .32, .48, .12, .07)

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
ii <- c(0.90,0.10,0,0)
mat <- matrix(nrow=12, ncol = 4)

# Markov Process Function
markov <- function(spraystrat){
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
return(mat)
}

  
#------------------------
# Model Optimization
#------------------------

# All possible combinations
zeroone <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1,0:1)

# Data frame for storing variables
dat <- data.frame(month = rep(0,12),
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

dat$month <- 1:12
check <- data.frame()

# Function to build optimization scenario
optimal <- function(x){
  newdat <- dat
  newdat$spraystrat <- t(x)
  markov_matrix <- markov(x)
  newdat$inflevel <- markov_matrix$cd
  #newdat$inflevel <- c(.09,.09,.09,.09,.09,.1026,.117,.1333,.152,.1733,.2339,.3158)
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

# Iterate through each combination
var <- 0   
pb <- txtProgressBar(min = 0, max = nrow(zeroone), initial = 0)
stepi <- 0

for (i in 1:nrow(zeroone)){
  x <- zeroone[i,]
  newmat <- optimal(x)
  x$net_revenue <- sum(newmat$net_revenue)
  if(x$net_revenue > var){
    var <- x$net_revenue
    maxmat <- x
  }
  stepi = stepi + 1
  setTxtProgressBar(pb, stepi)
}

# Find maximum value
optimal_scenario <- optimal(maxmat[1:12])
optimal_scenario
markov(maxmat[1:12])
