## Create time-averaged networks over different amounts of time
## Author: Emily Coco
### 

#### 1. Load libraries ####
if(!require(dynsbm)) {install.packages("dynsbm")}
if(!require(igraph)) {install.packages("igraph")}
if(!require(rlist)) {install.packages("rlist")}
require(tidyr)

#### 2. Load multiple timeslice data ####
data("foodwebWoodward") #pulls in both datasets from Woodward
foodwebs <- foodwebWoodward$Y #3D array in format: (timestep, species, species)

#### 3. Define time-averaging function ####
timeaverage <- function(numslices) { #time-averaging specified number of timeslices
  subtractlast <- numslices - 1
  matrix.list <- list()
  for(i in 1:(nrow(foodwebs) - subtractlast)) {
    fw.matrix <- foodwebs[i,,]
    for(j in 1:numslices-1) {
      fw.matrix <- fw.matrix + foodwebs[i+j,,]
    }
    fw.matrix[fw.matrix >= 1] <- 1
    matrix.list[[i]] <- fw.matrix
  }
  return(matrix.list)
}

#### 4. Time-average across all possible timestep combinations ####
create.timeaveraged.webs <- function(num.timesteps) {
  all.ta.webs <- list()
  for(i in 2:num.timesteps) {
    all.ta.webs[[i-1]] <- timeaverage(i)
  }
  return(all.ta.webs)
}

all.ta.webs <- create.timeaveraged.webs(6) 
#list is index first by degree of time-averaging, and then by specific time-averaged combinations

#### 5. Foodweb Comparison ####
get.all.measures <- function(label, web) {
  data <- c(label, 
            calc.diam(web), 
            calc.S2(web), 
            calc.mean.degree2(web), 
            calc.edge.dens(web),
            calc.cc(web),
            calc.mean.between(web)
  )
}

get.all.network.measures <- function(ts.orig.web, foodwebs, ta.webs) {
  compare.df <- data.frame(
    time.average = 0, 
    diam = calc.diam(foodwebs[ts.orig.web,,]), 
    size = calc.S2(foodwebs[ts.orig.web,,]), 
    dg = calc.mean.degree2(foodwebs[ts.orig.web,,]), 
    dens = calc.edge.dens(foodwebs[ts.orig.web,,]), 
    clust = calc.cc(foodwebs[ts.orig.web,,]), 
    btwn = calc.mean.between(foodwebs[ts.orig.web,,]) 
  )
  for(i in 2:(length(ta.webs)+1)) {
    if(ts.orig.web == 1) {
      compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[1]])
    }else if(ts.orig.web == (length(ta.webs) + 1)) {
      l <- length(ta.webs[[i-1]])
      compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[l]])
    }else {
      if(i == 2) {
        compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[ts.orig.web-1]])
        compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[ts.orig.web]])
      }else if(i == 3) {
        if(ts.orig.web < 4) {
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[1]])
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[2]])
        } else {
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[ts.orig.web-2]])
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[ts.orig.web-1]])
        }
      }else if(i == 4) {
        if(ts.orig.web < 5) {
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[1]])
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[2]])
        } else {
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[ts.orig.web-3]])
          compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[ts.orig.web-2]])
        }
      }else if(i == 5) {
        compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[1]])
        compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[2]])
      }else if(i == 6) {
        compare.df[nrow(compare.df) + 1, ] <- get.all.measures(i, ta.webs[[i-1]][[1]])
      }
    }
  }
  return(compare.df)
}

#will work for any foodwebs with six timeslices
get.node.data <- function(ts.orig.web, foodwebs, ta.webs, FUN = calc.node.cc) {
  nd <- FUN(foodwebs[ts.orig.web,,])
  if(ts.orig.web == 1) {
    nd$ta.2 <- FUN(ta.webs[[1]][[1]])[,2]
    nd$ta.3 <- FUN(ta.webs[[2]][[1]])[,2]
    nd$ta.4 <- FUN(ta.webs[[3]][[1]])[,2]
    nd$ta.5 <- FUN(ta.webs[[4]][[1]])[,2]
    nd$ta.6 <- FUN(ta.webs[[5]][[1]])[,2]
  }else if(ts.orig.web == length(foodwebs)) {
    nd$ta.2 <- FUN(ta.webs[[1]][[length(ta.webs[[1]])]])[,2]
    nd$ta.3 <- FUN(ta.webs[[2]][[length(ta.webs[[2]])]])[,2]
    nd$ta.4 <- FUN(ta.webs[[3]][[length(ta.webs[[3]])]])[,2]
    nd$ta.5 <- FUN(ta.webs[[4]][[length(ta.webs[[4]])]])[,2]
    nd$ta.6 <- FUN(ta.webs[[5]][[length(ta.webs[[5]])]])[,2]
  }else if(ts.orig.web == 2) {
    nd$ta.2.1 <- FUN(ta.webs[[1]][[1]])[,2]
    nd$ta.2.2 <- FUN(ta.webs[[1]][[2]])[,2]
    nd$ta.3.1 <- FUN(ta.webs[[2]][[1]])[,2]
    nd$ta.3.2 <- FUN(ta.webs[[2]][[2]])[,2]
    nd$ta.4.1 <- FUN(ta.webs[[3]][[1]])[,2]
    nd$ta.4.2 <- FUN(ta.webs[[3]][[2]])[,2]
    nd$ta.5.1 <- FUN(ta.webs[[4]][[1]])[,2]
    nd$ta.5.2 <- FUN(ta.webs[[4]][[2]])[,2]
    nd$ta.6 <- FUN(ta.webs[[5]][[1]])[,2]
  }else if(ts.orig.web == 3) {
    nd$ta.2.1 <- FUN(ta.webs[[1]][[2]])[,2]
    nd$ta.2.2 <- FUN(ta.webs[[1]][[3]])[,2]
    nd$ta.3.1 <- FUN(ta.webs[[2]][[1]])[,2]
    nd$ta.3.2 <- FUN(ta.webs[[2]][[2]])[,2]
    nd$ta.4.1 <- FUN(ta.webs[[3]][[1]])[,2]
    nd$ta.4.2 <- FUN(ta.webs[[3]][[2]])[,2]
    nd$ta.5.1 <- FUN(ta.webs[[4]][[1]])[,2]
    nd$ta.5.2 <- FUN(ta.webs[[4]][[2]])[,2]
    nd$ta.6 <- FUN(ta.webs[[5]][[1]])[,2]
  }else if(ts.orig.web == 4) {
    nd$ta.2.1 <- FUN(ta.webs[[1]][[3]])[,2]
    nd$ta.2.2 <- FUN(ta.webs[[1]][[4]])[,2]
    nd$ta.3.1 <- FUN(ta.webs[[2]][[2]])[,2]
    nd$ta.3.2 <- FUN(ta.webs[[2]][[3]])[,2]
    nd$ta.4.1 <- FUN(ta.webs[[3]][[1]])[,2]
    nd$ta.4.2 <- FUN(ta.webs[[3]][[2]])[,2]
    nd$ta.5.1 <- FUN(ta.webs[[4]][[1]])[,2]
    nd$ta.5.2 <- FUN(ta.webs[[4]][[2]])[,2]
    nd$ta.6 <- FUN(ta.webs[[5]][[1]])[,2]
  }else if(ts.orig.web == 5) {
    nd$ta.2.1 <- FUN(ta.webs[[1]][[4]])[,2]
    nd$ta.2.2 <- FUN(ta.webs[[1]][[5]])[,2]
    nd$ta.3.1 <- FUN(ta.webs[[2]][[3]])[,2]
    nd$ta.3.2 <- FUN(ta.webs[[2]][[4]])[,2]
    nd$ta.4.1 <- FUN(ta.webs[[3]][[2]])[,2]
    nd$ta.4.2 <- FUN(ta.webs[[3]][[3]])[,2]
    nd$ta.5.1 <- FUN(ta.webs[[4]][[1]])[,2]
    nd$ta.5.2 <- FUN(ta.webs[[4]][[2]])[,2]
    nd$ta.6 <- FUN(ta.webs[[5]][[1]])[,2]
  }
  return(nd)
}

