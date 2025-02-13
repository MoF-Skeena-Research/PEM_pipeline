# df <- data.frame(
#   a = runif(1000000),
#   b = rnorm(1000000),
#   c = rexp(1000000)*4,
#   d = rgamma(1000000, shape = 2)
# )
# size = 100
# 
# system.time(clhs_fast(df,size, iter = 1000))
# system.time(clhs(df,size, iter = 1000

#library(Rcpp)
##Rcpp::sourceCpp('./_functions/CppCLHS.cpp')

lhs_obj <- function(
  data_continuous_sampled,
  continuous_strata,
  cor_mat,
  eta = 1
) {
  
  n_cont_variables <- ncol(data_continuous_sampled)
  
  cont_data_strata <- lapply(1:n_cont_variables, function(i) list(data_continuous_sampled[, i, drop = TRUE], continuous_strata[, i, drop = TRUE]) )
  cont_obj_sampled <- lapply(cont_data_strata, 
                             function(x) .Call(graphics:::C_BinCount, x[[1]], x[[2]], TRUE,TRUE))
  cont_obj_sampled <- matrix(unlist(cont_obj_sampled), ncol = n_cont_variables, byrow = FALSE)
  
  delta_obj_continuous <- rowSums(abs(cont_obj_sampled - eta))
  cor_sampled <- c_cor(data_continuous_sampled)
  
  delta_obj_cor <- sum(abs(cor_mat - cor_sampled))
  obj <- sum(delta_obj_continuous) +  delta_obj_cor*2
  list(obj = obj, delta_obj_continuous = delta_obj_continuous, delta_obj_cor = delta_obj_cor)
}

clhs_fast <- function(
  x, # data.frame
  size, # Number of samples you want
  include = NULL, # row index of data that must be in the final sample
  cost = NULL, # Number or name of the attribute used as a cost
  iter = 10000, # Number of max iterations
  temp = 1, # initial temperature
  tdecrease = 0.95, # temperature decrease rate
  eta = 1,
  obj.limit = -Inf, # Stopping criterion
  length.cycle = 8, # Number of cycles done at each constant temperature value
  simple = FALSE, # only return selected indices (if false, return a more complex S3 object)
  progress = TRUE, # progress bar,
  track = NULL # just to have the cost computed without having it guiding the process
) {
  
  
  ## No cost taking into account during optimisation
  if (is.null(cost)) {
    cost_mode <- FALSE
    
    if (!is.null(track)) {
      ## Track mode: tracking cost (without taking it into account during optimisation)
      
      # Get the id of the column used as a cost attribute
      if (is.numeric(track)) i_cost <- track
      else i_cost <- which(names(x) == track)
      
      # Get cost attribute column 
      cost <- x[ , i_cost, drop = FALSE]
      # Remove cost attribute from attribute table
      x <- x[, -1*i_cost, drop = FALSE]
      
      # Flags
      cost_mode <- TRUE
      track_mode <- TRUE
      
    } else { 
      ## No cost tracking, just plain optimisation of attributes
      track_mode <- FALSE
    }
    
  } else {
    ## Cost is taken into account for optimisation
    
    # Get the id of the column used as a cost attribute
    if (is.numeric(cost)) i_cost <- cost
    else i_cost <- which(names(x) == cost)
    
    if (!length(i_cost)) stop("Could not find the cost attribute.") 
    
    # Get cost attribute column 
    cost <- x[ , i_cost, drop = FALSE]
    # Remove cost attribute from attribute table
    x <- x[, -1*i_cost, drop = FALSE]
    
    # Si include, cost is 0
    if (!is.null(include)) {
      cost[include, ] <- 0
    }
    
    # Flags
    cost_mode <- TRUE
    track_mode <- FALSE # cost is taken into account, therefore computed
    
  }
  
  if (!is.null(include)) {
    if (size <= length(include)) {
      stop(paste0("size (", size, ") should be larger than length of include (", length(include), ")"))
    }
  }
  
  data_continuous <- as.matrix(x)
  
  metropolis <- exp(-1*0/temp) # Initial Metropolis value
  n_data <- nrow(data_continuous) # Number of individuals in the data set
  
  # Edge of the strata
  continuous_strata <- apply(
    data_continuous, 
    2, 
    function(x) {
      quantile(x, probs = seq(0, 1, length.out = size + 1), na.rm = TRUE)
    }
  )
  
  # Data correlation
  cor_mat <- c_cor(data_continuous)
  
  # Mandatory data in the sample
  sampled_size <- size - length(include)
  not_included <- setdiff(1:n_data, include) ##slow
  
  # initialise, pick randomly
  n_remainings <- n_data - size # number of individuals remaining unsampled
  i_sampled <- c(sample(not_included, size = sampled_size, replace = FALSE), include) # individuals randomly chosen
  i_unsampled <- setdiff(1:n_data, i_sampled) # individuals remaining unsampled
  data_continuous_sampled <- data_continuous[i_sampled, , drop = FALSE] # sampled continuous data
  data_continuous_sampled <- data_continuous_sampled[complete.cases(data_continuous_sampled),]
  
  res <- lhs_obj(data_continuous_sampled,continuous_strata, cor_mat)
  
  obj <- res$obj
  delta_obj_continuous <- res$delta_obj_continuous
  
  if (cost_mode) {
    # (initial) operational cost
    op_cost <- sum(cost[i_sampled, ])
    # vector storing operational costs
    op_cost_values <- vector(mode = 'numeric', length = iter)
  } else op_cost_values <- NULL
  
  # vector storing the values of the objective function
  obj_values <- vector(mode = 'numeric', length = iter)
  
  # progress bar
  if (progress) pb <- txtProgressBar(min = 1, max = iter, style = 3)
  
  if (any(duplicated(i_sampled))) browser() 
  if (any(i_sampled %in% i_unsampled)) browser()
  
  # storing previous values
  previous <- list()
  
  for (i in 1:iter) {
    previous$obj <- obj
    previous$i_sampled <- i_sampled
    previous$i_unsampled <- i_unsampled
    previous$delta_obj_continuous <- delta_obj_continuous
    
    if (cost_mode) previous$op_cost <- op_cost
    
    if (runif(1) < 0.5) {
      # pick a random sampled point and random unsampled point and swap them
      idx_removed <- sample(1:length(setdiff(i_sampled, include)), size = 1, replace = FALSE)
      spl_removed <- setdiff(i_sampled, include)[idx_removed]
      idx_added <- sample(1:length(i_unsampled), size = 1, replace = FALSE)
      i_sampled <- setdiff(i_sampled, include)[-idx_removed]
      i_sampled <- c(i_sampled, i_unsampled[idx_added], include)
      i_unsampled <- i_unsampled[-idx_added]
      i_unsampled <- c(i_unsampled, spl_removed)
      
      # creating new data sampled
      data_continuous_sampled <- data_continuous[i_sampled, , drop = FALSE]
    }else{
      # remove the worse sampled & resample
      worse <- max(delta_obj_continuous[!i_sampled %in% include])
      i_worse <- which(delta_obj_continuous[!i_sampled %in% include] == worse)
      # If there's more than one worse candidate, we pick one at random
      if (length(i_worse) > 1) i_worse <- sample(i_worse, size = 1)
      
      # swap with reservoir
      spl_removed <- setdiff(i_sampled, include)[i_worse] # will be removed from the sampled set. 
      idx_added <- sample(1:n_remainings, size = 1, replace = FALSE) # new candidate that will take their place
      i_sampled <- setdiff(i_sampled, include)[-i_worse]
      i_sampled <- c(i_sampled, i_unsampled[idx_added], include)
      i_unsampled <- i_unsampled[-idx_added]
      i_unsampled <- c(i_unsampled, spl_removed)
      
      # creating new data sampled
      data_continuous_sampled <- data_continuous[i_sampled, , drop = FALSE]
    }
    
    # calc obj
    res <- lhs_obj(data_continuous_sampled,continuous_strata,cor_mat)
    
    obj <- res$obj
    delta_obj_continuous <- res$delta_obj_continuous
    # Compare with previous iterations
    delta_obj <- obj - previous$obj
    metropolis <- exp(-1*delta_obj/temp) #+ runif(1)*temp
    
    if (cost_mode) {
      # op costs
      op_cost <- sum(cost[i_sampled, ])
      delta_cost <- op_cost - previous$op_cost
      if (track_mode) metropolis_cost <- Inf # runif(1) >= Inf is always FALSE
      else metropolis_cost <- exp(-1*delta_cost/temp)
    }
    else metropolis_cost <- Inf # runif(1) >= Inf is always FALSE
    
    # If the optimum has been reached
    if (obj <= obj.limit) {
      warning("\nThe objective function has reached its minimum value, as specified by the obj.limit option.")
      
      if (progress) {
        setTxtProgressBar(pb, i)
        close(pb)
      }
      
      obj_values[i] <- obj
      
      if (cost_mode) op_cost_values[i] <- op_cost
      
      break
    }
    
    # Revert change
    if (delta_obj > 0 & runif(1) >= metropolis | runif(1) >= metropolis_cost) {
      i_sampled <- previous$i_sampled
      i_unsampled <- previous$i_unsampled
      data_continuous_sampled <- data_continuous[i_sampled, , drop = FALSE]
      
      obj <- previous$obj
      delta_obj_continuous <- previous$delta_obj_continuous
      
      if (cost_mode) op_cost <- previous$op_cost
    }
    
    # Storing the objective function value of the current iteration
    obj_values[i] <- obj
    
    if (cost_mode) op_cost_values[i] <- op_cost
    
    # Temperature decrease
    if ((i %% length.cycle) == 0) temp <- temp*tdecrease
    
    # Update progress bar
    if (progress) setTxtProgressBar(pb, i)
  }
  
  # Close progress bar
  if (progress) close(pb)
  
  sampled_data <- data_continuous_sampled
  
  # Simple output - just the sampled object
  if (simple) res <- i_sampled
  else {
    # Making up the object to be returned
    res <- list(
      initial_object = x,
      index_samples = i_sampled, 
      sampled_data = sampled_data, 
      obj = obj_values,
      cost = op_cost_values
    )
    class(res) = c("cLHS_fast","list")
  }
  
  res
}
