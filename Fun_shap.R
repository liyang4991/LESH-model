# utils for calculate SHAP value

library(stats)

# get_value_by_vars <- function(vars, pd_series) {
#   # Get the value from pd_series by variables in vars
#   # Returns: a scalar value
#   if (length(vars) == 0) {
#     pd <- 0
#   } else if (length(vars) == 1) {
#     pd <- pd_series[vars]
#   } else {
#     key_vars <- paste(vars, collapse = "_")
#     pd <- pd_series[key_vars]
#   }
#   return(pd)
# }

get_value_by_vars <- function(vars, pd_series) {
  
  # Get the value from pd_series by variables in vars
  # Returns: a scalar value
  
  if (length(vars) == 0) {
    pd <- 0
  } else if (length(vars) == 1) {
    pd <- pd_series[vars,'q.variance']
  } else {
    key_vars <- paste(vars, collapse = "_")
    pd <- pd_series[key_vars,'q.variance']
  }
  return(pd)
}

# Create function to calculate Cnm
Cnm <- function(n, m) {
  return(factorial(n)/(factorial(m)*factorial(n-m)))
}

# Create a function to sort variables
sorted_var_to_key <- function(vars, VARS_LIST) {
  sorted_vars <- c()
  for (i in VARS_LIST){
    if (i %in% vars){
      sorted_vars <- append(sorted_vars, i)
    }
  }
  return(sorted_vars)
}

# Create a function
Shap_calcute <- function(vars, pd_data) {
  
  # Calculate SHAP value for each variable in vars
  # Return: a named list of SHAP values
  
  VARS_LIST = vars
  result_dict <- setNames(rep(0, length(vars)), nm = vars)
  M <- length(vars)
  
  for (var in vars) {
    vars_copy = vars
    vars_copy <- vars_copy[vars_copy != var]
    pd <- 0
    for (i in 0:(M-1)) {
      all_situations = combn(vars_copy, i)
      dim_all_situations = dim(all_situations)
      
      for (s in 1:(dim_all_situations[2])) {
        situation = all_situations[,s]
        merge_situation <- sorted_var_to_key(c(var, situation), VARS_LIST)
        pd_patch1 <- get_value_by_vars(c(merge_situation),  pd_data)
        
        situation <- sorted_var_to_key(situation, VARS_LIST)
        pd_patch2 <- get_value_by_vars(c(situation),  pd_data)
        
        pd_patch3 <- 1/(Cnm(M-1, i))*(pd_patch1 - pd_patch2)
        pd_patch4 <- 1/M
        pd <- pd + pd_patch4 * pd_patch3
      }
    }
    result_dict[var] <- pd
  }
  return(result_dict)
}

Shap_calculate_by_var <- function(var, sets, pd_data) {
  # Calculate SHAP value for a given variable var in the set sets
  # Return: a scalar SHAP value
  
  M <- length(sets) # Equivalent to set M
  sets_copy <- sets[sets != var]
  pd <- 0
  for (i in 0:(M-1)) {
    all_situations = combn(sets_copy, i)
    dim_all_situations = dim(all_situations)
    
    for (s in 1:(dim_all_situations[2]) ) {
      situation = all_situations[,s]
      merge_situation <- sorted_var_to_key(c(var, situation))
      pd_patch1 <- get_value_by_vars(merge_situation, pd_data)
      
      situation <- sorted_var_to_key(situation)
      pd_patch2 <- get_value_by_vars(situation, pd_data)
      
      pd_patch3 <- 1/(Cnm(M-1, i))*(pd_patch1 - pd_patch2)
      pd_patch4 <- 1/M
      pd <- pd + pd_patch4 * pd_patch3
    }
  }
  return(pd)
}

