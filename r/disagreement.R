# Return a vector with the alternatives names
get_alternatives <- function(criterion) {
  number <- unlist(criterion_number[[criterion]])
  c(paste("Alt.", number, letters[1:5], sep = ""), paste("Alt.", number, "p", sep = ""))
}

disagreement <- function(criterion, results) {
  results <- results@data

  number <- unlist(criterion_number[[criterion]])
  results <- results[complete.cases(results[,paste("Alt.", number, "a.value", sep = "")]), ]
  alternatives <- get_alternatives(criterion)
  alternatives_nr <- c(paste(alternatives, ".nr", sep = ""))
  alternatives_cop <- c(paste(alternatives, ".cop", sep = ""))
  alternatives_nr_con <- c(paste(alternatives, ".nrcon", sep = ""))
  alternatives_nr_pro <- c(paste(alternatives, ".nrpro", sep = ""))
  alternatives_cval <- c(paste(alternatives, ".cval", sep = ""))
  alternatives_pval <- c(paste(alternatives, ".pval", sep = ""))
  alternatives_val <- c(paste(alternatives, ".val", sep = ""))
  result_names <- c()
  result_values <- c()
  tmp_results <- c()
  tmp_names <- c()
  
  for (i in 1:(length(alternatives) - 1)) {
    
    # Summarize nr, con, and pro 
    tmp_names <- c(tmp_names, c(alternatives_nr_con[i]))
    tmp_results <- c(tmp_results, c(sum(results[, alternatives_cop[i]] == 0)))
    tmp_names <- c(tmp_names, c(alternatives_nr_pro[i]))
    tmp_results <- c(tmp_results, c(sum(results[, alternatives_cop[i]] == 1)))
    tmp_names <- c(tmp_names, c(alternatives_nr[i]))
    tmp_results <- c(tmp_results, c(sum(sum(results[, alternatives_cop[i]] == 0), sum(results[, alternatives_cop[i]] == 1))))
    names(tmp_results) <- tmp_names
  
    tot_num <- tmp_results[alternatives_nr[i]]
    lambda <- 1 / tot_num
    
    # Con index
    result_names <- c(result_names, c(alternatives_cval[i]))
    result_values <- c(result_values, c(sum(results[, alternatives_cval[i]]) * lambda))
    
    # Pro index
    result_names <- c(result_names, c(alternatives_pval[i]))
    result_values <- c(result_values, c(sum(results[, alternatives_pval[i]]) * lambda))
    
    # Avg value
    result_names <- c(result_names, c(alternatives_val[i]))
    result_values <- c(result_values, c(sum(results[, alternatives_val[i]]) * lambda))
    
    # Number of members of the con group
    result_names <- c(result_names, c(alternatives_nr_con[i]))
    result_values <- c(result_values, c(tmp_results[alternatives_nr_con[i]]))
    
    # Number of members of the pro group
    result_names <- c(result_names, c(alternatives_nr_pro[i]))
    result_values <- c(result_values, c(tmp_results[alternatives_nr_pro[i]]))
    
  }
  
  names(result_values) <- result_names
  return(result_values)
}