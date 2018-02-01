# Return a vector with the alternatives names
get_alternatives <- function(criterion) {
  number <- unlist(criterion_number[[criterion]])
  c(paste("Alt.", number, letters[1:5], sep = ""), paste("Alt.", number, "p", sep = ""))
}

disagreement <- function(criterion, spdf) {
  data <- spdf@data
  lambda <- 1 / nrow(data)
  
  number <- unlist(criterion_number[[criterion]])
  data <- data[complete.cases(data[,paste("Alt.", number, "a.value", sep = "")]), ]
  alternatives <- get_alternatives(criterion)
  alternatives_nr <- c(paste(alternatives, ".nr", sep = ""))
  alternatives_cop <- c(paste(alternatives, ".cop", sep = ""))
  alternatives_nr_con <- c(paste(alternatives, ".nrcon", sep = ""))
  alternatives_nr_pro <- c(paste(alternatives, ".nrpro", sep = ""))
  alternatives_cval <- c(paste(alternatives, ".cval", sep = ""))
  alternatives_pval <- c(paste(alternatives, ".pval", sep = ""))
  alternatives_cvar <- c(paste(alternatives, ".cvar", sep = ""))
  alternatives_pvar <- c(paste(alternatives, ".pvar", sep = ""))
  alternatives_var <- c(paste(alternatives, ".var", sep = ""))
  alternatives_cmean <- c(paste(alternatives, ".cmean", sep = ""))
  alternatives_pmean <- c(paste(alternatives, ".pmean", sep = ""))
  alternatives_val <- c(paste(alternatives, ".val", sep = ""))
  mean_values <- c()
  mean_names <- c()
  mean_valuesc <- c()
  mean_namesc <- c()
  mean_valuesp <- c()
  mean_namesp <- c()
  result_names <- c()
  result_values <- c()
  tmp_data <- c()
  tmp_names <- c()
  
  for (i in 1:(length(alternatives) - 1)) {
    # mean both groups
    mean_names <- c(mean_names, c(alternatives_cmean[i]))
    mean_values <- c(mean_values, c(mean(data[, alternatives_val[i]])))    

    # Con mean
    mean_namesc <- c(mean_names, c(alternatives_cmean[i]))
    mean_valuesc <- c(mean_valuesc, c(mean(data[, alternatives_cval[i]])))

    # Pro mean
    mean_namesp <- c(mean_names, c(alternatives_pmean[i]))
    mean_valuesp <- c(mean_valuesp, c(mean(data[, alternatives_pval[i]])))
  }
  
  for (i in 1:(length(alternatives) - 1)) {
    data[alternatives_cvar[i]] <-
      apply(data, 1, function(x) {
        if (as.numeric(x[alternatives_cop[i]]) == 0) {
          cvar <- ((as.numeric(x[alternatives_cval[i]]) - as.numeric(mean_valuesc[i]))^2)*lambda
        } else {0}
      })
    
      data[alternatives_pvar[i]] <-
      apply(data, 1, function(x) {
        if (as.numeric(x[alternatives_cop[i]]) == 1) {
          pvar <- ((as.numeric(x[alternatives_pval[i]]) - as.numeric(mean_valuesp[i]))^2)*lambda
        } else {0}
      })
    
      data[alternatives_var[i]] <-
        apply(data, 1, function(x) {
          var <- ((as.numeric(x[alternatives_val[i]]) - as.numeric(mean_values[i]))^2)*lambda
        }) 
  }
    # prepare results
  for (i in 1:(length(alternatives) - 1)) {
    
    # Summarize nr, con, and pro 
    tmp_names <- c(tmp_names, c(alternatives_nr_con[i]))
    tmp_data <- c(tmp_data, c(sum(data[, alternatives_cop[i]] == 0)))
    tmp_names <- c(tmp_names, c(alternatives_nr_pro[i]))
    tmp_data <- c(tmp_data, c(sum(data[, alternatives_cop[i]] == 1)))
    tmp_names <- c(tmp_names, c(alternatives_nr[i]))
    tmp_data <- c(tmp_data, c(sum(sum(data[, alternatives_cop[i]] == 0), sum(data[, alternatives_cop[i]] == 1))))
    names(tmp_data) <- tmp_names

    # result_values is the distance between the pro and con groups for alternative i.
    result_names <- c(result_names, c(alternatives_var[i]))
    result_values <- c(result_values, c(abs(sum(data[, alternatives_cvar[i]])+sum(data[, alternatives_pvar[i]])-sum(data[, alternatives_var[i]]))))
    
    # Con index
    result_names <- c(result_names, c(alternatives_cval[i]))
    result_values <- c(result_values, c(sum(data[, alternatives_cval[i]]) * lambda))
     
    # Pro index
    result_names <- c(result_names, c(alternatives_pval[i]))
    result_values <- c(result_values, c(sum(data[, alternatives_pval[i]]) * lambda))
     
    # Avg value
    result_names <- c(result_names, c(alternatives_val[i]))
    result_values <- c(result_values, c(sum(data[, alternatives_val[i]])) * lambda)
 
    # Number of members of the con group
    result_names <- c(result_names, c(alternatives_nr_con[i]))
    result_values <- c(result_values, c(tmp_data[alternatives_nr_con[i]]))
     
    # Number of members of the pro group
    result_names <- c(result_names, c(alternatives_nr_pro[i]))
    result_values <- c(result_values, c(tmp_data[alternatives_nr_pro[i]]))
  }
  
  names(result_values) <- result_names
  return(result_values)
}
