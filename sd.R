## Standard deviation of alternative values.

  get_alternatives_sd <- function(criterion) {
    number <- unlist(criterion_number[[criterion]])
    c(paste("Alt.", number, letters[1:5], sep = ""))
  }
  
  calculateSD <- function(criterion, results) {
    results <- results@data
    number <- unlist(criterion_number[[criterion]])
    results <- results[complete.cases(results),]
    alternatives <- get_alternatives_sd(criterion)
    alternatives_cop <- c(paste(alternatives, ".cop", sep = ""))
    alternatives_val <- c(paste(alternatives, ".val", sep = ""))
    alternatives_con <- c(paste(alternatives, ".con", sep = ""))
    alternatives_pro <- c(paste(alternatives, ".pro", sep = ""))
    result_names <- c()
    result_values <- c()
    tmp_results <- c()
    tmp_names <- c()
    
    for (i in 1:(length(alternatives))) {
      result_names <- c(result_names, c(alternatives_con[i], alternatives_pro[i], alternatives_val[i]))
      results_con <- subset(results, eval(parse(text = paste0(alternatives_cop[i]))) == "0")
      results_pro <- subset(results, eval(parse(text = paste0(alternatives_cop[i]))) == "1")
      results_sd_con <- sd(unlist(results_con[alternatives_val[i]]))
      results_sd_pro <- sd(unlist(results_pro[alternatives_val[i]]))
      results_sd <- sd(unlist(results[alternatives_val[i]]))
      result_values <- c(result_values, c(respults_sd_con, results_sd_pro, results_sd))
    }
    
    names(result_values) <- result_names
    return(result_values)
  }