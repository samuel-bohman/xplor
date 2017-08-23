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
    alternatives_sd <- c(paste(alternatives, ".sd", sep = ""))
    alternatives_con_sd <- c(paste(alternatives, ".con_sd", sep = ""))
    alternatives_pro_sd <- c(paste(alternatives, ".pro_sd", sep = ""))
    alternatives_mean <- c(paste(alternatives, ".mean", sep = ""))
    alternatives_con_mean <- c(paste(alternatives, ".con_mean", sep = ""))
    alternatives_pro_mean <- c(paste(alternatives, ".pro_mean", sep = ""))
    result_names <- c()
    result_values <- c()
    tmp_results <- c()
    tmp_names <- c()
    
    for (i in 1:(length(alternatives))) {
      
      result_names <- c(result_names, c(alternatives_mean[i], alternatives_con_mean[i], alternatives_pro_mean[i], alternatives_sd[i], alternatives_con_sd[i], alternatives_pro_sd[i]))
      
      results_con <- subset(results, eval(parse(text = paste0(alternatives_cop[i]))) == "0")
      results_pro <- subset(results, eval(parse(text = paste0(alternatives_cop[i]))) == "1")
      
      results_mean <- mean(unlist(results[alternatives_val[i]]))
      results_mean_con <- mean(unlist(results_con[alternatives_val[i]]))
      results_mean_pro <- mean(unlist(results_pro[alternatives_val[i]]))
      
      results_sd <- sd(unlist(results[alternatives_val[i]]))
      results_sd_con <- sd(unlist(results_con[alternatives_val[i]]))
      results_sd_pro <- sd(unlist(results_pro[alternatives_val[i]]))

      result_values <- c(result_values, c(results_mean, results_mean_con, results_mean_pro, results_sd, results_sd_con, results_sd_pro))
    }
    
    names(result_values) <- result_names
    return(result_values)
  }