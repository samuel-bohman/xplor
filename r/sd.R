# Standard deviation of alternative values
  get_alternatives_sd <- function(criterion) {
    number <- unlist(criterion_number[[criterion]])
    c(paste("Alt.", number, letters[1:5], sep = ""))
  }
  
  calculateSD <- function(criterion, data) {
    data <- data@data
    number <- unlist(criterion_number[[criterion]])
    data <- data[complete.cases(data),]
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
    tmp_data <- c()
    tmp_names <- c()
    
    for (i in 1:(length(alternatives))) {
      
      result_names <- c(result_names, c(alternatives_mean[i], alternatives_con_mean[i], alternatives_pro_mean[i], alternatives_sd[i], alternatives_con_sd[i], alternatives_pro_sd[i]))
      
      data_con <- subset(data, eval(parse(text = paste0(alternatives_cop[i]))) == "0")
      data_pro <- subset(data, eval(parse(text = paste0(alternatives_cop[i]))) == "1")
      
      data_mean <- mean(unlist(data[alternatives_val[i]]))
      data_mean_con <- mean(unlist(data_con[alternatives_val[i]]))
      data_mean_pro <- mean(unlist(data_pro[alternatives_val[i]]))
      
      data_sd <- sd(unlist(data[alternatives_val[i]]))
      data_sd_con <- sd(unlist(data_con[alternatives_val[i]]))
      data_sd_pro <- sd(unlist(data_pro[alternatives_val[i]]))

      result_values <- c(result_values, c(data_mean, data_mean_con, data_mean_pro, data_sd, data_sd_con, data_sd_pro))
    }
    
    names(result_values) <- result_names
    return(result_values)
  }
  