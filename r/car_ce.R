# Calculate bipolar cardinal ranking
calculateCAR_CE <- function(results) {
  
  for (j in 1:10) {
    alternatives <- c(paste("Alt.", j, letters[1:5], sep = ""), paste("Alt.", j, "p", sep = ""))
    alternatives_prop_scores <- c(paste(alternatives, ".value", sep = ""))
    alternatives_cop <- c(paste(alternatives, ".cop", sep = ""))
    alternatives_cval <- c(paste(alternatives, ".cval", sep = ""))
    alternatives_pval <- c(paste(alternatives, ".pval", sep = ""))
    alternatives_val <- c(paste(alternatives, ".val", sep = ""))
    alternatives_valuew <- c(paste(alternatives, ".valuew", sep = ""))
    pseudo.name <- tail(alternatives, n = 1)
    pseudo.name_prop_score <- tail(alternatives_prop_scores, n = 1)
    criterion_name <- paste("wc", j, ".car", sep = "")
    
    # Add new columns for the proportional scores in the bipolar cardinal ranking of alternatives
    results[pseudo.name] <- 7
    
    for (i in 1:length(alternatives)) {
      
      # Add columns with proportial scores
      results[alternatives_prop_scores[i]] <- 
        apply(results, 1, function(x) {
          
          # Find the min and max values for question 1 and action 1-5 and the pseudo action (6)
          v.max <- max(as.numeric(x[c(alternatives)]))
          v.min <- min(as.numeric(x[c(alternatives)]))
          
          # Calculate the proportinal score for question j and action i
          #(as.numeric(x[alternatives[i]]) - v.min) / (v.max - v.min)
          # Changed to eq. 16 in "Scaling issues in additive multicriteria portfolio analysis" by Almeida et al 2014.
          as.numeric(x[alternatives[i]]) / (v.max - v.min)
        })
    }
    
    # Iterate over the alternatives-column # a6 is the pseudo alternative!
    for (i in 1:length(alternatives)) {
      
      # Add a con/pro column for each alternative
      results[alternatives_cop[i]] <-
        apply(results, 1, function(x) {
          if (as.numeric(x[alternatives[i]]) < as.numeric(x[pseudo.name])) {
            res <- 0
          } else {res <- 1}
        })
      
      # Add a column with weighted con values
      results[alternatives_cval[i]] <-
        apply(results, 1, function(x) {
          if (as.numeric(x[alternatives_cop[i]]) == 0) {
            qShAlpha = (as.numeric(x[criterion_name]) * as.numeric(x[pseudo.name_prop_score]))
            qShPartWorth = (as.numeric(x[criterion_name]) * as.numeric(x[alternatives_prop_scores[i]]))
            (qShPartWorth - qShAlpha)
          } else {0}
        })
      
      # Add a column with weighted pro values
      results[alternatives_pval[i]] <-
        apply(results, 1, function(x) {
          if (as.numeric(x[alternatives_cop[i]]) == 1) {
            qShAlpha = (as.numeric(x[criterion_name]) * as.numeric(x[pseudo.name_prop_score]))
            qShPartWorth = (as.numeric(x[criterion_name]) * as.numeric(x[alternatives_prop_scores[i]]))
            (qShPartWorth - qShAlpha)
          } else {0}
        })
      
      # Add a column with weighted values
      results[alternatives_val[i]] <-
        apply(results, 1, function(x) {
          qShAlpha = (as.numeric(x[criterion_name]) * as.numeric(x[pseudo.name_prop_score]))
          qShPartWorth = (as.numeric(x[criterion_name]) * as.numeric(x[alternatives_prop_scores[i]]))
          (qShPartWorth - qShAlpha)
        })
      
      # Add a column with weighted proportional scores
      results[alternatives_valuew[i]] <-
        apply(results, 1, function(x) {
          #qShAlpha = (as.numeric(x[criterion_name]) * as.numeric(x[pseudo.name_prop_score]))
          qShPartWorth = (as.numeric(x[criterion_name]) * as.numeric(x[alternatives_prop_scores[i]]))
        })
    }
  }
  return(results)
}
