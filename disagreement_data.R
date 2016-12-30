# Returns a vector with the alternatives names
get_alternatives <- function(criterion) {
  number <- unlist(criterion_number[[criterion]])
  c(paste("Alt.", number, letters[1:5], sep = ""), paste("Alt.", number, "p", sep = ""))
}

# 
get_criterion <- function(criterion) {
  number <- unlist(criterion_number[[criterion]])
  paste("wc", number, ".car", sep = "")
}

disagreement <- function(criterion, results){
  results <- results@data
  # Remove all NaN 
  results <- results[complete.cases(results), ]
  
  alternatives <- get_alternatives(criterion)
  alternatives_nr <- c(paste(alternatives, ".nr", sep = ""))
  alternatives_cop <- c(paste(alternatives, ".cop", sep = ""))
  alternatives_nrcon <- c(paste(alternatives, ".nrcon", sep = ""))
  alternatives_nrpro <- c(paste(alternatives, ".nrpro", sep = ""))
  alternatives_cval <- c(paste(alternatives, ".cval", sep = ""))
  alternatives_pval <- c(paste(alternatives, ".pval", sep = ""))
  alternatives_val <- c(paste(alternatives, ".val", sep = ""))
  resultNames.vec <- c()
  resultValues.vec <- c()

  tmpresults.vec <- c()
  tmpnames.vec <- c()
  
  for (i in 1:length(alternatives)) {
    # Sums nr, con, pro 
    tmpnames.vec <- c(tmpnames.vec, c(alternatives_nrcon[i]))
    tmpresults.vec <- c(tmpresults.vec, c(sum(results[, alternatives_cop[i]] == 0)))
    tmpnames.vec <- c(tmpnames.vec, c(alternatives_nrpro[i]))
    tmpresults.vec <- c(tmpresults.vec, c(sum(results[, alternatives_cop[i]] == 1)))
    tmpnames.vec <- c(tmpnames.vec, c(alternatives_nr[i]))
    tmpresults.vec <- c(tmpresults.vec, c(sum(sum(results[, alternatives_cop[i]] == 0), sum(results[, alternatives_cop[i]] == 1))))
    names(tmpresults.vec) <- tmpnames.vec
    
    # Calculate the con and pro index for q1a1
    # Lambda = stakeholder weight, 1/qXaY.nr
    # Create the total nr and the nr of cons and pros for each alternative
    
    
    # Add a column with weighted con values
    totNum <- tmpresults.vec[alternatives_nr[i]]
    lambda <- 1 / totNum
    #lambda_con_sum <- lambda * tmpresults.vec[alternatives_nrcon[i]]
    #lambda_pro_sum <- lambda * tmpresults.vec[alternatives_nrpro[i]]
    
    # Preparing return values
    # Con index
    resultNames.vec <- c(resultNames.vec, c(alternatives_cval[i]))
    resultValues.vec <- c(resultValues.vec, c(sum(results[, alternatives_cval[i]])*lambda))
    
    # Pro index
    resultNames.vec <- c(resultNames.vec, c(alternatives_pval[i]))
    resultValues.vec <- c(resultValues.vec, c(sum(results[, alternatives_pval[i]])*lambda))
    
    # Avg value
    resultNames.vec <- c(resultNames.vec, c(alternatives_val[i]))
    resultValues.vec <- c(resultValues.vec, c(sum(results[, alternatives_val[i]])))
    
    # Number of members of the con group
    resultNames.vec <- c(resultNames.vec, c(alternatives_nrcon[i]))
    resultValues.vec <- c(resultValues.vec, c(tmpresults.vec[alternatives_nrcon[i]]))
    
    # Number of members of the pro group
    resultNames.vec <- c(resultNames.vec, c(alternatives_nrpro[i]))
    resultValues.vec <- c(resultValues.vec, c(tmpresults.vec[alternatives_nrpro[i]]))
  }
  names(resultValues.vec) <- resultNames.vec
  return(resultValues.vec)
}