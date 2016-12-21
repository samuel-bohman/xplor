library(lpSolveAPI)
library(ggplot2)
library(plyr)
library('reshape2')

source("calculations.R")
source("helper.R")

# get all efficient portfolios
getAllPortfolios <- function(actions, values, disagreements, initialBudgetConstraint, direction) {
  portfolios <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  allSolutions <- findAllSolutions(actions, values, disagreements, initialBudgetConstraint, direction)
  portfolios <- rbind(portfolios, allSolutions)
  colnames(portfolios) <- c(actions, "value", "disagreement")
  portfolios
}

# find all solutions
findAllSolutions <- function(actions, values, disagreements, budgetConstraint, direction) {
  df <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  
  # first problem
  lp_model <- createModel(actions, values, disagreements, budgetConstraint, direction)
  solutions <- findSolutions(lp_model, actions, disagreements, direction)
  df <- rbind(df,solutions)
  colnames(df) <- c(actions, "value", "disagreement")

  # find more solutions
  while (TRUE) {
    budgetConstraint <- solutions[1,length(solutions)] - 0.0001
    lp_model <- createModel(actions, values, disagreements, budgetConstraint, direction)
    solutions <- findSolutions(lp_model, actions, disagreements, direction)
    colnames(solutions) <- c(actions, "value", "disagreement")
    df <- rbind(df, solutions)
    if (length(unique(as.list(solutions[, 1:length(actions)]))) == 1) {break}
  }
  df
}

# create knapsack model
createModel <- function(actions, values, disagreements, budgetConstraint, direction) {
  nrOfActions <- length(actions)
  budget <- sum(disagreements)
  lp_model <- make.lp(0, nrOfActions)
  set.objfn(lp_model, values)
  add.constraint(lp_model, disagreements, "<=", budgetConstraint)
  lp.control(lp_model, sense = direction)
  set.type(lp_model, 1:nrOfActions, "binary")
  lp_model
}

# find all subsolutions (same cost and value)
findSolutions <- function(lp_model, actions, disagreements, direction) {
  df <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  
  # first problem
  rc <- solve(lp_model)
  sols <- list()
  obj0 <- get.objective(lp_model)
  
  # find more solutions
  while (TRUE) {
    sol <- round(get.variables(lp_model))
    sum <- 0
    for (v in 1:length(sol)) {
      if (sol[v] == 1) {
        sum <- sum + disagreements[v]
      }
    }
    
    if (get.objective(lp_model) != obj0 - 1e-6) {
      newSolution <- c(sol, get.objective(lp_model), sum)
      df <- rbind(df, newSolution)
    }
    
    add.constraint(lp_model, 2 * sol - 1, "<=", sum(sol) - 1)
    rc <- solve(lp_model)

    if (rc != 0) {break}

    if (direction == "min") {
      if (get.objective(lp_model) > obj0 - 1e-6) {break} 
    }
    
    if (direction == "max") {
      if (get.objective(lp_model) < obj0 - 1e-6) {break} 
    }
  }
  df
}