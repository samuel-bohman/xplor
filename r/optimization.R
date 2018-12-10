# Get all efficient portfolios
get_all_portfolios <- function(actions, values, distance, initial_budget_constraint, direction) {

  if(is.nan(values) || is.nan(distance) || is.null(distance) || is.null(values) || is.na(values) || is.na(distance)){
    portfolios <- data.frame(matrix(data = 0, ncol = length(actions) + 2, nrow = 2))
  } else {
    portfolios <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
    all_solutions <- find_all_solutions(actions, values, distance, initial_budget_constraint, direction)
    portfolios <- rbind(portfolios, all_solutions)
  }
  colnames(portfolios) <- c(actions, "value", "distance")
  portfolios
}

# Find all solutions
find_all_solutions <- function(actions, values, distance, budget_constraint, direction) {
  df <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  
  # First problem
  lp_model <- create_model(actions, values, distance, budget_constraint, direction)
  solutions <- find_solutions(lp_model, actions, distance, direction)
  df <- rbind(df, solutions)
  colnames(df) <- c(actions, "value", "distance")

  # Find more solutions
  while (TRUE) {
    budget_constraint <- solutions[1,length(solutions)] - 0.0001
    lp_model <- create_model(actions, values, distance, budget_constraint, direction)
    solutions <- find_solutions(lp_model, actions, distance, direction)
    colnames(solutions) <- c(actions, "value", "distance")
    df <- rbind(solutions, df)
    if (length(unique(as.list(solutions[, 1:length(actions)]))) == 1) {break}
  }
  df
}

# Create knapsack model
create_model <- function(actions, values, distance, budget_constraint, direction) {
  no_of_actions <- length(actions)
  lp_model <- make.lp(0, no_of_actions)
  set.objfn(lp_model, values)
  add.constraint(lp_model, distance, "<=", budget_constraint)
  lp.control(lp_model, sense = direction)
  set.type(lp_model, 1:no_of_actions, "binary")
  lp_model
}

# Find all subsolutions (same cost and value)
find_solutions <- function(lp_model, actions, distance, direction) {
  
  # Warning: Error in if: missing value where TRUE/FALSE needed
  # Stack trace (innermost first):
  #   62: find_solutions [optimization.R#58]
  #     61: find_all_solutions [optimization.R#24]
  #       60: get_all_portfolios [optimization.R#4]
  #         59: eval [/home/samuel/xplor/r/server.R#698]
  #           58: eval
  #           57: withProgress
  #           56: observerFunc [/home/samuel/xplor/r/server.R#203]
  #             1: runApp 
  #req(lp_model)
  #req(actions)
  #req(distance)
  #req(direction)
  
  df <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  
  # First problem
  rc <- solve(lp_model)
  obj0 <- get.objective(lp_model)
  
  # Find more solutions
  while (TRUE) {
    
    sol <- round(get.variables(lp_model))
    sum <- 0
    
    # Warning: Error in if: missing value where TRUE/FALSE needed
    # Stack trace (innermost first):
    #   62: find_solutions [optimization.R#73]
    #     61: find_all_solutions [optimization.R#24]
    #       60: get_all_portfolios [optimization.R#4]
    #         59: eval [/home/samuel/xplor/r/server.R#793]
    #           58: eval
    #           57: withProgress
    #           56: observerFunc [/home/samuel/xplor/r/server.R#203]
    #             1: runApp
    if (!NA %in% sol){
      for (v in 1:length(sol)) {
        if (sol[v] == 1) {
          sum <- sum + distance[v]
        }
      }
    }
    
    if (get.objective(lp_model) != obj0 - 1e-6) {
      new_solution <- c(sol, get.objective(lp_model), sum)
      df <- rbind(df, new_solution)
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
