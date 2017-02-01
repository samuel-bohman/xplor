# Get all efficient portfolios
get_all_portfolios <- function(actions, values, disagreements, initial_budget_constraint, direction) {
  portfolios <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  all_solutions <- find_all_solutions(actions, values, disagreements, initial_budget_constraint, direction)
  portfolios <- rbind(portfolios, all_solutions)
  colnames(portfolios) <- c(actions, "value", "disagreement")
  portfolios
}

# Find all solutions
find_all_solutions <- function(actions, values, disagreements, budget_constraint, direction) {
  df <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  
  # First problem
  lp_model <- create_model(actions, values, disagreements, budget_constraint, direction)
  solutions <- find_solutions(lp_model, actions, disagreements, direction)
  df <- rbind(df, solutions)
  colnames(df) <- c(actions, "value", "disagreement")

  # Find more solutions
  while (TRUE) {
    budget_constraint <- solutions[1,length(solutions)] - 0.0001
    lp_model <- create_model(actions, values, disagreements, budget_constraint, direction)
    solutions <- find_solutions(lp_model, actions, disagreements, direction)
    colnames(solutions) <- c(actions, "value", "disagreement")
    df <- rbind(df, solutions)
    if (length(unique(as.list(solutions[, 1:length(actions)]))) == 1) {break}
  }
  df
}

# Create knapsack model
create_model <- function(actions, values, disagreements, budget_constraint, direction) {
  no_of_actions <- length(actions)
  budget <- sum(disagreements)
  lp_model <- make.lp(0, no_of_actions)
  set.objfn(lp_model, values)
  add.constraint(lp_model, disagreements, "<=", budget_constraint)
  lp.control(lp_model, sense = direction)
  set.type(lp_model, 1:no_of_actions, "binary")
  lp_model
}

# Find all subsolutions (same cost and value)
find_solutions <- function(lp_model, actions, disagreements, direction) {
  df <- data.frame(matrix(ncol = length(actions) + 2, nrow = 0))
  
  # First problem
  rc <- solve(lp_model)
  sols <- list()
  obj0 <- get.objective(lp_model)
  
  # Find more solutions
  while (TRUE) {
    sol <- round(get.variables(lp_model))
    sum <- 0
    for (v in 1:length(sol)) {
      if (sol[v] == 1) {
        sum <- sum + disagreements[v]
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