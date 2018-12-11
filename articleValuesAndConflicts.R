source("r/distance.R")
source("r/optimization.R")
source("global.R")
source("r/car.R")
source("r/car_ce.R")
library(xtable)

## Create alternative strings
get_portfolio_alternatives <- function(criterion) {
  number <- unlist(criterion_number[[criterion]])
  c(paste("Alt.", number, letters[1:5], sep = ""))
}

# Load the data
data <- readRDS("data-derived/data_spdf.rds")
data_g1 <- data[data$Gender %in% "Woman", ]
data_g2 <- data[data$Gender %in% "Man", ]
data_t <- rbind(data_g1,data_g2)
df <- NULL


createValueConflictLatexTables <- function(df){
  
  is.num <- sapply(df, is.numeric)
  df[is.num] <- lapply(df[is.num], round, 4)
  df <- format(df, digits=4) 
  
  xtab <- xtable(df[,1:7], caption = "The actions associated value and conflict index.", label = "tab:valcon")
  align(xtab) <- c("l","l","r","r","r","r","r","r")
  
  options(xtable.comment = FALSE)
  #filename <- paste('latex/portfolios_', focusarea, '_', grupp, '.txt', sep="")
  filename <- paste('latex/actionvaluesconflict.tex')
  sink(filename, append = FALSE)
  print.xtable(xtab, include.rownames=FALSE, caption.placement = "top")
  sink()
}

createValueConflictDataFrame <- function(focusarea, data_g1, data_g2, data_t){
  ### Distances
  data.vec1 <- distance(focusarea, data_g1)
  data.vec2 <- distance(focusarea, data_g2)
  data.vec12 <- distance(focusarea, data_t)
  
  ### Calculate total mean weighted values
  val_total <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec12[x + 6] + data.vec12[x + 7])
    return(data.vec12[x + 5] * lambda)
  })
  
  ### Calculate total distance
  dis_total <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec12[x + 6] + data.vec12[x + 7]) ^ 2
    beta <- 1 / (lambda * (data.vec12[x + 6] + data.vec12[x + 7]))
    T1 <- data.vec1[x] * lambda
    T2 <- data.vec2[x] * lambda
    T12 <- data.vec12[x] * lambda
    C1 <- data.vec1[x + 1] * lambda
    C2 <- data.vec2[x + 1] * lambda
    C12 <- data.vec12[x + 1] * lambda
    P1 <- data.vec1[x + 2] * lambda
    P2 <- data.vec2[x + 2] * lambda
    P12 <- data.vec12[x + 2] * lambda
    sqrt(beta * abs((T12 - (T1 + T2)) - ((C12 - (C1 + C2)) + (P12 - (P1 + P2)))))
  })
  
  ### Calculate distance within the group
  dis_group_1 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec1[x + 6] + data.vec1[x + 7]) ^ 2
    beta <- 1 / (lambda * (data.vec1[x + 6] + data.vec1[x + 7]))
    sqrt(beta * (data.vec1[x] * lambda - (data.vec1[x + 1] * lambda + data.vec1[x + 2] * lambda)))
  })
  
  ### Calculate total mean weighted values
  val_group_1 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec1[x + 6] + data.vec1[x + 7])
    return(data.vec1[x + 5] * lambda)
  })
  
  ### Calculate distance within the group
  dis_group_2 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec2[x + 6] + data.vec2[x + 7]) ^ 2
    beta <- 1 / (lambda * (data.vec2[x + 6] + data.vec2[x + 7]))
    sqrt(beta * (data.vec2[x] * lambda - (data.vec2[x + 1] * lambda + data.vec2[x + 2] * lambda)))
  })
  
  ### Calculate total mean weighted values 
  val_group_2 <- lapply(seq(1, 45, by = 9), function(x) {
    lambda = 1 / (data.vec2[x + 6] + data.vec2[x + 7])
    return(data.vec2[x + 5] * lambda)
  })
  
  df <- data.frame(unlist(val_group_1), unlist(val_group_2), unlist(val_total), unlist(dis_group_1), unlist(dis_group_2), unlist(dis_total))
  rownames(df) <- get_portfolio_alternatives(focusarea)
  return(df)
}

for(i in 1:length(theme)){
  focusarea <- theme[i]
  df <- rbind(df, createValueConflictDataFrame(focusarea, data_g1, data_g2, data_t))
}  

df <- data.frame(names = row.names(df), df)
rownames(df) = 1:nrow(df)
colnames(df) <- c("Action", "Value G1", "Value G2", "Value T", "Conflict G1", "Conflict G2", "Conflict T")

createValueConflictLatexTables(df)
