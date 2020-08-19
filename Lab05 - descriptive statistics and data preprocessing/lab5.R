# Exercise 1. and 2.
fill_data_diferent_methods <- function(df){
  A <- impute(df, fun="random")
  B <- impute(df, fun=mean)
  C <- impute(df, fun=median)
  print("Random method")
  print(A[1])
  print("By mean")
  print(B[1])
  print("By median")
  print(C[1])
}

# Exercise 3. (FALSE = 0, TRUE = 1)
data_scaling <- function(df, mean, sd){
  D <- scale(df, center=mean, scale=sd)
  print(D)
}

# Exercise 4.
transform_column_as_factors <- function(df, col){
  E <- transform(df, col = as.factor(col))
  print(E)
}

# Exercise 5.
find_IQR_and_corr <- function(df, col1, col2){
  iqr <- IQR(col1)
  cat("IQR = ", iqr)
  cors = cor(col1, col2)
  cat(", Correlation = ", cors)
}

# Exercise 6.
find_summaries_and_corr <- function(col1, col2){
  A_sum <- summary(col1)
  B_sum <- summary(col2)
  AB_cors = cor(A_sum, B_sum)
  print(A_sum)
  print(B_sum)
  print(AB_cors)
}