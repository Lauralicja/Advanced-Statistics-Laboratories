# Exercise 1.
shapiro_and_chi_test <- function(vec1, vec2){
  print(shapiro.test(vec1))
  print(chisq.test(vec2))
}

# Exercise 2.and 3.
ks_testing <- function(vec1, vec2){
  res = ks.test(vec1, vec2)
  if (res[2] > 0.05){
    print("The same distributions")
  }
  else {
    print("different distributions")
  }
}

# Exercise 4.
test_mean <- function(vec, mean){
  test <- t.test(vec, mu=mean)
  if (round(test[5]$estimate, 2) == mean){
    print("The same mean")
  }
  else{
    cat("Different mean: ", test[5]$estimate)
  }
}

# Exercise 5.
test_two_means <- function(vec1, vec2){
  test <- t.test(vec1, vec2, var.equal = FALSE)
  print(test)
}

# Exercise 6.
test_variances <- function(vec1, vec1){
  test <- var.test(vec1, vec2)
  print(test)
}

# Exercise 7.
test_correlations <- function(vec1, vec2){
  test <- cor.test(vec1, vec2)
  print(test)
}


