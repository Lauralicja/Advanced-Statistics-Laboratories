# Exercise 1.
create_estimates <- function(vec){
  out <- tryCatch({
      normal <- fitdistr(vec, "normal")
      print("Normal: ")
      print(normal)
  }, error=function(e){
      message("Cannot estimate normal values for this vector")
    }
  )
  out <- tryCatch({
      t_student <- fitdistr(vec, "t")
      print("t-Student:")
      print(t_student)
  }, error=function(e){
    message("Cannot estimate t-student values for this vector")
    }
  )
  out <- tryCatch({
      exponential <- fitdistr(vec, "exponential") 
      # remember to keep it in 0-1 range
      print("Exponential: ")
      print(exponential)
  }, error=function(e){
    message("Cannot estimate exponential values for this vector")
    }
  )
  out <- tryCatch({
      geometric <- fitdistr(vec, "geometric") 
      print("Geometric: ")
      print(geometric)
  }, error=function(e){
    message("Cannot estimate geometric values for this vector")
    }
  )
}

# Exercise 2. and 3
create_normal_and_exponential <- function(file1, file2){
  v1 <- as.numeric(file1$x)
  v2 <- as.numeric(file2$x)
  estimation_normal <- fitdistr(v1, "normal")
  estimation_exponential <- fitdistr(v2, "exponential")
  print(estimation_normal)
  print(estimation_exponential)
}

# Exercise 4. is just normal distr with estimation
