# Exercise 1.
add_two <- function(first, second= 1){
  first + second
}

# Exercise 2. 
check_if_in_cector <- function(integer, vector){
  if (any(vector == integer)) {
    return(TRUE)
  } else {
    return(FALSE) 
    }
}

# Exercise 3.
# square if even function
square_even_if_even <- function(vector){
  if (length(vector) %% 2 == 0){ # if even
    for (i in 1:length(vector)){ 
      if(i %% 2 == 0){ # for every even index
        vector[i] <- vector[i]*vector[i]
      }
    }
  } else {
    for (i in 1:length(vector)){
      if(i %% 2 == 1){ # for every odd index
        vector[i] <- vector[i]*vector[i]
      }
    }
  }
  print(vector)
}
# square if odd function
square_even_if_odd <- function(vector){
  if (length(vector) %% 2 == 1){ # if odd
    for (i in 1:length(vector)){ 
      if(i %% 2 == 0){ # for every even index
        vector[i] <- vector[i]*vector[i]
      }
    }
  } else {
    for (i in 1:length(vector)){
      if(i %% 2 == 1){ # for every odd index
        vector[i] <- vector[i]*vector[i]
      }
    }
  }
  print(vector)
}

evenVector <- c(2,2,2,2)
oddVector <- c(2,2,2,2,2)

square_even_if_even(evenVector) # prints: [1] 2 4 2 4
square_even_if_even(oddVector) # prints: [1] 4 2 4 2 4
square_even_if_odd(evenVector) # prints: [1] 4 2 4 2
square_even_if_odd(oddVector) # prints: [1] 2 4 2 4 2

# Exercise 4.
print_three_smallest <- function(vector){
  print(sort(vector)[1:3])
}

# Exercise 5.
# used "expm" library
power_that_matrix <- function(matrx = matrix(0,nrow=2, ncol=2)){
  library(expm)
  matrx %^% dim(matrx)
}

# Exercise 6.
# operator for sum of two values given squared 
"%oper%" <- function(x,y){
  (x + y)^2
}

# Exercise 7.
# write out the logical values
# if the argument is logical, write out the square of all values if the argument is numerical, and write out the
# dimension of the matrix if the argument is a matrix.
# so if I have a numerical matrix it should write both
# + size by default
write_values_by_type <- function(some_object){
  if (is.logical(some_object)){
    print(some_object)
  }
  if (is.numeric(some_object) || is.integer(some_object)){
    print(some_object^2)
  }
  if (is.matrix(some_object)) {
    print(dim(some_object))
  }
    return(length(some_object))
}
