# Exercise 1.
randomize_super_boxmueller <- function(){
  RNGkind("Super", "Box-Muller")
  .Random.seed
}

# Exercise 2.
rnorm_with_seed <- function(seed){
  set.seed(seed)
  .Random.seed
  v1 = rnorm(7, mean=2, sd=1)
  print(v1)
}

# Exercise 3.
create_particular_distibutions <- function(){
  vec1=c(punif(-1,min=0, max=3),punif(0.5,min=0, max=3),
         punif(2,min=0, max=3),punif(5,min=0, max=3))
  print("Uniform distribution with min=0, max=3: ")
  print(vec1)
  
  vec2=c(punif(-1,min=-3, max=6),punif(0.5,min=-3, max=6),
         punif(2,min=-3, max=6),punif(5,min=-3, max=6))
  print("Uniform distribution with min=-3, max=6: ")
  print(vec2)
  
  vec3 = c(pnorm(-1,0,4),pnorm(0.5,0,4),pnorm(2,0,4),pnorm(5,0,4))
  print("Normal distribution with mean=0, sd=4: ")
  print(vec3)
  
  vec4 = c(pchisq(-1,5),pchisq(0.5,5),pchisq(2,5),pchisq(5,5))
  print("Chi-square distribution with 5 degrees freedom: ")
  print(vec4)
}

# Exercise 4.
check_particular_probabilities <- function(size, prob){
  r1 <- dbinom(x=4,size=size,prob=prob)
  cat("Dbinom with x = 4: ", r1)
  
  r2 <- dbinom(x=1,size=size,prob=prob)
  cat(", Dbinom with x = 1: ", r2)
  
  r3 <- pbinom(q=4,size=size,prob=prob,lower.tail = TRUE)
  cat(", Pbinom with q = 4: ", r3)
  
  r4 <- pbinom(q=9,size=size,prob=prob,lower.tail = FALSE)
  cat(", Pbinom with q = 9: ", r4)
}

# Exercise 5.
compare_particular_distibutions <- function(range1, range2, step){
  qn <- qnorm(seq(range1, range2, by=step),3,2)
  qu <- qunif(seq(range1, range2, by=step), -3, 3)
  qts <- qt(seq(range1, range2, by=step), 6)
  print("Normal: ")
  print(qn)
  print("Uniform: ")
  print(qu)
  print("t-Student: ")
  print(qts)
}

# Exercise 6.
create_table_with_norms <- function(size, mean1, sd1, mean2, sd2){
  vech = rnorm(size, mean1, sd1)
  vecw = rnorm(size, mean2, sd2)
  mat = matrix(c(vech, vecw), nrow=20, ncol=2)
  View(mat)
  write.table(mat,"Lab04.csv",
              col.names=c("Height","Weight"),row.names=FALSE)
}
