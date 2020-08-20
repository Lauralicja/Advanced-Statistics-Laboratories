# Exercise 1.
plot_ten_digits <- function(){
  x <- c(1:10)
  plot(x,x)
}

# Exercise 2. is just a histogram

# Exercise 3.
histogram_with_buckets <- function(df, buckets){
  hist(df, freq=FALSE, breaks=buckets)
}

# Exercise 4.
calculate_density <- function(col){
  density_g <- density(col)
  plot(density_g)
}

# Exercise 5. is just a boxplot

# Exercise 6.
spatial_data_multiple <- function(col1, col2, col3){
  sp(col2, col1)
  sp(col3, col1)
}