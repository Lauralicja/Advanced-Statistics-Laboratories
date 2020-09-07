# Exercise 1.
plot_reg <- function(col1, col2){
  plot(col1, col2)
  reg <- lm(col2~col1)
  abline(reg)
  print(reg)
}

# Exercise 2.
lin_reg_predictions <- function(col1, col2, data){
  mod <- lm(col1~col2, data=data)
  df <- data.frame(col2 = c(40,60,80))
  pred <- predict(mod, newdata=df)
  plot(col2, col1)
  abline(mod)
  points(df$col2, pred, col="red")
}

# Exercise 3.
create_models <- function(data, col_d, col2, col3){
  mod_1 <- lm(col_d~col2, data=data)
  mod_2 <- lm(col_d~col3, data=data)
  mod_both <- lm(col_d~col2+col3)
  
  plot(col2, col_d)
  abline(mod_1)
  abline(mod_both)
  
  plot(col3, col_d)
  abline(mod_2)
  abline(mod_both)
}

# Exercise 4.
stepping <- function(model){
  res <- step(model, direction="forward")
}