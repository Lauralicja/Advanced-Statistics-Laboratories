# Exercise 1. and 2.

data <- read.csv("Fam.csv", sep=";")
aov <- aov(number~cost, data=data)
summary(aov)

# Exercise 3.
get_tukey <- function(obj) {
  tukey <- TukeyHSD(obj)
  plot(tukey)
}

# Exercise 4.
data2 <- read.csv("Pri.csv", sep=";")
ex4 <- aov(price~district+type, data=data2)
summary(ex4)
