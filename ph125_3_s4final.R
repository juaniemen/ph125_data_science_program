options(digits = 3)
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

# Questions 1 and 2: Insurance rates, part 1
female_50 <- death_prob %>% filter(sex == "Female" & age == 50)
E <- 1150 * (1-female_50$prob) + (female_50$prob) * -150000
SE <- abs(-150000-1150)*sqrt(female_50$prob * (1-female_50$prob))
n <- 1000
En <- n*E
SEn <- abs(-150000-1150)*sqrt(n*female_50$prob * (1-female_50$prob))
pnorm(0, mean=En, sd=SEn)

# Question 2a: regarding male_50
male_50 <- death_prob %>% filter(sex == "Male" & age == 50)
En <- 700000
n <- 1000
p <- male_50$prob
a <- -150000
b <- ((En/n)-(p*a))/(1-p)
SEn <- abs(b-a)*sqrt(n*p*(1-p))
# Probabilidad de perder dinero
loose_money_p <- pnorm(0, mean=En, sd=SEn)

# Questions 3 and 4: insurance rates, part 2
male_50 <- death_prob %>% filter(sex == "Male" & age == 50)

n <- 1000
p <- 0.015
a <- -150000
b <- 1150
En <- n*(p*a+(1-p)*b)
SEn <- abs(b-a)*sqrt(n*p*(1-p))
# Probabilidad de perder dinero
loose_money_p <- pnorm(0, mean=En, sd=SEn)
loose_1Mdollars_p <- pnorm(-1000000, mean=En, sd=SEn)

# Question 3e
p <- seq(.01, .03, .0025)
exp_value_fun <- function(p1){
  En1 <- n*(p1*a+(1-p1)*b)
  SEn1 <- abs(b-a)*sqrt(n*p1*(1-p1))
  pnorm(0, mean=En1, sd=SEn1)
}
p[sapply(p, exp_value_fun)>=0.9]

# Question 3f
p <- seq(.01, .03, .0025)
exp_value_fun <- function(p1){
  En1 <- n*(p1*a+(1-p1)*b)
  SEn1 <- abs(b-a)*sqrt(n*p1*(1-p1))
  pnorm(-1000000, mean=En1, sd=SEn1)
}
p[sapply(p, exp_value_fun)>=0.9]

# Question 4a
set.seed(25, sample.kind="Rounding")
n <- 1000
p_loss = .015
a = -150000
b = 1150
sum(sample(c(a,b), n, c(p_loss, 1-p_loss), replace=TRUE))/10^6

# Question 4b
set.seed(27, sample.kind="Rounding")
B <- 10000
n <- 1000
p_loss = .015
a = -150000
b = 1150
sampleo <- replicate(B, {
  sum(sample(c(a,b), n, c(p_loss, 1-p_loss), replace=TRUE))
})
mean(sampleo<=-10^6)

# Questions 5 and 6: Insurance rates, part 3
n <- 1000
p = .015
l = -150000
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
E <- (p*-150000 + (1-p)*x)
En <- E*n

# Question 5d
set.seed(28, sample.kind = "Rounding")
B <- 10000
sampleo <- replicate(B, {
  sum(sample(c(l, x), n, c(p, 1-p), replace = TRUE))
  
})
mean(sampleo < 0)

# Question 6
set.seed(29, sample.kind = "Rounding")
B <- 10000
p <- .015
sampleo <- replicate(B, {
  p1 <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  sum(sample(c(l, x), n, c(p1, 1-p1), replace = TRUE))
  
})
# Question 6a
mean(sampleo)

# Question 6b
mean(sampleo<=0)

# Question 6c
mean(sampleo<=-10^6)

