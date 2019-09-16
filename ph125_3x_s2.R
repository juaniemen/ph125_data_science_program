set.seed(16, sample.kind = "Rounding")
act_mean <- 20.9
act_sd <- 5.7
act_scores <- rnorm(10000, mean=act_mean, sd=act_sd)
act_mean_gen <- mean(act_scores)
act_sd_gen <- sd(act_scores)
e1c <- sum(act_scores >= 36)
e1d <- mean(act_scores > 30)
e1e <- mean(act_scores <= 10)

# Distribución acumulada
dnormf_x <- function(a){
  dnorm(a, mean=act_mean, sd=act_sd)
}
f_x <- sapply(seq(1,36), dnormf_x) 
plot(seq(1,36), f_x)

# Probabilidad
pnormf_x <- function(a){
  pnorm(a, mean=act_mean, sd=act_sd)
}
pf_x <- sapply(seq(1,36), pnormf_x) 
plot(seq(1,36), pf_x)

# Redefining act_scores para Q3 y Q4 
# 3a
act_zscore <- scale(act_scores)
mean(act_zscore > 2)
mean(act_zscore < 2)

# 3b
2*act_sd_gen + act_mean_gen

# 3c
qnorm(0.975, act_mean_gen, act_sd_gen)

# CDF of ACT score
rg <- 1:36
prob <- function(a){
  mean(act_scores <= a)
}


dt <- data.frame(x=rg, probability=sapply(rg, prob))

qnorm(0.95, mean=20.9, sd=5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(probs=p, act_scores)

theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qqplot(theoretical_quantiles, sample_quantiles)
