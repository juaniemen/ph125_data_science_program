#Brexit poll analysis part 1
# suggested libraries and options
library(tidyverse)
options(digits = 3)
# load brexit_polls object
library(dslabs)
data(brexit_polls)
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500
X <- (p*N)
se1 <- sqrt(N*p*(1-p))
X_hat <- X/N
se <- sqrt(X_hat*(1-X_hat)/N)
d_hat <- 2*X_hat-1
d_se <- 2*se

brexit_polls <- brexit_polls %>% mutate(x_hat = (spread+1)/2)
mean_spread <- mean(brexit_polls$spread)
sd_spread <- sd(brexit_polls$spread)
mean_x_hat <- mean(brexit_polls$x_hat)
sd_x_hat <- sd(brexit_polls$x_hat)

first_poll <- brexit_polls[1,]
first_poll <- first_poll %>%  mutate(se_x_hat= sqrt(x_hat*(1-x_hat)/samplesize))
ci_lower <-  first_poll$x_hat - qnorm(0.975)*first_poll$se_x_hat
ci_upper <- first_poll$x_hat + qnorm(0.975)*first_poll$se_x_hat

# Brexit poll analysis part 2
june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>% mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize), se_spread=2*se_x_hat, lower_spread=spread - qnorm(.975)*se_spread, upper_spread=spread + qnorm(.975)*se_spread,
                                                                          hit_true=-0.038 >=lower_spread & -0.038 <= upper_spread)
nrow(june_polls)
mean(0 >=june_polls$lower_spread & 0 <= june_polls$upper_spread)
mean(0 < june_polls$lower_spread)
mean(june_polls$hit_true)

june_polls %>% group_by(pollster) %>% summarize(proportion_hits=sum(hit_true)/n(), n_hits=sum(hit_true), n_polls= n()) %>% arrange(proportion_hits)
june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot()


# Question 7
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se = sqrt(p_hat*(1-p_hat)/N),
            se_spread= 2*se,
            lower= spread-qnorm(.975)*se_spread,
            upper= spread+qnorm(.975)*se_spread)
combined_by_type


# Brexit poll analysis - Part 3
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_hit2by2 <- table(brexit_hit$hit, brexit_hit$poll_type)
brexit_hit2by2
chisq.test(brexit_hit2by2)
# Q11
brexit_polls %>% ggplot(aes(x=enddate, y=spread, color=poll_type)) + geom_smooth(method="loess",span=.4) + geom_hline(yintercept = -0.038) + geom_point()

#Q12
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% ggplot(aes(x=enddate, y=proportion, color=vote)) + geom_smooth(method="loess", span=.3)
