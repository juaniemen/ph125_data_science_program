install.packages("gtools")
library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

# Olympic games
medals <- permutations(8,3)
n_permutation_medals <- nrow(medals)

jamaica_medals <- permutations(3,3)
probalidad_ganar_3_jamaica <-nrow(jamaica_medals)/n_permutation_medals

set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

jamaica_medals_winner <- replicate(10000, {
  winners <- sample(runners, 3)
  all(winners == "Jamaica")
})
mean(jamaica_medals_winner)

# Restaurant management 
# question 2d
choices <- function(entree_choices){
  (nrow(combinations(6,2)) * entree_choices * 3)
}
m <- sapply(c(1:12), choices)
plot(c(1:12), m)
# question 2e
side_choices <- function(side_choices){
  (nrow(combinations(side_choices,2)) * 6 * 3)
}
m1 <- sapply(c(2:12), side_choices)
plot(c(2:12), m1)


# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1
head(esoph)
nrow(eshop)
levels(esoph$agegp)
levels(esoph$alcgp)
levels(esoph$tobgp)
all_cases <- sum(esoph$ncases, na.rm=TRUE)
all_controls <- sum(esoph$ncontrols, na.rm=TRUE)
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# Question 4c
esoph %>% summarize(person= sum(ncases), person_smoke_much= sum(ifelse(tobgp != "0-9g/day", ncases, 0)))

# Question 4d
esoph %>% summarize(person= sum(ncontrols), person_smoke_much= sum(ifelse(tobgp != "0-9g/day", ncontrols, 0)))

# Asi lo hacen ellos (4d)
tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

tob_controls/all_controls

# Questions 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2
esoph %>% summarize(person= sum(ncases), person_high_alcohol= sum(ifelse(alcgp == "120+", ncases, 0)))

esoph %>% summarize(person= sum(ncases+ncontrols), person_high_alcohol_nd_tobaco= sum(ifelse(tobgp == "30+" & alcgp == "120+", ncases+ncontrols, 0)))

