options(digits = 3)
set.seed(21, sample.kind = "Rounding") # Seed for MonteCarlo simulations
# Script de PH125.3x DataSience: Probability Section 3 Assesment
n <- 44 # Numero de preguntas
choices <- 5 # Numero de opciones por cada pregunta
correct_choices <- 1 # Número de opciones correctas
correct_choice_value <- 1
incorrect_choice_value <- -0.25
p_correct_choice <- correct_choices/choices 
E <- (p_correct_choice*correct_choice_value)+(1-p_correct_choice)*incorrect_choice_value # Expected value
SE <-  abs(incorrect_choice_value-correct_choice_value)*sqrt(p_correct_choice*(1-p_correct_choice)) # Standard error for a single question
SE44 <- SE*sqrt(n)
# Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
p_8_poing_or_higher <- 1-pnorm(8, mean=44*E, sd=SE44)

# Same exercise than above with MonteCarlo Simulations
B <- 10000
X <- replicate(B, {
  sum(sample(c(correct_choice_value, incorrect_choice_value), n, prob=c(p_correct_choice, 1-p_correct_choice), replace=TRUE))
}) # Creamos 10000 puntuaciones globales (44 preguntas)

mean(X>=8)

# ---- Changing play rules 
choices <- 4 # Numero de opciones por cada pregunta
correct_choices <- 1 # Número de opciones correctas
correct_choice_value <- 1
incorrect_choice_value <- 0
p_correct_choice <- 1/4
E <- p_correct_choice*correct_choice_value + incorrect_choice_value*(1-p_correct_choice)
E44 <- n*E
SE44 <- sqrt(n)*abs(correct_choice_value-incorrect_choice_value)*sqrt(p_correct_choice*(1-p_correct_choice))
1-pnorm(30, mean=E44, SE44)

correct_answer_prob <- seq(0.25, 0.95, 0.05)
se_fun <- function(p_correct_choice){
  n*(p_correct_choice*correct_choice_value + incorrect_choice_value*(1-p_correct_choice))
}
correct_answer_prob[sapply(correct_answer_prob,se_fun)>35]

# Question 3: Betting on Roulette
p_win <- 5/38
p_loose <- (38-5)/38
win_bet <- 6
loose_bet <- -1
n <- 500
E <- p_win*win_bet + p_loose*loose_bet
SE <- abs(win_bet-loose_bet)*sqrt(p_win*p_loose)
SE/sqrt(n)

