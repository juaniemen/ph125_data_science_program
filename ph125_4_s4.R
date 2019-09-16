# PH125.4x
# Section 5 -> Assessment: Finding a missing airplane
options(digits = 3)
p_A <- .2
p_B <- .6
p_C <- .15
p_D <- .05
# G -> Find the plane
# H -> The plane is the area
p_G_H <- .9
p_notG_H <- .1
p_G_notH <- 0
p_notG_notH <- 1

# Q3
p_notB <- 1-p_B
p_notG_b <- p_notG_notH*(1-p_B)+p_notG_H*p_B
p_H <- p_B
p_H_notG <- (p_notG_H*p_H)/p_notG_b

p_X_given_plane_not_found_in_B <- function(p_X){
  1*p_X/p_notG_b
}

ary_p <- c(p_A, p_C,p_D)
sapply(ary_p, p_X_given_plane_not_found_in_B)

