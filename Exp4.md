```R

## Creating the randomly sampled polynomials

# quadratic 1
set.seed(3333)
first_points = runif(15, -1, 1)
roots_2 = runif(2, -1, 1)

t_real_2 = c(-1*roots_2[1]*roots_2[2], (roots_2[1]+roots_2[2]), -1)/sqrt(0.2875)

# quartic 1
set.seed(9999)
r4 = runif(4, -1, 1)

t_real_4 = c(-1*r4[1]*r4[2]*r4[3]*r4[4], r4[1]*r4[2]*r4[4]+r4[1]*r4[3]*r4[4]+r4[2]*r4[3]*r4[4], -1*r4[1]*r4[2]-1*r4[1]*r4[3]-1*r4[1]*r4[4]-1*r4[2]*r4[3]-1*r4[2]*r4[4]-1*r4[3]*r4[4], r4[1]+r4[2]+r4[3]+r4[4], -1)/sqrt(0.18997)
real_x_4 = find_maximum_4(t_real_4)

# quartic 2
set.seed(33)
r4 = runif(4, -1, 1)

t_real_4 = c(-1*r4[1]*r4[2]*r4[3]*r4[4], r4[1]*r4[2]*r4[4]+r4[1]*r4[3]*r4[4]+r4[2]*r4[3]*r4[4], -1*r4[1]*r4[2]-1*r4[1]*r4[3]-1*r4[1]*r4[4]-1*r4[2]*r4[3]-1*r4[2]*r4[4]-1*r4[3]*r4[4], r4[1]+r4[2]+r4[3]+r4[4], -1)/sqrt(0.1789)
real_x_4 = find_maximum_4(t_real_4)

#######################################################################################################

# quadratic 1

exp4.1_BO = list()
exp4.1_KW = list()

for (j in seq(0, 1, 0.2)) {

  p = j
  
  j_results2 = list()
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y2(first_point, s)
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i)
      estimations = rbind(estimations, abs(iterative_quadratic_final(first_point, n = 100, y_0, p)$X0[, 2] - real_x_2))
    }
    means = apply(estimations, 2, mean)
    
    j_results2[[as.character(k)]] = means
  }
  
  exp4.1_BO[[as.character(j)]] = j_results2
}


for (k in first_points) {
  first_point = k
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i)
    estimations1 = rbind(estimations1, abs(KW_scheme_quadratic(n=50, first_point, s = 0.1, a=1, c=-1/3) - real_x_2))
  }

  means = apply(estimations1, 2, mean)
  
  exp4.1_KW[[as.character(k)]] = means
}

# quartic 1

exp4.3_BO = list()
exp4.3_KW = list()
x = 1:101
y = numeric(101)
x2 = 1:50
y2 = numeric(50)

for (j in seq(0, 1, 0.2)) {

  p = j
  
  j_results2 = list()
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y_4(first_point, s)
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i)
      estimations = rbind(estimations, abs(iterative_quartic_final(first_point, n = 100, y_0, p)$X0[, 2] - real_x_4))
    }
    means = apply(estimations, 2, mean)
    
    j_results2[[as.character(k)]] = means
  }
  
  exp4.3_BO[[as.character(j)]] = j_results2
}


for (k in first_points) {
  first_point = k
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i)
    estimations1 = rbind(estimations1, abs(KW_scheme_quartic(n=50, first_point, s = 0.1, a=1, c=-1/3) - real_x_4))
  }

  means = apply(estimations1, 2, mean)

  exp4.3_KW[[as.character(k)]] = means

}

# quartic 2

exp4.4_BO = list()
exp4.4_KW = list()
x = 1:101
y = numeric(101)
x2 = 1:50
y2 = numeric(50)

for (j in seq(0, 1, 0.2)) {

  p = j
  
  j_results2 = list()
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y_4(first_point, s)
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i)
      estimations = rbind(estimations, abs(iterative_quartic_final(first_point, n = 100, y_0, p)$X0[, 2] - real_x_4))
    }
    means = apply(estimations, 2, mean)
    
    j_results2[[as.character(k)]] = means
  }
  
  exp4.4_BO[[as.character(j)]] = j_results2
}

for (k in first_points) {
  first_point = k
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i)
    estimations1 = rbind(estimations1, abs(KW_scheme_quartic(n=50, first_point, s = 0.1, a=1, c=-1/3) - real_x_4))
  }

  means = apply(estimations1, 2, mean)

  exp4.4_KW[[as.character(k)]] = means

}

#####################################################################################################

# Store results

saveRDS(exp4.1_BO, file = "14exp4.1_BO.RDS")
saveRDS(exp4.1_KW, file = "14exp4.1_KW.RDS")

saveRDS(exp4.3_BO, file = "14exp4.3_BO.RDS")
saveRDS(exp4.3_KW, file = "14exp4.3_KW.RDS")

saveRDS(exp4.4_BO, file = "14exp4.4_BO.RDS")
saveRDS(exp4.4_KW, file = "14exp4.4_KW.RDS")





```
