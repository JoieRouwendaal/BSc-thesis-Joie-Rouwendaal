```R
## g1

# Fixing parameters
t_real = c(1,1,-2)/(2*sqrt(2/5))
s = 0.1
n = 100
n1 = n/2
A = diag(3)
first_points = c(-0.3023365,  0.2831206,  0.7277992, -0.8247558,  0.8017311, -0.4212103,  0.5538337,  0.3444435, -0.8456241, -0.1132634,  0.5050754, -0.1532192, -0.9786938, -0.8894055, 0.7606035) # is determined by runif(15,-1,1) 
real_x_qd = find_maximum(t_real)


results2_qd_BO = list()
results2_qd_KW = list()

for (j in seq(0, 1, 0.2)) {
  p = j
  
  j_results2 = list()
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y(first_point,s,t_real)
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i+1000)
      estimations = rbind(estimations, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
    }
    means = apply(estimations, 2, mean)
    
    j_results2[[as.character(k)]] = means
  }
  
  results2_qd_BO[[as.character(j)]] = j_results2
}


for (k in first_points) {
  first_point = k
  y_0 = observation_y(first_point, s, t_real)
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i+1000)
    estimations1 = rbind(estimations1, abs(KW_scheme_quadratic(n1, first_point, s, a = 1, c = -1/3) - real_x_qd))
  }

  means = apply(estimations1, 2, mean)
  
  results2_qd_KW[[as.character(k)]] = means
}

#######################################################################################################################################################################################

## g2

t_real_q = c(0,0,0,-8,-8)/(32*sqrt(14)/21)
s = 0.1
A = diag(5)
real_x_qr = find_maximum_q(t_real_q)

results2_qr_BO = list()
results2_qr_KW = list()

for (j in seq(0, 0.04, 0.01)) {
  p = j
  
  j_results2 = list()
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y_quartic(first_point,s,t_real_q)
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i+1000)
      estimations = rbind(estimations, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
    }
    means = apply(estimations, 2, mean)
    
    j_results2[[as.character(k)]] = means
  }
  
  results2_qr_BO[[as.character(j)]] = j_results2
}
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y_quartic(k,s,t_real_q)
    estimations1 = c()
    
    for (i in 1:1000) {
      set.seed(i+1000)
      estimations1 = rbind(estimations1,abs(KW_scheme_quartic(n1,first_point,s,a=1,c=-1/3)-real_x_qr))
    }

    means = apply(estimations1, 2, mean)
  
    results2_qr_KW[[as.character(k)]] = means

  }

####################################################################################################################################################################################################################

## g3

# Fixing parameters
t_real_triangle = c(0,0,1,1,-1,-1)/(16/(3*sqrt(385)))
s = 0.1
A = diag(6)
real_x_triangle = find_maximum_triangle(t_real_triangle)

results2_tr_BO = list()
results2_tr_KW = list()

for (j in seq(0.8,1,0.2)) {
  p=j
  
  j_results2 = list()
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y_triangle(k,s,t_real_triangle)
    estimations3 = c()
      
    for (i in 1:1000) {
      set.seed(i+1000)
      estimations3 = rbind(estimations3,t(abs(iterative_triangle_final(first_point,n,y_0,p)$X0-real_x_triangle)))
    }
    means = apply(estimations3, 2, mean)

    j_results2[[as.character(k)]] = means
  }
  
  results2_tr_BO[[as.character(j)]] = j_results2
  
}
 
  for (k in first_points) {
    first_point = k
    y_0 = observation_y_triangle(k,s,t_real_triangle)
    estimations4 = c()
    
    for (i in 1:1000) {
      set.seed(i+1000)
      estimations4 = rbind(estimations4,abs(KW_scheme_triangle(n1,first_point,s,a=1,c=-1/3)-real_x_triangle))
    }

    means = apply(estimations4, 2, mean)
    
    results2_tr_KW[[as.character(k)]] = means
}

#########################################################################################################################################################################################################################

## g4

# Fixing parameters
t_real_sin = c(1,1,1)/(2*sqrt(2))
s = 0.1
A = diag(3)
real_x_sin = find_maximum_sin(t_real_sin)

results2_sin_BO = list()
results2_sin_KW = list()

for (j in seq(0,1,0.2)) {
  p=j
  
  j_results2 = list()
  
  for (k in first_points) {
    first_point = k
    y_0 = observation_y_sin(k,s,t_real_sin)
    estimations6 = c()
    
    for (i in 1:1000) {
      set.seed(i+1000)
      estimations6 = rbind(estimations6,t(abs(iterative_sin_final(first_point,n,y_0,p)$X0-real_x_sin)))
    }
    means = apply(estimations6, 2, mean)
    
    j_results2[[as.character(k)]] = means
  }
  
  results2_sin_BO[[as.character(j)]] = j_results2
  
}
  
  for (k in first_points) {
  y_0 = observation_y_sin(k,s,t_real_sin)
  estimations7 = c()
    
  for (i in 1:1000) {
      set.seed(i+1000)
      estimations7 = rbind(estimations7,abs(KW_scheme_sin(n1,k,s,a=1,c=-1/3)-real_x_sin))
      }

    means = apply(estimations7, 2, mean)
    
    results2_sin_KW[[as.character(k)]] = means
}

###############################################################################################

## Saving results

saveRDS(results2_qd_BO, file = "2resultsqdBO_n100.RDS")
saveRDS(results2_qd_KW, file = "2resultsqdKW_n100.RDS")

saveRDS(results2_qr_BO, file = "2resultsqrBO_n100.RDS")
saveRDS(results2_qr_KW, file = "2resultsqrKW_n100.RDS")

saveRDS(results2_tr_BO, file = "2resultstrBO_n100.RDS")
saveRDS(results2_tr_KW, file = "2resultstrKW_n100.RDS")

saveRDS(results2_sin_BO, file = "2resultssinBO_n100.RDS")
saveRDS(results2_sin_KW, file = "2resultssinKW_n100.RDS")


```
