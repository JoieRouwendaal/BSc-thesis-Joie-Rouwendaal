```R
## Functions Bayesian optimization

# Explanation symbols:
# t = unknown parameter theta
# A = covariance matrix prior of theta
# sigmaa or s = sigma^2, variance of error
# Sigmaa or S = covariance matrix posterior distribution theta
# g = objective function
# y = observations

creating_Sigmaa <- function(sigmaa,X,A){
  inv(A) + 1/sigmaa*X%*%t(X) + 0.2*A # To prevent non-singularity 

}

symmetrize <- function(matrix) {
  (matrix + t(matrix)) / 2
}

check_if_symmetric <- function(matrix) {
  all(matrix == t(matrix))
}

posterior_theta <- function(y,X,sigmaa,Sigmaa){
  mean_result = (1/sigmaa)*inv(Sigmaa)%*%X%*%y
  variance_result = inv(Sigmaa)
  
  if (!check_if_symmetric(variance_result)) {
  variance_result = symmetrize(variance_result)
}
  
  result = c(list(theta_mean = mean_result, theta_var = variance_result))
  
}

# Iterative Bayesian Optimization scheme for quadratic function
iterative_quadratic_final <- function(new_design_point, n, y_0, p) {
  X_0 = input_vector(new_design_point)
  for (i in 1:n) {
    S = creating_Sigmaa(s, t(X_0), A)
    theta_posterior = posterior_theta(y_0, t(X_0), s, S)

    choice = rbinom(1, 1, p) # Implementing epsilon greedy
    if (choice == 0) {
      t_estimate = theta_posterior$theta_mean[,1] # mean sampling
    } else {
      t_estimate = rmvnorm(1, theta_posterior$theta_mean, theta_posterior$theta_var)[1,] # Thompson sampling
    }

    new_design_point = find_maximum(t_estimate)

     if (design_point > 1) {
      design_point = 1
    }
    if (design_point < -1) {
      design_point = -1
    }

    new_design_maximum = observation_y(new_design_point, s, t_real)

    y_0 = rbind(y_0, new_design_maximum)
    X_0 = rbind(X_0, input_vector(new_design_point))
  }
  return(list(X0 = X_0, mean = theta_posterior$theta_mean, var = theta_posterior$theta_var, y= y_0))
}



# Iterative Bayesian Optimization scheme for quartic function
iterative_quartic_final <- function(new_design_point, n, y_0, p) {
  X_0 = input_vector_quartic(new_design_point)
  for (i in 1:n) {
    S = creating_Sigmaa(s, t(X_0), A)
    theta_posterior = posterior_theta(y_0, t(X_0), s, S)

    choice = rbinom(1, 1, p)
    if (choice == 0) {
      t_estimate = theta_posterior$theta_mean[,1]
    } else {
      t_estimate = rmvnorm(1, theta_posterior$theta_mean, theta_posterior$theta_var)[1,]
    }

    new_design_point = find_maximum_q(t_estimate)

     if (design_point > 1) {
      design_point = 1
    }
    if (design_point < -1) {
      design_point = -1
    }

    new_design_maximum = observation_y_quartic(new_design_point, s, t_real_q)

    y_0 = rbind(y_0, new_design_maximum)
    X_0 = rbind(X_0, input_vector_quartic(new_design_point))
  }
  return(list(X0 = X_0, mean = theta_posterior$theta_mean, var = theta_posterior$theta_var, y = y_0))
}


# Iterative Bayesian Optimization scheme for triangle function
iterative_triangle_final <- function(new_design_point, n, y_0, p) {
  estimations = c(new_design_point)
  X_0 = input_vector_triangle(new_design_point)
  for (i in 1:n) {
    S = creating_Sigmaa(s, t(X_0), A)
    theta_posterior = posterior_theta(y_0, t(X_0), s, S)

    choice = rbinom(1, 1, p)
    if (choice == 0) {
      t_estimate = theta_posterior$theta_mean[,1]
    } else {
      t_estimate = rmvnorm(1, theta_posterior$theta_mean, theta_posterior$theta_var)[1,]
    }

    new_design_point = find_maximum_triangle(t_estimate)

    if (design_point > 1) {
      design_point = 1
    }
    if (design_point < -1) {
      design_point = -1
    }

    new_design_maximum = observation_y_triangle(new_design_point, s, t_real_triangle)

    y_0 = rbind(y_0, new_design_maximum)
    X_0 = rbind(X_0, input_vector_triangle(new_design_point))
    estimations = rbind(estimations, new_design_point)
  }
  return(list(X0 = estimations, mean = theta_posterior$theta_mean, var = theta_posterior$theta_var, y = y_0))
}


# Iterative scheme Bayesian optimization sine function
iterative_sin_final <- function(new_design_point, n, y_0, p){
  estimations = c(new_design_point)
  X_0 = input_vector_sin(new_design_point)
  for (i in 1:n) {
    S = creating_Sigmaa(s, t(X_0), A)
    theta_posterior = posterior_theta(y_0, t(X_0), s, S)

    choice = rbinom(1, 1, p)
    if (choice == 0) {
      t_estimate = theta_posterior$theta_mean[,1]
    } else {
      t_estimate = rmvnorm(1, theta_posterior$theta_mean, theta_posterior$theta_var)[1,]
    }

    new_design_point = find_maximum_sin(t_estimate)

     if (design_point > 1) {
      design_point = 1
    }
    if (design_point < -1) {
      design_point = -1
    }

    new_design_maximum = observation_y_sin(new_design_point, s, t_real_sin)

    y_0 = rbind(y_0, new_design_maximum)
    X_0 = rbind(X_0, input_vector_sin(new_design_point))
    estimations = rbind(estimations, new_design_point)
  }
  return(list(X0 = estimations, mean = theta_posterior$theta_mean, var = theta_posterior$theta_var, y = y_0))
}

# Iterative scheme Bayesian optimization quadratic model, while observations are from quartic function

iterative_quadratic_toy2 = function(new_design_point,n,y_0,p){
  X_0 = input_vector(first_point)
  for (i in 1:n) {
    S = creating_Sigmaa(s,t(X_0),A)
    theta_posterior = posterior_theta(y_0,t(X_0),s,S)

    choice = rbinom(1,1,p)
    if(choice == 0){
      t_estimate = theta_posterior$theta_mean[,1]               
    }else{
      t_estimate = rmvnorm(1, theta_posterior$theta_mean, theta_posterior$theta_var)[1,] 
    }
    new_design_point = find_maximum(t_estimate)

   if (design_point > 1) {
      design_point = 1
    }
    if (design_point < -1) {
      design_point = -1
    }

    new_design_maximum = observation_y_quartic(new_design_point,s,t_real_q)

    y_0 = rbind(y_0, new_design_maximum)
    X_0 = rbind(X_0, input_vector(new_design_point)) 
}
return(list(X0 = X_0, mean = theta_posterior$theta_mean, var = theta_posterior$theta_var))

}

```
