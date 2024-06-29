```R
# Explanation symbols:
# t = unknown parameter theta
# A = covariance matrix prior of theta
# sigmaa or s = sigma^2, variance of error
# Sigmaa or S= covariance matrix posterior distribution theta
# g = objective function
# y = observations


## Quadratic (g1)

# Input vector for quadratic function

input_vector <- function(input_x) {
  if (length(input_x) == 1) {
    input_x <- c(1, input_x, input_x^2)
  } else {
    input_x <- cbind(rep(1, length(input_x)), input_x, input_x^2)
  }
  return(t(input_x))
}

# Real g function for quadratic case
real_g <- function(x, t) {
  result = numeric(length(x))
  for (i in 1:length(x)) {
    if (abs(x[i]) > 1) {
      result[i] = 0
    } else {
      result[i] = input_vector(x[i]) %*% t
    }
  }
  return(t(t(result)))
}

# Observation function with noise
observation_y <- function(x, sigmaa, t) {
  result = numeric(length(x))
  for (i in 1:length(x)) {
    if (abs(x[i]) > 1) {
      result[i] = 0
    } else {
      result[i] = real_g(x[i], t) + rnorm(1, 0, sigmaa)
    }
  }
  return(result)
}

# Function to find maximum
find_maximum <- function(t) {
  x = optimize(real_g, t = t, c(-1, 1), maximum = TRUE)$maximum
  return(x)
}

## Quartic (g2)

# Input vector for quartic function
input_vector_quartic <- function(input_x) {
  if (length(input_x) == 1) {
    input_x <- c(1, input_x, input_x^2, input_x^3, input_x^4)
  } else {
    input_x <- cbind(rep(1, length(input_x)), input_x, input_x^2, input_x^3, input_x^4)
  }
  return(t(input_x))
}

# Real g function for quartic function
real_g_quartic <- function(x, t) {
  result = numeric(length(x))
  for (i in 1:length(x)) {
    if (abs(x[i]) > 1) {
      result[i] = 0
    } else {
      result[i] = input_vector_quartic(x[i]) %*% t
    }
  }
  return(t(t(result)))
}

# Observation with noise for quartic function
observation_y_quartic <- function(x, sigmaa, t) {
  result = numeric(length(x))
  for (i in 1:length(x)) {
    if (abs(x[i]) > 1) {
      result[i] = 0
    } else {
      result[i] = real_g_quartic(x[i], t) + rnorm(1, 0, sigmaa)
    }
  }
  return(result)
}

## Polynomial of degree 5, triangle called in this case (g3)

# Input vector for triangle function
input_vector_triangle <- function(input_x) {
  if (length(input_x) == 1) {
    input_x <- c(1, input_x, input_x^2, input_x^3, input_x^4, input_x^5)
  } else {
    input_x <- cbind(rep(1, length(input_x)), input_x, input_x^2, input_x^3, input_x^4, input_x^5)
  }
  return(t(input_x))
}

# Real g function for triangle function
real_g_triangle <- function(x, t) {
  result = numeric(length(x))
  for (i in 1:length(x)) {
    if (abs(x[i]) > 1) {
      result[i] = 0
    } else {
      result[i] = input_vector_triangle(x[i]) %*% t
    }
  }
  return(t(t(result)))
}

# Observation function with noise for triangle function
observation_y_triangle <- function(x, sigmaa, t) {
  result = numeric(length(x))
  for (i in 1:length(x)) {
    if (abs(x[i]) > 1) {
      result[i] = 0
    } else {
      result[i] = real_g_triangle(x[i], t) + rnorm(1, 0, sigmaa)
    }
  }
  return(result)
}

# Function to find maximum for triangle function
find_maximum_triangle <- function(t) {
  x = optimize(real_g_triangle, t = t, c(-1, 1), maximum = TRUE)$maximum
  return(x)
}

# Function to find maximum for a quartic function
find_maximum_q <- function(t) {
  x = optimize(real_g_quartic, t = t, c(-1, 1), maximum = TRUE)$maximum
  return(x)
}

## Goniometric function, called sine function (g4)

# Input vector for sine function
input_vector_sin <- function(input_x){
  if (length(input_x) == 1) {
    input_x <- c(cos(5*input_x), sin(5*input_x), sin(5*input_x)*cos(5*input_x))
  } else {
    input_x <- cbind(cos(5*input_x), sin(5*input_x), sin(5*input_x)*cos(5*input_x))
  }
  return(t(input_x))
}

# Real g function for sine function
real_g_sin <- function(x, t){
  result = numeric(length(x))
  for(i in 1:length(x)){
    if(abs(x[i]) > 1){
      result[i] = 0
    } else {
      result[i] = input_vector_sin(x[i]) %*% t
    }
  }
  return(t(t(result)))
}

# Observation with noise for sine function
observation_y_sin <- function(x, sigmaa, t){
  result = numeric(length(x))
  for(i in 1:length(x)){
    if(abs(x[i]) > 1){
      result[i] = 0
    } else {
      result[i] = real_g_sin(x[i], t) + rnorm(1, 0, sigmaa)
    }
  }
  return(result)
}

# Finding maximizer for sine function
find_maximum_sin <- function(t){
  x = optimize(real_g_sin, t = t, c(-1, 1), maximum = TRUE)$maximum
  return(x)
}

```
