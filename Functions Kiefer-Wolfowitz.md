```R
## function for KW algorithms 

# Explanation symbols:
# n = number of iterations
# s = noise, sigma^2
# a = scalar who's inverse is to be mulitplied by step size 1/n
# c = scalar parameter
# X = matrix X  

# Quadratic function iterative scheme

KW_scheme_quadratic <- function(n, first_point, s, a, c) {
  X = numeric(n)
  for (i in 1:n) {
    if (i > 2 && (X[i] - X[i-1]) * (X[i-1] - X[i-2]) < 0) {
      index = index
    } else {
      index = i
    }
    X[i] = first_point
    first_point = first_point + (1/(index*a)) * ((observation_y(first_point + i^(c), s, t_real) - observation_y(first_point - i^(c), s, t_real)) / (2 * i^(c)))

    if (first_point > 1) {
      first_point = 1
    }
    if (first_point < -1) {
      first_point = -1
    }
  }
  return(cbind(X, first_point))
}

# Quartic function iterative scheme

KW_scheme_quartic <- function(n, first_point, s, a, c) {
  X = numeric(n)
  for (i in 1:n) {
    if (i > 2 && (X[i] - X[i-1]) * (X[i-1] - X[i-2]) < 0) {
      index = index
    } else {
      index = i
    }
    X[i] = first_point

    first_point = first_point + (1/(index*a)) * (((real_g_quartic(first_point + i^(c), t_real_q) + rnorm(1, 0, s)) - (real_g_quartic(first_point - i^(c), t_real_q) + rnorm(1, 0, s))) / (2 * i^(c)))

    if (first_point > 1) {
      first_point = 1
    }
    if (first_point < -1) {
      first_point = -1
    }
  }
  return(cbind(X, first_point))
}

# Polynomial of degree 5 (referred to as triangle function)

KW_scheme_triangle = function(n, first_point, s, a, c) {
  X = numeric(n)
  for (i in 1:n) {
    if (i > 2 && (X[i] - X[i-1]) * (X[i-1] - X[i-2]) < 0) {
      index = index
    } else {
      index = i
    }
    X[i] = first_point
    first_point = first_point + (1/(index*a)) * ((observation_y_triangle(first_point + i^(c), s, t_real_triangle) - observation_y_triangle(first_point - i^(c), s, t_real_triangle)) / (2 * i^(c)))

    if (first_point > 1) {
      first_point = 1
    }
    if (first_point < -1) {
      first_point = -1
    }
  }
  return(cbind(X, first_point))
}

# Trigoniometric function (referred to as sine function)

KW_scheme_sin <- function(n, first_point,s,a,c){
  X = numeric(n)
for (i in 1:n) {
 if (i>2 && (X[i]-X[i-1])*(X[i-1]-X[i-2])<0){
    index = index
  }else{
    index = i
  }
  X[i] = first_point

  first_point = first_point + (1/(index*a))*((observation_y_sin(first_point+i^(c),s,t_real_sin)-observation_y_sin(first_point-i^(c),s,t_real_sin))/(2*i^(c)))

  if (first_point > 1){
    first_point = 1
  }
  if(first_point < -1){
    first_point = -1
  }

}  
  return(cbind(X, first_point))
}

```
