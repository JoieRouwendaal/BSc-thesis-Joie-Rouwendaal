```R
## g1

# Fixing parameters

t_real = c(1,1,-2)/(2*sqrt(2/5))
s = 0.1
n = 100
n1 = n/2
x = 0:n
y = numeric(n+1)
x2 = 1:n1
y2 = numeric(n1)
A = diag(3)
first_point = 0.3 # randomly sampled from U(-1,1)
y_0 = observation_y(first_point,s,t_real)
real_x_qd = find_maximum(t_real)

# Mean sampling:
p = 0

estimations.0 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y(first_point, s, t_real)
  estimations.0 = rbind(estimations.0, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
}

# epsilon greedy, epsilon=0.2
p = 0.2

estimations.2 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y(first_point, s, t_real)
  estimations.2 = rbind(estimations.2, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
}
    
# epsilon greedy, epsilon=0.4
p = 0.4

estimations.4 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y(first_point, s, t_real)
  estimations.4 = rbind(estimations.4, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
}
    
# epsilon greedy, epsilon=0.6    
p = 0.6

estimations.6 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y(first_point, s, t_real)
  estimations.6 = rbind(estimations.6, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
}

# epsilon greedy, epsilon=0.8
p = 0.8
  
estimations.8 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y(first_point, s, t_real)
  estimations.8 = rbind(estimations.8, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
}

#Thompson sampling
p = 1

estimations..1 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y(first_point, s, t_real)
  estimations..1 = rbind(estimations..1, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
}
 
#KW

estimations.kw = c()
for (i in 1:1000) {
  set.seed(i)
  estimations.kw = rbind(estimations.kw, abs(KW_scheme_quadratic(n1, first_point, s, a = 1, c = -1/3) - real_x_qd))
}

# Visualizing
png("Boxplot_g1_n10.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,11], ε_0.2 = estimations.2[,11], ε_0.4 = estimations.4[,11], ε_0.6 = estimations.6[,11], ε_0.8 = estimations.8[,11], TS = estimations..1[,11], KW = estimations.kw[,6])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

# adding x-axis without labels 
axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g1_n50.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,51], ε_0.2 = estimations.2[,51], ε_0.4 = estimations.4[,51], ε_0.6 = estimations.6[,51], ε_0.8 = estimations.8[,51], TS = estimations..1[,51], KW = estimations.kw[,26])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g1_n100.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,101], ε_0.2 = estimations.2[,101], ε_0.4 = estimations.4[,101], ε_0.6 = estimations.6[,101], ε_0.8 = estimations.8[,101], TS = estimations..1[,101], KW = estimations.kw[,51])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

#################################################################################################################################

## g2

t_real_q = c(0,0,0,-8,-8)/(sqrt(32)*14/21)
s = 0.1
plot(seq(-1,1,0.05),observation_y_quartic(seq(-1,1,0.05), s, t_real_q), type = "l", col = "red")
curve(real_g_quartic(x,t_real_q), add=TRUE)
A = diag(5)
real_x_qr = find_maximum_q(t_real_q)

# Mean sampling:
p = 0

estimations.0 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_quartic(first_point, s, t_real_q)
  estimations.0 = rbind(estimations.0, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
}

# epsilon greedy, epsilon=0.2
p = 0.2

estimations.2 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_quartic(first_point, s, t_real_q)
  estimations.2 = rbind(estimations.2, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
}
    
# epsilon greedy, epsilon=0.4
p = 0.4

estimations.4 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_quartic(first_point, s, t_real_q)
  estimations.4 = rbind(estimations.4, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
}
    
# epsilon greedy, epsilon=0.6    
p = 0.6

estimations.6 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_quartic(first_point, s, t_real_q)
  estimations.6 = rbind(estimations.6, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
}

# epsilon greedy, epsilon=0.8
p = 0.8
  
estimations.8 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_quartic(first_point, s, t_real_q)
  estimations.8 = rbind(estimations.8, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
}

#Thompson sampling
p = 1

estimations..1 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_quartic(first_point, s, t_real_q)
  estimations..1 = rbind(estimations..1, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
}
 
#KW

estimations.kw = c()
for (i in 1:1000) {
  set.seed(i)
  estimations.kw = rbind(estimations.kw, abs(KW_scheme_quartic(n1, first_point, s, a = 1, c = -1/3) - real_x_qr))
}

# Visualizing
png("Boxplot_g2_n10.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,11], ε_0.2 = estimations.2[,11], ε_0.4 = estimations.4[,11], ε_0.6 = estimations.6[,11], ε_0.8 = estimations.8[,11], TS = estimations..1[,11], KW = estimations.kw[,11])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g2_n50.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,51], ε_0.2 = estimations.2[,51], ε_0.4 = estimations.4[,51], ε_0.6 = estimations.6[,51], ε_0.8 = estimations.8[,51], TS = estimations..1[,51], KW = estimations.kw[,26])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g2_n100.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,101], ε_0.2 = estimations.2[,101], ε_0.4 = estimations.4[,101], ε_0.6 = estimations.6[,101], ε_0.8 = estimations.8[,101], TS = estimations..1[,101], KW = estimations.kw[,51])
boxplot(data_g2_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g2_ind), labels = FALSE)
dev.off()

#################################################################################################################################

## g3

t_real_triangle = c(0,0,1,1,-1,-1)/(16/(3*sqrt(385)))
s = 0.1
plot(seq(-1,1,0.01),observation_y_triangle(seq(-1,1,0.01), s, t_real_triangle), type = "l", col = "red")
curve(real_g_triangle(x,t_real_triangle), add=TRUE)
A = diag(6)
real_x_triangle = find_maximum_triangle(t_real_triangle)

# Mean sampling:
p = 0

estimations.0 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_triangle(first_point, s, t_real_triangle)
  estimations.0 = rbind(estimations.0, abs(iterative_triangle_final(first_point, n, y_0, p)$X0[, 2] - real_x_triangle))
}

# epsilon greedy, epsilon=0.2
p = 0.2

estimations.2 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_triangle(first_point, s, t_real_q)
  estimations.2 = rbind(estimations.2, abs(iterative_triangle_final(first_point, n, y_0, p)$X0[, 2] - real_x_triangle))
}
    
# epsilon greedy, epsilon=0.4
p = 0.4

estimations.4 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_triangle(first_point, s, t_real_triangle)
  estimations.4 = rbind(estimations.4, abs(iterative_triangle_final(first_point, n, y_0, p)$X0[, 2] - real_x_triangle))
}
    
# epsilon greedy, epsilon=0.6    
p = 0.6

estimations.6 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_triangle(first_point, s, t_real_triangle)
  estimations.6 = rbind(estimations.6, abs(iterative_triangle_final(first_point, n, y_0, p)$X0[, 2] - real_x_triangle))
}

# epsilon greedy, epsilon=0.8
p = 0.8
  
estimations.8 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_triangle(first_point, s, t_real_triangle)
  estimations.8 = rbind(estimations.8, abs(iterative_triangle_final(first_point, n, y_0, p)$X0[, 2] - real_x_triangle))
}

#Thompson sampling
p = 1

estimations..1 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_triangle(first_point, s, t_real_triangle)
  estimations..1 = rbind(estimations..1, abs(iterative_triangle_final(first_point, n, y_0, p)$X0[, 2] - real_x_triangle))
}
 
#KW

estimations.kw = c()
for (i in 1:1000) {
  set.seed(i)
  estimations.kw = rbind(estimations.kw, abs(KW_scheme_triangle(n1, first_point, s, a = 1, c = -1/3) - real_x_triangle))
}

# Visualizing
png("Boxplot_g3_n10.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,11], ε_0.2 = estimations.2[,11], ε_0.4 = estimations.4[,11], ε_0.6 = estimations.6[,11], ε_0.8 = estimations.8[,11], TS = estimations..1[,11], KW = estimations.kw[,11])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g3_n50.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,51], ε_0.2 = estimations.2[,51], ε_0.4 = estimations.4[,51], ε_0.6 = estimations.6[,51], ε_0.8 = estimations.8[,51], TS = estimations..1[,51], KW = estimations.kw[,26])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g3_n100.png", width = 900, height = 450)
data_g3_ind = data.frame(MS = estimations.0[,101], ε_0.2 = estimations.2[,101], ε_0.4 = estimations.4[,101], ε_0.6 = estimations.6[,101], ε_0.8 = estimations.8[,101], TS = estimations..1[,101], KW = estimations.kw[,51])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g3_ind), labels = FALSE)
dev.off()

#################################################################################################################################

## g4

# Fixing parameters
t_real_sin = c(1,1,1)/(2*sqrt(2))
s = 0.1
plot(seq(-1,1,0.001),observation_y_sin(seq(-1,1,0.001), s, t_real_sin), type = "l", col = "red")
curve(real_g_sin(x,t_real_sin), add=TRUE)
A = diag(3)
real_x_sin = find_maximum_sin(t_real_sin)

# Mean sampling:
p = 0

estimations.0 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_sin(first_point, s, t_real_sin)
  estimations.0 = rbind(estimations.0, abs(iterative_sin_final(first_point, n, y_0, p)$X0[, 2] - real_x_sin))
}

# epsilon greedy, epsilon=0.2
p = 0.2

estimations.2 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_sin(first_point, s, t_real_sin)
  estimations.2 = rbind(estimations.2, abs(iterative_sin_final(first_point, n, y_0, p)$X0[, 2] - real_x_sin))
}
    
# epsilon greedy, epsilon=0.4
p = 0.4

estimations.4 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_sin(first_point, s, t_real_sin)
  estimations.4 = rbind(estimations.4, abs(iterative_sin_final(first_point, n, y_0, p)$X0[, 2] - real_x_sin))
}
    
# epsilon greedy, epsilon=0.6    
p = 0.6

estimations.6 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_sin(first_point, s, t_real_sin)
  estimations.6 = rbind(estimations.6, abs(iterative_sin_final(first_point, n, y_0, p)$X0[, 2] - real_x_sin))
}

# epsilon greedy, epsilon=0.8
p = 0.8
  
estimations.8 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_sin(first_point, s, t_real_sin)
  estimations.8 = rbind(estimations.8, abs(iterative_sin_final(first_point, n, y_0, p)$X0[, 2] - real_x_sin))
}

#Thompson sampling
p = 1

estimations..1 = c()
    
for (i in 1:1000) {
  set.seed(i)
  y_0 = observation_y_sin(first_point, s, t_real_sin)
  estimations..1 = rbind(estimations..1, abs(iterative_sin_final(first_point, n, y_0, p)$X0[, 2] - real_x_sin))
}
 
#KW

estimations.kw = c()
for (i in 1:1000) {
  set.seed(i)
  estimations.kw = rbind(estimations.kw, abs(KW_scheme_sin(n1, first_point, s, a = 1, c = -1/3) - real_x_sin))
}

# Visualizing
png("Boxplot_g4_n10.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,11], ε_0.2 = estimations.2[,11], ε_0.4 = estimations.4[,11], ε_0.6 = estimations.6[,11], ε_0.8 = estimations.8[,11], TS = estimations..1[,11], KW = estimations.kw[,11])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g4_n50.png", width = 900, height = 450)
data_g1_ind = data.frame(MS = estimations.0[,51], ε_0.2 = estimations.2[,51], ε_0.4 = estimations.4[,51], ε_0.6 = estimations.6[,51], ε_0.8 = estimations.8[,51], TS = estimations..1[,51], KW = estimations.kw[,26])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g1_ind), labels = FALSE)
dev.off()

png("Boxplot_g4_n100.png", width = 900, height = 450)
data_g4_ind = data.frame(MS = estimations.0[,101], ε_0.2 = estimations.2[,101], ε_0.4 = estimations.4[,101], ε_0.6 = estimations.6[,101], ε_0.8 = estimations.8[,101], TS = estimations..1[,101], KW = estimations.kw[,51])
boxplot(data_g1_ind, xlab = "", cex.axis = 2, xaxt = "n")

axis(1, at = 1:ncol(data_g4_ind), labels = FALSE)
dev.off()

```
