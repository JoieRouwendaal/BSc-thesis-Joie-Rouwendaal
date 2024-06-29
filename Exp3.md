```R
noise_levels = c(0.1,1,10)
n = 100
n1 = n/2

## g1

results3_qd_BO = list()
results3_qd_KW = list()

for (j in seq(0, 1, 0.2)) {
  
  p = j
  
  j_results3 = list()
  
  for (m in noise_levels) {
    s = m
    
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i)
      y_0 = observation_y(first_point,s,t_real)
      estimations = rbind(estimations, abs(iterative_quadratic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qd))
    }
    
    j_results3[[as.character(s)]] = estimations
  }
  
  results3_qd_BO[[as.character(j)]] = j_results3
}

for (m in noise_levels) {
  s = m
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i)
    estimations1 = rbind(estimations1, abs(KW_scheme_quadratic(n1, first_point, s, a = 1, c = -1/3) - real_x_qd))
  }

  results3_qd_KW[[as.character(s)]] = estimations1

########################################################################################################

##g2

results3_qr_BO = list()
results3_qr_KW = list()

for (j in seq(0, 1, 0.2)) {
  
  p = j
  
  j_results3 = list()
  
  for (m in noise_levels) {
    s = m
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i)
      y_0 = observation_y_quartic(first_point,s,t_real_q)
      estimations = rbind(estimations, abs(iterative_quartic_final(first_point, n, y_0, p)$X0[, 2] - real_x_qr))
    }
    
    j_results3[[as.character(s)]] = estimations
  }
  
  results3_qr_BO[[as.character(j)]] = j_results3
}

for (m in noise_levels) {
  s = m
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i)
    estimations1 = rbind(estimations1, abs(KW_scheme_quartic(n1, first_point, s, a = 1, c = -1/3) - real_x_qr))
  }

  results3_qr_KW[[as.character(s)]] = estimations1
}

#######################################################################################################

## g3
results3_tr_BO = list()
results3_tr_KW = list()

for (j in seq(0, 1, 0.2)) {
  
  p = j
  
  
  j_results3 = list()
  
  for (m in noise_levels) {
    s = m
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i)
      y_0 = observation_y_triangle(first_point,s,t_real_triangle)
      estimations = rbind(estimations, t(abs(iterative_triangle_final(first_point, n, y_0, p)$X0 - real_x_triangle)))
    }
    
    j_results3[[as.character(s)]] = estimations
  }
  
  results3_tr_BO[[as.character(j)]] = j_results3
}

for (m in noise_levels) {
  s = m
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i)
    estimations1 = rbind(estimations1, abs(KW_scheme_triangle(n1, first_point, s, a = 1, c = -1/3) - real_x_triangle))
  }

  results3_tr_KW[[as.character(s)]] = estimations1
  
}

###################################################################################################################################

## g4

results3_sin_BO = list()
results3_sin_KW = list()

for (j in seq(0, 1, 0.2)) {
  
  p = j
  
  j_results3 = list()
  
  for (m in noise_levels) {
    s = m
    estimations = c()
    
    for (i in 1:1000) {
      set.seed(i)
      y_0 = observation_y_sin(first_point,s,t_real_sin)
      estimations = rbind(estimations, t(abs(iterative_sin_final(first_point, n, y_0, p)$X0 - real_x_sin)))
    }
    
    j_results3[[as.character(s)]] = estimations
  }
  
  results3_sin_BO[[as.character(j)]] = j_results3
}

for (m in noise_levels) {
  s = m
  estimations1 = c()
  
  for (i in 1:1000) {
    set.seed(i)
    estimations1 = rbind(estimations1, abs(KW_scheme_sin(n1, first_point, s, a = 1, c = -1/3) - real_x_sin))
  }

  results3_sin_KW[[as.character(s)]] = estimations1
  
}


###############################################################################################

## Visualizations

# g1

# n = 10
means_g1_n10_ls = numeric(7)
sd_g1_n10_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n10_ls[i] =  mean(results3_qd_BO[[as.character(j)]][["0.1"]][,11])
  sd_g1_n10_ls[i] = sd(results3_qd_BO[[as.character(j)]][["0.1"]][,11])
  i = i+1
}

means_g1_n10_ls[7] = mean(results3_qd_KW[["0.1"]][,6])
sd_g1_n10_ls[7] = sd(results3_qd_KW[["0.1"]][,6])

means_g1_n10_ms = numeric(7)
sd_g1_n10_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n10_ms[i] =  mean(results3_qd_BO[[as.character(j)]][["1"]][,11])
  sd_g1_n10_ms[i] = sd(results3_qd_BO[[as.character(j)]][["1"]][,11])
  i = i+1
}

means_g1_n10_ms[7] = mean(results3_qd_KW[["1"]][,6])
sd_g1_n10_ms[7] = sd(results3_qd_KW[["1"]][,6])

means_g1_n10_hs = numeric(7)
sd_g1_n10_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n10_hs[i] =  mean(results3_qd_BO[[as.character(j)]][["10"]][,11])
  sd_g1_n10_hs[i] = sd(results3_qd_BO[[as.character(j)]][["10"]][,11])
  i = i+1
}

means_g1_n10_hs[7] = mean(results3_qd_KW[["10"]][,6])
sd_g1_n10_hs[7] = sd(results3_qd_KW[["10"]][,6])

png("exp3.2_g1_n10.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g1_n10_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g1_n10_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g1_n10_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g1_n10_ls - sd_g1_n10_ls,0), (1:7)-0.2, means_g1_n10_ls + sd_g1_n10_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g1_n10_ms - sd_g1_n10_ms,0), 1:7, means_g1_n10_ms + sd_g1_n10_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g1_n10_hs - sd_g1_n10_hs,0), (1:7)+0.2, means_g1_n10_hs + sd_g1_n10_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 50
means_g1_n50_ls = numeric(7)
sd_g1_n50_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n50_ls[i] =  mean(results3_qd_BO[[as.character(j)]][["0.1"]][,51])
  sd_g1_n50_ls[i] = sd(results3_qd_BO[[as.character(j)]][["0.1"]][,51])
  i = i+1
}

means_g1_n50_ls[7] = mean(results3_qd_KW[["0.1"]][,26])
sd_g1_n50_ls[7] = sd(results3_qd_KW[["0.1"]][,26])

means_g1_n50_ms = numeric(7)
sd_g1_n50_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n50_ms[i] =  mean(results3_qd_BO[[as.character(j)]][["1"]][,51])
  sd_g1_n50_ms[i] = sd(results3_qd_BO[[as.character(j)]][["1"]][,51])
  i = i+1
}

means_g1_n50_ms[7] = mean(results3_qd_KW[["1"]][,26])
sd_g1_n50_ms[7] = sd(results3_qd_KW[["1"]][,26])

means_g1_n50_hs = numeric(7)
sd_g1_n50_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n50_hs[i] =  mean(results3_qd_BO[[as.character(j)]][["10"]][,51])
  sd_g1_n50_hs[i] = sd(results3_qd_BO[[as.character(j)]][["10"]][,51])
  i = i+1
}

means_g1_n50_hs[7] = mean(results3_qd_KW[["10"]][,26])
sd_g1_n50_hs[7] = sd(results3_qd_KW[["10"]][,26])

png("exp3.2_g1_n50.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g1_n50_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g1_n50_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g1_n50_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g1_n50_ls - sd_g1_n50_ls,0), (1:7)-0.2, means_g1_n50_ls + sd_g1_n50_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g1_n50_ms - sd_g1_n50_ms,0), 1:7, means_g1_n50_ms + sd_g1_n50_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g1_n50_hs - sd_g1_n50_hs,0), (1:7)+0.2, means_g1_n50_hs + sd_g1_n50_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 100

means_g1_n100_ls = numeric(7)
sd_g1_n100_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n100_ls[i] =  mean(results3_qd_BO[[as.character(j)]][["0.1"]][,101])
  sd_g1_n100_ls[i] = sd(results3_qd_BO[[as.character(j)]][["0.1"]][,101])
  i = i+1
}

means_g1_n100_ls[7] = mean(results3_qd_KW[["0.1"]][,51])
sd_g1_n100_ls[7] = sd(results3_qd_KW[["0.1"]][,51])

means_g1_n100_ms = numeric(7)
sd_g1_n100_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n100_ms[i] =  mean(results3_qd_BO[[as.character(j)]][["1"]][,101])
  sd_g1_n100_ms[i] = sd(results3_qd_BO[[as.character(j)]][["1"]][,101])
  i = i+1
}

means_g1_n100_ms[7] = mean(results3_qd_KW[["1"]][,51])
sd_g1_n100_ms[7] = sd(results3_qd_KW[["1"]][,51])

means_g1_n100_hs = numeric(7)
sd_g1_n100_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g1_n100_hs[i] =  mean(results3_qd_BO[[as.character(j)]][["10"]][,101])
  sd_g1_n100_hs[i] = sd(results3_qd_BO[[as.character(j)]][["10"]][,101])
  i = i+1
}

means_g1_n100_hs[7] = mean(results3_qd_KW[["10"]][,51])
sd_g1_n100_hs[7] = sd(results3_qd_KW[["10"]][,51])

png("exp3.2_g1_n100.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g1_n100_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g1_n100_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g1_n100_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g1_n100_ls - sd_g1_n100_ls,0), (1:7)-0.2, means_g1_n100_ls + sd_g1_n100_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g1_n100_ms - sd_g1_n100_ms,0), 1:7, means_g1_n100_ms + sd_g1_n100_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g1_n100_hs - sd_g1_n100_hs,0), (1:7)+0.2, means_g1_n100_hs + sd_g1_n100_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

###############################################################################################

# g2

# n = 10
means_g2_n10_ls = numeric(7)
sd_g2_n10_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n10_ls[i] =  mean(results3_qr_BO[[as.character(j)]][["0.1"]][,11])
  sd_g2_n10_ls[i] = sd(results3_qr_BO[[as.character(j)]][["0.1"]][,11])
  i = i+1
}

means_g2_n10_ls[7] = mean(results3_qr_KW[["0.1"]][,6])
sd_g2_n10_ls[7] = sd(results3_qr_KW[["0.1"]][,6])

means_g2_n10_ms = numeric(7)
sd_g2_n10_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n10_ms[i] =  mean(results3_qr_BO[[as.character(j)]][["1"]][,11])
  sd_g2_n10_ms[i] = sd(results3_qr_BO[[as.character(j)]][["1"]][,11])
  i = i+1
}

means_g2_n10_ms[7] = mean(results3_qr_KW[["1"]][,6])
sd_g2_n10_ms[7] = sd(results3_qr_KW[["1"]][,6])

means_g2_n10_hs = numeric(7)
sd_g2_n10_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n10_hs[i] =  mean(results3_qr_BO[[as.character(j)]][["10"]][,11])
  sd_g2_n10_hs[i] = sd(results3_qr_BO[[as.character(j)]][["10"]][,11])
  i = i+1
}

means_g2_n10_hs[7] = mean(results3_qr_KW[["10"]][,6])
sd_g2_n10_hs[7] = sd(results3_qr_KW[["10"]][,6])

png("exp3.2_g2_n10.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g2_n10_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g2_n10_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g2_n10_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g2_n10_ls - sd_g2_n10_ls,0), (1:7)-0.2, means_g2_n10_ls + sd_g2_n10_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g2_n10_ms - sd_g2_n10_ms,0), 1:7, means_g2_n10_ms + sd_g2_n10_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g2_n10_hs - sd_g2_n10_hs,0), (1:7)+0.2, means_g2_n10_hs + sd_g2_n10_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1,, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 50
means_g2_n50_ls = numeric(7)
sd_g2_n50_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n50_ls[i] =  mean(results3_qr_BO[[as.character(j)]][["0.1"]][,51])
  sd_g2_n50_ls[i] = sd(results3_qr_BO[[as.character(j)]][["0.1"]][,51])
  i = i+1
}

means_g2_n50_ls[7] = mean(results3_qr_KW[["0.1"]][,26])
sd_g2_n50_ls[7] = sd(results3_qr_KW[["0.1"]][,26])

means_g2_n50_ms = numeric(7)
sd_g2_n50_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n50_ms[i] =  mean(results3_qr_BO[[as.character(j)]][["1"]][,51])
  sd_g2_n50_ms[i] = sd(results3_qr_BO[[as.character(j)]][["1"]][,51])
  i = i+1
}

means_g2_n50_ms[7] = mean(results3_qr_KW[["1"]][,26])
sd_g2_n50_ms[7] = sd(results3_qr_KW[["1"]][,26])

means_g2_n50_hs = numeric(7)
sd_g2_n50_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n50_hs[i] =  mean(results3_qr_BO[[as.character(j)]][["10"]][,51])
  sd_g2_n50_hs[i] = sd(results3_qr_BO[[as.character(j)]][["10"]][,51])
  i = i+1
}

means_g2_n50_hs[7] = mean(results3_qr_KW[["10"]][,26])
sd_g2_n50_hs[7] = sd(results3_qr_KW[["10"]][,26])

png("exp3.2_g2_n50.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g2_n50_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g2_n50_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g2_n50_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g2_n50_ls - sd_g2_n50_ls,0), (1:7)-0.2, means_g2_n50_ls + sd_g2_n50_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g2_n50_ms - sd_g2_n50_ms,0), 1:7, means_g2_n50_ms + sd_g2_n50_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g2_n50_hs - sd_g2_n50_hs,0), (1:7)+0.2, means_g2_n50_hs + sd_g2_n50_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1,, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 100
means_g2_n100_ls = numeric(7)
sd_g2_n100_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n100_ls[i] =  mean(results3_qr_BO[[as.character(j)]][["0.1"]][,101])
  sd_g2_n100_ls[i] = sd(results3_qr_BO[[as.character(j)]][["0.1"]][,101])
  i = i+1
}

means_g2_n100_ls[7] = mean(results3_qr_KW[["0.1"]][,51])
sd_g2_n100_ls[7] = sd(results3_qr_KW[["0.1"]][,51])

means_g2_n100_ms = numeric(7)
sd_g2_n100_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n100_ms[i] =  mean(results3_qr_BO[[as.character(j)]][["1"]][,101])
  sd_g2_n100_ms[i] = sd(results3_qr_BO[[as.character(j)]][["1"]][,101])
  i = i+1
}

means_g2_n100_ms[7] = mean(results3_qr_KW[["1"]][,51])
sd_g2_n100_ms[7] = sd(results3_qr_KW[["1"]][,51])

means_g2_n100_hs = numeric(7)
sd_g2_n100_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g2_n100_hs[i] =  mean(results3_qr_BO[[as.character(j)]][["10"]][,101])
  sd_g2_n100_hs[i] = sd(results3_qr_BO[[as.character(j)]][["10"]][,101])
  i = i+1
}

means_g2_n100_hs[7] = mean(results3_qr_KW[["10"]][,51])
sd_g2_n100_hs[7] = sd(results3_qr_KW[["10"]][,51])

png("exp3.2_g2_n100.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g2_n100_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g2_n100_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g2_n100_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g2_n100_ls - sd_g2_n100_ls,0), (1:7)-0.2, means_g2_n100_ls + sd_g2_n100_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g2_n100_ms - sd_g2_n100_ms,0), 1:7, means_g2_n100_ms + sd_g2_n100_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g2_n100_hs - sd_g2_n100_hs,0), (1:7)+0.2, means_g2_n100_hs + sd_g2_n100_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1,, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

###############################################################################################

# g3

# n = 10
means_g3_n10_ls = numeric(7)
sd_g3_n10_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n10_ls[i] =  mean(results3_tr_BO[[as.character(j)]][["0.1"]][,11])
  sd_g3_n10_ls[i] = sd(results3_tr_BO[[as.character(j)]][["0.1"]][,11])
  i = i+1
}

means_g3_n10_ls[7] = mean(results3_tr_KW[["0.1"]][,6])
sd_g3_n10_ls[7] = sd(results3_tr_KW[["0.1"]][,6])

means_g3_n10_ms = numeric(7)
sd_g3_n10_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n10_ms[i] =  mean(results3_tr_BO[[as.character(j)]][["1"]][,11])
  sd_g3_n10_ms[i] = sd(results3_tr_BO[[as.character(j)]][["1"]][,11])
  i = i+1
}

means_g3_n10_ms[7] = mean(results3_tr_KW[["1"]][,6])
sd_g3_n10_ms[7] = sd(results3_tr_KW[["1"]][,6])

means_g3_n10_hs = numeric(7)
sd_g3_n10_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n10_hs[i] =  mean(results3_tr_BO[[as.character(j)]][["10"]][,11])
  sd_g3_n10_hs[i] = sd(results3_tr_BO[[as.character(j)]][["10"]][,11])
  i = i+1
}

means_g3_n10_hs[7] = mean(results3_tr_KW[["10"]][,6])
sd_g3_n10_hs[7] = sd(results3_tr_KW[["10"]][,6])

png("exp3.2_g3_n10.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g3_n10_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g3_n10_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g3_n10_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g3_n10_ls - sd_g3_n10_ls,0), (1:7)-0.2, means_g3_n10_ls + sd_g3_n10_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g3_n10_ms - sd_g3_n10_ms,0), 1:7, means_g3_n10_ms + sd_g3_n10_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g3_n10_hs - sd_g3_n10_hs,0), (1:7)+0.2, means_g3_n10_hs + sd_g3_n10_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 50
means_g3_n50_ls = numeric(7)
sd_g3_n50_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n50_ls[i] =  mean(results3_tr_BO[[as.character(j)]][["0.1"]][,51])
  sd_g3_n50_ls[i] = sd(results3_tr_BO[[as.character(j)]][["0.1"]][,51])
  i = i+1
}

means_g3_n50_ls[7] = mean(results3_tr_KW[["0.1"]][,26])
sd_g3_n50_ls[7] = sd(results3_tr_KW[["0.1"]][,26])

means_g3_n50_ms = numeric(7)
sd_g3_n50_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n50_ms[i] =  mean(results3_tr_BO[[as.character(j)]][["1"]][,51])
  sd_g3_n50_ms[i] = sd(results3_tr_BO[[as.character(j)]][["1"]][,51])
  i = i+1
}

means_g3_n50_ms[7] = mean(results3_tr_KW[["1"]][,26])
sd_g3_n50_ms[7] = sd(results3_tr_KW[["1"]][,26])

means_g3_n50_hs = numeric(7)
sd_g3_n50_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n50_hs[i] =  mean(results3_tr_BO[[as.character(j)]][["10"]][,51])
  sd_g3_n50_hs[i] = sd(results3_tr_BO[[as.character(j)]][["10"]][,51])
  i = i+1
}

means_g3_n50_hs[7] = mean(results3_tr_KW[["10"]][,26])
sd_g3_n50_hs[7] = sd(results3_tr_KW[["10"]][,26])

png("exp3.2_g3_n50.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g3_n50_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g3_n50_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g3_n50_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g3_n50_ls - sd_g3_n50_ls,0), (1:7)-0.2, means_g3_n50_ls + sd_g3_n50_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g3_n50_ms - sd_g3_n50_ms,0), 1:7, means_g3_n50_ms + sd_g3_n50_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g3_n50_hs - sd_g3_n50_hs,0), (1:7)+0.2, means_g3_n50_hs + sd_g3_n50_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 100
means_g3_n100_ls = numeric(7)
sd_g3_n100_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n100_ls[i] =  mean(results3_tr_BO[[as.character(j)]][["0.1"]][,101])
  sd_g3_n100_ls[i] = sd(results3_tr_BO[[as.character(j)]][["0.1"]][,101])
  i = i+1
}

means_g3_n100_ls[7] = mean(results3_tr_KW[["0.1"]][,51])
sd_g3_n100_ls[7] = sd(results3_tr_KW[["0.1"]][,51])

means_g3_n100_ms = numeric(7)
sd_g3_n100_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n100_ms[i] =  mean(results3_tr_BO[[as.character(j)]][["1"]][,101])
  sd_g3_n100_ms[i] = sd(results3_tr_BO[[as.character(j)]][["1"]][,101])
  i = i+1
}

means_g3_n100_ms[7] = mean(results3_tr_KW[["1"]][,51])
sd_g3_n100_ms[7] = sd(results3_tr_KW[["1"]][,51])

means_g3_n100_hs = numeric(7)
sd_g3_n100_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g3_n100_hs[i] =  mean(results3_tr_BO[[as.character(j)]][["10"]][,101])
  sd_g3_n100_hs[i] = sd(results3_tr_BO[[as.character(j)]][["10"]][,101])
  i = i+1
}

means_g3_n100_hs[7] = mean(results3_tr_KW[["10"]][,51])
sd_g3_n100_hs[7] = sd(results3_tr_KW[["10"]][,51])

png("exp3.2_g3_n100.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g3_n100_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g3_n100_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g3_n100_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g3_n100_ls - sd_g3_n100_ls,0), (1:7)-0.2, means_g3_n100_ls + sd_g3_n100_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g3_n100_ms - sd_g3_n100_ms,0), 1:7, means_g3_n100_ms + sd_g3_n100_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g3_n100_hs - sd_g3_n100_hs,0), (1:7)+0.2, means_g3_n100_hs + sd_g3_n100_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

###############################################################################################

# g4

# n = 10
means_g4_n10_ls = numeric(7)
sd_g4_n10_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n10_ls[i] =  mean(results3_sin_BO[[as.character(j)]][["0.1"]][,11])
  sd_g4_n10_ls[i] = sd(results3_sin_BO[[as.character(j)]][["0.1"]][,11])
  i = i+1
}

means_g4_n10_ls[7] = mean(results3_sin_KW[["0.1"]][,6])
sd_g4_n10_ls[7] = sd(results3_sin_KW[["0.1"]][,6])

means_g4_n10_ms = numeric(7)
sd_g4_n10_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n10_ms[i] =  mean(results3_sin_BO[[as.character(j)]][["1"]][,11])
  sd_g4_n10_ms[i] = sd(results3_sin_BO[[as.character(j)]][["1"]][,11])
  i = i+1
}

means_g4_n10_ms[7] = mean(results3_sin_KW[["1"]][,6])
sd_g4_n10_ms[7] = sd(results3_sin_KW[["1"]][,6])

means_g4_n10_hs = numeric(7)
sd_g4_n10_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n10_hs[i] =  mean(results3_sin_BO[[as.character(j)]][["10"]][,11])
  sd_g4_n10_hs[i] = sd(results3_sin_BO[[as.character(j)]][["10"]][,11])
  i = i+1
}

means_g4_n10_hs[7] = mean(results3_sin_KW[["10"]][,6])
sd_g4_n10_hs[7] = sd(results3_sin_KW[["10"]][,6])

png("exp3.2_g4_n10.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g4_n10_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g4_n10_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g4_n10_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g4_n10_ls - sd_g4_n10_ls,0), (1:7)-0.2, means_g4_n10_ls + sd_g4_n10_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g4_n10_ms - sd_g4_n10_ms,0), 1:7, means_g4_n10_ms + sd_g4_n10_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g4_n10_hs - sd_g4_n10_hs,0), (1:7)+0.2, means_g4_n10_hs + sd_g4_n10_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 50
means_g4_n50_ls = numeric(7)
sd_g4_n50_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n50_ls[i] =  mean(results3_sin_BO[[as.character(j)]][["0.1"]][,51])
  sd_g4_n50_ls[i] = sd(results3_sin_BO[[as.character(j)]][["0.1"]][,51])
  i = i+1
}

means_g4_n50_ls[7] = mean(results3_sin_KW[["0.1"]][,26])
sd_g4_n50_ls[7] = sd(results3_sin_KW[["0.1"]][,26])

means_g4_n50_ms = numeric(7)
sd_g4_n50_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n50_ms[i] =  mean(results3_sin_BO[[as.character(j)]][["1"]][,51])
  sd_g4_n50_ms[i] = sd(results3_sin_BO[[as.character(j)]][["1"]][,51])
  i = i+1
}

means_g4_n50_ms[7] = mean(results3_sin_KW[["1"]][,26])
sd_g4_n50_ms[7] = sd(results3_sin_KW[["1"]][,26])

means_g4_n50_hs = numeric(7)
sd_g4_n50_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n50_hs[i] =  mean(results3_sin_BO[[as.character(j)]][["10"]][,51])
  sd_g4_n50_hs[i] = sd(results3_sin_BO[[as.character(j)]][["10"]][,51])
  i = i+1
}

means_g4_n50_hs[7] = mean(results3_sin_KW[["10"]][,26])
sd_g4_n50_hs[7] = sd(results3_sin_KW[["10"]][,26])

png("exp3.2_g4_n50.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g4_n50_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g4_n50_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g4_n50_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g4_n50_ls - sd_g4_n50_ls,0), (1:7)-0.2, means_g4_n50_ls + sd_g4_n50_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g4_n50_ms - sd_g4_n50_ms,0), 1:7, means_g4_n50_ms + sd_g4_n50_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g4_n50_hs - sd_g4_n50_hs,0), (1:7)+0.2, means_g4_n50_hs + sd_g4_n50_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# n = 100
means_g4_n100_ls = numeric(7)
sd_g4_n100_ls = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n100_ls[i] =  mean(results3_sin_BO[[as.character(j)]][["0.1"]][,101])
  sd_g4_n100_ls[i] = sd(results3_sin_BO[[as.character(j)]][["0.1"]][,101])
  i = i+1
}

means_g4_n100_ls[7] = mean(results3_sin_KW[["0.1"]][,51])
sd_g4_n100_ls[7] = sd(results3_sin_KW[["0.1"]][,51])

means_g4_n100_ms = numeric(7)
sd_g4_n100_ms = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n100_ms[i] =  mean(results3_sin_BO[[as.character(j)]][["1"]][,101])
  sd_g4_n100_ms[i] = sd(results3_sin_BO[[as.character(j)]][["1"]][,101])
  i = i+1
}

means_g4_n100_ms[7] = mean(results3_sin_KW[["1"]][,51])
sd_g4_n100_ms[7] = sd(results3_sin_KW[["1"]][,51])

means_g4_n100_hs = numeric(7)
sd_g4_n100_hs = numeric(7)
i = 1

for (j in seq(0,1,0.2)) {
  means_g4_n100_hs[i] =  mean(results3_sin_BO[[as.character(j)]][["10"]][,101])
  sd_g4_n100_hs[i] = sd(results3_sin_BO[[as.character(j)]][["10"]][,101])
  i = i+1
}

means_g4_n100_hs[7] = mean(results3_sin_KW[["10"]][,51])
sd_g4_n100_hs[7] = sd(results3_sin_KW[["10"]][,51])

png("exp3.2_g4_n100.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,2.5), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
points(seq(1:7)-0.2, means_g4_n100_ls, col = rgb(0, 0, 1, alpha=0.4), pch = 19, cex=3)
points(seq(1:7), means_g4_n100_ms, col = rgb(0, 0, 0.6, alpha=0.7), pch = 19, cex=3)
points(seq(1:7)+0.2, means_g4_n100_hs, col = rgb(0, 0.2, 0.2, alpha=1), pch = 19, cex=3)
arrows((1:7)-0.2, pmax(means_g4_n100_ls - sd_g4_n100_ls,0), (1:7)-0.2, means_g4_n100_ls + sd_g4_n100_ls, col = rgb(0, 0, 1, alpha = 0.4), angle = 90, code = 3, length = 0.1, lwd=3)
arrows(1:7, pmax(means_g4_n100_ms - sd_g4_n100_ms,0), 1:7, means_g4_n100_ms + sd_g4_n100_ms, col = rgb(0, 0, 0.6, alpha = 0.7), angle = 90, code = 3, length = 0.1, lwd=3)
arrows((1:7)+0.2, pmax(means_g4_n100_hs - sd_g4_n100_hs,0), (1:7)+0.2, means_g4_n100_hs + sd_g4_n100_hs, col = rgb(0, 0, 0.2, alpha = 1), angle = 90, code = 3, length = 0.1, lwd=3)
axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c("SNR = 10", "SNR = 1", "SNR = 0.1"), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()




  


```
