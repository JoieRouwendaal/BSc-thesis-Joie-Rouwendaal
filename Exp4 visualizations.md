```R

exp4g1_BO = readRDS("14exp4.1_BO.RDS")
exp4g1_KW = readRDS("14exp4.1_KW.RDS")


exp4g3_BO = readRDS("14exp4.3_BO.RDS")
exp4g3_KW = readRDS("14exp4.3_KW.RDS")

exp4g4_BO = readRDS("14exp4.4_BO.RDS")
exp4g4_KW = readRDS("14exp4.4_KW.RDS")

x = seq(1:101)
y = numeric(101)
x2 = seq(1:51)
y2 = numeric(51)

######################################################################################################

## Convergence lines

# quadratic 1

for (j in seq(0,1,0.2)) {
  label = paste(j, "g1_exp4.png")
  png(label, width = 900, height = 600)
plot(x, y, log = "x", col="white", ylim=c(-5,0.5), ylab = "", xlab = "", cex.axis = 2.5)
  
for (k in first_points) {
  lines(x, log(exp4g1_BO[[as.character(j)]][[as.character(k)]]), col=rgb(0,0,0,alpha=0.3) , type="l", lwd=3, cex = 2.5)
  
}
dev.off()
}

png("KW_g1_exp4.png", width = 900, height = 600)

plot(x2,y2, col="white", log = "x", ylim = c(-5,0.5), ylab = "", xlab = "", cex.axis = 2.5)

for (k in first_points){
  lines(x2, log(exp4g1_KW[[as.character(k)]]), col=rgb(0,0,0,alpha=0.3) , type="l", lwd=3, cex = 2.5)
  
}
dev.off()

# quartic 1

for (j in seq(0,1,0.2)) {
  label = paste(j, "g3_exp4.png")
  png(label, width = 900, height = 600)

plot(x, y, log = "x", col="white", ylim=c(-5,0.5), ylab = "", xlab = "", cex.axis = 2.5)
  
for (k in first_points) {
  lines(x, log(exp4g3_BO[[as.character(j)]][[as.character(k)]]), col=rgb(0,0,0,alpha=0.3) , type="l", lwd=3, cex = 2.5)
  
}
dev.off()
}

png("KW_g3_exp4.png", width = 900, height = 600)

plot(x2,y2, col="white", log = "x", ylim = c(-5,0.5), ylab = "", xlab = "", cex.axis = 2.5)

for (k in first_points){
  lines(x2, log(exp4g3_KW[[as.character(k)]]), col=rgb(0,0,0,alpha=0.3) , type="l", lwd=3, cex = 2.5)
  
}
dev.off()

# quartic 2

for (j in seq(0,1,0.2)) {
  label = paste(j, "g4_exp4.png")
  png(label, width = 900, height = 600)
  
plot(x, y, log = "x", col="white", ylim=c(-5,0.5), ylab = "", xlab = "", cex.axis = 2.5)
  
for (k in first_points) {
  lines(x, log(exp4g4_BO[[as.character(j)]][[as.character(k)]]), col=rgb(0,0,0,alpha=0.3) , type="l", lwd=3, cex=2.5)
  
}
dev.off()
}

png("KW_g4_exp4.png", width = 900, height = 600)
plot(x2,y2, col="white", log = "x", ylim = c(-5,0.5), ylab = "", xlab = "", cex.axis = 2.5)

for (k in first_points){
  lines(x2, log(exp4g4_KW[[as.character(k)]]), col=rgb(0,0,0,alpha=0.3) , type="l", lwd=3, cex=2.5)
  
}
dev.off()

#################################################################################################################################

## Aggregated error

# Computing aggregated error

# quadratic 1

ag_error_g1exp4_100 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()

  for(k in first_points){
  individuals = c(individuals, sum(exp4g1_BO[[as.character(j)]][[as.character(k)]]))
  }

ag_error_g1exp4_100[i] = mean(individuals)/100
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g1_KW[[as.character(k)]]))
  }

ag_error_g1exp4_100[7] = mean(2*individuals)/100

ag_error_g1exp4_50 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()

  for(k in first_points){
  individuals = c(individuals, sum(exp4g1_BO[[as.character(j)]][[as.character(k)]][1:51]))
  }

ag_error_g1exp4_50[i] = mean(individuals)/50
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g1_KW[[as.character(k)]][1:26]))
  }

ag_error_g1exp4_50[7] = mean(2*individuals)/50

ag_error_g1exp4_10 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()

  for(k in first_points){
  individuals = c(individuals, sum(exp4g1_BO[[as.character(j)]][[as.character(k)]][1:11]))
  }

ag_error_g1exp4_10[i] = mean(individuals)/10
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g1_KW[[as.character(k)]][1:6]))
  }

ag_error_g1exp4_10[7] = mean(2*individuals)/10



# quartic 1

ag_error_g3exp4_100 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(exp4g3_BO[[as.character(j)]][[as.character(k)]]))
  }
  
ag_error_g3exp4_100[i] = mean(individuals)/100
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g3_KW[[as.character(k)]]))
  }

ag_error_g3exp4_100[7] = mean(2*individuals)/100

ag_error_g3exp4_50 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(exp4g3_BO[[as.character(j)]][[as.character(k)]][1:51]))
  }
  
ag_error_g3exp4_50[i] = mean(individuals)/50
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g3_KW[[as.character(k)]][1:26]))
  }

ag_error_g3exp4_50[7] = mean(2*individuals)/50

ag_error_g3exp4_10 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(exp4g3_BO[[as.character(j)]][[as.character(k)]][1:11]))
  }
  
ag_error_g3exp4_10[i] = mean(individuals)/10
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g3_KW[[as.character(k)]][1:6]))
  }

ag_error_g3exp4_10[7] = mean(2*individuals)/10




# quartic 2

ag_error_g4exp4_100 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(exp4g4_BO[[as.character(j)]][[as.character(k)]]))
  }
  
ag_error_g4exp4_100[i] = mean(individuals)/100
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g4_KW[[as.character(k)]]))
  }

ag_error_g4exp4_100[7] = mean(2*individuals)/100


ag_error_g4exp4_50 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(exp4g4_BO[[as.character(j)]][[as.character(k)]][1:51]))
  }
  
ag_error_g4exp4_50[i] = mean(individuals)/50
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g4_KW[[as.character(k)]][1:26]))
  }

ag_error_g4exp4_50[7] = mean(2*individuals)/50

ag_error_g4exp4_10 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(exp4g4_BO[[as.character(j)]][[as.character(k)]][1:11]))
  }
  
ag_error_g4exp4_10[i] = mean(individuals)/10
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(exp4g4_KW[[as.character(k)]][1:6]))
  }

ag_error_g4exp4_10[7] = mean(2*individuals)/10



###############################################################################################################

## Visualizing aggregated error

# quadratic 1

png("exp4_g1_agg_error.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,1), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
lines(1:7, ag_error_g1exp4_10, cex = 3, lwd = 3, col = rgb(0, 0, 1, alpha=0.4)) 
points(1: 7, ag_error_g1exp4_10, pch = 19, cex=3, col = rgb(0, 0, 1, alpha=0.4))
lines(1:7, ag_error_g1exp4_50, cex = 3, lwd = 3, col= rgb(0, 0, 0.6, alpha=0.7)) 
points(1: 7, ag_error_g1exp4_50, pch = 19, cex=3, col = rgb(0, 0, 0.6, alpha=0.7))
lines(1:7, ag_error_g1exp4_100, cex = 3, lwd = 3, col = rgb(0, 0.2, 0.2, alpha=1)) 
points(1: 7, ag_error_g1exp4_100, pch = 19, cex=3, col = rgb(0, 0.2, 0.2, alpha=1))

axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c(expression(n == 10), expression(n == 50), expression(n == 100)), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# quartic 1

png("exp4_g3_agg_error.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,1), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
lines(1:7, ag_error_g3exp4_10, cex = 3, lwd = 3, col = rgb(0, 0, 1, alpha=0.4)) 
points(1: 7, ag_error_g3exp4_10, pch = 19, cex=3, col = rgb(0, 0, 1, alpha=0.4))
lines(1:7, ag_error_g3exp4_50, cex = 3, lwd = 3, col= rgb(0, 0, 0.6, alpha=0.7)) 
points(1: 7, ag_error_g3exp4_50, pch = 19, cex=3, col = rgb(0, 0, 0.6, alpha=0.7))
lines(1:7, ag_error_g3exp4_100, cex = 3, lwd = 3, col = rgb(0, 0.2, 0.2, alpha=1)) 
points(1: 7, ag_error_g3exp4_100, pch = 19, cex=3, col = rgb(0, 0.2, 0.2, alpha=1))

axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c(expression(n == 10), expression(n == 50), expression(n == 100)), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()


# quartic 2

png("exp4_g4_agg_error.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,1), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
lines(1:7, ag_error_g4exp4_10, cex = 3, lwd = 3, col = rgb(0, 0, 1, alpha=0.4)) 
points(1: 7, ag_error_g4exp4_10, pch = 19, cex=3, col = rgb(0, 0, 1, alpha=0.4))
lines(1:7, ag_error_g4exp4_50, cex = 3, lwd = 3, col= rgb(0, 0, 0.6, alpha=0.7)) 
points(1: 7, ag_error_g4exp4_50, pch = 19, cex=3, col = rgb(0, 0, 0.6, alpha=0.7))
lines(1:7, ag_error_g4exp4_100, cex = 3, lwd = 3, col = rgb(0, 0.2, 0.2, alpha=1)) 
points(1: 7, ag_error_g4exp4_100, pch = 19, cex=3, col = rgb(0, 0.2, 0.2, alpha=1))

axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c(expression(n == 10), expression(n == 50), expression(n == 100)), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()




```
