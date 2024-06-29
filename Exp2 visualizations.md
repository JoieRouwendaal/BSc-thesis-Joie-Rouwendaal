```R

## Loading results
g1_BO2 = readRDS('2resultsqdBO_n100.RDS')
g1_KW2 = readRDS('2resultsqdKW_n100.RDS')

g2_BO2 = readRDS('2resultsqrBO_n100.RDS')
g2_KW2 = readRDS('2resultsqrKW_n100.RDS')

g3_BO2 = readRDS('2resultstrBO_n100.RDS')
g3_KW2 = readRDS('2resultstrKW_n100.RDS')

g4_BO2 = readRDS('2resultssinBO_n100.RDS')
g4_KW2 = readRDS('2resultssinKW_n100.RDS')

###################################################################################################

## Computing aggregated errors

# g1
# n = 10
ag_error_g1_n10 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g1_BO2[[as.character(j)]][[as.character(k)]][1:11]))
  }
  
ag_error_g1_n10[i] = mean(individuals)/10
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g1_KW2[[as.character(k)]][1:6]))/10
  }

ag_error_g1_n10[7] = mean(2*individuals)

# n = 50
ag_error_g1_n50 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g1_BO2[[as.character(j)]][[as.character(k)]][1:51]))
  }
  
ag_error_g1_n50[i] = mean(individuals)/50
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g1_KW2[[as.character(k)]][1:26]))
  }

ag_error_g1_n50[7] = mean(2*individuals)/50

#n=100
ag_error_g1 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g1_BO2[[as.character(j)]][[as.character(k)]]))
  }
  
ag_error_g1[i] = mean(individuals)/100
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g1_KW2[[as.character(k)]]))
  }

ag_error_g1[7] = mean(2*individuals)/100

##################################################################################################
# g2
# n = 10
ag_error_g2_n10 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g2_BO2[[as.character(j)]][[as.character(k)]][1:11]))
  }
  
ag_error_g2_n10[i] = mean(individuals)/10
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g2_KW2[[as.character(k)]][1:6]))/10
  }

ag_error_g2_n10[7] = mean(2*individuals)

# n = 50
ag_error_g2_n50 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g2_BO2[[as.character(j)]][[as.character(k)]][1:51]))
  }
  
ag_error_g2_n50[i] = mean(individuals)/50
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g2_KW2[[as.character(k)]][1:26]))
  }

ag_error_g2_n50[7] = mean(2*individuals)/50

#n=100
ag_error_g2 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g2_BO2[[as.character(j)]][[as.character(k)]]))
  }
  
ag_error_g2[i] = mean(individuals)/100
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g2_KW2[[as.character(k)]]))
  }

ag_error_g2[7] = mean(2*individuals)/100

###############################################################################################

# g3
# n = 10
ag_error_g3_n10 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g3_BO2[[as.character(j)]][[as.character(k)]][1:11]))
  }
  
ag_error_g3_n10[i] = mean(individuals)/10
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g3_KW2[[as.character(k)]][1:6]))/10
  }

ag_error_g3_n10[7] = mean(2*individuals)

# n = 50
ag_error_g3_n50 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g3_BO2[[as.character(j)]][[as.character(k)]][1:51]))
  }
  
ag_error_g3_n50[i] = mean(individuals)/50
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g3_KW2[[as.character(k)]][1:26]))
  }

ag_error_g3_n50[7] = mean(2*individuals)/50

#n=100
ag_error_g3 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g3_BO2[[as.character(j)]][[as.character(k)]]))
  }
  
ag_error_g3[i] = mean(individuals)/100
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g3_KW2[[as.character(k)]]))
  }

ag_error_g3[7] = mean(2*individuals)/100

###############################################################################################

#g4
# n = 10
ag_error_g4_n10 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g4_BO2[[as.character(j)]][[as.character(k)]][1:11]))
  }
  
ag_error_g4_n10[i] = mean(individuals)/10
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g4_KW2[[as.character(k)]][1:6]))/10
  }

ag_error_g4_n10[7] = mean(2*individuals)

# n = 50
ag_error_g4_n50 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g4_BO2[[as.character(j)]][[as.character(k)]][1:51]))
  }
  
ag_error_g4_n50[i] = mean(individuals)/50
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g4_KW2[[as.character(k)]][1:26]))
  }

ag_error_g4_n50[7] = mean(2*individuals)/50

#n=100
ag_error_g4 = numeric(7)
i = 1
for (j in seq(0,1,0.2)) {
  individuals = c()
  
  for(k in first_points){
  individuals = c(individuals, sum(g4_BO2[[as.character(j)]][[as.character(k)]]))
  }
  
ag_error_g4[i] = mean(individuals)/100
i = i+1
}

individuals = c()
for(k in first_points){
  individuals = c(individuals, sum(g4_KW2[[as.character(k)]]))
  }

ag_error_g4[7] = mean(2*individuals)/100

###############################################################################################

## Aggregated error visualized

# g1 

png("exp2_g1_agg_error.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,1), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
lines(1:7, ag_error_g1_n10, cex = 3, lwd = 3, col = rgb(0, 0, 1, alpha=0.4)) 
points(1: 7, ag_error_g1_n10, pch = 19, cex=3, col = rgb(0, 0, 1, alpha=0.4))
lines(1:7, ag_error_g1_n50, cex = 3, lwd = 3, col= rgb(0, 0, 0.6, alpha=0.7)) 
points(1: 7, ag_error_g1_n50, pch = 19, cex=3, col = rgb(0, 0, 0.6, alpha=0.7))
lines(1:7, ag_error_g1, cex = 3, lwd = 3, col = rgb(0, 0.2, 0.2, alpha=1)) 
points(1: 7, ag_error_g1, pch = 19, cex=3, col = rgb(0, 0.2, 0.2, alpha=1))

axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c(expression(n == 10), expression(n == 50), expression(n == 100)), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# g2 

png("exp2_g2_agg_error.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,1), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
lines(1:7, ag_error_g2_n10, cex = 3, lwd = 3, col = rgb(0, 0, 1, alpha=0.4)) 
points(1: 7, ag_error_g2_n10, pch = 19, cex=3, col = rgb(0, 0, 1, alpha=0.4))
lines(1:7, ag_error_g2_n50, cex = 3, lwd = 3, col= rgb(0, 0, 0.6, alpha=0.7)) 
points(1: 7, ag_error_g2_n50, pch = 19, cex=3, col = rgb(0, 0, 0.6, alpha=0.7))
lines(1:7, ag_error_g2, cex = 3, lwd = 3, col = rgb(0, 0.2, 0.2, alpha=1)) 
points(1: 7, ag_error_g2, pch = 19, cex=3, col = rgb(0, 0.2, 0.2, alpha=1))

axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topleft", legend = c(expression(n == 10), expression(n == 50), expression(n == 100)), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# g1 

png("exp2_g3_agg_error.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,1.2), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
lines(1:7, ag_error_g3_n10, cex = 3, lwd = 3, col = rgb(0, 0, 1, alpha=0.4)) 
points(1: 7, ag_error_g3_n10, pch = 19, cex=3, col = rgb(0, 0, 1, alpha=0.4))
lines(1:7, ag_error_g3_n50, cex = 3, lwd = 3, col= rgb(0, 0, 0.6, alpha=0.7)) 
points(1: 7, ag_error_g3_n50, pch = 19, cex=3, col = rgb(0, 0, 0.6, alpha=0.7))
lines(1:7, ag_error_g3, cex = 3, lwd = 3, col = rgb(0, 0.2, 0.2, alpha=1)) 
points(1: 7, ag_error_g3, pch = 19, cex=3, col = rgb(0, 0.2, 0.2, alpha=1))

axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topright", legend = c(expression(n == 10), expression(n == 50), expression(n == 100)), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

# g4

png("exp2_g4_agg_error.png", width = 900, height = 600)
plot(1, type = "n", xlim = c(0.7, 7.3), ylim = c(0,1.2), ylab = "", xlab = "", xaxt = "n", cex.axis = 3)
lines(1:7, ag_error_g4_n10, cex = 3, lwd = 3, col = rgb(0, 0, 1, alpha=0.4)) 
points(1: 7, ag_error_g4_n10, pch = 19, cex=3, col = rgb(0, 0, 1, alpha=0.4))
lines(1:7, ag_error_g4_n50, cex = 3, lwd = 3, col= rgb(0, 0, 0.6, alpha=0.7)) 
points(1: 7, ag_error_g4_n50, pch = 19, cex=3, col = rgb(0, 0, 0.6, alpha=0.7))
lines(1:7, ag_error_g4, cex = 3, lwd = 3, col = rgb(0, 0.2, 0.2, alpha=1)) 
points(1: 7, ag_error_g4, pch = 19, cex=3, col = rgb(0, 0.2, 0.2, alpha=1))

axis(1, at = 1:7, labels = FALSE)
legend( bty="n", "topleft", legend = c(expression(n == 10), expression(n == 50), expression(n == 100)), col = c(rgb(0,0,1,alpha=0.4), rgb(0,0,0.6,alpha=0.7), rgb(0,0,0.2,alpha=1)), pch = 19, cex = 3)
dev.off()

###############################################################################################

## Visualizing convergence lines

x = seq(1:101)
y = numeric(101)
x2 = seq(1:51)
y2 = numeric(51)

# g1

for (j in seq(0,1,0.2)) {
  label = paste(as.character(j), "linesg1.png") 
  png(label, width = 900, height = 600)
 plot(x, y, log = "x", col="white", ylim=c(-5,0.5), ylab = "", xlab = "",  cex.axis=2.5)
  
for (k in first_points) {
  lines(x, log(g1_BO2[[as.character(j)]][[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()
}


png("KW_linesg1.png", width = 900, height = 600)
plot(x2,y2, col="white", log = "x", ylim = c(-5,0.5), ylab = "", xlab = "",  cex.axis=2.5)
for (k in first_points){
  lines(x2, log(g1_KW2[[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()

# g2

for (j in seq(0,1,0.2)) {
  label = paste(as.character(j), "linesg2.png") 
  png(label, width = 900, height = 600)
plot(x, y, log = "x", col="white", ylim=c(-5,0.5), ylab = "", xlab = "",  cex.axis=2.5)
for (k in first_points) {
  lines(x, log(g2_BO2[[as.character(j)]][[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()
}

png("KW_linesg2.png", width = 900, height = 600)
plot(x2,y2, col="white", log = "x", ylim = c(-5,0.5), ylab = "", xlab = "",  cex.axis=2.5)
for (k in first_points){
  lines(x2, log(g2_KW2[[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()

# g3

for (j in seq(0,1,0.2)) {

  label = paste(as.character(j), "linesg3.png") 
  png(label, width = 900, height = 600)
  plot(x, y, log = "x", col="white", ylim=c(-5,0.5), ylab = "", xlab = "",  cex.axis=2.5)
  
for (k in first_points) {
  lines(x, log(g3_BO2[[as.character(j)]][[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()
}

png("KW_linesg3.png", width = 900, height = 600)
plot(x2,y2, col="white", log = "x", ylim = c(-5,0.5), ylab = "", xlab = "",  cex.axis=2.5)
for (k in first_points){
  lines(x2, log(g3_KW2[[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()

# g4

for (j in seq(0,1,0.2)) {

   label = paste(as.character(j), "linesg4.png") 
  png(label, width = 900, height = 600)
plot(x, y, log = "x", col="white", ylim=c(-5,0.5), ylab = "", xlab = "", cex.axis=2.5)
  
for (k in first_points) {
  lines(x, log(g4_BO2[[as.character(j)]][[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()
}

png("KW_linesg4", width = 900, height = 600)
plot(x2,y2, col="white", log = "x", ylim = c(-5,0.5), ylab = "", xlab = "", cex.axis=2.5)
for (k in first_points){
  lines(x2, log(g4_KW2[[as.character(k)]]), col=rgb(0,0,0,alpha=0.5) , type="l", lwd=2)
  
}
dev.off()


```
