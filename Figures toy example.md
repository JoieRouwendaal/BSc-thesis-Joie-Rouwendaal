```R
blue_gradient <- colorRampPalette(c("lightblue", "darkblue"))

## Quadratic objective function and quadratic model
g1d = data.frame(x = seq(-1,1,0.0001), y = real_g(seq(-1,1,0.0001), t_real))

toy1 = iterative_quadratic_final(first_point, 1, observation_y(first_point, s, t_real), p=1)

  g1 = ggplot(g1d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), toy1$mean[,1])), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=1), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
    geom_point(data = data.frame(x = toy1$X0[,2], y = (numeric(length(toy1$X0[,2]))-1)), aes(x = x, y = y), color = blue_gradient(11)[1:2], size = 4) +
  scale_x_continuous(limits = c(-1,1)) + 
   ylim(-1,1.5) +
  geom_point(aes(x = real_x_qd, y = real_g(real_x_qd, t_real)), color = "red", size = 4) +
  labs(x = "", y = "") +
  theme(
    axis.title = element_text(size = 1),
    axis.text = element_text(size = 20), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA), 
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )
ggsave(filename = "toy1.png", plot = g1, width = 7, height = 7.5)

g1d = data.frame(x = seq(-1,1,0.0001), y = real_g(seq(-1,1,0.0001), t_real))

toy1 = iterative_quadratic_final(first_point, 10, observation_y(first_point, s, t_real), p=1)

  g1 = ggplot(g1d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), toy1$mean[,1])), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=1), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy1$mean, toy1$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
    geom_point(data = data.frame(x = toy1$X0[,2], y = (numeric(length(toy1$X0[,2]))-1)), aes(x = x, y = y), color = blue_gradient(11)[1:2], size = 4) +
  scale_x_continuous(limits = c(-1,1)) + 
   ylim(-1,1.5) +
  geom_point(aes(x = real_x_qd, y = real_g(real_x_qd, t_real)), color = "red", size = 4) +
  labs(x = "", y = "") +
  theme(
    axis.title = element_text(size = 1),
    axis.text = element_text(size = 20), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA), 
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )
ggsave(filename = "toy2.png", plot = g1, width = 7, height = 7.5)

######################################################################################################################

## Quartic function and quadratic model

toy2 = iterative_quadratic_toy2(0.3, 1, observation_y(0.3, s, t_real), 1)

 g2 = ggplot(g2d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), toy2$mean[,1])), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=1), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
  scale_x_continuous(limits = c(-1,1)) + 
  geom_point(aes(x = real_x_qr, y = real_g_quartic(real_x_qr, t_real_q)), color = "red", size = 4) +
   geom_point(data = data.frame(x = toy2$X0[,2], y = (numeric(length(toy2$X0[,2]))-2.3)), aes(x = x, y = y), color = blue_gradient(11), size = 4) +
  labs(x = "", y = "") +
   ylim(-2.5,2)+
  theme(
    #plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 1),
    axis.text = element_text(size = 20), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA), 
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )
ggsave(filename = "toy3.png", plot = g2, width = 7, height = 7.5)

toy2 = iterative_quadratic_toy2(0.3, 1, observation_y(0.3, s, t_real), 1)

 g2 = ggplot(g2d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), toy2$mean[,1])), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=1), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
   geom_line(data = data.frame(x = seq(-1,1,0.05), y = real_g(seq(-1,1,0.05), t(rmvnorm(1, toy2$mean, toy2$var)))), aes(x = x, y = y), color = rgb(0,0,0.6,alpha=0.3), lwd = 1.2) +
  scale_x_continuous(limits = c(-1,1)) + 
  geom_point(aes(x = real_x_qr, y = real_g_quartic(real_x_qr, t_real_q)), color = "red", size = 4) +
   geom_point(data = data.frame(x = toy2$X0[,2], y = (numeric(length(toy2$X0[,2]))-2.3)), aes(x = x, y = y), color = blue_gradient(11), size = 4) +
  labs(x = "", y = "") +
   ylim(-2.5,2)+
  theme(
    #plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 1),
    axis.text = element_text(size = 20), 
    panel.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "white", color = NA), 
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )
ggsave(filename = "toy4.png", plot = g2, width = 7, height = 7.5)




```
