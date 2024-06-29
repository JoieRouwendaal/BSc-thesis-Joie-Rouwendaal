```R
g1d = data.frame(x = seq(-1,1,0.0001), y = real_g(seq(-1,1,0.0001), t_real))

g1 = ggplot(g1d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
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
ggsave(filename = "g1_pwp.png", plot = g1, width = 10, height = 7.5)

g2d = data.frame(x = seq(-1,1,0.001), y = real_g_quartic(seq(-1,1,0.001), t_real_q))

g2 = ggplot(g2d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
  scale_x_continuous(limits = c(-1,1)) + 
  geom_point(aes(x = real_x_qr, y = real_g_quartic(real_x_qr, t_real_q)), color = "red", size = 4) +
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
ggsave(filename = "g2_pwp.png", plot = g2, width = 10, height = 7.5)

g3d = data.frame(x = seq(-1,1,0.0001), y = real_g_triangle(seq(-1,1,0.0001), t_real_triangle))

g3 = ggplot(g3d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
  scale_x_continuous(limits = c(-1,1)) + 
  geom_point(aes(x = real_x_triangle, y = real_g_triangle(real_x_triangle, t_real_triangle)), color = "red", size = 4) +
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
ggsave(filename = "g3_pwp.png", plot = g3, width = 10, height = 7.5)

g4d = data.frame(x = seq(-1,1,0.0001), y = real_g_sin(seq(-1,1,0.0001), t_real_sin))

g4 = ggplot(g4d, aes(x = x, y = y)) + 
  geom_line(size = 1.2, color = "black") +
  scale_x_continuous(limits = c(-1,1)) + 
  geom_point(aes(x = real_x_sin, y = real_g_sin(real_x_sin, t_real_sin)), color = "red", size = 4) +
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
ggsave(filename = "g4_pwp.png", plot = g4, width = 10, height = 7.5)


```
