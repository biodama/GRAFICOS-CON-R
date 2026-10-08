

temazo_verano <- theme_cowplot()+ 
  theme(
    plot.title = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4)
  )

temazo_otono <- theme_cowplot()+ 
  theme(
    plot.title = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 15),
  )