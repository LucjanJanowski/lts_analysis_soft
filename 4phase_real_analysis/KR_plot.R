library(tidyverse)
require(scdensity)
library(tikzDevice)


read_csv('../pyits/log/hdtv.csv')->hdtv
read_csv('../pyits/log/its.csv')->its

tikz("kde.tex", width = 4, height = 3)

pl<-ggplot( data = hdtv,aes(x=psi, y=rho)) +
  geom_density_2d(aes(color = ..level..),show.legend = TRUE, size=1.)+
  geom_point(data=its, aes(x=psi, y=rho,shape="LTS"),size=4,show.legend = TRUE, color="black") +
  labs(color = "HDTV", shape="",    x = "expression(psi)",
       y = "expression(rho)",)+ scale_color_viridis_c(option="A")+
  scale_shape_manual(values = 4) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
pl
# Save the plot as a high-resolution image (e.g., PDF or PNG) for publication
# ggsave("kde.pdf", pl, width = 8, height = 6, dpi = 300)

#theme_bw()
dev.off()
