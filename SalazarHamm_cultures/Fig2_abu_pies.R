library(tidyverse)
library(readxl)

pie_data = read_xlsx("~/batbact/Fig 2 pie chart meta.xlsx")
pie1 = pie_data[1:7,]
pie2 = pie_data[9:dim(pie_data)[1],]


pie1_plt = ggplot(pie1, aes(x="", y=percent, fill  = taxa)) +
  geom_bar(width=1, stat="identity", color="white") +
  coord_polar(theta = "y", start=3) +
  scale_fill_hp_d("Ravenclaw") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks.length = unit(0,'cm'),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    ) #+
  #guides(fill="none")
pie1_plt

ggsave(
  filename = "~/batbact/pie1.pdf",
  plot = pie1_plt,
  width = 11,
  height = 8.5,
  unit = "in",
  device = "pdf"
)

pie2_plt = ggplot(pie2, aes(x="", y=percent, fill  = taxa)) +
  geom_bar(width=1, stat="identity", color="white") +
  coord_polar(theta = "y", start=0) +
  scale_fill_hp_d("Ravenclaw") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks.length = unit(0,'cm'),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) #+
#guides(fill="none")
pie2_plt

ggsave(
  filename = "~/batbact/pie2.pdf",
  plot = pie2_plt,
  width = 11,
  height = 8.5,
  unit = "in",
  device = "pdf"
)
