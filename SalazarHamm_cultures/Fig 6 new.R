library(tidyverse)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

library(patchwork)

setwd("/Users/psalazarhamm/Documents/Bat_WNS_Microbiome/Culture_collection/Figs and SI/")

dat = read_delim("Fig 6 new.csv", delim=',')
heatpal = brewer.pal(n=3, name="RdYlBu")[1:3]

##NEED to switch low to blue and medium to yellow## 
inhibit = ggplot(dat, aes(x=reorder(isolate, inhibit_value), y=inhibit_value, fill=Inhibit_cat)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = heatpal) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y=element_blank(),
    #axis.text.y=element_blank(),
    legend.position="right"
  )

denovo <- ggplot (dat, aes(x=reorder(isolate, inhibit_value), y=num_bats_denovo, fill=num_park_denovo)) +
  scale_color_brewer(palette = 'OrRd') +
  geom_bar(stat = 'identity') +
  coord_flip() +
  scale_y_reverse() +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title.y=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.margin = unit(c(1,-1.5,1,0), "mm"),
    legend.position="left"
  )

denovo + inhibit + plot_layout(ncol=2)
