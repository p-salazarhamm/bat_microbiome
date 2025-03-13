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


#####Barplots#####
site_bar = tibble(site = c("BLM", 
                           "PARA", 
                           "ELMA", 
                           "FS", 
                           "CAVE", 
                           "SEAZ"), 
                  site_num = c(8, 8, 29, 20, 23, 9))
bat_bar = tibble(bat = c("Parastrellus hesperus",
                         "Myotis californicus",
                         "Corynorhinus townsendii",
                         "Myotis velifer",
                         "Myotis ciliolabrum",
                         "Eptesicus fuscus",
                         "Myotis thysanodes", 
                         "Tadarida brasiliensis", 
                         "Antrozous pallidus",
                         "Lasionycteris noctivagans",
                         "Myotis evotis",
                         "Myotis volans"),
                 bat_num = c(2, 2, 13, 17, 14, 8, 15, 10, 3, 4, 7, 2))
inhibit_bar = tibble(cat = c("high", "medium", "low"), cat_num = c(17, 25, 55))

bat_barplot <- ggplot(bat_bar, aes(x=bat, y=bat_num), fill=bat) +
  geom_bar(stat = 'identity') +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y=element_blank(),
    legend.position="right"
  )

site_barplot <- ggplot(site_bar, aes(x=site, y=site_num), fill=site) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = sitepal) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y=element_blank(),
    legend.position="right"
  )

cat_barplot <- ggplot(inhibit_bar, aes(x=reorder(cat,cat_num), y=cat_num), fill=cat) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = sitepal) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    #axis.ticks.y=element_blank(),
    legend.position="right"
  )


