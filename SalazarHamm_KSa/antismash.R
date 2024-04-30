library(ggplot2)
library(ggthemes)
library(tidyverse)
library(viridis)

# Points at my working folder.
setwd("~/Documents/cave_bacteria_genomes/")

# This sets a global theme for all my plots. 
theme_set(theme_bw() +
            theme(
              plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.background = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
            ))

# Read in the long formatted antismash data
sec_meta_data <- read_delim("antismash_raw_data_2021.txt", "\t", na = c('.','-999',''))
sec_meta_data

# The putative clusters aren't informative so we remove them for plotting.
sec_meta_plot_data <- sec_meta_data %>% filter(Type != "Cf_putative") %>% 
  add_column(gene_count = 1) %>% select(id, Type, gene_count) %>% 
  pivot_wider(names_from = Type, values_from = gene_count, values_fn = sum,
              values_fill = 0)

sec_meta_plot_data  

# Plotting the data by secondary metabolite type
sec_meta_plot_data %>% pivot_longer(!id, names_to = "gene_type", values_to = "count") %>% 
  ggplot(., aes(x=id, y=gene_type, size = count, color = count)) + geom_point() +
  scale_colour_viridis(discrete = FALSE)

sec_meta_data %>% ggplot(., aes(x=id, y=Similarity, color = Similarity)) + geom_point() +
  scale_colour_viridis(discrete = FALSE)
