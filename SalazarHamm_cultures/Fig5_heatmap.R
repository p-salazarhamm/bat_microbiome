library(tidyverse)
library(readxl)
library(harrypotter)

setwd("./SalazarHamm_cultures/")
hm = read.table("Fig 5 ecoregion_sp_metadata.csv", sep =",", skip=1, header=TRUE)

# Inhibitors
hm_inhib = hm[1:12,]
hm_inhib = hm_inhib[,-1]
hm_inhib = apply(hm_inhib, 2, as.numeric)
rownames(hm_inhib) = hm[1:12,1]

# Non-inhibs
hm_not = hm[17:28,]
hm_not = hm_not[,-1]
hm_not = apply(hm_not, 2, as.numeric)
rownames(hm_not) = hm[17:28,1]

# Sum the inhibs and non-inhibs and divide the inhibitor number by that sum
totals=hm_inhib+hm_not
hm_cells = hm_inhib/totals

inhib_count_long = as_tibble(hm_inhib, rownames="Species") %>%
  pivot_longer(-c("Species"), names_to="Ecoregion", values_to="count")
props_long = as_tibble(hm_cells, rownames="Species") %>%
  pivot_longer(-c("Species"), names_to="Ecoregion", values_to="proportion")

# Plot
plthm = inhib_count_long %>%
  left_join(props_long, by = c("Species", "Ecoregion")) %>%
  mutate(perc = proportion*100)

# Scale
log2_breaks = function(limits) {
  c(0, 2^(seq(0, ceiling(log2(limits[2])))))
}

b=(log2_breaks(c(0,16))/16)*100

hpp = harrypotter::hp(100, house='Ravenclaw')
plthm$percbin = cut(plthm$perc, breaks=b, include.lowest=TRUE)

names(pal) = levels(plthm$percbin)

# Visualize
hm_fig = ggplot(plthm, aes(x=Species, y=Ecoregion, fill = percbin)) +
  geom_tile(color="white") +
  scale_fill_manual(values=pal, na.value="grey88") + 
  geom_text(data = subset(plthm, !is.na(perc)), aes(label = paste(count, "\n", format(round(perc,1), nsmall=1), "%", sep="")), size =3) +
  theme(aspect.ratio = 1.0,
        panel.grid = element_blank(),
        panel.background = element_blank())

pdf("./Fig 5 heatmap_propheat.pdf", width=11, height=8.5)
print(hm_fig)
dev.off()
