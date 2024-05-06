library(tidyverse)
library(readxl)

hm = read_delim("~batbact/Fig 5 ecoregion_sp_metadata.csv", delim =",")
full_names = hm[1,][-1]
hm = hm[-1,]
hm = cbind(hm, rep(0,12), rep(0,12))

totals = read_xlsx("~/batbact/meta.xlsx") %>%
  group_by(Ecoregion, Species) %>%
  filter(Inhibitor != "nd") %>%
  summarise(total = n())
#write_delim(file = "~/batbact/tested_sp_ecoregion.tsv", x = totals, delim="\t", quote=NULL)

colnames(hm) = c("Species", as.character(t(full_names)[,1]), "Aubrey Montane Conifer Forest", "Eastern Mojave Basin")
plthm = hm %>%
  gather(key = "Ecoregion", value = "count", -Species) %>%
  mutate(count = as.numeric(count)) %>%
  full_join(totals) %>%
  mutate(proportion = count/total) %>%
  mutate(count = ifelse(is.na(total), NA, count))

plthm
hm_fig = ggplot(plthm, aes(x=Species, y=Ecoregion, fill = count)) +
  geom_tile(color="white") +
  scale_fill_hp("Ravenclaw") +
  geom_text(data = subset(plthm, !is.na(proportion)), aes(label = paste(count, "\n", format(round(proportion*100,1), nsmall=1), "%", sep="")), size =3) +
  theme(aspect.ratio = 1.0,
        panel.grid = element_blank(),
        panel.background = element_blank())
hm_fig
ggsave(
  filename = "~/batbact/heatmap.pdf",
  plot = hm_fig,
  width = 11,
  height = 8.5,
  unit = "in",
  device = "pdf"
)
