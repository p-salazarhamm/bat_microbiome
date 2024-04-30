library(tidyverse)
library(patchwork)

arnm = map_data("state") %>%
  as_tibble %>%
  filter (region %in% c("arizona", "new mexico"))

data = read_delim("~/batbact/Fig 1 Map metadata.csv", delim=",")

maps = ggplot(arnm, aes(long, lat, group=group)) +
  geom_polygon(colour = "#006699FF", lwd=2, fill = "#D7C3A1") +
  geom_point(data = data, aes(long, lat), pch = 21, size=7, inherit.aes = F, fill = "black") +
  coord_quickmap() +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title=element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,'cm')
  )

ggsave(
  filename = "~/DATA/batbact/map.pdf",
  plot = maps,
  width = 24,
  height = 12,
  unit = "in",
  device = "pdf"
)

pltdata = data %>%
  select(site, per_inhibitory) %>%
  mutate(per_not = 100-per_inhibitory) %>%
  gather(key = "effect", value = "perc", -site)

pal = harrypotter(50,option = "Ravenclaw")
dapies = list()
q=1
for (i in unique(data$site)) {
  this = pltdata %>%
    filter(site == i)
  dapies[[q]] = ggplot(this, aes(x="", y=perc, fill = effect)) +
    geom_bar(width=1, stat="identity", color="white") +
    coord_polar(theta = "y", start=1) +
    scale_fill_manual(values = c("#B35900FF", "#D9802AFF")) +
    theme_bw() +
    ggtitle(i) +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks.length = unit(0,'cm'),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    guides(fill="none")
  q=q+1
}
pltpies = dapies[[1]] + dapies[[2]] + dapies[[3]] + dapies[[4]] + dapies[[5]] + dapies[[6]] + plot_layout(nrow = 2)

ggsave(
  filename = "~/DATA/batbact/map_pies.pdf",
  plot = pltpies,
  width = 11,
  height = 8.5,
  unit = "in",
  device = "pdf"
)
