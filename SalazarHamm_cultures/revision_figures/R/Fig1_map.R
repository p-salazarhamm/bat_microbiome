library(grid)
library(tidyverse)
library(patchwork)
library(harrypotter)
library(ComplexHeatmap)


# %%
get_point_locs_native = function(p) {
  g <- ggplotGrob(p)
  panel_idx <- grep("panel", g$layout$name)

  # Find the panel (where the points live)
  panel_grob <- g$grobs[[panel_idx]]

  # Extract the points grob from the panel children
  # Points are typically stored in a 'points' or 'geom_point' grob
  points_grob <- panel_grob$children[[grep("geom_point|points", names(panel_grob$children))]]

  list(x=points_grob$x, y=points_grob$y)
}



# %%

# %%
################################
# %% Make the mazaps {{{

usa = map_data("state")

az_nm = map_data("state") %>%
  as_tibble() %>%
  filter (region %in% c("arizona", "new mexico"))

data = read_delim("~/dev/bat_microbiome/SalazarHamm_cultures/Fig 1 Map metadata.csv", delim=",")

# %%
inset = ggplot(usa, aes(long, lat, group=group)) +
  geom_polygon(colour = "#000000", lwd=2, fill = "#D7C3A1") +
  geom_polygon(data=az_nm, colour = "#000000", lwd=0, fill = "#B35900", alpha = 1.0) +
  coord_quickmap(expand=TRUE) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill="grey88", linewidth=0.0),
    panel.border = element_rect(color="#000000", linewidth=3),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    axis.title=element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,'cm')
  )

maps = ggplot(az_nm, aes(long, lat, group=group)) +
  geom_polygon(colour = "#000000", lwd=2, fill = "#D7C3A1") +
  geom_point(data = data, aes(long, lat), pch = 21, size=7, inherit.aes = F, fill = "black") +
  coord_quickmap() +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    axis.title=element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0,'cm')
  )

cairo_pdf("~/figures/batbact/map.pdf", width=24, height=12)
print(maps)
dev.off()

# }}}

################################
# %% Make the pie charts {{{
# %%
pltdata = data %>%
  select(site, per_inhibitory) %>%
  mutate(per_not = 100-per_inhibitory) %>%
  gather(key = "effect", value = "perc", -site) %>%
  mutate(site = as.factor(site))

# %%
pal = harrypotter(50, option = "Ravenclaw")
dapies = map(levels(pltdata$site), \(.s) {
  this = pltdata %>%
    filter(site == .s)
  ggplot(this, aes(x="", y=perc, fill = effect)) +
    geom_bar(width=1, stat="identity", color="black", linewidth=0.1) +
    coord_polar(theta = "y", start=1) +
    scale_fill_manual(values = c("#B35900FF", "#D9802AFF")) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks.length = unit(0,'cm'),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    guides(fill="none")
}) %>%
  set_names(levels(pltdata$site))

# }}}

################################
# %% Figure out where to put the pie charts on the main maps
locs = map(get_point_locs_native(maps), as.numeric)
locs = map2(locs$x, locs$y, \(.x, .y) {
       list(x=.x, y=.y)
        }) %>%
  set_names(names(dapies))

# %%
pie_sizes = data %>%
  mutate(pie_size = tested/max(tested))

biggest_pie_width = 0.25
incr = biggest_pie_width/2
# %%
pies_incr = pie_sizes %>%
  mutate(pie_incr = pie_size*incr) %>%
  (\(.df) {
     as.list(pull(.df, pie_incr)) %>%
       set_names(pull(.df, site))
        })

stopifnot(names(pies_incr) == names(locs))
stopifnot(names(pies_incr) == names(dapies))

patch = maps +
  inset_element(inset, left=0.7, bottom=0.7, right=1.0, top=1.0)

for (n in names(locs)) {
  patch = patch +
    inset_element(dapies[[n]], 
                  left=locs[[n]]$x-pies_incr[[n]],
                  bottom=locs[[n]]$y-pies_incr[[n]],
                  right=locs[[n]]$x+pies_incr[[n]],
                  top=locs[[n]]$y+pies_incr[[n]]
                  )
}

################################
# %% Legend creation; incomplete
################################

# lgd_center = 1.0
# lgd_top = 0.9
# lgd_drop_incr=112.5
# lgd_abs_sizes = c(450, 350, 250, 150, 50)
# lgd_sizes = data.frame(abs=lgd_abs_sizes, rel=lgd_abs_sizes/max(data$tested))
# lgd_sizes
#
# pies_incr = biggest_pie_width/2
# for (i in 1:5) {
#   a = lgd_sizes[i,"abs"]
#   r = lgd_sizes[i,"rel"]
#   print(r)
#   vert_center = (lgd_top-(i*0.125))
#   print(r*pies_incr)
#   patch = patch +
#     inset_element(dapies[[1]], 
#                   left=lgd_center-(r*pies_incr), 
#                   bottom=vert_center-(r*pies_incr), 
#                   right=lgd_center+(r*pies_incr), 
#                   top=vert_center+(r*pies_incr)
#     )
# } 


################################
# %% Print map
################################

cairo_pdf("~/figures/batbact/map.pdf", width=24, height=12)
print(patch)
dev.off()

# %%
