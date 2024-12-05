library(tidyverse)
library(harrypotter)
library(patchwork)

dat = t(read_delim("~/batbact/Fig 4 Prop_Ecoregion_metadata.csv", delim=',')[-c(1,5,6,7,8),])
ecos = rownames(dat)[-1]
colnames(dat) = dat[1,]
dat = dat[-1,] %>%
  as_tibble %>%
  mutate(ecoregion = ecos) %>%
  group_by(ecoregion) %>%
  mutate_all(as.numeric) %>%
  ungroup %>%
  rename(inhib = Inhibition, noinhib = `No inhibition`, total = `Count Total`) %>%
  mutate(perc_inhib = inhib/total) %>%
  mutate(perc_noinhib = 1-perc_inhib)
dat

lo = dat %>% arrange(noinhib) %>% pull(ecoregion)

plt_dat = dat %>%
  select(ecoregion, inhib, noinhib, perc_inhib, perc_noinhib) %>%
  mutate(ecoregion = factor(ecoregion, levels = lo))

pal = hp(15, option="Ravenclaw")
inhib = ggplot(plt_dat, aes(x = ecoregion, y = perc_inhib)) +
  geom_bar(stat = "identity", position = "dodge", fill = "blue", color="black") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )
no_inhib = ggplot(plt_dat, aes(x = ecoregion, y = perc_noinhib)) +
  geom_bar(stat = "identity", position = "dodge",fill = "red", color="black") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )

inhib_abs = ggplot(plt_dat, aes(x = ecoregion, y = inhib)) +
  geom_segment(aes(x=ecoregion, xend=ecoregion, y=0, yend=inhib), color = "black", linetype = "dashed") +
  geom_point(pch = 21, size = 4, stat = "identity", position = "dodge", fill = "#CFAF8EFF", color="black") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )
no_inhib_abs = ggplot(plt_dat, aes(x = ecoregion, y = noinhib)) +
  geom_segment(aes(x=ecoregion, xend=ecoregion, y=0, yend=noinhib), color = "black", linetype = "dashed") +
  geom_point(pch = 21, size = 4, stat = "identity", position = "dodge",fill = "#006699FF", color="black") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )
inhib_prob = ggplot(plt_dat, aes(x = ecoregion, y = perc_inhib)) +
  geom_segment(aes(x=ecoregion, xend=ecoregion, y=0, yend=perc_inhib), color = "black", linetype = "dashed") +
  geom_point(pch = 21, size = 4, stat = "identity", position = "dodge", fill = "#CFAF8EFF", color="black") +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )

plt = no_inhib_abs + inhib_abs +plot_layout(nrow=1)
ggsave(
  filename = "~/batbact/dots.pdf",
  plot = plt,
  width = 11,
  height = 8.5,
  unit = "in",
  device = "pdf"
)
