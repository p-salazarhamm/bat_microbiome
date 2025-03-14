library(tidyverse)
library(tublerone)
library(harrypotter)
library(ggtree)
library(ggtreeExtra)
library(phytools)
library(readxl)
library(ggnewscale)
library(RColorBrewer)

tree = read.newick("~/DATA/batbact/batbact_red_iqtee_npb.contree")
tree$tip.label = gsub("[.][0-9]*","",tree$tip.label)

sheet = read_delim("~/DATA/batbact/Strep_backbone.csv", delim = ",")[,-4]
old_to_new = sheet$taxon
names(old_to_new) = sheet$accession
tree = tublerone::phylo_rename_tips(tree, old_to_new)
tree = midpoint.root(tree)

drop = getDescendants(tree, 265)
drop = drop[-which(drop %in% getDescendants(tree, 269))]
drop = drop[drop <= length(tree$tip.label)]
tree = drop.tip(tree, drop, trim.internal = T)
#tree = splitTree(tree, split = list(node = 269, bp = 0.5))

meta = read_xlsx("~/DATA/batbact/Supplemental2_Pdinhibition.xlsx") %>%
  separate(col = "Inhibition Activity", into = c("inhib_cat", "inhib_val"), sep = " ") %>%
  select(-inhib_val, -References, -"16S Accession", -Bacteria) %>%
  rename(label = Isolate)
meta = meta %>%
  mutate(inhib_cat = factor(meta$inhib_cat, levels = c("Low", "Medium", "High")))

meta_long = meta %>%
  rename(ID = label) %>%
  pivot_longer(cols = Site:inhib_cat, names_to ="key", values_to = "value")
ecoreg = meta_long %>% filter(key == "Ecoregion") %>% pull(value) %>% unique
sites = meta_long %>% filter(key == "Site") %>% pull(value) %>% unique
bats = meta_long %>% filter(key == "Bat") %>% pull(value) %>% unique
meta_long = meta_long %>%
  mutate(value = factor(meta_long$value, levels = c(sites[c(6,1,2,3,4,5)], bats[c(2,4,5,7,11,12,1,3,6,8,9,10)]))) %>%
  filter(key %in% c("Site", "Bat"))

heatpal = brewer.pal(n=3, name="RdYlBu")[3:1]
sitepal = colorRampPalette(brewer.pal(n=7, name="YlOrBr"))(6)
batpal = colorRampPalette(brewer.pal(n=7, name="PRGn"))(16)[-c(7,8,9,10)]
#pal = c(heatpal, ecoregpal, batpal)
pal = c(sitepal, batpal)

plttree = ggtree(tree, layout="fan", open.angle = 90)
plttree$data = plttree$data %>%
  mutate(paris = ifelse(isTip & startsWith(label,"AC"), T, F)) %>%
  left_join(meta) %>%
  mutate(bootstrap = as.numeric(label)) %>%
  mutate(bootstrap = ifelse(isTip, 1.0, bootstrap)) %>%
  mutate(bootstrap = ifelse(is.na(bootstrap), 0.0, bootstrap))

plttree = plttree +
  aes(lwd = bootstrap) +
  geom_tiplab2(data = subset(plttree$data, paris == T), aes(color = paris), size = 2.5, align = T, linesize = 0.1, face="bold") +
  geom_tiplab2(data = subset(plttree$data, paris == F), size = 2.0, align = F, linesize = 0.1) +
  geom_point(data = subset(plttree$data, paris == T), aes(x=0.211, y=y, fill = inhib_cat), pch=21, color = "black", size=5, alpha=1.0) +
  scale_fill_manual(values = heatpal) +
  scale_size_continuous(range = c(0.10,0.85), limits = c(0,1)) +
  new_scale_fill() +
  geom_fruit(data = meta_long,
             geom = geom_tile,
             aes(y = ID, x = key, fill = value),
             pwidth = 0.025,
             offset = 0.01,
             color = "black") +
  scale_fill_manual(values = pal) +
  #geom_text(aes(label = node), size = 2.0, color="red") +
  #geom_tiplab(data = subset(plttree$data, paris), aes(color = paris), size = 1.5, align = T) +
  scale_color_manual(values = c("black", "#006699FF"))
plttree
ggsave(
  filename = "~/DATA/batbact/reduced_tree.pdf",
  plot = plttree,
  width = 24,
  height = 24,
  unit = "in",
  device = "pdf"
)

### Nodes to get rid of
tree = read.newick("~/DATA/batbact/combined.tre")
tree = midpoint.root(tree)

dump = read.table("~/DATA/batbact/nodes_to_dump")[,1]

tip_num_to_go = c()
for (d in dump) {
  tip_num_to_go = c(tip_num_to_go, getDescendants(tree, d))
}
tip_num_to_go = tip_num_to_go[tip_num_to_go <= length(tree$tip.label)]
tip_names_to_go = unique(tree$tip.label[tip_num_to_go])

write.table(x = tip_names_to_go, file = "~/DATA/batbact/tips_to_dump.txt",
            quote = F, row.names = F, col.names = F)


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
    axis.ticks.y=element_blank(),
    legend.position="right"
  )

