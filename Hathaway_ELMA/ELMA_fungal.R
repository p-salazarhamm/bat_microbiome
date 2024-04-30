library(phyloseq)
library(vegan) 
library(ggplot2)
library(RColorBrewer)
library(dichromat) 
library(dplyr)
library(grid)
library(ggpubr)

plot_bar2 = function (physeq, x = "Sample", y = "Abundance", fill = NULL, title = NULL, 
                      facet_grid = NULL) {
  mdf = psmelt(physeq)
  p = ggplot(mdf, aes_string(x = x, y = y, fill = fill))
  p = p + geom_bar(stat = "identity", position = "stack")
  p = p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  return(p)
}

# OTU table - otus.csv
otu<-read.table("Fungal_otutab.csv",header=TRUE,sep=",",row.names=1)
# Make otu table a matrix
cotus<-as.matrix(otu)
# Taxonomy - taxonomy.csv
taxonomy<-read.table("Fungal_Taxonomy.csv",header=TRUE,sep=",",row.names=1)
ctax<-as.matrix(taxonomy)
# Metadata - meta.csv
metadata<-read.table("Fungal_meta.csv", header=TRUE,sep=",", row.names=1)

# Phyloseq object
otutab<-otu_table(cotus,taxa_are_rows=TRUE)
taxtab<-tax_table(ctax)
samtab<-sample_data(metadata)
physeq<-merge_phyloseq(otutab,taxtab,samtab)

# Transform data into relative abundance values
physeq_rel<- transform_sample_counts(physeq, function(x) x/sum(x))

# Number of OTUs
ntaxa(physeq)
# Number of samples
nsamples(physeq)
# Sample names
sample_names(physeq)
# Taxa
taxa_names(physeq)
# List taxonomy ranks from tax table
rank_names(physeq)
# Metadata variables
sample_variables(physeq)

# Basic statistics on the number of reads in the sample collection
mean(sample_sums(physeq))
min(sample_sums(physeq))
max(sample_sums(physeq))
sd(sample_sums(physeq))

# NMDS ordination plots for beta diversity
# Bray-Curtis dissimilarity metric

## By type
ord<-ordinate(physeq_rel,"NMDS", "bray", try=100)
plot.ord.type<-plot_ordination(physeq_rel, ord, type ="lab", color = "Type", shape = "Type")
plot.ord.type + stat_ellipse() + theme_classic() + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")

## By Cave
plot.ord.cave<-plot_ordination(physeq_rel, ord, type ="lab", color = "Cave", shape = "Cave")
plot.ord.cave + stat_ellipse() + theme_classic()

# Subset for Bat/Mat samples only
physeq_relBM<- subset_samples(physeq,Type!="Air")
ordBM<-ordinate(physeq_relBM,"NMDS", "bray", try=100)
plot.ord.typeBM<-plot_ordination(physeq_relBM, ordBM, type ="lab", color = "Type", shape = "Type")
plot.ord.typeBM + stat_ellipse() + theme_classic() + scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2")

# Subset for Bat samples only by bat species
physeq_relB<-subset_samples(physeq_rel,Bat!="Air")
physeq_relB<-subset_samples(physeq_relB,Bat!="Mat")
ordB<-ordinate(physeq_relB,"NMDS", "bray", try=100)
plot.ord.typeB<-plot_ordination(physeq_relB, ordB, type ="lab", color = "Bat", shape = "Bat")
plot.ord.typeB + stat_ellipse() + theme_classic() + scale_fill_brewer(palette = "Spectral") + scale_color_brewer(palette = "Spectral")

# Abundance by Phylum of samples faceted by cave
plot_bar2(physeq_rel, x = "Sample", fill="Phylum") +
  scale_fill_brewer(palette = "Dark2") + facet_grid(cols=vars(Cave),scales="free_x") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.background = element_blank())

# Abundance by Phylum of samples faceted by Bat species
  plot_bar2(physeq_relB, x = "Sample", fill="Phylum") +
    scale_fill_brewer(palette = "Dark2") + facet_grid(cols=vars(Bat),scales="free_x") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.background = element_blank()) 
  
# Abundance by Phylum of samples faceted by Type
  ## Can change fill="Phylum" to any taxonomic rant
  plot_bar2(physeq_rel, x = "Sample", fill="Phylum") +
    scale_fill_brewer(palette = "Set1") + facet_grid(cols=vars(Type),scales="free_x") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    panel.background = element_blank())
  
  plot_bar2(physeq_rel, x = "Sample", fill="Class") + facet_grid(cols=vars(Type),scales="free_x") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=0.5),
      panel.background = element_blank())

# Box plots to view variation among samples
  phyl<-tax_glom(physeq_rel,taxrank="Phylum")
  gg_Cave<-psmelt(phyl)
  gg_Type<-psmelt(phyl)
  f<-ggplot(gg_Cave,aes(x=Phylum,y=Abundance,fill=Cave)) #colors will be by cave
  g<-ggplot(gg_Type, aes(x=Phylum,y=Abundance,fill=Type)) #colors will be by type
    # By type
  f + geom_boxplot() + facet_grid(cols=vars(Type))
  f + theme_pubr()
    # By cave
  g + geom_boxplot() + facet_grid(cols=vars(Cave)) 
  g + theme_pubr()
