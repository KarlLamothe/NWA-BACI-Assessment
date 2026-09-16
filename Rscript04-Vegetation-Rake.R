# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)
colnames(Site.info)

Rake.data.full <- read.csv("Data/Rake_data_full_revised_commonnames.csv", header=T)
colnames(Rake.data.full)
str(Rake.data.full)
Rake.data.full$Year <- as.character(Rake.data.full$Year)
Rake.data.full[Rake.data.full$Species=="No Vegetation",]

################################################################################
# Data Summaries
################################################################################
aggregate(Rake.data.full$Volume_mL, list(Rake.data.full$Year,
                                         Rake.data.full$Cell), sum, na.rm=T)
aggregate(Rake.data.full$Weight_g, list(Rake.data.full$Year,
                                         Rake.data.full$Cell), sum, na.rm=T)

ggplot(Rake.data.full, aes(x=Species, y=Volume_mL, color=Year))+
  geom_point(size=2, pch=20) +
  coord_flip() +
  facet_wrap(Year~Cell, scales="free_y") +
  scale_color_manual(values=c("#134A8E", "#E8291C"))

## 
Rake.data.full.rev <- Rake.data.full[!Rake.data.full$Species == "No Vegetation",]
Rake.data.full.rev <- Rake.data.full.rev[!Rake.data.full.rev$Species == "Unknown Submerged",]

# unique species per year per species
unique_species <- Rake.data.full.rev %>%
  distinct(Year, Cell, Species)
unique_species

# species richness (note that this includes genera and some unknown)
species_richness <- Rake.data.full.rev %>%
  group_by(Cell, Year) %>%
  summarise(
    n_species = n_distinct(Species),
    .groups = "drop"
  )
species_richness

unique(Rake.data.full.rev$Species)

#2023 East    
# Ceratophyllum demersum, Potamogeton pusillus, Elodea canadensis, Nymphaea sp.,
# Hydrocharis morsus-ranae, Myriophyllum spicatum, Utricularia sp., Stuckenia pectinatus
# Lemna trisulca, Najas sp., Lemna sp., Algae, Chara sp.

#2023 West
# Utricularia sp., Ceratophyllum demersum,  Lemna sp., Lemna trisulca
# Hydrocharis morsus-ranae, Potamogeton pusillus, Nymphaea sp.
# Elodea canadensis, Nelumbo lutea, Stuckenia pectinatus

#2024 East    
# Ceratophyllum demersum, Elodea canadensis, Hydrocharis morsus-ranae
# Juncus sp., Lemna sp., Nitellopsis obtusa, Nymphaea sp., Pontederia cordata
# Potamogeton pusillus, Utricularia sp., Utricularia vulgaris, Lemna minor
# Spirodela polyrhiza, Stuckenia pectinatus, Riccia fluitans, Typha sp.
# Decodon verticillatus, Lemna trisulca, Potamogeton zosteriformis, Nitella sp
# Ranunculus sp., Myriophyllum sibiricum, Myriophyllum spicatum, Utricularia gibba
# Algae, Najas sp., Najas flexilis, Sagittaria sp., Najas minor, Potamogeton crispus
# Wolffia sp., Potamogeton robbinsii, Sparganium sp., Myriophyllum sp.

#2024 West
# Elodea canadensis, Hydrocharis morsus-ranae, Lemna minor, Myriophyllum spicatum, 
# Nymphaea sp., Spirodela polyrhiza, Wolffia sp., Ceratophyllum demersum, 
# Lemna trisulca, Stuckenia pectinatus ,Potamogeton pusillus, Potamogeton zosteriformis
# Utricularia sp., Nelumbo lutea, Myriophyllum sp., Pontederia cordata
# Sagittaria sp., Sparganium sp., Nitellopsis obtusa, Potamogeton robbinsii
# Utricularia vulgaris, Lemna sp., Utricularia gibba, Algae, Nitella sp.,
# Potamogeton sp., Riccia fluitans, Typha sp., Potamogeton natans
# Myriophyllum sibiricum, Ranunculus sp., Najas minor, Schoenplectus sp.

# grams per species per year and cell
species.g <- Rake.data.full.rev %>%
  group_by(Cell, Year, Species) %>%
  summarise(
    weight = sum(Weight_g),
    volume = sum(Volume_mL),
    .groups = "drop"
  )
species.g

################################################################################
################################################################################
# convert rake data to presence absence and wide
Rake.data.full.rev$pres <- 1
colnames(Rake.data.full.rev)

Rake.pres.wide <- Rake.data.full.rev %>%
  select(Field.Number, Year, Cell, Common.Name, pres) %>%
  group_by(Field.Number, Year, Cell, Common.Name) %>%
  summarise(
    pres = max(pres, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Common.Name,
    values_from = pres,
    values_fill = 0
  )

species.table <- Rake.data.full.rev %>%
  group_by(Common.Name, Year, Cell) %>%
  summarise(pres = max(pres, na.rm = TRUE),
            .groups = "drop") %>%
  unite("Year_Cell",Year,Cell,sep = "_") %>%
  pivot_wider(
    names_from = Year_Cell,
    values_from = pres,
    values_fill = 0) %>%
  arrange(Common.Name)

species.table
#write.csv(species.table, "Results/species.table.presabs.csv")

#########################################
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="West" & Rake.pres.wide$Year=="2023"])
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="West" & Rake.pres.wide$Year=="2024"])
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="East" & Rake.pres.wide$Year=="2023"])
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="East" & Rake.pres.wide$Year=="2024"])

###########################################
# combine Bladderwort species, humped, common
# remove Pondweed species.
# combine Stonewort species, starry
# combine Water nymph species, slender naiad, britte naiad
# combine Milfoil species, northern water, Eurasian water,
# combine Duckweed species, lesser, star

colnames(Rake.pres.wide)
Rake.pres.analysis <- Rake.pres.wide %>%
  # Utricularia
  mutate(Bladderwort = pmax(`Bladderwort sp.`, `Humped bladderwort`, `Common bladderwort`, na.rm = TRUE)) %>%
  select(-`Bladderwort sp.`, -`Humped bladderwort`, -`Common bladderwort`) %>%
  # Nitella
  mutate(Stonewort = pmax(`Stonewort sp.`, `Starry stonewort`,na.rm = TRUE)) %>%
  select(-`Stonewort sp.`,-`Starry stonewort`) %>%
  # Najas
  mutate(Nymph = pmax(`Water nymph sp.`,`Slender naiad`,`Brittle water nymph`,na.rm = TRUE)) %>%
  select(-`Water nymph sp.`,-`Slender naiad`,-`Brittle water nymph`) %>%
  # Myriophyllum
  mutate(Milfoil = pmax(`Milfoil sp.`,`Northern water milfoil`,`Eurasian water milfoil`,na.rm = TRUE)) %>%
  select(-`Milfoil sp.`,-`Northern water milfoil`,-`Eurasian water milfoil`) %>%
  # Lemna
  mutate(Duckweed = pmax(`Duckweed sp.`,`Star duckweed`,`Lesser duckweed`, na.rm = TRUE)) %>%
  select(-`Duckweed sp.`,-`Star duckweed`,-`Lesser duckweed`) %>%
  # Remove unidentified Potamogeton
  select(-`Pondweed sp.`)
colnames(Rake.pres.analysis)

################################################################################
veg.cols <- setdiff(
  names(Rake.pres.analysis),
  c("Field.Number", "Year", "Cell")
)

veg <- Rake.pres.analysis[, veg.cols]
veg[veg>0]<-1 # presence absence

Rake.pres.analysis <- cbind.data.frame(
  Year = Rake.pres.analysis$Year,
  Cell = Rake.pres.analysis$Cell,
  Field.Number = Rake.pres.analysis$Field.Number, 
  veg)

################################################################################
################################################################################
# NMDS
################################################################################
################################################################################
# elbow method to determine an ideal number of dimensions
set.seed(0528)
k_values <- 1:6
nmds_models <- lapply(1:6, function(k) {
  metaMDS(veg, 
          distance = "jaccard", 
          k = k, trymax = 100)})

stress_df <- data.frame(k = 1:6,
                        stress = sapply(nmds_models, function(x) x$stress))
stress_df

ggplot(stress_df, aes(x = k, y = stress)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = stress_df$k) +
  labs(x = "Number of NMDS dimensions (k)", y = "Stress")

# final nmds
veg.nmds <- metaMDS(veg, distance = "jaccard", binary = TRUE, k = 2, trymax = 100)
plot(veg.nmds)
stressplot(veg.nmds)
veg.nmds

# Extract NMDS coordinates
nmds_scores <- as.data.frame(scores(veg.nmds, display = "sites"))

# Add metadata
nmds_scores <- nmds_scores %>%
  mutate(
    Field.Number = Rake.pres.analysis$Field.Number,
    Year = factor(Rake.pres.analysis$Year),
    Cell = factor(Rake.pres.analysis$Cell)
  )

nmds_scores <- nmds_scores %>%
  mutate(Group = interaction(Cell, Year))

# group centroids
centroids <- nmds_scores %>%
  group_by(Cell, Year) %>%
  summarise(
    NMDS1 = mean(NMDS1),
    NMDS2 = mean(NMDS2),
    .groups = "drop"
  )
centroids

# plot
nmds_scores <- nmds_scores %>%
  mutate(Group = interaction(Cell, Year))

# plot nmds
veg.comp.gg<-ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2)) +
  stat_ellipse(aes(colour = Cell, lty = Year, group = Group), lwd = 0.6, alpha = 0.7,
               level=0.95) +
  geom_point(aes(colour = Cell, shape = Year), size = 1, alpha = 0.4) +
  geom_path(data = centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, group = Cell),
            lwd = 0.5, arrow = arrow(length = unit(0.20, "cm"), type = "closed")) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(data = centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, shape = Year),
             size = 2) +
  #annotate("text", label="Stress = 0.16", x = 0.6, y = 1.5) +
  coord_fixed(ratio=1)+
  labs(x = "NMDS1", y = "NMDS2", colour = "Cell", shape = "Year", lty = "Year",
       title="Vegetation community")

# Figure 3
#png("Results/Figures/Vegation.cover.comp.nmds.png", height=3.25, width=6, units='in',res=800)
veg.cover.gg + veg.comp.gg
#dev.off()
################################################################################
################################################################################
# Permanova
################################################################################
################################################################################
table(Rake.pres.analysis$Cell, Rake.pres.analysis$Year)
sort(unique(unlist(veg)))

# jaccard dissimilarity
veg.jac <- vegdist(veg, method = "jaccard", binary = TRUE)

# Permanova
set.seed(0934)
adonis2(veg.jac ~ Cell * Year, data = Rake.pres.analysis, 
        by='terms', permutations = 9999)

# homogeneity of multivariate dispersion
Rake.pres.analysis$Group <- interaction(Rake.pres.analysis$Cell,  
                                        Rake.pres.analysis$Year, sep = "_")

disp <- betadisper(veg.jac, Rake.pres.analysis$Group)
set.seed(123)
permutest(disp, permutations = 9999, pairwise = T)

aggregate(disp$distances, by = list(Group = disp$group), FUN = mean)

boxplot(disp, ylab = "Distance to group centroid", xlab = "Cell × Year", las=1)
