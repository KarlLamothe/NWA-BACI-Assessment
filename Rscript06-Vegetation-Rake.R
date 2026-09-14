# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)
colnames(Site.info)

Rake.data.full <- read.csv("Data/Rake_data_full.csv", header=T)
colnames(Rake.data.full)
str(Rake.data.full)
Rake.data.full$Year <- as.character(Rake.data.full$Year)

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

# species richness (note that this includes genera and some unknonw)
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


# count number of sites for each taxa not identified to species
###########################
# 2023 East
###########################
Rake.data.full.rev[Rake.data.full.rev$Species=="Chara sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Algae" &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Lemna sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Najas sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Nymphaea sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Utricularia sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "East",]
###########################
################# 2023 West
###########################
Rake.data.full.rev[Rake.data.full.rev$Species=="Lemna sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Nymphaea sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Utricularia sp." &
                     Rake.data.full.rev$Year == "2023" &
                     Rake.data.full.rev$Cell == "West",]
###########################
# 2024 East
###########################
Rake.data.full.rev[Rake.data.full.rev$Species=="Algae" &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Juncus sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Lemna sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Myriophyllum sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Najas sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Nitella sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Nymphaea sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Ranunculus sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Sagittaria sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Sparganium sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Typha sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Wolffia sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "East",]

###########################
# 2024 West
###########################
Rake.data.full.rev[Rake.data.full.rev$Species=="Algae" &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Lemna sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Myriophyllum sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Nitella sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Nymphaea sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Potamogeton sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Ranunculus sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Sagittaria sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Sparganium sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Schoenplectus sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Typha sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Utricularia sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

Rake.data.full.rev[Rake.data.full.rev$Species=="Wolffia sp." &
                     Rake.data.full.rev$Year == "2024" &
                     Rake.data.full.rev$Cell == "West",]

