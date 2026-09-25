# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)
colnames(Site.info)

Rake.data.full <- read.csv("Data/2023-2024-LCS-NWA_VegetationData (2026-09-22).csv", header=T)
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

ggplot(Rake.data.full, aes(x=Common_Name, y=Volume_mL, color=Year))+
  geom_point(size=2, pch=20) +
  coord_flip() +
  facet_wrap(Year~Cell, scales="free_y") +
  scale_color_manual(values=c("#134A8E", "#E8291C"))

## removing no vegetation and unknown submerged from data set
#Rake.data.full[Rake.data.full$Species=="No Vegetation",]
Rake.data.full[Rake.data.full$Species=="Unknown Submerged",]

#Rake.data.full.rev <- Rake.data.full[!Rake.data.full$Species == "No Vegetation",]
Rake.data.full.rev <- Rake.data.full[!Rake.data.full$Species == "Unknown Submerged",]

# unique species per year per species
unique_species <- Rake.data.full.rev %>%
  distinct(Year, Cell, Common_Name)
unique_species

# species richness (note that this includes genera and some unknown)
species_richness <- Rake.data.full.rev %>%
  group_by(Cell, Year) %>%
  summarise(
    n_species = n_distinct(Common_Name),
    .groups = "drop"
  )
species_richness

sort(unique(Rake.data.full.rev$Species))

# grams per species per year and cell
species.g <- Rake.data.full.rev %>%
  group_by(Cell, Year, Common_Name) %>%
  summarise(
    weight = sum(Weight_g),
    volume = sum(Volume_mL),
    .groups = "drop"
  )
species.g

################################################################################
################################################################################
# convert vegetation composition data to presence absence
# Includes both rake and observed veg
head(Rake.data.full.rev)
colnames(Rake.data.full.rev)
str(Rake.data.full.rev$Present.at.Site)

Rake.pres.wide <- Rake.data.full.rev %>%
  select(Field.Number, Year, Cell, Common_Name, Present.at.Site) %>%
  group_by(Field.Number, Year, Cell, Common_Name) %>%
  summarise(
    pres = max(Present.at.Site, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Common_Name,
    values_from = pres,
    values_fill = 0
  )

species.table <- Rake.data.full.rev %>%
  group_by(Common_Name, Year, Cell) %>%
  summarise(pres = max(Present.at.Site, na.rm = TRUE),
            .groups = "drop") %>%
  unite("Year_Cell",Year,Cell,sep = "_") %>%
  pivot_wider(
    names_from = Year_Cell,
    values_from = pres,
    values_fill = 0) %>%
  arrange(Common_Name)

species.table
#write.csv(species.table, "Results/species.table.presabs.csv")

#########################################
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="West cell" & Rake.pres.wide$Year=="2023"])
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="West cell" & Rake.pres.wide$Year=="2024"])
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="East cell" & Rake.pres.wide$Year=="2023"])
length(Rake.pres.wide$Year[Rake.pres.wide$Cell=="East cell" & Rake.pres.wide$Year=="2024"])

###########################################
# combine Bladderwort species, humped, common
# remove Pondweed species.
# combine Stonewort species, starry
# combine Water nymph species, slender naiad, britte naiad
# combine Milfoil species, northern water, Eurasian water,
# combine Duckweed species, lesser, star

sort(colnames(Rake.pres.wide))
Rake.pres.analysis <- Rake.pres.wide %>%
  mutate(Bladderwort = pmax(`Bladderwort sp.`, `Humped bladderwort`, `Common bladderwort`, na.rm = TRUE)) %>%
  select(-`Bladderwort sp.`, -`Humped bladderwort`, -`Common bladderwort`) %>%
  mutate(Stonewort = pmax(`Stonewort sp.`, `Starry stonewort`,na.rm = TRUE)) %>%
  select(-`Stonewort sp.`,-`Starry stonewort`) %>%
  mutate(Nymph = pmax(`Naiad sp.`,`Slender naiad/ water nymph`,`Brittle naiad/Brittle water nymph`,na.rm = TRUE)) %>%
  select(-`Naiad sp.`,-`Slender naiad/ water nymph`,-`Brittle naiad/Brittle water nymph`) %>%
  mutate(Milfoil = pmax(`Milfoil sp.`,`Northern water milfoil`,`Eurasian water milfoil`,na.rm = TRUE)) %>%
  select(-`Milfoil sp.`,-`Northern water milfoil`,-`Eurasian water milfoil`) %>%
  mutate(Duckweed = pmax(`Duckweed sp.`,`Star duckweed`,`Lesser duckweed`, na.rm = TRUE)) %>%
  select(-`Duckweed sp.`,-`Star duckweed`,-`Lesser duckweed`) %>%
  select(-`Pondweed sp.`)

################################################################################
veg.cols <- setdiff(
  names(Rake.pres.analysis),
  c("Field.Number", "Year", "Cell"))

veg <- Rake.pres.analysis[, veg.cols]

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

# plot
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
  #geom_path(data = centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, group = Cell),
  #          lwd = 0.5, arrow = arrow(length = unit(0.20, "cm"), type = "closed")) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(data = centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, shape = Year),
             size = 2) +
  #annotate("text", label="Stress = 0.16", x = 0.6, y = 1.5) +
  coord_fixed(ratio=1)+
  labs(x = "NMDS1", y = "NMDS2", colour = "Cell", shape = "Year", lty = "Year")+
  theme(legend.title = element_blank(),
        legend.position = 'none')
veg.comp.gg

# Figure 3
# note that the veg cover plot comes from Rscript03-Vegetation.R
#png("Results/Figures/Vegation.cover.comp.nmds.png", height=3.25, width=7, units='in',res=800)
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
D <- cbind.data.frame(Distance = disp$distances, 
                      Group = disp$group)

# plot
ggplot(D, aes(x=Group, y=Distance))+
  geom_boxplot()+
  geom_jitter(width=0.1, height=0, alpha=0.25, color="blue")+
  labs(x="Cell - Year Group", y = "Distance to Group Centroid")

################################################################################
################################################################################
# Only rake data
Rake <- cbind.data.frame(
  Field.Number= Rake.data.full.rev$Field.Number,
  In.Rake = Rake.data.full.rev$Present.in.Observation,
  Cell = Rake.data.full.rev$Cell,
  Year = Rake.data.full.rev$Year, 
  Depth = Rake.data.full.rev$Observation.Depth..m.,
  Common_Name = Rake.data.full.rev$Common_Name,
  Weight = Rake.data.full.rev$Weight_g,
  Volume = Rake.data.full.rev$Volume_mL)
Rake <- Rake[Rake$In.Rake >0,]
Rake <- Rake[c(1,3:8)]
colnames(Rake)

# make make individual df for volume and weight
Rake.vol <- Rake[c(1:3,5,7)]
Rake.weight <- Rake[c(1:3,5:6)]
Depth <- Rake[c(1:4)]
Depth <- Depth[!duplicated(Depth), ]

# wide versions of volume and weight
Rake.vol.wide <- Rake.vol %>%
  pivot_wider(names_from = Common_Name, values_from = Volume, values_fill = 0)
head(Rake.vol.wide)

Rake.weight.wide <- Rake.weight %>%
  pivot_wider(names_from = Common_Name, values_from = Weight, values_fill = 0)

# merge depth and volume/weight df
Rake.vol.wide <- merge(Rake.vol.wide, Depth, by="Field.Number")
Rake.weight.wide <- merge(Rake.weight.wide, Depth, by="Field.Number")
colnames(Rake.vol.wide)

Rake.vol.wide <- Rake.vol.wide[c(1:32, 35)]
Rake.weight.wide <- Rake.weight.wide[c(1:32, 35)]
colnames(Rake.vol.wide)[2:3] <- c("Cell","Year")
colnames(Rake.weight.wide)[2:3] <- c("Cell","Year")
colnames(Rake.weight.wide)

# Weights
Weights <- Rake.weight.wide[c(4:32)]
Weights$Site.Weight <- rowSums(Weights)
Rake.weight.wide <- cbind.data.frame(Rake.weight.wide, Site.Weight = Weights$Site.Weight)
colnames(Rake.weight.wide)
Rake.weight.wide$Weight_per_m <- Rake.weight.wide$Site.Weight/Rake.weight.wide$Depth
Rake.weight.wide$Year_Cell <- interaction(Rake.weight.wide$Year, 
                                          Rake.weight.wide$Cell,
                                          sep="_")

# Volume
colnames(Rake.vol.wide)
Vols <- Rake.vol.wide[c(4:32)]
Vols$Site.Volume <- rowSums(Vols)
Rake.vol.wide <- cbind.data.frame(Rake.vol.wide, Site.Volume = Vols$Site.Volume)
colnames(Rake.vol.wide)
Rake.vol.wide$Volume_per_m <- Rake.vol.wide$Site.Volume/Rake.vol.wide$Depth
Rake.vol.wide$Year_Cell <- interaction(Rake.vol.wide$Year, 
                                       Rake.vol.wide$Cell,
                                       sep="_")

# look at differences by year-cell-combination
aggregate(Rake.weight.wide$Weight_per_m, 
          list(Rake.weight.wide$Year_Cell), mean, na.rm=T)
aggregate(Rake.vol.wide$Volume_per_m, 
          list(Rake.vol.wide$Year_Cell), mean, na.rm=T)

#######################
# develop linear models
#######################
# weight
Rake.weight.wide2 <- Rake.weight.wide[complete.cases(Rake.weight.wide),]
colnames(Rake.weight.wide2)

# plot
# Means for plotting
Weight.means <- Rake.weight.wide2 %>%
  group_by(Cell, Year) %>%
  summarise(Mean = mean(Weight_per_m, na.rm = TRUE), .groups = "drop")

# Create data for line segments
Weight.lines <- Weight.means %>%
  pivot_wider(
    names_from = Year,
    values_from = Mean)

weightgg<-ggplot(Rake.weight.wide2, aes(x=Cell, y = log(Weight_per_m), color=Year))+
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge(width = 0.8))+
  geom_jitter(aes(color=Year), 
              position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.0,
                                              dodge.width = 0.8), 
              size=2, alpha=0.5, pch=20) +
  geom_segment(data = Weight.lines, aes(x = as.numeric(factor(Cell)) - 0.2,
                                       xend = as.numeric(factor(Cell)) + 0.2,
                                       y = log(`2023`), yend = log(`2024`)),
               inherit.aes = FALSE, colour = "black", linewidth = 0.5) +
  geom_point(data = Weight.means, aes(x = Cell, y = log(Mean), group = Year),
             position = position_dodge(width = 0.8), size = 2, show.legend = FALSE,
             colour='black')+
  scale_color_manual(values=c("#E66100", "#149A37"))+
  labs(x = "Wetland cell", y = "log(Weight / m)") +
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank())

# model
mod.weight <- lm(log(Weight_per_m) ~ Year * Cell, data = Rake.weight.wide2)
summary(mod.weight)
anova(mod.weight)
weight.sim <- simulateResiduals(fittedModel = mod.weight, plot = TRUE)
(emm.weight <- emmeans(mod.weight, ~ Year * Cell))

# Annual change within East and West
pairs(emmeans(mod.weight, ~ Year | Cell))
did.weight <- contrast(emm.weight, interaction = c("pairwise", "pairwise"))
summary(did.weight, infer = TRUE)

# volume
Rake.vol.wide2 <- Rake.vol.wide[complete.cases(Rake.vol.wide),]
Rake.vol.wide3 <- Rake.vol.wide2[Rake.vol.wide2$Volume_per_m > 0,]

# plot
Vol.means <- Rake.vol.wide3 %>%
  group_by(Cell, Year) %>%
  summarise(Mean = mean(Volume_per_m, na.rm = TRUE), .groups = "drop")

# Create data for line segments
Vol.lines <- Vol.means %>%
  pivot_wider(
    names_from = Year,
    values_from = Mean)

volumegg<-ggplot(Rake.vol.wide3, aes(x=Cell, y = log(Volume_per_m), color=Year))+
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge(width = 0.8))+
  geom_jitter(aes(color=Year), 
              position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.0,
                                              dodge.width = 0.8), 
              size=2, alpha=0.5, pch=20) +
  geom_segment(data = Vol.lines, aes(x = as.numeric(factor(Cell)) - 0.2,
                                        xend = as.numeric(factor(Cell)) + 0.2,
                                        y = log(`2023`), yend = log(`2024`)),
               inherit.aes = FALSE, colour = "black", linewidth = 0.5) +
  geom_point(data = Vol.means, aes(x = Cell, y = log(Mean), group = Year),
             position = position_dodge(width = 0.8), size = 2, show.legend = FALSE,
             colour='black')+
  labs(x = "Wetland cell", y = "log(Displacement volume / m)") +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  theme(axis.title.x = element_blank(),
        legend.position = 'right',
        legend.title = element_blank())

#png("Results/Figures/Rake.weight.volume.png", height=2.5, width = 7, units='in', res=800)
weightgg + volumegg
#dev.off()

# model
mod.volume <- lm(log(Volume_per_m) ~ Year * Cell, data = Rake.vol.wide3)
summary(mod.volume)
anova(mod.volume)
volume.sim <- simulateResiduals(fittedModel = mod.volume, plot = TRUE)
emm.volume <- emmeans(mod.volume, ~ Year * Cell)
emm.volume

# Annual change within East and West
pairs(emmeans(mod.volume, ~ Year | Cell))

did.volume <- contrast(emm.volume, interaction = c("pairwise", "pairwise"))
summary(did.volume, infer = TRUE)
