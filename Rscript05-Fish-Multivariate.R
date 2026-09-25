# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Fish <- read.csv("Data/Fish-captures(20260902).csv", header=T)
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)

# summarize effort
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), mean)
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), sd)
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), length)
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), max)
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), min)

# clean fish data
colnames(Fish)
Fish <- Fish[c(2,11,12,5,7)]
colnames(Fish) <- c("Field.Number", "Species", "Number.Captured","Year", "Cell")
head(Fish)
Fish$Cell[Fish$Cell=="St. Clair NWA - East Cell SCU"] <- "East cell"
Fish$Cell[Fish$Cell=="St. Clair NWA - West Cell SCU"] <- "West cell"

# summary
unique(Fish$Species)
sum(Fish$Number.Captured)
aggregate(Fish$Number.Captured, list(Fish$Year, Fish$Cell), sum)
aggregate(Fish$Number.Captured, list(Fish$Cell), sum)

# number of fish captured per species per year
List<-aggregate(Fish$Number.Captured, list(Fish$Species, Fish$Year, Fish$Cell), sum)
sum(List$x)
head(List)
colnames(List) <- c("Species", "Year", "Cell", "Count")

wide_df <- List %>%
  mutate(Column = paste(Year, Cell)) %>%
  select(Species, Column, Count) %>%
  pivot_wider(names_from = Column, values_from = Count, values_fill = 0) %>%
  select(
    Species,
    `2023 East cell`,
    `2024 East cell`,
    `2023 West cell`,
    `2024 West cell`
  )

# Effort data
Effort <- cbind.data.frame(Field.Number = Site.info$Field.Number, 
                           Effort = Site.info$Effort)

#make to wide data frame
fish_wide <- dcast(
  Fish,
  Field.Number + Cell + Year ~ Species,
  value.var = "Number.Captured",
  fun.aggregate = sum
)
head(fish_wide)

#remove no fish captured
colnames(fish_wide)
fish_wide <- merge(fish_wide, Effort, by="Field.Number")
fish_wide <- fish_wide[,-18]

# CPUE
colnames(fish_wide)
fish_wide_CPUE <- fish_wide[,4:22] / fish_wide[,23]
fish_wide$YearCell <- interaction(fish_wide$Year, fish_wide$Cell, sep="-")

# Presence Absence
fish_wide_PA   <- fish_wide_CPUE
fish_wide_PA[fish_wide_PA>0] <-1

# Add columns 
fish_wide_CPUE <- cbind.data.frame(Year = fish_wide$Year,
                                   Field.Number = fish_wide$Field.Number,
                                   Cell = fish_wide$Cell,
                                   fish_wide_CPUE)
colnames(fish_wide_CPUE)

fish_wide_PA   <- cbind.data.frame(Year = fish_wide$Year,
                                   Field.Number = fish_wide$Field.Number,
                                   Cell = fish_wide$Cell,
                                   YearCell = fish_wide$YearCell,
                                   fish_wide_PA)
colnames(fish_wide_PA)

# remove Lepomis sp, and hybrids from CPUE data
fish_wide_CPUE2 <- fish_wide_CPUE[-c(9,14,16)]

# remove rows that have zero fish counts after removal of lepomis sp and hybrids
sort(colSums(fish_wide_CPUE2[c(4:ncol(fish_wide_CPUE2))]), decreasing = TRUE)
which(rowSums(fish_wide_CPUE2[c(4:ncol(fish_wide_CPUE2))]) == 0)
fish_wide_CPUE2 <- fish_wide_CPUE2[-c(50,72,77,131,139),]
fish_wide_CPUE2$YearCell <- interaction(fish_wide_CPUE2$Year,
                                        fish_wide_CPUE2$Cell,
                                        sep = "_")

###################
Fish.Counts.CPUE <- merge(Fish, Effort, "Field.Number")
Fish.Counts.CPUE$CPUE <- Fish.Counts.CPUE$Number.Captured/Fish.Counts.CPUE$Effort

##################
# Combine counts and CPUE into a single table:
# Calculate mean CPUE +/- SD
CPUE_summary <- fish_wide_CPUE %>%
  pivot_longer(cols = -c(Year, Field.Number, Cell),
               names_to = "Species",
               values_to = "CPUE") %>%
  group_by(Species, Year, Cell) %>%
  summarise(
    mean_CPUE = mean(CPUE, na.rm = TRUE),
    SD_CPUE = sd(CPUE, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(CPUE_SD = sprintf("%.2f \u00b1 %.2f", mean_CPUE, SD_CPUE),
         column = paste(Year, Cell)) %>%
  select(Species, column, CPUE_SD) %>%
  pivot_wider(names_from = column,
              values_from = CPUE_SD,
              names_glue = "{column} CPUE")

# Combine Count and CPUE tables
fish_summary <- wide_df %>%
  left_join(CPUE_summary, by = "Species") %>%
  select(
    Species,
    `2023 East cell`,
    `2023 East cell CPUE`,
    `2024 East cell`,
    `2024 East cell CPUE`,
    `2023 West cell`,
    `2023 West cell CPUE`,
    `2024 West cell`,
    `2024 West cell CPUE`)

head(fish_summary)
#write.csv(fish_summary, "Results/Fish.summary.csv", row.names = F)

################################################################################
################################################################################
# Fish = Raw fish counts in long form data frame
# fish_wide = Raw fish counts in wide form data frame
# fish_wide_CPUE = Fish counts / Effort
# fish_wide_CPUE2 = fish_wide_CPUE with hybrids/Lepomis sp removed and resulting
#     rows with sum = 0
# fish_wide_PA = presence absence data, which includes hybrids and Lepomis sp.
################################################################################
################################################################################
# species richness
Richness <- cbind.data.frame(
  Richness = rowSums(fish_wide_PA[5:23]),
  Cell     = fish_wide_PA$Cell,
  Year     = fish_wide_PA$Year,
  YearCell = fish_wide_PA$YearCell
)
Richnessaov <- lm(Richness~YearCell, data=Richness)
summary(Richnessaov)
emmeans(Richnessaov, pairwise ~ YearCell)

#summary
aggregate(Richness$Richness, list(Richness$YearCell), sd)
aggregate(Richness$Richness, list(Richness$YearCell), mean)

# Look at species richness estimators and accumulation curves
colnames(fish_wide)
specpool(fish_wide[c(4:8,10:13,15,17:22)], fish_wide$YearCell)

east23 <- fish_wide[fish_wide$YearCell == "2023-East cell", ]
east24 <- fish_wide[fish_wide$YearCell == "2024-East cell", ]
west23 <- fish_wide[fish_wide$YearCell == "2023-West cell", ]
west24 <- fish_wide[fish_wide$YearCell == "2024-West cell", ]
colnames(east23)

east23 <- east23[c(4:8,10:13,15,17:22)]
east24 <- east24[c(4:8,10:13,15,17:22)]
west23 <- west23[c(4:8,10:13,15,17:22)]
west24 <- west24[c(4:8,10:13,15,17:22)]

sa_e23 <- specaccum(east23, method = "random")
sa_e24 <- specaccum(east24, method = "random")
sa_w23 <- specaccum(west23, method = "random")
sa_w24 <- specaccum(west24, method = "random")

# Convert to dataframe
accum_df <- bind_rows(
  data.frame(Sites = sa_e23$sites,Richness = sa_e23$richness,
             SD = sa_e23$sd,YearCell = "2023 East cell"),
  data.frame(Sites = sa_e24$sites, Richness = sa_e24$richness,
             SD = sa_e24$sd, YearCell = "2024 East cell"),
  data.frame(Sites = sa_w23$sites, Richness = sa_w23$richness,
             SD = sa_w23$sd, YearCell = "2023 West cell"),
  data.frame(Sites = sa_w24$sites, Richness = sa_w24$richness,
             SD = sa_w24$sd, YearCell = "2024 West cell"))

accum_df <- accum_df %>%
  mutate(Lower = Richness - SD, Upper = Richness + SD)

accum_df$Year <- c(rep("2023",35),rep("2024",40),rep("2023",36),rep("2024",40))
accum_df$Cell <- c(rep("East cell",75),rep("West cell",76))

accum.plotgg<-ggplot(accum_df, aes(x = Sites, y = Richness, colour = Year, fill = Year)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, colour = NA) +
  geom_line(lwd = 0.5) +
  facet_wrap(~Cell)+
  scale_color_manual(values=c("#E66100", "#149A37"))+
  scale_fill_manual(values=c("#E66100", "#149A37"))+
  labs(x = "Number of Sites", y = "Accumulated Species Richness", 
       colour = "Year", fill = "Year")+
  ylim(0,15)+
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.4),
        legend.background = element_blank())

# figure 4
#png("Results/Figures/spec.accum.gg.png", width=6, height=2.5, units='in', res=800)
accum.plotgg
#dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Permanova of relative abundance CPUE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
comm <- fish_wide_CPUE2[4:(ncol(fish_wide_CPUE2)-1)]

set.seed(0936)
adonis2(comm ~ Year*Cell, 
        data = fish_wide_CPUE2, 
        method='bray',
        permutations = 9999,
        by='terms')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# calculate multivariate dispersion #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dist <- vegdist(comm, method = "bray")
mod <- betadisper(dist, group=fish_wide_CPUE2$YearCell) # multivariate dispersion
permutest(mod, pairwise = T, permutations = 9999) 

# extract distances to centroid
Year_distance <- mod$distance
Year_distance <- cbind.data.frame(Distance=Year_distance, 
                                  Year_Cell=fish_wide_CPUE2$YearCell)

# create boxplot
ggplot(Year_distance, aes(x=Year_Cell, y=Distance))+
  geom_boxplot()

aggregate(Year_distance$Distance, 
          list(Group = Year_distance$Year_Cell), mean)
aggregate(Year_distance$Distance, 
          list(Group = Year_distance$Year_Cell), sd)

################################################################################
################################################################################
# NMDS
################################################################################
################################################################################
# elbow method to determine an ideal number of dimensions
set.seed(0528)
k_values <- 1:6
nmds_models <- lapply(1:6, function(k) {
  metaMDS(fish_wide_CPUE2[4:(ncol(fish_wide_CPUE2)-1)], 
          distance = "bray", 
          k = k, trymax = 100)})

stress_df <- data.frame(k = 1:6,
                        stress = sapply(nmds_models, function(x) x$stress))
stress_df

ggplot(stress_df, aes(x = k, y = stress)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = stress_df$k) +
  labs(x = "Number of NMDS dimensions (k)", y = "Stress")

# final model
fish.nmds <- metaMDS(fish_wide_CPUE2[4:(ncol(fish_wide_CPUE2)-1)], 
                     distance = "bray", k = 3, trymax = 500)
plot(fish.nmds)
stressplot(fish.nmds)
fish.nmds

# Extract NMDS coordinates
fish_scores <- as.data.frame(
  scores(fish.nmds, display = "sites", choices = 1:3)
)

fish_scores$Cell <- factor(fish_wide_CPUE2$Cell)
fish_scores$Year <- factor(fish_wide_CPUE2$Year)

fish_scores$Group <- interaction(
  fish_scores$Cell,
  fish_scores$Year
)

fish_centroids <- fish_scores %>%
  group_by(Cell, Year) %>%
  summarise(
    NMDS1 = mean(NMDS1),
    NMDS2 = mean(NMDS2),
    NMDS3 = mean(NMDS3),
    .groups = "drop"
  )

# plot nmds
p1<-ggplot(fish_scores, aes(x = NMDS1, y = NMDS2)) +
  stat_ellipse(aes(colour = Cell, lty = Year, group = Group), lwd = 0.6, alpha = 0.7,
               level=0.95) +
  geom_point(aes(colour = Cell, shape = Year), size = 1, alpha = 0.4) +
  #geom_path(data = fish_centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, group = Cell),
  #          lwd = 0.5, arrow = arrow(length = unit(0.20, "cm"), type = "closed")) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(data = fish_centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, shape = Year),
             size = 2) +
  coord_fixed(ratio=1)+
  labs(x = "NMDS1", y = "NMDS2", colour = "Cell", shape = "Year", lty = "Year")+theme(legend.position = "none")

p2<-ggplot(fish_scores, aes(x = NMDS1, y = NMDS3)) +
  stat_ellipse(aes(colour = Cell, lty = Year, group = Group), lwd = 0.6, alpha = 0.7,
               level=0.95) +
  geom_point(aes(colour = Cell, shape = Year), size = 1, alpha = 0.4) +
  #geom_path(data = fish_centroids, aes(x = NMDS1, y = NMDS3, colour = Cell, group = Cell),
  #          lwd = 0.5, arrow = arrow(length = unit(0.20, "cm"), type = "closed")) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(data = fish_centroids, aes(x = NMDS1, y = NMDS3, colour = Cell, shape = Year),
             size = 2) +
  coord_fixed(ratio=1)+
  labs(x = "NMDS1", y = "NMDS3", colour = "Cell", shape = "Year", lty = "Year")+
  theme(legend.position = "none")

p3<-ggplot(fish_scores, aes(x = NMDS2, y = NMDS3)) +
  stat_ellipse(aes(colour = Cell, lty = Year, group = Group), lwd = 0.6, alpha = 0.7,
               level=0.95) +
  geom_point(aes(colour = Cell, shape = Year), size = 1, alpha = 0.4) +
  #geom_path(data = fish_centroids, aes(x = NMDS2, y = NMDS3, colour = Cell, group = Cell),
  #          lwd = 0.5, arrow = arrow(length = unit(0.20, "cm"), type = "closed")) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(data = fish_centroids, aes(x = NMDS2, y = NMDS3, colour = Cell, shape = Year),
             size = 2) +
  coord_fixed(ratio=1)+
  labs(x = "NMDS2", y = "NMDS3", colour = "Cell", shape = "Year", lty = "Year")+
  theme(legend.position = "none")

#png("Results/Figures/fish.nmds.png", height=2.5, width=7, units='in', res=800)
p1 + p2 + p3 + plot_layout(guides = "collect") &
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(0.05, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.5, "cm"))
#dev.off()

veg.cover.gg <- veg.cover.gg +
  scale_x_continuous(breaks = c(-1, 0, 1))

wq.comp.gg <- wq.comp.gg +
  scale_y_continuous(breaks = c(-2, -1, 0, 1))

wq.comp.gg <- wq.comp.gg +
  scale_y_continuous(breaks = c(-1, 0, 1))

veg.comp.gg <- veg.comp.gg +
  scale_y_continuous(breaks = c(-1, 0, 1))

p2 <- p2 +
  scale_y_continuous(breaks = c(-1, 0, 1))

p3 <- p3 +
  scale_y_continuous(breaks = c(-1, 0, 1))

#png("Results/Figures/All.NMDS.png", height=4.5, width=6.5, units='in', res=800)
(wq.comp.gg + veg.cover.gg + veg.comp.gg) / 
  (p1 + p2 + p3) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = "collect", heights=c(1,1), nrow=2) &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(0.05, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.5, "cm"))
#dev.off()

################################################################################
################################################################################
# Look at CPUE variance
################################################################################
################################################################################
species_cols <- c(
  "Ameiurus melas",
  "Ameiurus natalis",
  "Ameiurus nebulosus",
  "Amia ocellicauda",
  "Carassius auratus",
  "Cyprinus carpio",
  "Erimyzon sucetta",
  "Esox lucius",
  "Lepomis gibbosus",
  "Lepomis macrochirus",
  "Micropterus nigricans",
  "Notemigonus crysoleucas",
  "Noturus gyrinus",
  "Perca flavescens",
  "Pomoxis nigromaculatus",
  "Umbra limi"
)
options(scipen=999)
var_df <- fish_wide_CPUE2 %>%
  group_by(YearCell) %>%
  summarise(
    across(
      all_of(species_cols),
      ~ var(.x, na.rm = TRUE),
      .names = "var_{.col}"
    )
  )
t(var_df)

fish_wide_CPUE %>%
  filter(Year == 2024, Cell == "East Cell") %>%
  select(Field.Number, `Notemigonus crysoleucas`) %>%
  arrange(desc(`Notemigonus crysoleucas`))

#################################################################################
#################################################################################
## Chubsucker only
#################################################################################
#################################################################################
Chubsucker <- Fish.Counts.CPUE[Fish.Counts.CPUE$Species=="Erimyzon sucetta",]
Chubsucker$Year <- as.character(Chubsucker$Year)
Chubsucker$Cell[Chubsucker$Cell=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Chubsucker$Cell[Chubsucker$Cell=="St. Clair NWA - West Cell SCU"] <- "West Cell"
Chubsucker$YearCell <- interaction(Chubsucker$Year, Chubsucker$Cell, sep="_")

ggplot(Chubsucker, aes(x = Year, y = CPUE)) +
  geom_jitter(position = position_jitterdodge(
    jitter.width = 0.2, jitter.height = 0.0,dodge.width = 0.8),  
    size=3, pch=20, alpha=0.5)+
  labs(y="CPUE") +
  facet_wrap(~Cell) +
  theme(legend.position = 'none',
        axis.title.x = element_blank())

ggplot(Chubsucker, aes(x=Year, y=Number.Captured, color=Year)) +
  facet_wrap(~Cell, scales='free_y') +
  scale_colour_manual(values=c("#134A8E", "#E8291C"))+
  geom_jitter(size=1, width=0.2, height=0) +
  theme(legend.position = 'none',
        axis.title.x = element_blank())

ggplot(Chubsucker, aes(x=Year, y=CPUE, color=Year)) +
  facet_wrap(~Cell, scales='free_y') +
  scale_colour_manual(values=c("#134A8E", "#E8291C"))+
  geom_jitter(size=1, width=0.2, height=0) +
  theme(legend.position = 'none',
        axis.title.x = element_blank())

aggregate(Chubsucker$Number.Captured, list(Chubsucker$Year, Chubsucker$Cell), sum)

# make factors
Chubsucker$Year <- factor(Chubsucker$Year)
Chubsucker$Cell <- factor(Chubsucker$Cell)

# identify reference levels for model
Chubsucker$Year <- relevel(Chubsucker$Year, ref = "2024")
Chubsucker$Cell <- relevel(Chubsucker$Cell, ref = "East Cell")

LCS.mod<-lm(log(CPUE)~Year*Cell, data=Chubsucker)
summary(LCS.mod)
emmeans(LCS.mod, pairwise ~ Year*Cell)

cite_packages(citation.style = "freshwater-science", out.format = "docx",
              pkgs = "Session", out.dir = getwd())
