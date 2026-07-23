# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Fish <- read.csv("Data/Fish-captures.csv", header=T)
Site.info <- read.csv("Data/Site-information.csv", header=T)

# summarize effort
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), mean)
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), sd)
aggregate(Site.info$Effort, list(Site.info$Year, Site.info$Waterbody.Name), length)

# clean fish data
colnames(Fish)
Fish <- Fish[c(2,4,5,8)]
Fish <- merge(Fish, Site.info, "Field.Number")
Fish <- Fish[c(1:4,13)]
colnames(Fish) <- c("Field.Number", "Number.Captured", "Species", "Year", "Cell")

# summary
unique(Fish$Species)
sum(Fish$Number.Captured)
aggregate(Fish$Number.Captured, list(Fish$Year, Fish$Cell), sum)

1168+1097
1230+546

# number of fish captured per species per year
List<-aggregate(Fish$Number.Captured, list(Fish$Species, Fish$Year, Fish$Cell), sum)
sum(List$x)
#write.csv(List, "Results/Species.counts.csv")

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
fish_wide$Cell[fish_wide$Cell=="St. Clair NWA - East Cell SCU"] <- "East Cell"
fish_wide$Cell[fish_wide$Cell=="St. Clair NWA - West Cell SCU"] <- "West Cell"

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

aggregate(Richness$Richness, list(Richness$YearCell), sd)
aggregate(Richness$Richness, list(Richness$YearCell), mean)

### Look at species richness estimators and accumulation curves
colnames(fish_wide)
specpool(fish_wide[c(4:8,10:13,15,17:22)], fish_wide$YearCell)

east23 <- fish_wide[fish_wide$YearCell == "2023-East Cell", ]
east24 <- fish_wide[fish_wide$YearCell == "2024-East Cell", ]
west23 <- fish_wide[fish_wide$YearCell == "2023-West Cell", ]
west24 <- fish_wide[fish_wide$YearCell == "2024-West Cell", ]
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
accum_df <- bind_rows(data.frame(
  Sites = sa_e23$sites, 
  Richness = sa_e23$richness,
  SD = sa_e23$sd,
  YearCell = "2023 East"),
  data.frame(
    Sites = sa_e24$sites,
    Richness = sa_e24$richness,
    SD = sa_e24$sd,
    YearCell = "2024 East"),
  data.frame(
    Sites = sa_w23$sites,
    Richness = sa_w23$richness,
    SD = sa_w23$sd,
    YearCell = "2023 West"),
  data.frame(
    Sites = sa_w24$sites,
    Richness = sa_w24$richness,
    SD = sa_w24$sd,
    YearCell = "2024 West")
)

accum_df <- accum_df %>%
  mutate(
    Lower = Richness - SD,
    Upper = Richness + SD
  )

accum_df$Year <- c(rep("2023",35),rep("2024",40),rep("2023",36),rep("2024",40))
accum_df$Cell <- c(rep("East Cell",75),rep("West Cell",76))

accum.plotgg<-ggplot(accum_df, aes(x = Sites, y = Richness, colour = Year, fill = Year)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, colour = NA) +
  geom_line(lwd = 0.5) +
  facet_wrap(~Cell)+
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  scale_fill_manual(values=c("#134A8E", "#E8291C"))+
  labs(x = "Number of Sites", y = "Accumulated Species Richness", 
       colour = "Year", fill = "Year")+
  ylim(0,15)+
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.4),
        legend.background = element_blank())

#png("Results/Figures/spec.accum.gg.png", width=6, height=2.5, units='in', res=800)
accum.plotgg
#dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Permanova of relative abundance CPUE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
comm <- fish_wide_CPUE2[4:(ncol(fish_wide_CPUE2)-1)]
adonis2(comm ~ YearCell, data = fish_wide_CPUE2, method='bray', by='margin')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# calculate multivariate dispersion #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dist <- vegdist(comm, method = "bray")
mod <- betadisper(dist, group=fish_wide_CPUE2$YearCell) # multivariate dispersion

# calculate difference in dispersion between years
set.seed(876)
permutest(mod, pairwise = T, permutations = 999) 

# extract distances to centroid
Year_distance <- mod$distance
Year_distance <- cbind.data.frame(Distance=Year_distance, 
                                  Year_Cell=fish_wide_CPUE2$YearCell)

# create boxplot
ggplot(Year_distance, aes(x=Year_Cell, y=Distance))+
  geom_boxplot()

Year_distance %>%
  group_by(Year_Cell) %>%
  summarise(
    MeanDistance = mean(Distance),
    SDDistance = sd(Distance),
    n = n()
  )

# Eigenvalues
eig <- mod$eig

# % variance explained
var_explained <- eig / sum(eig[eig > 0]) * 100
var_explained[1:2]

# Site coordinates (samples)
sites <- as.data.frame(scores(mod, display = "sites"))
sites$Year <- as.character(fish_wide_CPUE2$Year)
sites$Cell <- fish_wide_CPUE2$Cell
sites$Field.Number <- fish_wide_CPUE2$Field.Number

# significant species
fit <- envfit(sites[, c("PCoA1", "PCoA2")],
              comm,
              permutations = 999)

species_fit <- as.data.frame(scores(fit, display = "vectors"))
species_fit$Species <- rownames(species_fit)
species_fit$r2 <- fit$vectors$r
species_fit$pval <- fit$vectors$pvals

species_fit %>%
  arrange(desc(r2))

# create vector for plotting
vec <- as.data.frame(scores(fit, display = "vectors"))
vec$Species <- rownames(vec)
vec$pval <- fit$vectors$pvals
vec$r2 <- fit$vectors$r

vec %>%
  arrange(desc(r2)) %>%
  head(20)

sig_vec <- vec %>%
  filter(pval < 0.01)
sig_vec

centroids <- sites %>%
  group_by(Cell, Year) %>%
  summarise(
    PCoA1 = mean(PCoA1),
    PCoA2 = mean(PCoA2),
    .groups = "drop"
  )

# Create hull points for each Cell × Year combination
hulls <- sites %>%
  group_by(Cell, Year) %>%
  slice(chull(PCoA1, PCoA2))

xlab <- paste0("PCoA1 (", round(var_explained[1], 1), "%)")
ylab <- paste0("PCoA2 (", round(var_explained[2], 1), "%)")

ggplot(sites, aes(PCoA1, PCoA2, colour = Year, fill = Year)) +
  geom_hline(yintercept = 0, linetype='dashed', lwd=0.5)+
  geom_vline(xintercept = 0, linetype='dashed', lwd=0.5)+
  #geom_polygon(data = hulls, aes(group = Year), alpha = 0.2, colour = NA) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  scale_fill_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(size = 2) +
  coord_cartesian() +
  #facet_wrap(~ Cell) +
  geom_segment(data = sig_vec, aes(x = 0, y = 0, xend = PCoA1/2, yend = PCoA2/2),
               inherit.aes = FALSE, arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = sig_vec, aes(x = PCoA1/2, y = PCoA2/2, label = Species),
            inherit.aes = FALSE, size = 3)+
  labs(x = xlab, y = ylab)

fish.ord.gg<-ggplot(sites, aes(PCoA1, PCoA2, colour = Year, fill = Year)) +
  geom_hline(yintercept = 0, linetype='dashed', lwd=0.5)+
  geom_vline(xintercept = 0, linetype='dashed', lwd=0.5)+
  stat_ellipse(aes(group = Year), geom = "polygon", alpha=0.2, level = 0.9) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  scale_fill_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(size = 1) +
  coord_cartesian() +
  facet_wrap(~ Cell) +
  geom_point(data = centroids, shape = 4, size = 3, stroke = 1.5)+
  #geom_text(data = sig_vec, aes(x = PCoA1/2, y = PCoA2/2, label = Species),
  #          inherit.aes = FALSE, size = 3, fontface='italic')+
  #geom_segment(data = sig_vec, aes(x = 0, y = 0, xend = PCoA1/2, yend = PCoA2/2),
  #             inherit.aes = FALSE) +
  labs(x = xlab, y = ylab)

#png("Results/Figures/Fish.Ordination.png", width=7, height=3, units='in', res=800)
fish.ord.gg
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
LCS.mod<-lm(log(CPUE)~Year*Cell, data=Chubsucker)
summary(LCS.mod)
emmeans(LCS.mod, pairwise ~ Year*Cell)

################################################################################
library(grateful)
cite_packages(out.format = "docx", out.dir = ".", citation.style = 'wetlands-ecology-and-management')
