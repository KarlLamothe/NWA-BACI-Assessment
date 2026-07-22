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
write.csv(List, "Results/Species.counts.csv")

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
fish_wide_CPUE <- cbind.data.frame(Year = fish_wide$Year,
                                   Field.Number = fish_wide$Field.Number,
                                   Cell = fish_wide$Cell,
                                   fish_wide_CPUE)
colnames(fish_wide_CPUE)

# remove Lepomis sp, and hybrids
fish_wide_CPUE2 <- fish_wide_CPUE[-c(9,14,16)]

# remove rows that have zero fish counts after removal of lepomis sp and hybrids
sort(colSums(fish_wide_CPUE2[c(4:ncol(fish_wide_CPUE2))]), decreasing = TRUE)
which(rowSums(fish_wide_CPUE2[c(4:ncol(fish_wide_CPUE2))]) == 0)
fish_wide_CPUE2 <- fish_wide_CPUE2[-c(50,72,77,131,139),]

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
################################################################################
################################################################################
# transform data
# relative abundance
comm <- fish_wide_CPUE2[4:ncol(fish_wide_CPUE2)]
comm_relative <- decostand(comm, method = "total")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Permanova of relative abundance CPUE 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
adonis2(comm_relative ~ Year*Cell, data = fish_wide_CPUE2, method='bray', by='margin')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# calculate multivariate dispersion #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
dist <- vegdist(comm_relative, method = "bray")
mod <- betadisper(dist, group=fish_wide_CPUE2$Year) # multivariate dispersion
mod # significant
# extract distances to centroid
Year_distance <- mod$distance
Year_distance <- cbind.data.frame(Distance=Year_distance, 
                                  Year=fish_wide_CPUE2$Year,
                                  Cell=fish_wide_CPUE2$Cell)
Year_distance$Year <- as.character(Year_distance$Year)

# create boxplot
ggplot(Year_distance, aes(x=interaction(Year,Cell), y=Distance))+
  geom_boxplot()

# calculate difference in dispersion between years
set.seed(876)
permutest(mod, pairwise = T, permutations = 999) # significant

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
sites[sites$PCoA1 > 0.5,]

# significant species
fit <- envfit(sites[, c("PCoA1", "PCoA2")],
              comm_relative,
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
  filter(pval < 0.05)
sig_vec

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

ggplot(sites, aes(PCoA1, PCoA2, colour = Year, fill = Year)) +
  geom_hline(yintercept = 0, linetype='dashed', lwd=0.5)+
  geom_vline(xintercept = 0, linetype='dashed', lwd=0.5)+
  #geom_polygon(data = hulls, aes(group = Year), alpha = 0.2, colour = NA) +
  stat_ellipse(level = 0.9) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  scale_fill_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(size = 2) +
  coord_cartesian() +
  facet_wrap(~ Cell) +
  geom_segment(data = sig_vec, aes(x = 0, y = 0, xend = PCoA1/2, yend = PCoA2/2),
               inherit.aes = FALSE, arrow = arrow(length = unit(0.2, "cm"))) +
  geom_text(data = sig_vec, aes(x = PCoA1/2, y = PCoA2/2, label = Species),
            inherit.aes = FALSE, size = 3)+
  labs(x = xlab, y = ylab)

################################################################################
################################################################################
# PERMANOVA: Does community composition differ with Year, Temp, Cond, or pH?
# distance based RDA: How much of the variation in community composition can be 
#      represented by these predictors?
################################################################################
################################################################################
Site.variables <- cbind.data.frame(
  Field.Number = Site.info$Field.Number,
  Cell = Site.info$Waterbody.Name,
  Year = Site.info$Year,
  Temp = Site.info$Water.Temperature,
  Cond = Site.info$Conductivity,
  pH = Site.info$pH,
  DO = Site.info$Dissolved.Oxygen,
  Turb = Site.info$Turbidity..ntu.,
  Emergent = Site.info$Emergent,
  Submerged = Site.info$Submerged, 
  Floating = Site.info$Floating
)
Site.variables$Cell[Site.variables$Cell=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Site.variables$Cell[Site.variables$Cell=="St. Clair NWA - West Cell SCU"] <- "West Cell"

# remove rows that had zero fish and also a row without a pH measure
Site.variables2 <- Site.variables[-c(50,62,72,77,131,139),]
Site.variables2$Year <- as.character(Site.variables2$Year)

# new fish data frames removing the row without a ph measure and remove rare species
# Perca flavescens, Noturus gyriunus, Carassius auratus, Ameiurus natalis
# Cyprinus carpio
colnames(comm_relative)
fish_fit_rel <- comm_relative[-c(1,5,6,13,14)]
fish_fit_rel <- fish_fit_rel[-c(62),]

# distance based RDA
mod <- dbrda(
  fish_fit_rel ~ Year*Cell + Temp + Cond + DO + Submerged + Emergent + Floating,
  data = Site.variables2,
  distance = "bray"
)

# Is the dbRDA model significant?
anova(mod) #yes
anova(mod, by = "term")
RsquareAdj(mod)
plot(mod)
summary(mod)

site_scores <- scores(mod, display = "sites")
site_scores <- as.data.frame(site_scores)
site_scores$Year <- Site.variables2$Year
site_scores$Temp <- Site.variables2$Temp
site_scores$Cell <- Site.variables2$Cell

env_scores <- as.data.frame(scores(mod, display = "bp"))
env_scores$Variable <- rownames(env_scores)

# plot
ggplot(site_scores, aes(dbRDA1, dbRDA2, colour = Year)) +
  geom_hline(yintercept = 0, linetype='dashed', lwd=0.5) +
  geom_vline(xintercept = 0, linetype='dashed', lwd=0.5) +
  geom_point(size = 2) +
  scale_color_manual(values=c("#134A8E","#E8291C"))+
  stat_ellipse(level = 0.95) +
  geom_segment(data = env_scores, aes(x = 0, y = 0, xend = dbRDA1*3, yend = dbRDA2*3),
               arrow = arrow(length = unit(0.2, "cm")),
               inherit.aes = FALSE) +
  geom_text(data = env_scores, aes(dbRDA1*3, dbRDA2*3, label = Variable), inherit.aes = FALSE) 
#labs(x = "dbRDA Axis 1 (34.5%)", y= "dbRDA Axis 2 (30.0%)")

# Does community composition differ with Year, Temp, Cond, or pH?
adonis2(
  fish_fit_rel ~ Year*Cell + Temp + Cond + DO + Submerged + Emergent + Floating,
  data = Site.variables2,
  method = "bray",
  by='margin'
)

################################################################################
################################################################################
# Chubsucker only
################################################################################
################################################################################
Chubsucker <- Fish.Counts.CPUE[Fish.Counts.CPUE$Species=="Erimyzon sucetta",]
Chubsucker$Year <- as.character(Chubsucker$Year)

ggplot(Chubsucker, aes(x = Year, y = CPUE)) +
  geom_jitter(position = position_jitterdodge(
    jitter.width = 0.2, jitter.height = 0.0,dodge.width = 0.8),  
    size=3, pch=20, alpha=0.5)+
  labs(y="CPUE") +
  facet_wrap(~Cell) +
  theme(legend.position = 'none',
        axis.title.x = element_blank())

aggregate(Chubsucker$Number.Captured, list(Chubsucker$Year, Chubsucker$Cell), sum)
summary(lm(log(CPUE)~Year*Cell, data=Chubsucker))
