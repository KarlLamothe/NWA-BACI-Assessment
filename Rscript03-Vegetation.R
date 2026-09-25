# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)
colnames(Site.info)

#vegetation
# long data frame for plotting
Veg.data <- cbind.data.frame(
  Measure = c(Site.info$Emergent, Site.info$Submerged,
              Site.info$Floating, Site.info$Open.Water),
  Year = rep(Site.info$Year, 4),
  Cell = rep(Site.info$Waterbody.Name, 4),
  Variable = rep(c("Emergent vegetation","Submerged vegetation",
                   "Floating vegetation","Open water"), 
                 each=length(Site.info$Water.Temperature)))
Veg.data$Year <- as.character(Veg.data$Year)
Veg.data$Cell[Veg.data$Cell=="St. Clair NWA - East Cell SCU"] <- "East cell"
Veg.data$Cell[Veg.data$Cell=="St. Clair NWA - West Cell SCU"] <- "West cell"

aggregate(Veg.data$Measure, list(Veg.data$Variable, Veg.data$Cell, Veg.data$Year), mean)
aggregate(Veg.data$Measure, list(Veg.data$Variable, Veg.data$Cell, Veg.data$Year), sd)

# plot
veg.gg<-ggplot(Veg.data, aes(y=Measure, x=Cell, color=Year))+
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge(width = 0.8)) + 
  geom_jitter(aes(color=Year), 
              position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.0,
                                              dodge.width = 0.8), 
              size=2, alpha=0.5, pch=20) +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  facet_wrap(~Variable)+
  labs(y="Percent cover")+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

# Figure S2
#png('Results/Figures/Veg.boxplots.png',height=3, width=5, units='in', res=800)
veg.gg
#dev.off()

# plot differences in means
ggplot(Veg.data, aes(x = factor(Year), y = Measure, colour = Cell,
                     group = Cell)) +
  stat_summary(fun = mean, geom = "point") +
  ylim(0,100)+
  stat_summary(fun = mean, geom = "line") +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  facet_wrap(~Variable)

# shorter dataframe for testing
colnames(Site.info)
Veg.data.df <- Site.info[33:36]/100
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - East Cell SCU"] <- "East cell"
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - West Cell SCU"] <- "West cell"

################################################################################
################################################################################
# permanova
################################################################################
################################################################################
set.seed(0232)
adonis2(Veg.data.df ~ Year*Waterbody.Name,
        data = Site.info,
        method = "bray",
        by='terms',
        permutations = 9999)

# homogeneity of multivariate dispersion
Site.info$Group <- interaction(Site.info$Waterbody.Name,  
                               Site.info$Year, sep = "_")

d <- vegdist(Veg.data.df, method = "bray")
disp <- betadisper(d, Site.info$Group)
permutest(disp, pairwise = TRUE, permutations = 9999)

# extract distances to centroid
distance.plot2 <- disp$distance
distance.plot2 <- cbind.data.frame(Distance=distance.plot2, 
                                   Year=Site.info$Year,
                                   Cell=Site.info$Waterbody.Name)
distance.plot2$Year <- as.character(distance.plot2$Year)

# create boxplot
ggplot(distance.plot2, aes(x=interaction(Year,Cell), y=Distance))+
  geom_boxplot()

################################################################################
################################################################################
# NMDS
################################################################################
################################################################################
set.seed(3462)
nmds.veg <- metaMDS(Veg.data.df, distance = "bray", 
                    binary = FALSE, k = 2, trymax = 500)
plot(nmds.veg)
stressplot(nmds.veg)
nmds.veg

# this is the outlier thats observed
## 2023-LCS-NWA-140823-006A
Site.info[Site.info$Field.Number=="2023-LCS-NWA-140823-006A",]
Site.info$Emergent

# Extract NMDS coordinates
scores_nmds <- as.data.frame(scores(nmds.veg, display = "sites"))

# Add metadata
scores_nmds <- scores_nmds %>%
  mutate(
    Field.Number = Site.info$Field.Number,
    Year = factor(Site.info$Year),
    Cell = factor(Site.info$Waterbody.Name)
  )

scores_nmds <- scores_nmds %>%
  mutate(Group = interaction(Cell, Year))

# group centroids
nmds_centroids <- scores_nmds %>%
  group_by(Cell, Year) %>%
  summarise(
    NMDS1 = mean(NMDS1),
    NMDS2 = mean(NMDS2),
    .groups = "drop"
  )
nmds_centroids

# plot
scores_nmds <- scores_nmds %>%
  mutate(Group = interaction(Cell, Year))

# part of Figure 3
# plot nmds
veg.cover.gg<-ggplot(scores_nmds, aes(x = NMDS1, y = NMDS2)) +
  stat_ellipse(aes(colour = Cell, lty = Year, group = Group), lwd = 0.6, alpha = 0.7,
               level=0.95) +
  geom_point(aes(colour = Cell, shape = Year), size = 1, alpha = 0.4) +
  #geom_path(data = nmds_centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, group = Cell),
  #          lwd = 0.5, arrow = arrow(length = unit(0.20, "cm"), type = "closed")) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(data = nmds_centroids, aes(x = NMDS1, y = NMDS2, colour = Cell, shape = Year),
             size = 2) +
  #annotate("text", label="Stress = 0.05", x = -1, y = 1.6) +
  coord_fixed(ratio=1)+
  labs(x = "NMDS1", y = "NMDS2", colour = "Cell", shape = "Year", lty = "Year")+
  theme(legend.position='none',
        legend.title=element_blank())
veg.cover.gg

################################################################################
################################################################################
# Linear models
veg.analysis <- data.frame(
  Year = Site.info$Year,
  Cell = Site.info$Waterbody.Name,
  Veg.data.df
)
str(veg.analysis)

# make
veg.analysis$Year <- factor(veg.analysis$Year)
veg.analysis$Cell <- factor(veg.analysis$Cell)

# identify reference levels for model
veg.analysis$Year <- relevel(veg.analysis$Year, ref = "2023")
veg.analysis$Cell <- relevel(veg.analysis$Cell, ref = "West Cell")

# summarize the floating veg data
veg.analysis %>%
  group_by(Cell, Year) %>%
  summarise(
    n = n(),
    mean = mean(Floating),
    sd = sd(Floating),
    median = median(Floating),
    IQR = IQR(Floating),
    .groups = "drop"
  )

# develop linear model
mod.float <- lm(Floating ~ Year * Cell, data = veg.analysis)
summary(mod.float)
anova(mod.float)

Float.sim <- simulateResiduals(fittedModel = mod.float, plot = TRUE)

emm.float <- emmeans(mod.float, ~ Year * Cell)
emm.float

# Annual change within East and West
pairs(emmeans(mod.float, ~ Year | Cell))

did.float <- contrast(emm.float, interaction = c("pairwise", "pairwise"))
summary(did.float, infer = TRUE)

# develop linear model
mod.sub <- lm(Submerged ~ Year * Cell, data = veg.analysis)
summary(mod.sub)
anova(mod.sub)

sub.sim <- simulateResiduals(fittedModel = mod.sub, plot = TRUE)

emm.sub <- emmeans(mod.sub, ~ Year * Cell)
emm.sub

# Annual change within East and West
pairs(emmeans(mod.sub, ~ Year | Cell))

did.sub <- contrast(emm.sub, interaction = c("pairwise", "pairwise"))
summary(did.sub, infer = TRUE)
