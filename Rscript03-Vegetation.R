# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information.csv", header=T)
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
Veg.data$Cell[Veg.data$Cell=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Veg.data$Cell[Veg.data$Cell=="St. Clair NWA - West Cell SCU"] <- "West Cell"

# plot
veg.gg<-ggplot(Veg.data, aes(y=Measure, x=Cell, color=Year))+
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge(width = 0.8)) + 
  geom_jitter(aes(color=Year), 
              position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.0,
                                              dodge.width = 0.8), 
              size=2, alpha=0.5, pch=20) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  facet_wrap(~Variable)+
  labs(y="Percent cover")+
  theme(axis.title.x = element_blank())

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
Veg.data.df <- Site.info[35:38]/100
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - West Cell SCU"] <- "West Cell"

adonis2(Veg.data.df ~ Year*Waterbody.Name,
        data = Site.info,
        method = "bray",
        by='terms',
        permutations = 999)

d <- vegdist(Veg.data.df, method = "bray")
disp <- betadisper(d, Site.info$Year)
anova(disp)
plot(disp, ellipse = TRUE, hull = FALSE) # 1 sd data ellipse

# extract distances to centroid
distance.plot2 <- disp$distance
distance.plot2 <- cbind.data.frame(Distance=distance.plot2, 
                                   Year=Site.info$Year,
                                   Cell=Site.info$Waterbody.Name)
distance.plot2$Year <- as.character(distance.plot2$Year)

# create boxplot
veg.dispersion.gg<-ggplot(distance.plot2, aes(x=interaction(Year,Cell), y=Distance))+
  geom_boxplot()

#png("Results/Figures/Veg.dispersion.png", height=3, width=7, units='in', res=800)
veg.dispersion.gg
#dev.off()

# Site coordinates (samples)
sites <- as.data.frame(scores(disp, display = "sites"))
sites$Year <- as.character(Site.info$Year)
sites$Cell <- Site.info$Waterbody.Name
sites$Field.Number <- Site.info$Field.Number

sites[sites$PCoA2 < (-0.5),]
Site.info[66,]

centroids <- sites %>%
  group_by(Cell, Year) %>%
  summarise(
    PCoA1 = mean(PCoA1),
    PCoA2 = mean(PCoA2),
    .groups = "drop"
  )

# Eigenvalues
eig <- disp$eig
var_explained <- eig / sum(eig[eig > 0]) * 100
var_explained[1:2]

xlab <- paste0("PCoA1 (", round(var_explained[1], 1), "%)")
ylab <- paste0("PCoA2 (", round(var_explained[2], 1), "%)")

veg.ordination<-ggplot(sites, aes(PCoA1, PCoA2, colour = Year, fill = Year)) +
  geom_hline(yintercept = 0, linetype='dashed', lwd=0.5)+
  geom_vline(xintercept = 0, linetype='dashed', lwd=0.5)+
  stat_ellipse(aes(group = Year), geom = "polygon", 
               alpha = 0.2, level = 0.95, lwd=0.5)+
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  scale_fill_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(size = 1) +
  geom_point(data = centroids, shape = 4, size = 3, stroke = 1.5)+
  coord_cartesian() +
  facet_wrap(~ Cell) +
  labs(x = xlab, y = ylab)

#png("Results/Figures/Vegetation.Ordination.png", width=7, height=3, units='in', res=800)
veg.ordination
#dev.off()
