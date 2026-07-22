# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information.csv", header=T)
colnames(Site.info)

# Water quality data
Water.data <- cbind.data.frame(
  Measure = c(log(Site.info$Water.Temperature), log(Site.info$Conductivity),
              log(Site.info$Dissolved.Oxygen), log(Site.info$Turbidity..ntu.),
              Site.info$pH),
  Year = rep(Site.info$Year, 5),
  Cell = rep(Site.info$Waterbody.Name, 5),
  Variable = rep(c("log(Water temperature [°C])", "log(Conductivity [µS/cm])", 
                   "log(Dissolved oxygen [mg/L])", "log(Turbidity [NTU])",'pH'), 
                 each=length(Site.info$Water.Temperature)))
Water.data$Year <- as.character(Water.data$Year)
Water.data$Cell[Water.data$Cell=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Water.data$Cell[Water.data$Cell=="St. Clair NWA - West Cell SCU"] <- "West Cell"

# Means for plotting
Water.means <- Water.data %>%
  group_by(Cell, Year, Variable) %>%
  summarise(Mean = mean(Measure, na.rm = TRUE), .groups = "drop")

# Create data for line segments
Water.lines <- Water.means %>%
  pivot_wider(
    names_from = Year,
    values_from = Mean)

# plot
Water.gg<-ggplot(Water.data, aes(y=Measure, x=Cell, color=Year))+
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge(width = 0.8))+
  geom_jitter(aes(color=Year), 
              position = position_jitterdodge(jitter.width = 0.2, jitter.height = 0.0,
                                              dodge.width = 0.8), 
              size=2, alpha=0.5, pch=20) +
  geom_segment(data = Water.lines, aes(x = as.numeric(factor(Cell)) - 0.2,
                                       xend = as.numeric(factor(Cell)) + 0.2,
                                       y = `2023`, yend = `2024`),
               inherit.aes = FALSE, colour = "black", linewidth = 0.5) +
  geom_point(data = Water.means, aes(x = Cell, y = Mean, group = Year),
             position = position_dodge(width = 0.8), size = 2, show.legend = FALSE,
             colour='black')+
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))+
  facet_wrap(~Variable, scales='free_y')+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = 'top')

#png('Results/Figures/WaterQ.boxplots.png',height=4, width=7, units='in', res=800)
Water.gg
#dev.off()

################################################################################
#Linear Models
################################################################################
# shorter dataframe for testing
Water.data.df <- Site.info[c(8,10,19:22,25)]
str(Water.data.df)
Water.data.df$Year <- as.character(Water.data.df$Year)
Water.data.df$Waterbody.Name[Water.data.df$Waterbody.Name=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Water.data.df$Waterbody.Name[Water.data.df$Waterbody.Name=="St. Clair NWA - West Cell SCU"] <- "West Cell"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# DO
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
DO.model<- lm(log(Dissolved.Oxygen) ~ Year * Waterbody.Name, data = Water.data.df)
DO.sim <- simulateResiduals(fittedModel = DO.model, plot = TRUE)
summary(DO.model)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Temperature
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Temp.model<- lm(log(Water.Temperature) ~ Year * Waterbody.Name, data = Water.data.df)
Temp.sim <- simulateResiduals(fittedModel = Temp.model, plot = TRUE)
plot(Temp.sim)
summary(Temp.model)
# significant interaction suggests that the relationship between temperature
# and year differs between waterbodies. 

# plot
ggplot(Water.data.df, aes(x = Year, y = log(Water.Temperature), colour = Waterbody.Name,
                          group = Waterbody.Name)) +
  stat_summary(fun = mean, geom = "point") +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_summary(fun = mean, geom = "line") +
  theme(legend.title = element_blank())
emmeans(Temp.model, pairwise ~ Waterbody.Name | Year)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Conductivity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Cond.model<- lm(log(Conductivity) ~ Year * Waterbody.Name, data = Water.data.df)
Cond.sim <- simulateResiduals(fittedModel = Cond.model, plot = TRUE)
summary(Cond.model)
# significant interaction suggests that the relationship between conductivity
# and year differs between waterbodies. 

# plot
ggplot(Water.data.df, aes(x = Year, y = log(Conductivity), colour = Waterbody.Name,
                          group = Waterbody.Name)) +
  stat_summary(fun = mean, geom = "point") +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_summary(fun = mean, geom = "line") +
  theme(legend.title = element_blank())
emmeans(Cond.model, pairwise ~ Waterbody.Name | Year)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# pH
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
ph.model<- lm(pH ~ Year * Waterbody.Name, data = Water.data.df)
ph.sim <- simulateResiduals(fittedModel = ph.model, plot = TRUE)
summary(ph.model)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Turbidity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Turb.model<- lm(log(Turbidity..ntu.) ~ Year * Waterbody.Name, data = Water.data.df)
Turb.sim <- simulateResiduals(fittedModel = Turb.model, plot = TRUE)
summary(Turb.model)

# plot
ggplot(Water.data.df, aes(x = Year, y = log(Turbidity..ntu.), colour = Waterbody.Name,
                          group = Waterbody.Name)) +
  stat_summary(fun = mean, geom = "point") +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_summary(fun = mean, geom = "line") +
  theme(legend.title = element_blank())
emmeans(Cond.model, pairwise ~ Waterbody.Name | Year)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Combined
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
Water.data.df
colnames(Water.data)

results <- Water.data %>%
  group_by(Variable) %>%
  do(tidy(lm(Measure ~ Year*Cell, data = .), conf.int = TRUE))
results

#write.csv(results, "Results/Water.Quality.csv")

################################################################################
################################################################################
# PERMANOVA
################################################################################
################################################################################
Water.data.df2 <- Water.data.df[complete.cases(Water.data.df),]
Water.data.df2$Water.Temperature <- log(Water.data.df2$Water.Temperature)
Water.data.df2$Conductivity <- log(Water.data.df2$Conductivity)
Water.data.df2$Turbidity..ntu. <- log(Water.data.df2$Turbidity..ntu.)
Water.data.df2$Dissolved.Oxygen <- log(Water.data.df2$Dissolved.Oxygen)
Water.data.df2$Waterbody.Name[Water.data.df2$Waterbody.Name=="St. Clair NWA - East Cell SCU"] <- "East Cell"
Water.data.df2$Waterbody.Name[Water.data.df2$Waterbody.Name=="St. Clair NWA - West Cell SCU"] <- "West Cell"

# permanova
perm1<-adonis2(Water.data.df2[c(3:7)] ~ Year*Waterbody.Name,
               data = Water.data.df2,
               method = "euclidean",
               permutations = 999,
               by='terms',
               na.rm=T)
perm1

#Multivariate homogeneity of groups dispersions (variances)
d <- vegdist(Water.data.df2[c(3:7)], method = "euclidean")
disp <- betadisper(d, Water.data.df2$Year)
anova(disp)
plot(disp)
permutest(disp, pairwise = TRUE, permutations = 999)

# Site coordinates (samples)
sites <- as.data.frame(scores(disp, display = "sites"))
sites$Year <- as.character(Water.data.df2$Year)
sites$Cell <- Water.data.df2$Waterbody.Name
sites$Field.Number <- Water.data.df2$Field.Number

# Eigenvalues
eig <- disp$eig
var_explained <- eig / sum(eig[eig > 0]) * 100
var_explained[1:2]

xlab <- paste0("PCoA1 (", round(var_explained[1], 1), "%)")
ylab <- paste0("PCoA2 (", round(var_explained[2], 1), "%)")

centroids <- sites %>%
  group_by(Cell, Year) %>%
  summarise(
    PCoA1 = mean(PCoA1),
    PCoA2 = mean(PCoA2),
    .groups = "drop"
  )

WQ.ordination<-ggplot(sites, aes(PCoA1, PCoA2, colour = Year, fill = Year)) +
  geom_hline(yintercept = 0, linetype='dashed', lwd=0.5)+
  geom_vline(xintercept = 0, linetype='dashed', lwd=0.5)+
  stat_ellipse(aes(group = Year), geom = "polygon", 
               alpha = 0.2, level = 0.95, lwd=0.5)+
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  scale_fill_manual(values=c("#134A8E", "#E8291C"))+
  geom_point(size = 1) +
  geom_point(data = centroids, shape = 4, size = 3, stroke = 1.5)+
  coord_cartesian() +
  facet_wrap(~ Cell)+
  labs(x = xlab, y = ylab)

#png("Results/Figures/Water.Quality.Ordination.png", width=7, height=3, units='in', res=800)
WQ.ordination
#dev.off()
