# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv file for TL and Weight
TL.W <- read.csv("Data/Fish-TL-W.csv", header=T)
TL.W <- TL.W[TL.W$Year=="2023" | TL.W$Year=="2024",]
colnames(TL.W)
unique(TL.W$Species)

# site information
Site.info <- read.csv("Data/Site-information.csv", header=T)
colnames(Site.info)

# merge so you can see east versus west cells
TL.W.site.info <- merge(TL.W, Site.info, "Field.Number")

# make additional dataframes
# Limit the number of columns and rename variables
TL.W.site.info2 <- TL.W.site.info[c(1,3,7,8,9,18)]
colnames(TL.W.site.info2) <- c("Field.Number","Year","Species","Total.Length","Weight","Cell")
TL.W.site.info2$Cell[TL.W.site.info2$Cell=="St. Clair NWA - East Cell SCU"] <- "East"
TL.W.site.info2$Cell[TL.W.site.info2$Cell=="St. Clair NWA - West Cell SCU"] <- "West"
TL.W.site.info2$Year <- as.character(TL.W.site.info2$Year)

# remove incomplete rows and rows with species that have weights of 0
TL.W.site.info3 <- TL.W.site.info2[complete.cases(TL.W.site.info2),]
TL.W.site.info3 <- TL.W.site.info3[TL.W.site.info3$Weight > 0,]
TL.W.site.info3$YearCell <- interaction(TL.W.site.info3$Year,
                                        TL.W.site.info3$Cell,
                                        sep = "_")
table(TL.W.site.info3$Species, TL.W.site.info3$YearCell)

# remove species with less than 15 individuals per year cell combo
rare_species <- c('Ameiurus melas','Ameiurus natalis','Ameiurus nebulosus',
                  'Carassius auratus','Carassius auratus X Cyprinus carpio',
                  'Cyprinus carpio','Erimyzon sucetta', "Esox lucius",
                  'Lepomis gibbosus X Lepomis macrochirus','Lepomis sp',
                  'Perca flavescens', 'Noturus gyrinus','Umbra limi')

TL.W.site.info4 <- subset(
  TL.W.site.info3,
  !Species %in% rare_species
)

###############################################################################
################################################################################
# TL.W is the full raw data for lengths and weights
# TL.W.site.info is the merged frame of TL.W and site information
# TL.W.site.info2 is the reduced variables merged frame of TL.W and site info
# TL.W.site.info3 is the reduced variables merged frame of TL.W and site info
#     but with the fishes with incomplete measurements removed
# TL.W.site.info4 is TL.W.site.info3 by with rare species removed.
################################################################################
################################################################################
# Summary plots
################################################################################
################################################################################
# make plotting dataframe
df <- TL.W.site.info4 %>%
  mutate(
    Year = as.factor(Year),
    Species = as.factor(Species))

# mean plus 95% CI based on t distribution
df_summary <- df %>%
  group_by(Year, Cell) %>%
  summarise(
    n = sum(!is.na(Total.Length)),
    mean = mean(Total.Length, na.rm = TRUE),
    sd = sd(Total.Length, na.rm = TRUE),
    se = sd / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    ci_lower = mean - t_crit * se,
    ci_upper = mean + t_crit * se,
    .groups = "drop"
  )
df_summary$Measure <- "Total Length (mm)"

# mean plus 95% CI based on t distribution
df_summary2 <- df %>%
  group_by(Year, Cell) %>%
  summarise(
    n = sum(!is.na(Weight)),
    mean = mean(Weight, na.rm = TRUE),
    sd = sd(Weight, na.rm = TRUE),
    se = sd / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    ci_lower = mean - t_crit * se,
    ci_upper = mean + t_crit * se,
    .groups = "drop"
  )
df_summary2$Measure <- "Weight (g)"
df_summary <- rbind(df_summary, df_summary2)

(TotalLength.Weight.gg<-ggplot(df_summary,aes(x = Year, y = mean, color = Cell, group = Cell)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values=c("#134A8E", "#E8291C"))+
    geom_errorbar(aes(ymin = ci_lower,ymax = ci_upper),width = 0.1) +
    facet_grid(~Measure)+
    theme(legend.position='top',
          legend.title=element_blank(),
          axis.title.y = element_blank()))

aggregate(TL.W.site.info4$Total.Length, list(TL.W.site.info4$Year), mean)
aggregate(TL.W.site.info4$Total.Length, list(TL.W.site.info4$Cell), mean)
aggregate(TL.W.site.info4$Total.Length, list(TL.W.site.info4$Year, TL.W.site.info4$Cell), mean)

################################################################################
################################################################################
# species specific for abundant taxa
################################################################################
################################################################################
unique(TL.W.site.info4$Species)

####################
# Lepomis gibbosus #
####################
L.gibbosus <- TL.W.site.info4[TL.W.site.info4$Species=="Lepomis gibbosus",]

ggplot(L.gibbosus, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Lepomis gibbosus"))))

# model
L.gib.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=L.gibbosus)
plot(L.gib.lm) # some outliers but lots of data so not worried
summary(L.gib.lm)
hoCoef(L.gib.lm, 2, 3)
confint(L.gib.lm)

pairs(emmeans(L.gib.lm, ~ Year * Cell))

#######################
# Lepomis macrochirus #
#######################
L.macrochirus <- TL.W.site.info4[TL.W.site.info4$Species=="Lepomis macrochirus",]

ggplot(L.macrochirus, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Lepomis macrochirus"))))

# model
L.mac.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=L.macrochirus)
summary(L.mac.lm) # some outliers but lots of data so not worried
plot(L.mac.lm)
hoCoef(L.mac.lm, 2, 3)
confint(L.mac.lm)

pairs(emmeans(L.mac.lm, ~ Year * Cell))

#########################
# Micropterus nigricans #
#########################
M.nigricans <- TL.W.site.info4[TL.W.site.info4$Species=="Micropterus nigricans",]

ggplot(M.nigricans, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Micropterus nigricans"))))

# model
M.nig.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=M.nigricans)
summary(M.nig.lm)
plot(M.nig.lm) # ac couple outliers but not too bad
hoCoef(M.nig.lm, 2, 3)
confint(M.nig.lm)

pairs(emmeans(M.nig.lm, ~ Year * Cell))

#########################
# Pomoxis nigromaculatus
P.nigromaculatus <- TL.W.site.info4[TL.W.site.info4$Species=="Pomoxis nigromaculatus",]

ggplot(P.nigromaculatus, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Pomoxis nigromaculatus"))))

# model
P.nig.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=P.nigromaculatus)
summary(P.nig.lm)
plot(P.nig.lm)
hoCoef(P.nig.lm, 2, 3)
confint(P.nig.lm)

####################
# Amia ocellicauda #
####################
A.ocellicauda <- TL.W.site.info4[TL.W.site.info4$Species=="Amia ocellicauda",]

ggplot(A.ocellicauda, aes(x=log(Total.Length), y=log(Weight), color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(method='lm', formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
    title=expression(paste(italic("Amia ocellicauda"))))

# model
A.oce.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=A.ocellicauda)
summary(A.oce.lm)
hoCoef(A.oce.lm, 2, 3)
confint(A.oce.lm)

#########################
# Notemigonus crysoleucas
N.crysoleucas <- TL.W.site.info4[TL.W.site.info4$Species=="Notemigonus crysoleucas",]

ggplot(N.crysoleucas, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Notemigonus crysoleucas"))))

# model
N.crys.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=N.crysoleucas)
summary(N.crys.lm)
plot(N.crys.lm)
hoCoef(N.crys.lm, 2, 3)
confint(N.crys.lm)

################################################################################
################################################################################
# Loop across species
# full model results
results <- TL.W.site.info4 %>%
  group_by(Species) %>%
  do(tidy(lm(log(Weight) ~ log(Total.Length) + Year*Cell, data = .), conf.int = TRUE))
results

#write.csv(results, "Results/Fish.LW.models.csv")

# for plotting
results2 <- results %>%
  filter(
    !term %in% c("(Intercept)", "log(Total.Length)")
  )

# convert terms for cleaner labels
results2 <- results2 %>%
  mutate(term = gsub("YearCell", "", term))

ggplot(results2, aes(x = estimate, y = reorder(Species, estimate))) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2) +
  facet_wrap(~term) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(y="Species", x="Estimate")
# Relative to 2023 East cell, Largemouth Bass captured in the 2024 
# East Cell were significantly lighter at a given length.

# pairwise comparisons
pairwise_results <- TL.W.site.info4 %>%
  group_by(Species) %>%
  group_modify(~{
    mod <- lm(log(Weight) ~ log(Total.Length) + YearCell, data = .x)
    out <- as.data.frame(
      confint(pairs(emmeans(mod, ~ YearCell)))
    )
    out
  })

#convert to percent difference
pairwise_results <- pairwise_results %>%
  mutate(
    pct_diff = 100 * (exp(estimate) - 1),
    pct_low  = 100 * (exp(lower.CL) - 1),
    pct_high = 100 * (exp(upper.CL) - 1)
  )
print(pairwise_results, n=36)

pairwise_results$sig <-
  pairwise_results$pct_low > 0 |
  pairwise_results$pct_high < 0

# plot
pairwise.diffs.gg<-ggplot(pairwise_results, aes(x = pct_diff, y = Species, color=sig)) +
  geom_vline(xintercept = 0, lty = "dashed") +
  geom_errorbar(aes(xmin = pct_low, xmax = pct_high), width = 0.2) +
  geom_point(size = 1.5) +
  facet_wrap(~contrast) +
  scale_colour_manual(values = c("black", "red"),labels = c("CI overlaps 0","CI excludes 0")) +
  labs(x = "Difference in weight (g) at a given length (%)", y = NULL, color=NULL) +
  theme(axis.text.y = element_text(face='italic'),
        legend.position='top',
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"))

#png("Results/Figures/Pairwise.lengthweight.png", width=7.5, height=4, units='in', res=800)
pairwise.diffs.gg
#dev.off()

# look at results
print(pairwise_results, n=36)

# largemouth bass East 2023-2024
(exp(0.111) - 1) * 100 
(exp(0.0334) - 1) * 100 
(exp(0.188) - 1) * 100 
# LMB captured in the 2023 East Cell were estimated to weigh approximately 11.7% 
# more at a given length than those captured in the 2024 East Cell (95% CI: 3.40%-20.69%).

# golden shiner East 2023-2024
(exp(-0.0430) - 1) * 100 
(exp(-0.0805) - 1) * 100 
(exp(-0.00541) - 1) * 100 
# Golden Shiner captured in the 2023 East Cell were estimated to weigh approximately 4.21% 
# less at a given length than those captured in the 2024 East Cell (95% CI: 0.54%-7.73%).

# Bluegill  2023-2024
(exp(0.061) - 1) * 100 
# Golden Shiner captured in the 2023 East Cell were estimated to weigh approximately 4.21% 
# less at a given length than those captured in the 2024 East Cell (95% CI: 0.54%-7.73%).

################################################################################
################################################################################
# ECDF
################################################################################
################################################################################
TL.ECDF<-ggplot(TL.W.site.info3, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Empirical Cumulative\nDistribution Function", x = "Total Length (mm)")+
  theme(legend.position='none')
TL.ECDF

# Compare where lines cross
#East Cell
dat <- subset(TL.W.site.info3, Cell == "East")
yrs <- levels(factor(dat$Year))
ecdf1 <- ecdf(dat$Total.Length[dat$Year == yrs[1]])
ecdf2 <- ecdf(dat$Total.Length[dat$Year == yrs[2]])

ecdf1(80)
ecdf2(80)
# approximately 79% of fish had lengths ≤ 80 mm in 2024 compared with 69% in 2023

# Common x values
x <- sort(unique(dat$Total.Length))
comp <- data.frame(
  Length = x,
  Diff = ecdf1(x) - ecdf2(x)
)
comp

#West Cell
dat <- subset(TL.W.site.info4, Cell == "West")
yrs <- levels(factor(dat$Year))
ecdf1 <- ecdf(dat$Total.Length[dat$Year == yrs[1]])
ecdf2 <- ecdf(dat$Total.Length[dat$Year == yrs[2]])

# Common x values
x <- sort(unique(dat$Total.Length))
comp <- data.frame(
  Length = x,
  Diff = ecdf1(x) - ecdf2(x)
)
comp

################################################################################
# KS-Test
################################################################################
ks.test(
  TL.W.site.info3$Total.Length[TL.W.site.info3$Year=="2023"],
  TL.W.site.info3$Total.Length[TL.W.site.info3$Year=="2024"]
)

by(TL.W.site.info3, TL.W.site.info3$Cell,
   function(x)
     ks.test(
       x$Total.Length[x$Year=="2023"],
       x$Total.Length[x$Year=="2024"]))

###############################################################################
# bin the data by length
TL.W.site.info3$LengthClass <-
  cut(TL.W.site.info3$Total.Length,
      breaks = seq(0, max(TL.W.site.info3$Total.Length)+50, by=50))

size.comp <- TL.W.site.info3 %>%
  group_by(Year, Cell, LengthClass) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Year, Cell) %>%
  mutate(Prop = N/sum(N))

Prop.length.gg<-ggplot(size.comp,aes(LengthClass, Prop,fill =Year)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("#134A8E", "#E8291C"))+
  labs(x = "Length Class (mm)", y = 'Proportion') +
  facet_wrap(~Cell) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))
Prop.length.gg

#png("Results/Figures/ECDF.lengclasses.tiff", height=5, width=6, units='in', res=800)
TL.ECDF/Prop.length.gg + plot_layout(guides='collect')
#dev.off()

tab <- table(TL.W.site.info3$Year, TL.W.site.info3$LengthClass)
chisq.test(tab)

################################################################################
################################################################################
# Lake Chubsucker only
################################################################################
################################################################################
Chubsucker <- TL.W.site.info3[TL.W.site.info3$Species=="Erimyzon sucetta",]
Chubsucker$Year <- as.character(Chubsucker$Year)

ggplot(Chubsucker, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(size=3, alpha=0.5)

ggplot(Chubsucker, aes(x=log(Total.Length), y=log(Weight), color=Year))+
  geom_point(size=3, alpha=0.5)

ggplot(Chubsucker, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(size=3, alpha=0.5)+
  scale_color_manual(values=c("#5D3A9B", "#E66100"))+
  facet_wrap(~Cell)

ggplot(Chubsucker, aes(x=Total.Length, fill=Year))+
  geom_histogram(binwidth=5, color='black', alpha=0.5)+
  scale_fill_manual(values=c("#5D3A9B", "#E66100"))+
  facet_grid(~Cell) +
  labs(y="Count", x="Total Length (mm)", title="Binwidth = 5 mm")
