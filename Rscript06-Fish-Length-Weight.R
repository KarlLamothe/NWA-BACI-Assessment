rm(list = ls(all.names = TRUE))

# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv file for TL and Weight
TL.W <- read.csv("Data/Fish-TL-W(20260902).csv", header=T)
colnames(TL.W)
unique(TL.W$Species)

# site information
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)
colnames(Site.info)

# merge so you can see east versus west cells
TL.W.site.info <- merge(TL.W, Site.info, "Field.Number")

# make additional dataframes
# Limit the number of columns and rename variables
colnames(TL.W.site.info)
TL.W.site.info2 <- TL.W.site.info[c(1,18,9,10,11,6)]
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

TotalLength.Weight.gg<-ggplot(df_summary,aes(x = Year, y = mean, color = Cell, group = Cell)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values=c("#134A8E", "#E8291C"))+
    geom_errorbar(aes(ymin = ci_lower,ymax = ci_upper),width = 0.1) +
    facet_grid(~Measure)+
    theme(axis.title.y = element_blank())

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

L.gibbosus <- L.gibbosus %>%
  mutate(
    Weight = case_when(
      Field.Number == "2023-LCS-NWA-220823-003A" & Total.Length == 76 ~ 7.6,
      Field.Number == "2023-LCS-NWA-080823-004A" & Total.Length == 69 ~ 5.13,
      TRUE ~ Weight))

ggplot(L.gibbosus, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() 

ggplot(L.gibbosus, aes(log(Total.Length), log(Weight), colour = interaction(Year, Cell))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Year ~ Cell)

ggplot(L.gibbosus, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Lepomis gibbosus"))))

# model
L.gib.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=L.gibbosus)
L.gib.sim <- simulateResiduals(fittedModel = L.gib.lm, plot = TRUE)
summary(L.gib.lm)
confint(L.gib.lm)
pairs(emmeans(L.gib.lm, ~ Year * Cell))

L.gibbosus_diag <- L.gibbosus %>%
  mutate(
    fitted = fitted(L.gib.lm),
    resid = residuals(L.gib.lm),
    std_resid = rstandard(L.gib.lm),
    cooksD = cooks.distance(L.gib.lm)
  )

L.gibbosus_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(
    Total.Length, Weight, Year, Cell,Field.Number,
    fitted, resid, std_resid, cooksD
  ) %>%
  head(20)

#######################
# Lepomis macrochirus #
#######################
L.macrochirus <- TL.W.site.info4[TL.W.site.info4$Species=="Lepomis macrochirus",]

L.macrochirus <- L.macrochirus %>%
  mutate(
    Weight = case_when(
      Field.Number == "2024-LCS-NWA-280824-003A" & Total.Length == 187 ~ 119,
      TRUE ~ Weight))

L.macrochirus <- L.macrochirus %>%
  filter(!(Field.Number == "2024-LCS-NWA-210824-003A" & Total.Length == 121))

ggplot(L.macrochirus, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Lepomis macrochirus"))))

ggplot(L.macrochirus, aes(log(Total.Length), log(Weight), colour = interaction(Year, Cell))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Year ~ Cell)

# model
L.mac.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=L.macrochirus)
L.mac.sim <- simulateResiduals(fittedModel = L.mac.lm, plot = TRUE)
summary(L.mac.lm) # some outliers but lots of data so not worried
confint(L.mac.lm)
pairs(emmeans(L.mac.lm, ~ Year * Cell))

L.macrochirus_diag <- L.macrochirus %>%
  mutate(
    fitted = fitted(L.mac.lm),
    resid = residuals(L.mac.lm),
    std_resid = rstandard(L.mac.lm),
    cooksD = cooks.distance(L.mac.lm)
  )

L.macrochirus_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(
    Total.Length, Weight, Year, Cell,Field.Number,
    fitted, resid, std_resid, cooksD
  ) %>%
  head(20)

#########################
# Micropterus nigricans #
#########################
M.nigricans <- TL.W.site.info4[TL.W.site.info4$Species=="Micropterus nigricans",]

ggplot(M.nigricans, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  #geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Micropterus nigricans"))))

ggplot(M.nigricans, aes(log(Total.Length), log(Weight), colour = interaction(Year, Cell))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Year ~ Cell)

# model
M.nig.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=M.nigricans)
M.nig.sim <- simulateResiduals(fittedModel = M.nig.lm, plot = TRUE)
summary(M.nig.lm)
confint(M.nig.lm)
pairs(emmeans(M.nig.lm, ~ Year * Cell))

M.nigricans_diag <- M.nigricans %>%
  mutate(
    fitted = fitted(M.nig.lm),
    resid = residuals(M.nig.lm),
    std_resid = rstandard(M.nig.lm),
    cooksD = cooks.distance(M.nig.lm)
  )

M.nigricans_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(
    Total.Length, Weight, Year, Cell,Field.Number,
    fitted, resid, std_resid, cooksD
  ) %>%
  head(20)

#########################
# Pomoxis nigromaculatus
P.nigromaculatus <- TL.W.site.info4[TL.W.site.info4$Species=="Pomoxis nigromaculatus",]

P.nigromaculatus <- P.nigromaculatus %>%
  mutate(
    Weight = case_when(
      Field.Number == "2023-LCS-NWA-280823-004A" & Total.Length == 229 ~ 190,
      Field.Number == "2023-LCS-NWA-140823-005A" & Total.Length == 194 ~ 117.8,
      Field.Number == "2023-LCS-NWA-100823-003A" & Total.Length == 127 ~ 66.8,
      TRUE ~ Weight))

ggplot(P.nigromaculatus, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Pomoxis nigromaculatus"))))

ggplot(P.nigromaculatus, aes(log(Total.Length), log(Weight), colour = interaction(Year, Cell))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Year ~ Cell)

# model
P.nig.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=P.nigromaculatus)
P.nig.sim <- simulateResiduals(fittedModel = P.nig.lm, plot = TRUE)
summary(P.nig.lm)
confint(P.nig.lm)

P.nigromaculatus_diag <- P.nigromaculatus %>%
  mutate(
    fitted = fitted(P.nig.lm),
    resid = residuals(P.nig.lm),
    std_resid = rstandard(P.nig.lm),
    cooksD = cooks.distance(P.nig.lm)
  )

P.nigromaculatus_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(
    Total.Length, Weight, Year, Cell,Field.Number,
    fitted, resid, std_resid, cooksD
  )%>%
  head(20)

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

ggplot(A.ocellicauda, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Amia ocellicauda"))))

# model
A.oce.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=A.ocellicauda)
A.oce.sim <- simulateResiduals(fittedModel = A.oce.lm, plot = TRUE)
summary(A.oce.lm)
confint(A.oce.lm)

A.ocellicauda_diag <- A.ocellicauda %>%
  mutate(
    fitted = fitted(A.oce.lm),
    resid = residuals(A.oce.lm),
    std_resid = rstandard(A.oce.lm),
    cooksD = cooks.distance(A.oce.lm)
  )

A.ocellicauda_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(
    Total.Length, Weight, Year, Cell,Field.Number,
    fitted, resid, std_resid, cooksD
  )%>%
  head(20)

#########################
# Notemigonus crysoleucas
N.crysoleucas <- TL.W.site.info4[TL.W.site.info4$Species=="Notemigonus crysoleucas",]

N.crysoleucas <- N.crysoleucas %>%
  mutate(
    Weight = case_when(
      Field.Number == "2023-LCS-NWA-210823-002A" & Total.Length == 49 ~ 0.6,
      TRUE ~ Weight))

N.crysoleucas <- N.crysoleucas %>%
  filter(!(Field.Number == "2024-LCS-NWA-140824-001A" & Weight == 1.50 & Total.Length == 47))

ggplot(N.crysoleucas, aes(log(Total.Length), log(Weight), colour = interaction(Year, Cell))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Year ~ Cell)

ggplot(N.crysoleucas, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Notemigonus crysoleucas"))))

# model
N.crys.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=N.crysoleucas)
N.crys.sim <- simulateResiduals(fittedModel = N.crys.lm, plot = TRUE)
summary(N.crys.lm)
confint(N.crys.lm)

N.crysoleucas_diag <- N.crysoleucas %>%
  mutate(
    fitted = fitted(N.crys.lm),
    resid = residuals(N.crys.lm),
    std_resid = rstandard(N.crys.lm),
    cooksD = cooks.distance(N.crys.lm)
  )

N.crysoleucas_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(
    Total.Length, Weight, Year, Cell,Field.Number,
    fitted, resid, std_resid, cooksD
  )%>%
  head(20)

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

# Figure 5
#png("Results/Figures/ECDF.length.classes.png", height=5, width=6, units='in', res=800)
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
