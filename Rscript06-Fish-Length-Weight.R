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
TL.W.site.info2$Cell[TL.W.site.info2$Cell=="St. Clair NWA - East Cell SCU"] <- "East cell"
TL.W.site.info2$Cell[TL.W.site.info2$Cell=="St. Clair NWA - West Cell SCU"] <- "West cell"
TL.W.site.info2$Year <- as.character(TL.W.site.info2$Year)

# remove incomplete rows and rows with species that have weights of 0
TL.W.site.info3 <- TL.W.site.info2[complete.cases(TL.W.site.info2),]
TL.W.site.info3 <- TL.W.site.info3[TL.W.site.info3$Weight > 0,]
TL.W.site.info3$YearCell <- interaction(TL.W.site.info3$Year,
                                        TL.W.site.info3$Cell,
                                        sep = "_")

###############################################################################
################################################################################
# TL.W is the full raw data for lengths and weights
# TL.W.site.info is the merged frame of TL.W and site information
# TL.W.site.info2 is the reduced variables merged frame of TL.W and site info
# TL.W.site.info3 is the reduced variables merged frame of TL.W and site info
#     but with the fishes with incomplete measurements removed
################################################################################
################################################################################
# species specific for abundant taxa linear models
# these models answer the question, "did fish condition differ between cells
# and years after accounting for fish length". The models do not test whether
# the length-weight relationship itself changes among year/cell combinations,
# which would require log(Total Length) * Year * Cell, which requires a greater
# sample size.
################################################################################
################################################################################
unique(TL.W.site.info3$Species)

####################
# Lepomis gibbosus #
####################
L.gibbosus <- TL.W.site.info3[TL.W.site.info3$Species=="Lepomis gibbosus",]

ggplot(L.gibbosus, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() + scale_x_log10(n.breaks=15) + scale_y_log10(n.breaks=15) 

ggplot(L.gibbosus, aes(log(Total.Length), log(Weight), colour = interaction(Year, Cell))) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + facet_grid(Year ~ Cell)

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

# look at influential points
L.gibbosus_diag <- L.gibbosus %>%
  mutate(fitted = fitted(L.gib.lm), resid = residuals(L.gib.lm),
         std_resid = rstandard(L.gib.lm), cooksD = cooks.distance(L.gib.lm))

L.gibbosus_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number, fitted, resid, std_resid, cooksD) %>%
  head(10)

ggplot(L.gibbosus, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

############# ~~~~~~~~~~~~~~~~ ##############
# Remove influential points or revise errors
############# ~~~~~~~~~~~~~~~~ ##############
L.gibbosus <- L.gibbosus %>%
  mutate(
    Weight = case_when(
      Field.Number == "2023-LCS-NWA-220823-003A" & Total.Length == 76 ~ 7.6,
      Field.Number == "2023-LCS-NWA-080823-004A" & Total.Length == 69 ~ 5.13,
      Field.Number == "2023-LCS-NWA-100823-006A" & Total.Length == 161 ~ 93.8,
      TRUE ~ Weight))

L.gibbosus <- L.gibbosus %>%
  filter(!(Field.Number == "2023-LCS-NWA-160823-005A" & Total.Length == 43 & Weight == 5.6),
         !(Field.Number == "2023-LCS-NWA-210823-001A" & Total.Length == 46 & Weight == 4.5),
         !(Field.Number == "2023-LCS-NWA-160823-002A" & Total.Length == 30 & Weight == 1.1),
         !(Field.Number == "2023-LCS-NWA-210823-002A" & Total.Length == 155 & Weight == 28.2),
         !(Field.Number == "2023-LCS-NWA-160823-001A" & Total.Length == 75 & Weight == 3),
         !(Field.Number == "2024-LCS-NWA-150824-004A" & Total.Length == 180 & Weight == 51.19),
         !(Field.Number == "2023-LCS-NWA-150823-005A" & Total.Length == 114 & Weight == 11.8))

# model
L.gib.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=L.gibbosus)
L.gib.sim <- simulateResiduals(fittedModel = L.gib.lm, plot = TRUE)
summary(L.gib.lm)
confint(L.gib.lm)
pairs(emmeans(L.gib.lm, ~ Year * Cell))

ggplot(L.gibbosus, aes(x = Total.Length, y = Weight, color=Year)) +
  facet_wrap(~Cell) + 
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

ggplot(L.gibbosus, aes(x=Total.Length, y=Weight, color=Year))+
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Lepomis gibbosus"))))

# ~~~~~~~~~~~~~~~~~~~~ length frequency
(ggplot(L.gibbosus, aes(x = Total.Length))+
  geom_histogram(color='white', binwidth = 5)+
  facet_grid(Year~Cell) +
  labs(y = "Count", x = "Total Length (mm)"))

# ~~~~~~~~~~~~~~~~~~~~ ecdf
ggplot(L.gibbosus, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative proportion", x = "Total Length (mm)", title = "Pumpkinseed")

#######################
# Lepomis macrochirus #
#######################
L.macrochirus <- TL.W.site.info3[TL.W.site.info3$Species=="Lepomis macrochirus",]

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

ggplot(L.macrochirus, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) 

# model
L.mac.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=L.macrochirus)
L.mac.sim <- simulateResiduals(fittedModel = L.mac.lm, plot = TRUE)

L.macrochirus_diag <- L.macrochirus %>%
  mutate(fitted = fitted(L.mac.lm),resid = residuals(L.mac.lm),
         std_resid = rstandard(L.mac.lm), cooksD = cooks.distance(L.mac.lm))

L.macrochirus_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number,fitted, resid, std_resid, cooksD) %>%
  head(10)

ggplot(L.macrochirus, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

L.macrochirus <- L.macrochirus %>%
  mutate(
    Weight = case_when(
      Field.Number == "2024-LCS-NWA-280824-003A" & Total.Length == 187 ~ 119,
      TRUE ~ Weight))

L.macrochirus <- L.macrochirus %>%
  filter(!(Field.Number == "2024-LCS-NWA-210824-003A" & Total.Length == 121))

# model
L.mac.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=L.macrochirus)
L.mac.sim <- simulateResiduals(fittedModel = L.mac.lm, plot = TRUE)
summary(L.mac.lm)
confint(L.mac.lm)
pairs(emmeans(L.mac.lm, ~ Year * Cell))

# ~~~~~~~~~~~~~~~~~~~~ length frequency
(ggplot(L.macrochirus, aes(x = Total.Length))+
    geom_histogram(color='white', binwidth = 5)+
    facet_grid(Year~Cell) +
    labs(y = "Count", x = "Total Length (mm)"))

# ~~~~~~~~~~~~~~~~~~~~ ecdf
ggplot(L.macrochirus, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative proportion", x = "Total Length (mm)", title = "Bluegill")

#########################
# Micropterus nigricans #
#########################
M.nigricans <- TL.W.site.info3[TL.W.site.info3$Species=="Micropterus nigricans",]

ggplot(M.nigricans, aes(x=Total.Length, y=Weight, color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
       title=expression(paste(italic("Micropterus nigricans"))))

ggplot(M.nigricans, aes(log(Total.Length), log(Weight), colour = interaction(Year, Cell))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Year ~ Cell)

# model
M.nig.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=M.nigricans)
M.nig.sim <- simulateResiduals(fittedModel = M.nig.lm, plot = TRUE)

M.nigricans_diag <- M.nigricans %>%
  mutate(fitted = fitted(M.nig.lm),resid = residuals(M.nig.lm),
         std_resid = rstandard(M.nig.lm), cooksD = cooks.distance(M.nig.lm))

M.nigricans_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number,fitted, resid, std_resid, cooksD) %>%
  head(20)

ggplot(M.nigricans, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

M.nigricans <- M.nigricans %>%
  mutate(
    Weight = case_when(
      Field.Number == "2023-LCS-NWA-160823-003A" & Weight == 28.9 & Total.Length == 58 ~ 2.89,
      TRUE ~ Weight))

M.nigricans <- M.nigricans %>%
  filter(!(Field.Number == "2023-LCS-NWA-160823-001A" & Weight == 3.00 & Total.Length == 95),
         !(Field.Number == "2024-LCS-NWA-080824-001A" & Weight == 1.00 & Total.Length == 56))

# model
M.nig.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=M.nigricans)
M.nig.sim <- simulateResiduals(fittedModel = M.nig.lm, plot = TRUE)
summary(M.nig.lm)
confint(M.nig.lm)
pairs(emmeans(M.nig.lm, ~ Year * Cell))

# ~~~~~~~~~~~~~~~~~~~~ length frequency
(ggplot(M.nigricans, aes(x = Total.Length))+
    geom_histogram(color='white', binwidth = 10)+
    facet_grid(Year~Cell) +
    labs(y = "Count", x = "Total Length (mm)"))

# ~~~~~~~~~~~~~~~~~~~~ ecdf
ggplot(M.nigricans, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative proportion", x = "Total Length (mm)", title = "Largemouth Bass")

aggregate(M.nigricans$Weight, list(M.nigricans$Year), mean)
aggregate(M.nigricans$Weight, list(M.nigricans$Year), length)
aggregate(M.nigricans$Weight, list(M.nigricans$Year), sd)

#########################
# Pomoxis nigromaculatus
#########################
P.nigromaculatus <- TL.W.site.info3[TL.W.site.info3$Species=="Pomoxis nigromaculatus",]

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

P.nigromaculatus_diag <- P.nigromaculatus %>%
  mutate(fitted = fitted(P.nig.lm),resid = residuals(P.nig.lm),
         std_resid = rstandard(P.nig.lm), cooksD = cooks.distance(P.nig.lm))

P.nigromaculatus_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number, fitted, resid, std_resid, cooksD)%>%
  head(10)

ggplot(P.nigromaculatus, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

P.nigromaculatus <- P.nigromaculatus %>%
  mutate(
    Weight = case_when(
      Field.Number == "2023-LCS-NWA-280823-004A" & Total.Length == 229 ~ 190,
      Field.Number == "2023-LCS-NWA-140823-005A" & Total.Length == 194 ~ 117.8,
      Field.Number == "2023-LCS-NWA-100823-003A" & Total.Length == 127 ~ 66.8,
      TRUE ~ Weight))

# model
P.nig.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=P.nigromaculatus)
P.nig.sim <- simulateResiduals(fittedModel = P.nig.lm, plot = TRUE)
summary(P.nig.lm)
confint(P.nig.lm)

# ~~~~~~~~~~~~~~~~~~~~ length frequency
(ggplot(P.nigromaculatus, aes(x = Total.Length))+
    geom_histogram(color='white', binwidth = 5)+
    facet_grid(Year~Cell) +
    labs(y = "Count", x = "Total Length (mm)"))

# ~~~~~~~~~~~~~~~~~~~~ ecdf
ggplot(P.nigromaculatus, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative proportion", x = "Total Length (mm)", title = "Black Crappie")

aggregate(P.nigromaculatus$Weight, list(P.nigromaculatus$Cell), mean)
aggregate(P.nigromaculatus$Weight, list(P.nigromaculatus$Cell), length)
aggregate(P.nigromaculatus$Weight, list(P.nigromaculatus$Cell), sd)

####################
# Amia ocellicauda #
####################
A.ocellicauda <- TL.W.site.info3[TL.W.site.info3$Species=="Amia ocellicauda",]

ggplot(A.ocellicauda, aes(x=log(Total.Length), y=log(Weight), color=Year))+
  geom_point(alpha=0.4) +
  facet_wrap(~Cell) + 
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  geom_smooth(method='lm', formula= y~x, se=F) +
  labs(x = 'Total Length (mm)', y = "Weight (g)", 
    title=expression(paste(italic("Amia ocellicauda"))))

ggplot(A.ocellicauda, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

# model
A.oce.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=A.ocellicauda)
A.oce.sim <- simulateResiduals(fittedModel = A.oce.lm, plot = TRUE)

A.ocellicauda_diag <- A.ocellicauda %>%
  mutate(fitted = fitted(A.oce.lm),resid = residuals(A.oce.lm),
         std_resid = rstandard(A.oce.lm),cooksD = cooks.distance(A.oce.lm))

A.ocellicauda_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number,fitted, resid, std_resid, cooksD)%>%
  head(20)

ggplot(A.ocellicauda, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

A.ocellicauda <- A.ocellicauda %>%
  filter(!(Field.Number == "2023-LCS-NWA-280823-006A" & Weight == 300 & Total.Length == 466),
         !(Field.Number == "2023-LCS-NWA-240823-005A" & Weight == 1390 & Total.Length == 454))

A.oce.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=A.ocellicauda)
A.oce.sim <- simulateResiduals(fittedModel = A.oce.lm, plot = TRUE)
summary(A.oce.lm)
confint(A.oce.lm)

# ~~~~~~~~~~~~~~~~~~~~ length frequency
(ggplot(A.ocellicauda, aes(x = Total.Length))+
    geom_histogram(color='white', binwidth = 10)+
    facet_grid(Year~Cell) +
    labs(y = "Count", x = "Total Length (mm)"))

# ~~~~~~~~~~~~~~~~~~~~ ecdf
ggplot(A.ocellicauda, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative proportion", x = "Total Length (mm)", title = "Emerald Bowfin")

#########################
# Notemigonus crysoleucas
#########################
N.crysoleucas <- TL.W.site.info3[TL.W.site.info3$Species=="Notemigonus crysoleucas",]

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

N.crysoleucas_diag <- N.crysoleucas %>%
  mutate(fitted = fitted(N.crys.lm), resid = residuals(N.crys.lm),
         std_resid = rstandard(N.crys.lm),cooksD = cooks.distance(N.crys.lm))

N.crysoleucas_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number,fitted, resid, std_resid, cooksD)%>%
  head(20)

ggplot(N.crysoleucas, aes(x = Total.Length, y = Weight, colour = interaction(Year, Cell))) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  scale_y_log10(n.breaks=15) +
  stat_smooth(method='lm', se=F)

N.crysoleucas <- N.crysoleucas %>%
  mutate(
    Weight = case_when(
      Field.Number == "2023-LCS-NWA-210823-002A" & Weight == 7.00 & Total.Length == 49 ~ 0.7,
      TRUE ~ Weight))

N.crysoleucas <- N.crysoleucas %>%
  filter(!(Field.Number == "2024-LCS-NWA-140824-001A" & Weight == 1.50 & Total.Length == 47))

# model
N.crys.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=N.crysoleucas)
N.crys.sim <- simulateResiduals(fittedModel = N.crys.lm, plot = TRUE)
summary(N.crys.lm)
confint(N.crys.lm)

# ~~~~~~~~~~~~~~~~~~~~ length frequency
(ggplot(N.crysoleucas, aes(x = Total.Length))+
    geom_histogram(color='white', binwidth = 5)+
    facet_grid(Year~Cell) +
    labs(y = "Count", x = "Total Length (mm)"))

# ~~~~~~~~~~~~~~~~~~~~ ecdf
ggplot(N.crysoleucas, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative proportion", x = "Total Length (mm)", title = "Golden Shiner")

aggregate(N.crysoleucas$Weight, list(N.crysoleucas$Year), mean)
aggregate(N.crysoleucas$Weight, list(N.crysoleucas$Year), length)
aggregate(N.crysoleucas$Weight, list(N.crysoleucas$Year), sd)

################################################################################
################################################################################
# Loop across species
# full model results
LW.model.data <- rbind(L.gibbosus,L.macrochirus,M.nigricans,P.nigromaculatus,
                      A.ocellicauda, N.crysoleucas)

species_models <- LW.model.data %>%
  group_by(Species) %>%
  nest() %>%
  mutate(model = map(
    data,~ lm(log(Weight) ~ log(Total.Length) + Year * Cell, data = .x)))

model_coefs <- species_models %>%
  mutate(coefficients = map(
    model, ~ tidy(.x, conf.int = TRUE))) %>%
  select(Species, coefficients) %>%
  unnest(coefficients)
#write.csv(model_coefs, "Results/Fish.LW.model_coefs.csv")

model_fit <- species_models %>%
  mutate(fit = map(model, broom::glance)) %>%
  select(Species, fit) %>%
  unnest(fit) %>%
  select(Species,r.squared,adj.r.squared,statistic,p.value,df,AIC,BIC,nobs) %>%
  rename(R2 = r.squared,
         Adj_R2 = adj.r.squared,
         F_statistic = statistic,
         Model_p = p.value)

coef_plot <- model_coefs %>%
  filter(term != "(Intercept)", term != "log(Total.Length)") %>%
  mutate(Effect = case_when(
    grepl(":", term) ~ "Year × Cell",
    grepl("Year", term) ~ "Year",
    grepl("Cell", term) ~ "Cell"),
    Effect = factor(Effect,levels = c("Cell", "Year", "Year × Cell")))

# Figure S3
#png("Results/Figures/LW.Model.coefficients.png", height=3.5, width=7, units='in', res=800)
ggplot(coef_plot,aes(x = estimate, y = Effect)) +
  geom_vline(xintercept = 0, lty = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.15, lwd = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ Species) +
  labs(x = "Coefficient estimate (95% CI)", y = NULL) +
  theme(strip.text = element_text(face = "italic"))
#dev.off()

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
  geom_point(size=3, alpha=0.5)+ facet_wrap(~Cell)

ggplot(Chubsucker, aes(x=Total.Length, fill=Year))+
  geom_histogram(binwidth=5, color='black', alpha=0.5)+
  facet_grid(~Cell) +
  labs(y="Count", x="Total Length (mm)", title="Binwidth = 5 mm")

ggplot(Chubsucker, aes(x = Total.Length, y = Weight)) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  stat_smooth(method='lm')+
  scale_y_log10(n.breaks=15) 

# model
E.succ<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=Chubsucker)
E.succ.sim <- simulateResiduals(fittedModel = E.succ, plot = TRUE)
summary(E.succ) 
confint(E.succ)
pairs(emmeans(E.succ, ~ Year * Cell))

Chubsucker_diag <- Chubsucker %>%
  mutate(
    fitted = fitted(E.succ),
    resid = residuals(E.succ),
    std_resid = rstandard(E.succ),
    cooksD = cooks.distance(E.succ)
  )

Chubsucker_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(
    Total.Length, Weight, Year, Cell,Field.Number,
    fitted, resid, std_resid, cooksD
  )%>%
  head(20)

Chubsucker <- Chubsucker %>%
  filter(!(Field.Number == "2023-LCS-NWA-160823-001A" & Total.Length == 295))

# ~~~~~~~~~~~~~~~~~~~~ length frequency
(ggplot(Chubsucker, aes(x = Total.Length))+
    geom_histogram(color='white', binwidth = 5)+
    facet_grid(Year~Cell) +
    labs(y = "Count", x = "Total Length (mm)"))

# ~~~~~~~~~~~~~~~~~~~~ ecdf
ggplot(Chubsucker, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative proportion", x = "Total Length (mm)", title = "Lake Chubsucker")


################################################################################
################################################################################
# investigate rare species measurements
################################################################################
################################################################################
rare_species <- c('Ameiurus melas','Ameiurus natalis','Ameiurus nebulosus',
                  'Carassius auratus','Carassius auratus X Cyprinus carpio',
                  'Cyprinus carpio','Erimyzon sucetta', "Esox lucius",
                  'Lepomis gibbosus X Lepomis macrochirus','Lepomis sp',
                  'Perca flavescens', 'Noturus gyrinus','Umbra limi')
# only the rare species
TL.W.site.info4 <- subset(
  TL.W.site.info3,
  Species %in% rare_species
)

Bl.bullhead <- TL.W.site.info4[TL.W.site.info4$Species=="Ameiurus melas",]
Y.bullhead <- TL.W.site.info4[TL.W.site.info4$Species=="Ameiurus natalis",]
Br.bullhead <- TL.W.site.info4[TL.W.site.info4$Species=="Ameiurus nebulosus",]
C.Carp <- TL.W.site.info4[TL.W.site.info4$Species=="Cyprinus carpio",]
N.Pike <- TL.W.site.info4[TL.W.site.info4$Species=="Esox lucius",]
Lepomis.sp <- TL.W.site.info4[TL.W.site.info4$Species=="Lepomis sp",] 
C.mudminnow <- TL.W.site.info4[TL.W.site.info4$Species=="Umbra limi",]
Rares <- rbind(Bl.bullhead,Y.bullhead,Br.bullhead,C.Carp,N.Pike,Lepomis.sp,C.mudminnow)

ggplot(Rares, aes(x = Total.Length, y = Weight)) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  stat_smooth(method='lm')+
  scale_y_log10(n.breaks=15) +
  facet_wrap(~Species, scales="free")

##################
# Black bullhead #
##################
ggplot(Bl.bullhead, aes(x = Total.Length, y = Weight)) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  stat_smooth(method='lm')+
  scale_y_log10(n.breaks=15) 
Bl.bullhead.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=Bl.bullhead)

Bl.bullhead_diag <- Bl.bullhead %>%
  mutate(fitted = fitted(Bl.bullhead.lm),resid = residuals(Bl.bullhead.lm),
         std_resid = rstandard(Bl.bullhead.lm),cooksD = cooks.distance(Bl.bullhead.lm))

Bl.bullhead_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number,fitted, resid, std_resid, cooksD)%>% 
  head(10)

Bl.bullhead <- Bl.bullhead %>%
  filter(!(Field.Number == "2023-LCS-NWA-140823-001A" & Total.Length == 260),
         !(Field.Number == "2023-LCS-NWA-240823-005A" & Total.Length == 215))

##################
# Brown bullhead #
##################
ggplot(Br.bullhead, aes(x = Total.Length, y = Weight)) +
  geom_point() +
  scale_x_log10(n.breaks=15) +
  stat_smooth(method='lm')+
  scale_y_log10(n.breaks=15) 
Br.bullhead.lm<-lm(log(Weight)~log(Total.Length) + Year*Cell, data=Br.bullhead)

Br.bullhead_diag <- Br.bullhead %>%
  mutate(fitted = fitted(Br.bullhead.lm),resid = residuals(Br.bullhead.lm),
         std_resid = rstandard(Br.bullhead.lm),cooksD = cooks.distance(Br.bullhead.lm))

Br.bullhead_diag %>%
  arrange(desc(abs(std_resid))) %>%
  select(Total.Length, Weight, Year, Cell,Field.Number,fitted, resid, std_resid, cooksD)%>% 
  head(10)

Br.bullhead <- Br.bullhead %>%
  filter(!(Field.Number == "2023-LCS-NWA-140823-005A" & Total.Length == 291))

################################################################################
# Summary plots
################################################################################
Total.revised.fishes <- rbind(
  A.ocellicauda, L.gibbosus, L.macrochirus, M.nigricans, N.crysoleucas,
  P.nigromaculatus, Bl.bullhead, Y.bullhead, Br.bullhead, C.Carp, N.Pike, C.mudminnow)

# make plotting dataframe
df <- Total.revised.fishes %>%
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
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin = ci_lower,ymax = ci_upper),width = 0.1) +
  facet_grid(~Measure)+
  guides(color = guide_legend(position='inside'))+
  scale_color_manual(values=c("#134A8E", "#E8291C"))+
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position.inside = c(0.1, 0.2),
        legend.background = element_blank(),
        legend.key = element_blank())
TotalLength.Weight.gg

aggregate(Total.revised.fishes$Total.Length, list(Total.revised.fishes$Year), mean)
aggregate(Total.revised.fishes$Total.Length, list(Total.revised.fishes$Cell), mean)
aggregate(Total.revised.fishes$Total.Length, list(Total.revised.fishes$Year, Total.revised.fishes$Cell), mean)

################################################################################
################################################################################
# ECDF
################################################################################
################################################################################

TL.ECDF<-ggplot(Total.revised.fishes, aes(Total.Length, colour =Year)) +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  stat_ecdf(lwd = 0.75) +
  facet_wrap(~Cell) +
  labs(y = "Cumulative Proportion", x = "Total Length (mm)")+
  theme(legend.position='none')
TL.ECDF

# Compare where lines cross
#East Cell
dat <- subset(Total.revised.fishes, Cell == "East cell")
yrs <- levels(factor(dat$Year))
ecdf1 <- ecdf(dat$Total.Length[dat$Year == yrs[1]])
ecdf2 <- ecdf(dat$Total.Length[dat$Year == yrs[2]])

ecdf1(80)
ecdf2(80)
# approximately 79% of fish had lengths ≤ 80 mm in 2024 compared with 63% in 2023

# Common x values
x <- sort(unique(dat$Total.Length))
comp <- data.frame(
  Length = x,
  Diff = ecdf1(x) - ecdf2(x)
)
comp

#West Cell
dat <- subset(Total.revised.fishes, Cell == "West cell")
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
  Total.revised.fishes$Total.Length[Total.revised.fishes$Year=="2023"],
  Total.revised.fishes$Total.Length[Total.revised.fishes$Year=="2024"]
)

by(Total.revised.fishes, Total.revised.fishes$Cell,
   function(x)
     ks.test(
       x$Total.Length[x$Year=="2023"],
       x$Total.Length[x$Year=="2024"]))

###############################################################################
# bin the data by length
Total.revised.fishes$LengthClass <-
  cut(Total.revised.fishes$Total.Length,
      breaks = seq(0, max(Total.revised.fishes$Total.Length)+50, by=50))

size.comp <- Total.revised.fishes %>%
  group_by(Year, Cell, LengthClass) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(Year, Cell) %>%
  mutate(Prop = N/sum(N))

scaleFUN <- function(x) sprintf("%.2f", x)


Prop.length.gg<-ggplot(size.comp,aes(LengthClass, Prop,fill =Year)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("#E66100", "#149A37"))+
  scale_y_continuous(labels=scaleFUN) +
  labs(x = "Length Class (mm)", y = 'Proportion') +
  guides(fill=guide_legend(position='inside'))+
  facet_wrap(~Cell) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),
        legend.title = element_blank(),
        legend.position.inside = c(0.3, 0.6),
        legend.background = element_blank())
Prop.length.gg

# Figure 5
#png("Results/Figures/ECDF.length.classes.png", height=5, width=6, units='in', res=800)
TL.ECDF/Prop.length.gg
#dev.off()

tab <- table(TL.W.site.info3$Year, TL.W.site.info3$LengthClass)
chisq.test(tab)