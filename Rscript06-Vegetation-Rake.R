# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)
colnames(Site.info)

Rake.data.full <- read.csv("Data/Rake_data_full.csv", header=T)
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

ggplot(Rake.data.full, aes(x=Species, y=Volume_mL, color=Year))+
  geom_point(size=2, pch=20) +
  coord_flip() +
  facet_grid(Year~Cell) +
  scale_color_manual(values=c("#134A8E", "#E8291C"))
