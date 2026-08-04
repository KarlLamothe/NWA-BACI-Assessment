# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information.csv", header=T)
colnames(Site.info)

Rake.data.full <- read.csv("Data/Rake_data_full.csv", header=T)
colnames(Rake.data.full)
str(Rake.data.full)

################################################################################
# Data Summaries
################################################################################
aggregate(Rake.data.full$Volume_mL, list(Rake.data.full$Year,
                                         Rake.data.full$Cell), sum, na.rm=T)
aggregate(Rake.data.full$Weight_g, list(Rake.data.full$Year,
                                         Rake.data.full$Cell), sum, na.rm=T)

ggplot()