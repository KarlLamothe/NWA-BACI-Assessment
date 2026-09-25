# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Site.info <- read.csv("Data/Site-information.csv", header=T)
colnames(Site.info)

# ~~~~~~~~~~~~~~ #
# 2023 rake data #
# ~~~~~~~~~~~~~~ #
Rake.data.2023.site <- read.csv("Data/Veg_data_2023_site.csv", header=T)
colnames(Rake.data.2023.site)

Rake.data.2023.main <- read.csv("Data/Veg_data_2023_main.csv", header=T)
colnames(Rake.data.2023.main)

# merge the two 
Rake.data.2023 <- merge(Rake.data.2023.main, Rake.data.2023.site, "Field.Number")
colnames(Rake.data.2023)

Rake.data.2023 <- Rake.data.2023[c(1:6,17,22:24)]
colnames(Rake.data.2023)

# revise the column names to better align with 2024 data
colnames(Rake.data.2023) <- c("Field.Number", "Year", "Species", "Common.Name",
                              "Weight_g", "Volume_mL", "Cell", "Rake_depth_m",
                              "SAV_category", "Algae_category")

# ~~~~~~~~~~~~~~ #
# 2024 rake data #
# ~~~~~~~~~~~~~~ #
Rake.data.2024 <- read.csv("Data/Veg_data_2024.csv", header=T)
colnames(Rake.data.2024)

Rake.data.2024 <- Rake.data.2024[c(1,3:7,11,12,19,20)]
colnames(Rake.data.2024)

# revise the column names to better align with 2023 data
colnames(Rake.data.2024) <- c("Field.Number", "Year", "Cell", "Rake_depth_m",
                              "SAV_category", "Algae_category", "Species", "Common.Name",
                              "Weight_g", "Volume_mL")

# ~~~~~~~~~~~~~~ #
# Full rake data #
# ~~~~~~~~~~~~~~ #
Rake.data.full <- rbind(Rake.data.2023, Rake.data.2024)
write.csv(Rake.data.full, "Rake_data_full.csv")
