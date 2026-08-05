# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# Folder containing logger folders
base_dir <- "Data/Data Loggers"

# Find all txt files in all subfolders
files <- list.files(
  path = base_dir,
  pattern = "\\.txt$",
  recursive = TRUE,
  full.names = TRUE
)
length(files)

# read in all the CSV files and combine them
logger_data <- files %>%
  map_dfr(function(f) {
    # skips the two rows at the top of the files which have metadata
    dat <- read.csv(f, skip = 2, check.names = FALSE)
    # Save logger ID (folder name)
    dat$LoggerID <- basename(dirname(f))
    # Save source file
    dat$FileName <- basename(f)
    dat
  })

# look at column names
colnames(logger_data)
head(logger_data)

# convert time data 
logger_data <- logger_data %>%
  mutate(
    DateTime = as.POSIXct(
      `Time (sec)`,
      origin = "1970-01-01",
      tz = "America/Toronto"
    )
  )

# revise column names
logger_data <- logger_data %>%
  rename(
    Time_sec = `Time (sec)`,
    BV_Volts = `BV (Volts)`,
    Temp_C = `T (deg C)`,
    DO_mgL = `DO (mg/l)`,
    Q = `Q ()`
  )

min(logger_data$DO_mgL)
max(logger_data$DO_mgL)
min(logger_data$Temp_C)
max(logger_data$Temp_C)

# remove any negative values
logger_data <- logger_data %>%
  filter(DO_mgL >= 0)

# write csv
write.csv(logger_data, "Data/Long-term-loggers.csv")

# plot
ggplot(logger_data, aes(x = DateTime, y = DO_mgL)) +
  geom_line() +
  facet_wrap(~LoggerID) +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())
