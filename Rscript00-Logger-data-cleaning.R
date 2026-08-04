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

# look at individual loggers
unique(logger_data$LoggerID)
Log.7450_390571 <- logger_data[logger_data$LoggerID=='7450-390571',]
Log.7450_400400 <- logger_data[logger_data$LoggerID=='7450-400400',]
Log.7450_431525 <- logger_data[logger_data$LoggerID=='7450-431525',]
Log.7450_439471 <- logger_data[logger_data$LoggerID=='7450-439471',]
Log.7450_561235 <- logger_data[logger_data$LoggerID=='7450-561235',]
Log.7450_571784 <- logger_data[logger_data$LoggerID=='7450-571784',]
Log.7450_592323 <- logger_data[logger_data$LoggerID=='7450-592323',]

# create a variable that identifies large gaps in measurements
Log.7450_390571 <- Log.7450_390571 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

Log.7450_400400 <- Log.7450_400400 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

Log.7450_431525 <- Log.7450_431525 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

Log.7450_439471 <- Log.7450_439471 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

Log.7450_561235 <- Log.7450_561235 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

Log.7450_571784 <- Log.7450_571784 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

Log.7450_592323 <- Log.7450_592323 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

################################################################################
################################################################################
# plot them individually
################################################################################
################################################################################
# ~~~~~~~~~~~~~~~~ #
# Dissolved oxygen
# ~~~~~~~~~~~~~~~~ #
ggplot(Log.7450_390571, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_400400, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_431525, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_439471, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_561235, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_571784, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_592323, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())

# ~~~~~~~~~~~ #
# Temperature #
# ~~~~~~~~~~~ #
colnames(Log.7450_390571)
ggplot(Log.7450_390571, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_400400, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_431525, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_439471, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_561235, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_571784, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())

ggplot(Log.7450_592323, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())

