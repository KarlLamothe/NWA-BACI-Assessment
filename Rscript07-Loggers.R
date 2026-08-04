# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Logger.2023 <- read.csv("Data/Logger Data 2023.csv", header=T)
colnames(Logger.2023)
Logger.2023$Date

# convert to plot format: Temp.Log.DS2
Logger.2023$Date <- as.Date(Logger.2023$Date, format=c("%m/%d/%Y"))
Logger.2023 <- Logger.2023 %>%
  mutate(DateTime = mdy_hm(DateTime)) %>%
  arrange(DateTime)

# summary
unique(Logger.2023$SN)
unique(Logger.2023$DateTime)

# Individual loggers
Logger_770855 <- Logger.2023[Logger.2023$SN=='7450-770855',]
Logger_049711 <- Logger.2023[Logger.2023$SN=='7450-049711',]
Logger_064567 <- Logger.2023[Logger.2023$SN=='7450-064567',]
Logger_778298 <- Logger.2023[Logger.2023$SN=='7450-778298',]

# In water
Logger_770855.IN <- Logger_770855[Logger_770855$InWater=="TRUE",]
Logger_049711.IN <- Logger_049711[Logger_049711$InWater=="TRUE",]
Logger_064567.IN <- Logger_064567[Logger_064567$InWater=="TRUE",]
Logger_778298.IN <- Logger_778298[Logger_778298$InWater=="TRUE",]

################################################################################
# Logger Logger_770855
################################################################################
# summarize
aggregate(Logger_770855.IN$DO_mgL, list(Logger_770855.IN$Date), mean)
aggregate(Logger_770855.IN$DO_mgL, list(Logger_770855.IN$Date), sd)
aggregate(Logger_770855.IN$DO_mgL, list(Logger_770855.IN$Date), range)

head(Logger_770855.IN)

str(Logger_770855.IN$DateTime)
head(Logger_770855.IN$DateTime)

# create gaps in data frame to plot properly
Logger_770855.IN <- Logger_770855.IN %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

# plot
Log_778298_DO_time <- ggplot(Logger_770855.IN, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(x = "Date Time", y = "DO (mg/L)")+
  theme(axis.title.x= element_blank())

################################################################################
# Logger Logger_049711
################################################################################
# summarize
aggregate(Logger_049711.IN$DO_mgL, list(Logger_049711.IN$Date), mean)
aggregate(Logger_049711.IN$DO_mgL, list(Logger_049711.IN$Date), sd)
aggregate(Logger_049711.IN$DO_mgL, list(Logger_049711.IN$Date), range)

# create gaps in data frame to plot properly
Logger_049711.IN <- Logger_049711.IN %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

# plot
Log_049711_DO_time <- ggplot(Logger_049711.IN, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(x = "Date Time", y = "DO (mg/L)")+
  theme(axis.title.x= element_blank())

################################################################################
# Logger Logger_064567
################################################################################
# summarize
aggregate(Logger_064567.IN$DO_mgL, list(Logger_064567.IN$Date), mean)
aggregate(Logger_064567.IN$DO_mgL, list(Logger_064567.IN$Date), sd)
aggregate(Logger_064567.IN$DO_mgL, list(Logger_064567.IN$Date), range)

# create gaps in data frame to plot properly
Logger_064567.IN <- Logger_064567.IN %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

# plot
Log_064567_DO_time <- ggplot(Logger_064567.IN, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(x = "Date Time", y = "DO (mg/L)")+
  theme(axis.title.x= element_blank())

################################################################################
# Logger Logger_770855
################################################################################
# summarize
aggregate(Logger_778298.IN$DO_mgL, list(Logger_778298.IN$Date), mean)
aggregate(Logger_778298.IN$DO_mgL, list(Logger_778298.IN$Date), sd)
aggregate(Logger_778298.IN$DO_mgL, list(Logger_778298.IN$Date), range)

# create gaps in data frame to plot properly
Logger_778298.IN <- Logger_778298.IN %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )

# plot
Log_778298_DO_time <- ggplot(Logger_778298.IN, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(x = "Date Time", y = "DO (mg/L)") +
  theme(axis.title.x= element_blank())

Log_778298_DO_time/Log_049711_DO_time/Log_064567_DO_time/Log_778298_DO_time +
  plot_layout(axis_titles = 'collect')
