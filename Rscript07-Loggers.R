# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Loggers <- read.csv("Data/Long-term-loggers.csv", header=T)
colnames(Loggers)

# create a separate date and time column 
str(Loggers$DateTime)
Loggers$DateTime <- as.POSIXct(
  Loggers$DateTime,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "America/Toronto"  # adjust if needed
)

Loggers$Date <- as.Date(Loggers$DateTime)
Loggers$Time <- format(Loggers$DateTime, "%H:%M:%S")
head(Loggers)
tail(Loggers)

################################################################################
################################################################################
# look at individual loggers
################################################################################
################################################################################
unique(Loggers$LoggerID)
Log.7450_390571 <- Loggers[Loggers$LoggerID=='7450-390571',]
Log.7450_400400 <- Loggers[Loggers$LoggerID=='7450-400400',]
Log.7450_431525 <- Loggers[Loggers$LoggerID=='7450-431525',]
Log.7450_439471 <- Loggers[Loggers$LoggerID=='7450-439471',]
Log.7450_561235 <- Loggers[Loggers$LoggerID=='7450-561235',]
Log.7450_571784 <- Loggers[Loggers$LoggerID=='7450-571784',]
Log.7450_592323 <- Loggers[Loggers$LoggerID=='7450-592323',]

# create a variable that identifies large gaps in measurements
Log.7450_390571 <- Log.7450_390571 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )
Log.7450_390571 <- Log.7450_390571[complete.cases(Log.7450_390571), ]

Log.7450_400400 <- Log.7450_400400 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )
Log.7450_400400 <- Log.7450_400400[complete.cases(Log.7450_400400), ]

Log.7450_431525 <- Log.7450_431525 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )
Log.7450_431525 <- Log.7450_431525[complete.cases(Log.7450_431525), ]

Log.7450_439471 <- Log.7450_439471 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )
Log.7450_439471 <- Log.7450_439471[complete.cases(Log.7450_439471), ]

Log.7450_561235 <- Log.7450_561235 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )
Log.7450_561235 <- Log.7450_561235[complete.cases(Log.7450_561235), ]

Log.7450_571784 <- Log.7450_571784 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )
Log.7450_571784 <- Log.7450_571784[complete.cases(Log.7450_571784), ]

Log.7450_592323 <- Log.7450_592323 %>%
  arrange(DateTime) %>%
  mutate(
    gap_mins = as.numeric(difftime(DateTime, lag(DateTime), units = "mins")),
    group = cumsum(ifelse(is.na(gap_mins), FALSE, gap_mins > 30))
  )
Log.7450_592323 <- Log.7450_592323[complete.cases(Log.7450_592323), ]

################################################################################
################################################################################
# inspect them individually
################################################################################
################################################################################
# ~~~~~~~~~~~~~~~ #
# Log.7450_390571 #
# ~~~~~~~~~~~~~~~ #
head(Log.7450_390571)
tail(Log.7450_390571, 50)

# summarize
aggregate(Log.7450_390571$DO_mgL, list(Log.7450_390571$Date), mean)
aggregate(Log.7450_390571$DO_mgL, list(Log.7450_390571$Date), sd)
aggregate(Log.7450_390571$DO_mgL, list(Log.7450_390571$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_390571$flag <- Log.7450_390571$Temp_C < -2 |
  Log.7450_390571$Temp_C > 40
subset(Log.7450_390571, flag)

# repeated identical values
rle_vals <- rle(Log.7450_390571$DO_mgL)
which(rle_vals$lengths > 30)

# plot
(ggplot(Log.7450_390571, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())) /
(ggplot(Log.7450_390571, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

# ~~~~~~~~~~~~~~~ #
# Log.7450_400400 #
# ~~~~~~~~~~~~~~~ #
head(Log.7450_400400)
tail(Log.7450_400400, 50)

aggregate(Log.7450_400400$DO_mgL, list(Log.7450_400400$Date), mean)
aggregate(Log.7450_400400$DO_mgL, list(Log.7450_400400$Date), sd)
aggregate(Log.7450_400400$DO_mgL, list(Log.7450_400400$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_400400$flag <- Log.7450_400400$Temp_C < -2 |
  Log.7450_400400$Temp_C > 40
subset(Log.7450_400400, flag)

# repeated identical values
rle_vals <- rle(Log.7450_400400$Temp_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_400400, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())) /
(ggplot(Log.7450_400400, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

## ~~~~~~~~~~~~~~~ #
## Log.7450_431525 #
## ~~~~~~~~~~~~~~~ #
#aggregate(Log.7450_431525$DO_mgL, list(Log.7450_431525$Date), mean)
#aggregate(Log.7450_431525$DO_mgL, list(Log.7450_431525$Date), sd)
#aggregate(Log.7450_431525$DO_mgL, list(Log.7450_431525$Date), range)
#
## checking for anomolies
## impossible values of temperature
#Log.7450_431525$flag <- Log.7450_431525$Temp_C < -2 |
#  Log.7450_431525$Temp_C > 40
#subset(Log.7450_431525, flag)
#
## repeated identical values
#rle_vals <- rle(Log.7450_431525$Temp_C)
#which(rle_vals$lengths > 10)
#
#ggplot(Log.7450_431525, aes(x = DateTime, y = DO_mgL, group=group)) +
#  geom_line() +
#  labs(y = "DO (mg/L)") +
#  theme(axis.title.x = element_blank())
#ggplot(Log.7450_431525, aes(x = DateTime, y = Temp_C, group=group)) +
#  geom_line() +
#  labs(y = "Water temperature (C)") +
#  theme(axis.title.x = element_blank())
#  
# ~~~~~~~~~~~~~~~ #
# Log.7450_439471 #
# ~~~~~~~~~~~~~~~ #
aggregate(Log.7450_439471$DO_mgL, list(Log.7450_439471$Date), mean)
aggregate(Log.7450_439471$DO_mgL, list(Log.7450_439471$Date), sd)
aggregate(Log.7450_439471$DO_mgL, list(Log.7450_439471$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_439471$flag <- Log.7450_439471$Temp_C < -2 |
  Log.7450_439471$Temp_C > 40
subset(Log.7450_439471, flag)

# repeated identical values
rle_vals <- rle(Log.7450_439471$Temp_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_439471, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_439471, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

# ~~~~~~~~~~~~~~~ #
# Log.7450_561235 #
# ~~~~~~~~~~~~~~~ #
aggregate(Log.7450_561235$DO_mgL, list(Log.7450_561235$Date), mean)
aggregate(Log.7450_561235$DO_mgL, list(Log.7450_561235$Date), sd)
aggregate(Log.7450_561235$DO_mgL, list(Log.7450_561235$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_561235$flag <- Log.7450_561235$Temp_C < -2 |
  Log.7450_561235$Temp_C > 40
subset(Log.7450_561235, flag)

# repeated identical values
rle_vals <- rle(Log.7450_561235$Temp_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_561235, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_561235, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

# ~~~~~~~~~~~~~~~ #
# Log.7450_571784 #
# ~~~~~~~~~~~~~~~ #
aggregate(Log.7450_571784$DO_mgL, list(Log.7450_571784$Date), mean)
aggregate(Log.7450_571784$DO_mgL, list(Log.7450_571784$Date), sd)
aggregate(Log.7450_571784$DO_mgL, list(Log.7450_571784$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_571784$flag <- Log.7450_571784$Temp_C < -2 |
  Log.7450_571784$Temp_C > 40
subset(Log.7450_571784, flag)

# repeated identical values
rle_vals <- rle(Log.7450_571784$Temp_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_571784, aes(x = DateTime, y = DO_mgL, group=group)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_571784, aes(x = DateTime, y = Temp_C, group=group)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

## ~~~~~~~~~~~~~~~ #
## Log.7450_592323 #
## ~~~~~~~~~~~~~~~ #
#aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), mean)
#aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), sd)
#aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), range)
#
## checking for anomolies
## impossible values of temperature
#Log.7450_592323$flag <- Log.7450_592323$Temp_C < -2 |
#  Log.7450_592323$Temp_C > 40
#subset(Log.7450_592323, flag)
#
## repeated identical values
#rle_vals <- rle(Log.7450_592323$Temp_C)
#which(rle_vals$lengths > 10)
#
#ggplot(Log.7450_592323, aes(x = DateTime, y = DO_mgL, group=group)) +
#  geom_line() +
#  labs(y = "DO (mg/L)") +
#  theme(axis.title.x = element_blank())
#ggplot(Log.7450_592323, aes(x = DateTime, y = Temp_C, group=group)) +
#  geom_line() +
#  labs(y = "Water temperature (C)") +
#  theme(axis.title.x = element_blank())
