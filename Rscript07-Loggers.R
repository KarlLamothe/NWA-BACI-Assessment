# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
Loggers <- read.csv("Data/Long-term-loggers.csv", header=T)
colnames(Loggers)
Loggers <- Loggers[c(1:18)]

Loggers$Local_Date_Time
# create a separate date and time column 
Loggers <- Loggers %>%
  mutate(
    Local_Date_Time = with_tz(ymd_hms(Local_Date_Time), "America/Toronto"),
    Date = as.Date(Local_Date_Time),
    Time = format(Local_Date_Time, "%H:%M:%S")
  )

################################################################################
# look at individual loggers
################################################################################
unique(Loggers$Serial_Number)
Log.7450_390571 <- Loggers[Loggers$Serial_Number=='7450-390571',]
Log.7450_400400 <- Loggers[Loggers$Serial_Number=='7450-400400',]
Log.7450_431525 <- Loggers[Loggers$Serial_Number=='7450-431525',]
Log.7450_439471 <- Loggers[Loggers$Serial_Number=='7450-439471',]
Log.7450_561235 <- Loggers[Loggers$Serial_Number=='7450-561235',]
Log.7450_571784 <- Loggers[Loggers$Serial_Number=='7450-571784',]
Log.7450_592323 <- Loggers[Loggers$Serial_Number=='7450-592323',]

################################################################################
# inspect them individually
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
Log.7450_390571$flag <- Log.7450_390571$Temperature_C < -2 |
  Log.7450_390571$Temperature_C > 40
subset(Log.7450_390571, flag)

# repeated identical values
rle_vals <- rle(Log.7450_390571$DO_mgL)
which(rle_vals$lengths > 30)

# plot
(ggplot(Log.7450_390571, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())) /
(ggplot(Log.7450_390571, aes(x = Local_Date_Time, y = Temperature_C)) +
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
Log.7450_400400$flag <- Log.7450_400400$Temperature_C < -2 |
  Log.7450_400400$Temperature_C > 40
subset(Log.7450_400400, flag)

# repeated identical values
rle_vals <- rle(Log.7450_400400$Temperature_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_400400, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())) /
(ggplot(Log.7450_400400, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

## ~~~~~~~~~~~~~~~ #
## Log.7450_431525 #
## ~~~~~~~~~~~~~~~ #
aggregate(Log.7450_431525$DO_mgL, list(Log.7450_431525$Date), mean)
aggregate(Log.7450_431525$DO_mgL, list(Log.7450_431525$Date), sd)
aggregate(Log.7450_431525$DO_mgL, list(Log.7450_431525$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_431525$flag <- Log.7450_431525$Temperature_C < -2 |
  Log.7450_431525$Temperature_C > 40
subset(Log.7450_431525, flag)

# repeated identical values
rle_vals <- rle(Log.7450_431525$Temperature_C)
which(rle_vals$lengths > 10)

ggplot(Log.7450_431525, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())
ggplot(Log.7450_431525, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())
#  
# ~~~~~~~~~~~~~~~ #
# Log.7450_439471 #
# ~~~~~~~~~~~~~~~ #
aggregate(Log.7450_439471$DO_mgL, list(Log.7450_439471$Date), mean)
aggregate(Log.7450_439471$DO_mgL, list(Log.7450_439471$Date), sd)
aggregate(Log.7450_439471$DO_mgL, list(Log.7450_439471$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_439471$flag <- Log.7450_439471$Temperature_C < -2 |
  Log.7450_439471$Temperature_C > 40
subset(Log.7450_439471, flag)

# repeated identical values
rle_vals <- rle(Log.7450_439471$Temperature_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_439471, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_439471, aes(x = Local_Date_Time, y = Temperature_C)) +
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
Log.7450_561235$flag <- Log.7450_561235$Temperature_C < -2 |
  Log.7450_561235$Temperature_C > 40
subset(Log.7450_561235, flag)

# repeated identical values
rle_vals <- rle(Log.7450_561235$Temperature_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_561235, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_561235, aes(x = Local_Date_Time, y = Temperature_C)) +
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
Log.7450_571784$flag <- Log.7450_571784$Temperature_C < -2 |
  Log.7450_571784$Temperature_C > 40
subset(Log.7450_571784, flag)

# repeated identical values
rle_vals <- rle(Log.7450_571784$Temperature_C)
which(rle_vals$lengths > 10)

(ggplot(Log.7450_571784, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_571784, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

## ~~~~~~~~~~~~~~~ #
## Log.7450_592323 #
## ~~~~~~~~~~~~~~~ #
aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), mean)
aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), sd)
aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), range)

# checking for anomolies
# impossible values of temperature
Log.7450_592323$flag <- Log.7450_592323$Temperature_C < -2 |
  Log.7450_592323$Temperature_C > 40
subset(Log.7450_592323, flag)

# repeated identical values
rle_vals <- rle(Log.7450_592323$Temperature_C)
which(rle_vals$lengths > 10)

ggplot(Log.7450_592323, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())
ggplot(Log.7450_592323, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank())
