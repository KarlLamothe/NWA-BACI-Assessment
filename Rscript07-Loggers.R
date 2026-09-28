# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
# Water loggers
Loggers <- read.csv("Data/RAW DATA - DO NOT EDIT/2023-2024-LCS-NWA_Master_DOTlongterm_Combined_Raw_2026-08-11.csv", header=T)
Logger.Hobo <- read.csv("Data/2023-LCS-NWA_MID-DEPTH_WEST.csv", header=T)
head(Loggers)

# Air loggers
Air.logger.West <- read.csv("Data/2023-LCS-NWA_AIR_WEST.csv", header=T)
Air.logger.East <- read.csv("Data/2023-LCS-NWA_AirTemp_EastCell_(2024-08-21).csv", header=T)
head(Air.logger.West)
head(Air.logger.East)

# fish site information
Site.info <- read.csv("Data/Site-information(20260902).csv", header=T)
colnames(Site.info)
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - East Cell SCU"] <- "East cell"
Site.info$Waterbody.Name[Site.info$Waterbody.Name=="St. Clair NWA - West Cell SCU"] <- "West cell"

# reduce it down
Site.info <- Site.info[c(3,5,8,10,11:14,19,20,22)]

######################################
# make sure date and time are correct
######################################
Site.info <- Site.info %>%
  mutate(Date = dmy(Date),
         Start.DateTime = parse_date_time(
           paste(Date, Start.Time), orders = "ymd IMS p", tz = "America/Toronto"),
         Stop.DateTime = parse_date_time(
           paste(Date, Stop.Time), orders = "ymd IMS p",tz = "America/Toronto"),
    # If stop time is earlier than start time,
    # sampling must have crossed midnight
    Stop.DateTime = if_else(Stop.DateTime < Start.DateTime,
                            Stop.DateTime + days(1),
                            Stop.DateTime))

# Water loggers
# create a separate date and time column 
Loggers <- Loggers %>%
  mutate(
    Local_Date_Time = with_tz(ymd_hms(Local_Date_Time), "America/Toronto"),
    Date = as.Date(Local_Date_Time),
    Time = format(Local_Date_Time, "%H:%M:%S")
  )
colnames(Loggers)
head(Loggers)

Loggers$Waterbody[Loggers$Waterbody=="St Clair NWA - East Cell"] <- "East cell"
Loggers$Waterbody[Loggers$Waterbody=="St Clair NWA - West Cell"] <- "West cell"

# reduce it down
Loggers <- Loggers[c(1,3:7,15,35,36)]

# create a separate date and time column  for hobo logger
Logger.Hobo <- Logger.Hobo %>%
  mutate(
    Local_Date_Time = with_tz(mdy_hm(Date.Time_UTC), "America/Toronto"),
    Date = as.Date(Local_Date_Time),
    Time = format(Local_Date_Time, "%H:%M:%S"))
head(Logger.Hobo)

colnames(Loggers)
colnames(Logger.Hobo)
Logger.Hobo$Latitude <- 42.36804
Logger.Hobo$Longitude <- -82.41415
Logger.Hobo$Waterbody <- "West cell"

Loggers <- cbind.data.frame(
  Serial_Number   = c(Loggers$Serial_Number, Logger.Hobo$SN),
  Latitude        = c(Loggers$Latitude, Logger.Hobo$Latitude),
  Longitude       = c(Loggers$Longitude, Logger.Hobo$Longitude),
  Local_Date_Time = c(Loggers$Local_Date_Time, Logger.Hobo$Local_Date_Time),
  Temperature_C   = c(Loggers$Temperature_C, Logger.Hobo$Temp...C.),
  DO_mgL          = c(Loggers$DO_mgL, Logger.Hobo$DO.conc..mg.L.),
  Date            = c(Loggers$Date, Logger.Hobo$Date),
  Time            = c(Loggers$Time, Logger.Hobo$Time),
  Waterbody       = c(Loggers$Waterbody, Logger.Hobo$Waterbody))

# look at individual loggers
unique(Loggers$Serial_Number)
Log.7450_390571 <- Loggers[Loggers$Serial_Number=='7450-390571',]
Log.7450_400400 <- Loggers[Loggers$Serial_Number=='7450-400400',]
Log.7450_431525 <- Loggers[Loggers$Serial_Number=='7450-431525',]
Log.7450_439471 <- Loggers[Loggers$Serial_Number=='7450-439471',]
Log.7450_561235 <- Loggers[Loggers$Serial_Number=='7450-561235',]
Log.7450_571784 <- Loggers[Loggers$Serial_Number=='7450-571784',]
Log.7450_592323 <- Loggers[Loggers$Serial_Number=='7450-592323',]
Log.20273341 <- Loggers[Loggers$Serial_Number=='20273341',]

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#                           inspect them individually
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
################################################################################
                          # Log.7450_390571 East Cell #
################################################################################
head(Log.7450_390571)
tail(Log.7450_390571, 50)

## checking for anomolies
## impossible values of temperature
#Log.7450_390571$flag <- Log.7450_390571$Temperature_C < -2 |
#  Log.7450_390571$Temperature_C > 40
#subset(Log.7450_390571, flag)

# repeated identical values
rle_vals <- rle(Log.7450_390571$DO_mgL)
which(rle_vals$lengths > 30)

# plot
(ggplot(Log.7450_390571, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
    annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
    annotate("rect",xmin = as.Date("2024-08-07"),xmax = as.Date("2024-09-05"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank())) /
(ggplot(Log.7450_390571, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
   annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
            ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
   annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
            ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

####################################
########### Sampling period specific
####################################
Log.7450_390571_2023 <- Log.7450_390571 %>%
  filter(Local_Date_Time >= as.POSIXct("2023-08-09") &
           Local_Date_Time <= as.POSIXct("2023-08-22"))

Log.7450_390571_2024 <- Log.7450_390571 %>%
  filter(Local_Date_Time >= as.POSIXct("2024-08-07") &
           Local_Date_Time <= as.POSIXct("2024-09-04"))

(ggplot(Log.7450_390571_2023, aes(x = Local_Date_Time, y = DO_mgL)) +
    geom_line() + labs(y = "DO (mg/L)") +
    theme(axis.title.x = element_blank())) /
  (ggplot(Log.7450_390571_2023, aes(x = Local_Date_Time, y = Temperature_C)) +
     geom_line() + labs(y = "Water temperature (C)") +
     theme(axis.title.x = element_blank()))

range(Log.7450_390571_2023$DO_mgL)
mean(Log.7450_390571_2023$DO_mgL)

range(Log.7450_390571_2023$Temperature_C)
mean(Log.7450_390571_2023$Temperature_C)

(ggplot(Log.7450_390571_2024, aes(x = Local_Date_Time, y = DO_mgL)) +
    geom_line() + labs(y = "DO (mg/L)") +
    theme(axis.title.x = element_blank())) /
  (ggplot(Log.7450_390571_2024, aes(x = Local_Date_Time, y = Temperature_C)) +
     geom_line() + labs(y = "Water temperature (C)") +
     theme(axis.title.x = element_blank()))

range(Log.7450_390571_2024$DO_mgL)
range(Log.7450_390571_2023$Temperature_C)

#############################
# Log.7450_400400 East Cell #
#############################
head(Log.7450_400400)
tail(Log.7450_400400, 50)

#Log.7450_400400$flag <- Log.7450_400400$Temperature_C < -2 |
#  Log.7450_400400$Temperature_C > 40
#subset(Log.7450_400400, flag)
#
## repeated identical values
#rle_vals <- rle(Log.7450_400400$Temperature_C)
#which(rle_vals$lengths > 10)

(ggplot(Log.7450_400400, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
            ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  theme(axis.title.x = element_blank())) /
(ggplot(Log.7450_400400, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  theme(axis.title.x = element_blank()))

#############################
### Log.7450_431525 ### Buried until April 18ish ### West cell
#############################
Log.7450_431525_rev <- Log.7450_431525 %>%
  filter( Local_Date_Time >= as.POSIXct("2024-04-18 14:00:00"))

#Log.7450_431525_rev$flag <- Log.7450_431525_rev$Temperature_C < -2 |
#  Log.7450_431525_rev$Temperature_C > 40
#subset(Log.7450_431525_rev, flag)
#
## repeated identical values
#rle_vals <- rle(Log.7450_431525_rev$Temperature_C)
#which(rle_vals$lengths > 10)

(ggplot(Log.7450_431525_rev, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_431525_rev, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

Log.7450_431525_2024 <- Log.7450_431525_rev %>%
  filter(Local_Date_Time >= as.POSIXct("2024-08-07") &
          Local_Date_Time <= as.POSIXct("2024-09-04"))

(ggplot(Log.7450_431525_2024, aes(x = Local_Date_Time, y = DO_mgL)) +
    geom_line() +
    labs(y = "DO (mg/L)") +
    annotate("rect",xmin = as.Date("2024-08-07"),xmax = as.Date("2024-09-05"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
    theme(axis.title.x = element_blank()))/
  (ggplot(Log.7450_431525_2024, aes(x = Local_Date_Time, y = Temperature_C)) +
     geom_line() +
     annotate("rect",xmin = as.Date("2024-08-07"),xmax = as.Date("2024-09-05"),
              ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
     labs(y = "Water temperature (C)") +
     theme(axis.title.x = element_blank()))

#############################
# Log.7450_439471 # West Cell
#############################
#Log.7450_439471$flag <- Log.7450_439471$Temperature_C < -2 |
#  Log.7450_439471$Temperature_C > 40
#subset(Log.7450_439471, flag)
#
## repeated identical values
#rle_vals <- rle(Log.7450_439471$Temperature_C)
#which(rle_vals$lengths > 10)

(ggplot(Log.7450_439471, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_439471, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  labs(y = "Water temperature (C)") +
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  theme(axis.title.x = element_blank()))

#############################
# Log.7450_561235  East Cell
#############################
#Log.7450_561235$flag <- Log.7450_561235$Temperature_C < -2 |
#  Log.7450_561235$Temperature_C > 40
#subset(Log.7450_561235, flag)
#
## repeated identical values
#rle_vals <- rle(Log.7450_561235$Temperature_C)
#which(rle_vals$lengths > 10)

(ggplot(Log.7450_561235, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
    annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
    annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  labs(y = "DO (mg/L)") +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_561235, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
   annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
            ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
   annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
            ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

#############################
# Log.7450_571784 # East Cell
#############################
#Log.7450_571784$flag <- Log.7450_571784$Temperature_C < -2 |
#  Log.7450_571784$Temperature_C > 40
#subset(Log.7450_571784, flag)
#
## repeated identical values
#rle_vals <- rle(Log.7450_571784$Temperature_C)
#which(rle_vals$lengths > 10)

(ggplot(Log.7450_571784, aes(x = Local_Date_Time, y = DO_mgL)) +
  geom_line() +
  labs(y = "DO (mg/L)") +
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  theme(axis.title.x = element_blank()))/
(ggplot(Log.7450_571784, aes(x = Local_Date_Time, y = Temperature_C)) +
  geom_line() +
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  labs(y = "Water temperature (C)") +
  theme(axis.title.x = element_blank()))

#############################
##### Log.7450_592323 # Buried West Cell
#############################
###aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), mean)
###aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), sd)
###aggregate(Log.7450_592323$DO_mgL, list(Log.7450_592323$Date), range)
###
##### checking for anomolies
##### impossible values of temperature
####Log.7450_592323$flag <- Log.7450_592323$Temperature_C < -2 |
####  Log.7450_592323$Temperature_C > 40
####subset(Log.7450_592323, flag)
####
##### repeated identical values
####rle_vals <- rle(Log.7450_592323$Temperature_C)
####which(rle_vals$lengths > 10)
###
###(ggplot(Log.7450_592323, aes(x = Local_Date_Time, y = DO_mgL)) +
###  geom_line() +
###  labs(y = "DO (mg/L)") +
###  theme(axis.title.x = element_blank()))/
###(ggplot(Log.7450_592323, aes(x = Local_Date_Time, y = Temperature_C)) +
###  geom_line() +
###  labs(y = "Water temperature (C)") +
###  theme(axis.title.x = element_blank()))
###

#############################
# Logger Hobo # West Cell Log.20273341 
#############################
Log.20273341$flag <- Log.20273341$Temperature_C < -2 |
  Log.20273341$Temperature_C > 40
subset(Log.20273341, flag)

# repeated identical values
rle_vals <- rle(Log.20273341$Temperature_C)
which(rle_vals$lengths > 10)

(ggplot(Log.20273341, aes(x = Local_Date_Time, y = DO_mgL)) +
    geom_line() +
    labs(y = "DO (mg/L)") +
    annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
    theme(axis.title.x = element_blank()))/
  (ggplot(Log.20273341, aes(x = Local_Date_Time, y = Temperature_C)) +
     geom_line() +
     annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
              ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
     labs(y = "Water temperature (C)") +
     theme(axis.title.x = element_blank()))

################################################################################
################################################################################
# All loggers
################################################################################
################################################################################
Loggers1 <- Loggers %>%
  filter(!(Loggers$Serial_Number=='7450-592323'), # Buried 
         !(Loggers$Serial_Number=='7450-431525')) # removes full time series
Loggers1 <- rbind(Loggers1, Log.7450_431525_rev) # reattaches revised 

head(Loggers1)
unique(Loggers1$Serial_Number)

# Mean daily temperature
Logger_daily <- Loggers1 %>%
  mutate(Date = as.Date(Local_Date_Time)) %>%
  group_by(Date, Serial_Number) %>%
  summarize(
    Tmean = mean(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )
head(Logger_daily)

Logger_daily <- Logger_daily %>%
  mutate(Waterbody = case_when(
    Serial_Number == '7450-390571' ~ "East cell",
    Serial_Number == '7450-400400' ~ "East cell",
    Serial_Number == '7450-431525' ~ "West cell",
    Serial_Number == '7450-439471' ~ "West cell",
    Serial_Number == '7450-561235' ~ "East cell",
    Serial_Number == '7450-571784' ~ "East cell",
    Serial_Number == '20273341' ~ "West cell"
  ))

# Daily daily across all loggers per cell
Logger_daily_allcomb <- Loggers1 %>%
  mutate(Date = as.Date(Local_Date_Time)) %>%
  group_by(Date, Waterbody) %>%
  summarize(
    Tmean = mean(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

# plot all data
ggplot(data=Loggers1, aes(x=Local_Date_Time, y=Temperature_C, 
                         group=Serial_Number, color=Waterbody))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line() +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  labs(y = "Water temperature (°C)") +
  theme(axis.title.x = element_blank())

# plot daily mean per logger
ggplot(data=Logger_daily, aes(x=Date, y=Tmean, 
                         group=Serial_Number, color=Waterbody))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line() +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  labs(y = "Water temperature (°C)") +
  theme(axis.title.x = element_blank())

# plot mean across loggers
temp.overall<-ggplot(data=Logger_daily_allcomb, aes(x=Date, y=Tmean, color=Waterbody))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line(lwd=1) +
  guides(color=guide_legend(position='inside'))+
  scale_color_manual(values=c("#E66100", "#149A37"))+
  scale_y_continuous(breaks=c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27))+
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  labs(y = "Water Temperature (°C)") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.5, 0.8),
        legend.key = element_blank(),
        legend.background = element_blank())
temp.overall

# summarize for the periods
Loggers.2023 <- Loggers1 %>%
  filter( Local_Date_Time >= as.POSIXct("2023-08-09 00:00:00"),
          Local_Date_Time <= as.POSIXct("2023-08-22 23:59:59"))
aggregate(Loggers.2023$Temperature_C, list(Loggers.2023$Waterbody), mean)
aggregate(Loggers.2023$Temperature_C, list(Loggers.2023$Waterbody), sd)

Loggers.2024 <- Loggers1 %>%
  filter( Local_Date_Time >= as.POSIXct("2024-08-08 00:00:00"),
          Local_Date_Time <= as.POSIXct("2024-09-05 23:59:59"))
aggregate(Loggers.2024$Temperature_C, list(Loggers.2024$Waterbody), mean)
aggregate(Loggers.2024$Temperature_C, list(Loggers.2024$Waterbody), sd)

aggregate(Loggers.2023$DO_mgL, list(Loggers.2023$Waterbody), mean)
aggregate(Loggers.2023$DO_mgL, list(Loggers.2023$Waterbody), sd)
aggregate(Loggers.2024$DO_mgL, list(Loggers.2024$Waterbody), mean)
aggregate(Loggers.2024$DO_mgL, list(Loggers.2024$Waterbody), sd)
aggregate(Loggers.2023$DO_mgL, list(Loggers.2023$Waterbody), range)
aggregate(Loggers.2024$DO_mgL, list(Loggers.2024$Waterbody), range)


##################
# DO all loggers #
##################
# Daily per logger
Logger_daily_DO <- Loggers1 %>%
  mutate(Date = as.Date(Local_Date_Time)) %>%
  group_by(Date, Serial_Number) %>%
  summarize(DOmean = mean(DO_mgL, na.rm = TRUE), .groups = "drop")

Logger_daily_DO <- Logger_daily_DO %>%
  mutate(Waterbody = case_when(
    Serial_Number == '7450-390571' ~ "East cell",
    Serial_Number == '7450-400400' ~ "East cell",
    Serial_Number == '7450-431525' ~ "West cell",
    Serial_Number == '7450-439471' ~ "West cell",
    Serial_Number == '7450-561235' ~ "East cell",
    Serial_Number == '7450-571784' ~ "East cell",
    Serial_Number == '7450-592323' ~ "West cell",
    Serial_Number == '20273341' ~ "West cell"))

# Daily across loggers
Logger_daily_allcomb_DO <- Loggers1 %>%
  mutate(Date = as.Date(Local_Date_Time)) %>%
  group_by(Date, Waterbody) %>%
  summarize(DOmean = mean(DO_mgL, na.rm = TRUE), .groups = "drop")

# all data plotted
ggplot(data=Loggers1, aes(x=Local_Date_Time, y=DO_mgL, 
                         group=Serial_Number, color=Serial_Number))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line() +
  facet_wrap(~Waterbody) +
  #scale_color_manual(values=c("#E66100", "#149A37"))+
  labs(y = "Dissolved oxygen (mg/L)") +
  theme(axis.title.x = element_blank())

# logger specific daily mean
ggplot(data=Logger_daily_DO, aes(x=Date, y=DOmean, 
                              group=Serial_Number, color=Serial_Number))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line(lwd=0.75) +
  facet_wrap(~Waterbody)+
  #scale_color_manual(values=c("#E66100", "#149A37"))+
  #scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  labs(y = "Dissolved oxygen (mg/L)") +
  theme(axis.title.x = element_blank())

# overall daily mean
DO.overall<-ggplot(data=Logger_daily_allcomb_DO, aes(x=Date, y=DOmean, color=Waterbody))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line(lwd=1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  labs(y = "Dissolved oxygen (mg/L)") +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  theme(axis.title.x = element_blank(),
        legend.position = 'none')
DO.overall

temp.overall/DO.overall

################################################################################
################################################################################
# Link loggers to different locations
################################################################################
################################################################################
Logger.locations <- Loggers1 %>%
  distinct(Serial_Number, Waterbody, Latitude, Longitude)

# fish to logger distance
fish.sf <- st_as_sf(Site.info, coords = c("Start.Longitude", "Start.Latitude"),
                    crs = 4326, remove = FALSE)

logger.sf <- st_as_sf(Logger.locations, coords = c("Longitude", "Latitude"),
                      crs = 4326, remove = FALSE)

# distance to logger
fish.utm <- st_transform(fish.sf, 32617)
logger.utm <- st_transform(logger.sf, 32617)

fish.logger.dist <- expand_grid(
  Fish.row = seq_len(nrow(fish.utm)),
  Logger.row = seq_len(nrow(logger.utm))) %>%
  mutate(Field.Number = fish.utm$Field.Number[Fish.row],
         Fish.Waterbody = fish.utm$Waterbody.Name[Fish.row],
         Serial_Number = logger.utm$Serial_Number[Logger.row],
         Logger.Waterbody = logger.utm$Waterbody[Logger.row]) %>%
  filter(Fish.Waterbody == Logger.Waterbody) %>%
  rowwise() %>%
  mutate(Distance_m = as.numeric(
    st_distance(fish.utm[Fish.row, ], logger.utm[Logger.row, ]))) %>%
  ungroup()

# find the nearest logger
nearest.logger <- fish.logger.dist %>%
  group_by(Field.Number) %>%
  slice_min(Distance_m, n = 1, with_ties = FALSE) %>%
  ungroup()
summary(nearest.logger$Distance_m)

# plot
ggplot(nearest.logger, aes(x = Distance_m)) +
  geom_histogram(binwidth = 50, colour = "white") +
  labs(x = "Distance to nearest logger (m)",
       y = "Number of fish sampling events")

nearest.logger %>%
  arrange(desc(Distance_m)) %>%
  select(Field.Number, Fish.Waterbody, Serial_Number, Distance_m)

# identify the periods that the loggers were running
logger.periods <- Loggers1 %>%
  group_by(Serial_Number) %>%
  summarise(First.Record = min(Local_Date_Time, na.rm = TRUE),
            Last.Record = max(Local_Date_Time, na.rm = TRUE),
            N = n(), .groups = "drop")
logger.periods

Site.info <- Site.info %>%
  mutate(Date = as.Date(Date),
         Start.DateTime = parse_date_time(
           paste(Date, Start.Time), orders = "ymd IMS p",tz = "America/Toronto"),
         Stop.DateTime = parse_date_time(paste(Date + days(1), Stop.Time),
                                         orders = "ymd IMS p",tz = "America/Toronto") )

# fishing events - multiple nets were set in a day.
Fish.events <- Site.info %>%
  group_by(Year, Waterbody.Name, Date) %>%
  summarise(
    n.sites = n(), 
    Event.Start = min(Start.DateTime),
    Last.Net.Set = max(Start.DateTime),
    First.Net.Lift = min(Stop.DateTime),
    Event.Stop = max(Stop.DateTime), .groups = "drop") %>%
  arrange(Waterbody.Name, Year, Date) %>%
  mutate(Event.ID = row_number(),
         All.Nets.Start = as.numeric(difftime(Last.Net.Set, Event.Start, units = "hours")),
         All.Nets.Stop = as.numeric(difftime(First.Net.Lift, Event.Start, units = "hours")),
         Event.Duration = as.numeric(difftime(Event.Stop, Event.Start, units = "hours")))
print(Fish.events, n=28)

# make the logger data and site data align with waterbody name
Loggers <- Loggers1 %>%
  mutate(
    Waterbody.Name = case_when(
      grepl("East Cell", Waterbody, ignore.case = TRUE) ~ "East cell",
      grepl("West Cell", Waterbody, ignore.case = TRUE) ~ "West cell",
      TRUE ~ Waterbody
    )
  )

# crosswalk the logger data with the fishing events
Event.logger.data <- lapply(seq_len(nrow(Fish.events)), function(i) {
  event <- Fish.events[i, ]
  Loggers %>%
    filter(Waterbody.Name == event$Waterbody.Name,
           Local_Date_Time >= event$Event.Start - hours(24),
           Local_Date_Time <= event$Event.Stop) %>%
    mutate(Event.ID = i, Fish.Date = event$Date,
           Event.Start = event$Event.Start,
           Event.Stop = event$Event.Stop,
           Period = case_when(
             Local_Date_Time < Event.Start ~ "Previous 24 h",
             Local_Date_Time >= Event.Start ~ "During sampling"))}) %>%
  bind_rows()

head(Event.logger.data)

# summarize loggers for events
Logger.event.summary <- Event.logger.data %>%
  group_by(Event.ID,Fish.Date,Waterbody.Name,Period,Serial_Number) %>%
  summarise(
    Temp.mean = mean(Temperature_C, na.rm = TRUE),
    Temp.min = min(Temperature_C, na.rm = TRUE),
    Temp.max = max(Temperature_C, na.rm = TRUE),
    DO.mean = mean(DO_mgL, na.rm = TRUE),
    DO.min = min(DO_mgL, na.rm = TRUE),
    DO.max = max(DO_mgL, na.rm = TRUE),
    n = n(), .groups = "drop")

Event.environment <- Logger.event.summary %>%
  group_by(Event.ID,Fish.Date,Waterbody.Name,Period) %>%
  summarise(
    n.loggers = n_distinct(Serial_Number),
    Temp.mean = mean(Temp.mean, na.rm = TRUE),
    Temp.min = min(Temp.min, na.rm = TRUE),
    Temp.max = max(Temp.max, na.rm = TRUE),
    DO.mean = mean(DO.mean, na.rm = TRUE),
    DO.min = min(DO.min, na.rm = TRUE),
    DO.max = max(DO.max, na.rm = TRUE),
    .groups = "drop")

Event.environment %>%
  select(Fish.Date,Waterbody.Name,Period,n.loggers)

# create data frame for plotting
plot.data <- Event.logger.data %>%
  filter(Period == "During sampling") %>%
  mutate(
    Hours = as.numeric(difftime(Local_Date_Time,Event.Start,units = "hours")),
    Event.Label = paste(Waterbody.Name, format(Fish.Date, "%b %d, %Y"),sep = "\n"))

Fish.events <- Fish.events %>%
  mutate(Event.ID = row_number(),
         All.Nets.Start = as.numeric(difftime(
           Last.Net.Set, Event.Start, units = "hours")),
         All.Nets.Stop = as.numeric(difftime(
           First.Net.Lift, Event.Start, units = "hours")) )

plot.data <- Event.logger.data %>%
  filter(Period == "During sampling") %>%
  mutate(Hours = as.numeric(difftime(
    Local_Date_Time,Event.Start, units = "hours")),
    Event.Label = paste(Waterbody.Name, format(Fish.Date, "%b %d, %Y"), sep = "\n")) %>%
  left_join(Fish.events %>%
              select(Event.ID,All.Nets.Start,All.Nets.Stop),by = "Event.ID")

event.order <- Fish.events %>%
  arrange(Waterbody.Name,Year, Date) %>%
  mutate(Event.Label = paste(
    Waterbody.Name, format(Date, "%b %d, %Y"), sep = "\n")) %>%
  pull(Event.Label)

plot.data <- plot.data %>%
  mutate(Event.Label = factor(Event.Label, levels = event.order))

plot.data <- plot.data %>%
  mutate(Date.Label = format(Fish.Date, "%b %d, %Y"))

shade.data <- plot.data %>%
  distinct(Event.ID,Waterbody.Name,Date.Label,All.Nets.Start,All.Nets.Stop)

date.order <- Fish.events %>%
  arrange(Year, Date) %>%
  mutate(Date.Label = format(Date, "%b %d, %Y")) %>%
  pull(Date.Label) %>%
  unique()

plot.data <- plot.data %>%
  mutate(Date.Label = factor(Date.Label, levels = date.order))

shade.data <- shade.data %>%
  mutate(Date.Label = factor(Date.Label, levels = date.order))

# East cell specific
DO.east <- ggplot(plot.data %>% filter(Waterbody.Name == "East cell"),
  aes(x = Hours,y = DO_mgL,colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Waterbody.Name == "East cell"),
    aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
    inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Date.Label, ncol = 5) +
  scale_x_continuous( breaks = breaks_width(6)) +
  labs(title = "East Cell", x = "Hours since first net set",
    y = expression("Dissolved oxygen (mg L"^{-1}*")"), colour = "Logger")
DO.east

Temp.east <- ggplot(
  plot.data %>% filter(Waterbody.Name == "East cell"),
  aes(x = Hours,y = Temperature_C,colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Waterbody.Name == "East cell"),
            aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Date.Label, ncol = 4, scales = "free_x" ) +
  scale_x_continuous( breaks = breaks_width(6)) +
  labs(title = "East Cell", x = "Hours since first net set",
       y = "Water temperature(C)",colour = "Logger")
Temp.east

# West cell specific
DO.west <- ggplot(plot.data %>% filter(Waterbody.Name == "West cell"),
                  aes(x = Hours,y = DO_mgL,colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Waterbody.Name == "West cell"),
            aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Date.Label, ncol = 5) +
  scale_x_continuous( breaks = breaks_width(6)) +
  labs(title = "West Cell", x = "Hours since first net set",
       y = expression("Dissolved oxygen (mg L"^{-1}*")"), colour = "Logger")
DO.west

Temp.West <- ggplot(
  plot.data %>% filter(Waterbody.Name == "West cell"),
  aes(x = Hours,y = Temperature_C,colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Waterbody.Name == "West cell"),
            aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~ Date.Label, ncol = 4, scales = "free_x" ) +
  scale_x_continuous( breaks = breaks_width(6)) +
  labs(title = "West Cell", x = "Hours since first net set",
       y = "Water temperature(C)",colour = "Logger")
Temp.West

#### Split it by year rather than cell
plot.data <- plot.data %>%
  mutate(Year = year(Fish.Date), 
         Date.Label = format(Fish.Date, "%b %d"))

shade.data <- plot.data %>%
  distinct(Event.ID,Year,Waterbody.Name,Date.Label,All.Nets.Start,All.Nets.Stop)

event.order <- plot.data %>%
  distinct(Waterbody.Name, Fish.Date, Date.Label) %>%
  arrange(Waterbody.Name, Fish.Date) %>%
  mutate(Event.Label = paste(Waterbody.Name, Date.Label,sep = "\n")) %>%
  pull(Event.Label) %>%
  unique()

plot.data <- plot.data %>%
  mutate(Event.Label = paste(Waterbody.Name,Date.Label,sep = "\n"),
    Event.Label = factor(Event.Label,levels = event.order))

shade.data <- shade.data %>%
  mutate(Event.Label = paste(Waterbody.Name, Date.Label, sep = "\n"),
    Event.Label = factor(Event.Label, levels = event.order))

DO.2023 <- ggplot(plot.data %>%filter(Year == 2023),
  aes(x = Hours, y = DO_mgL, colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Year == 2023),
    aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
    inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap( ~ Event.Label, ncol = 3) +
  scale_x_continuous(breaks = breaks_width(6)) +
  labs(title = "2023", x = "Hours since first net set",
    y = expression("Dissolved oxygen (mg L"^{-1}*")"),colour = "Logger") 
DO.2023

DO.2024 <- ggplot(plot.data %>%filter(Year == 2024),
                  aes(x = Hours, y = DO_mgL, colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Year == 2024),
            aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap( ~ Event.Label, ncol = 4) +
  scale_x_continuous(breaks = breaks_width(6)) +
  labs(title = "2024", x = "Hours since first net set",
       y = expression("Dissolved oxygen (mg L"^{-1}*")"),colour = "Logger") 
DO.2024

Temp.2023 <- ggplot(plot.data %>%filter(Year == 2023),
                  aes(x = Hours, y = Temperature_C, 
                      colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Year == 2023),
            aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap( ~ Event.Label, ncol = 3) +
  scale_x_continuous(breaks = breaks_width(6)) +
  labs(title = "2023", x = "Hours since first net set",y="Temperature",colour = "Logger")
Temp.2023

Temp.2024 <- ggplot(plot.data %>%filter(Year == 2024),
                  aes(x = Hours, y = Temperature_C, colour = Serial_Number, group = Serial_Number)) +
  geom_rect(data = shade.data %>% filter(Year == 2024),
            aes(xmin = All.Nets.Start,xmax = All.Nets.Stop,ymin = -Inf,ymax = Inf),
            inherit.aes = FALSE, fill = "grey50", alpha = 0.15) +
  geom_line(linewidth = 0.75) +
  facet_wrap( ~ Event.Label, ncol = 4) +
  scale_x_continuous(breaks = breaks_width(6)) +
  labs(title = "2024", x = "Hours since first net set",y="Temperature",colour = "Logger")

Temp.2024

#################################################################################
###### compare logger to point estimates
#################################################################################
head(Site.info)
head(Loggers)

Point.logger <- lapply(seq_len(nrow(Site.info)), function(i) {
  point <- Site.info[i, ]
  # Logger data from the same cell
  tmp <- Loggers %>%
    filter(Waterbody.Name == point$Waterbody.Name) %>%
    mutate(Time.diff.min = abs(as.numeric(difftime(
      Local_Date_Time,
      point$Start.DateTime, units = "mins"))))
  
  # For each logger, retain observation closest in time
  tmp %>% group_by(Serial_Number) %>%
    slice_min(Time.diff.min, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      Field.Number = point$Field.Number,
      Fish.Date = point$Date,
      Year = point$Year,
      Fish.Start.DateTime = point$Start.DateTime,
      Fish.Latitude = point$Start.Latitude,
      Fish.Longitude = point$Start.Longitude,
      Point.Temp = point$Water.Temperature,
      Point.DO = point$Dissolved.Oxygen)}) %>%
  bind_rows()

summary(Point.logger$Time.diff.min)

Point.logger %>%
  arrange(desc(Time.diff.min)) %>%
  select(Field.Number, Fish.Date, Serial_Number, Fish.Start.DateTime,
         Local_Date_Time, Time.diff.min) %>%
  head(20)

Point.logger <- Point.logger %>%
  filter(Time.diff.min <= 10)

Point.logger <- Point.logger %>%
  mutate(DO.difference = Point.DO - DO_mgL,
         Temp.difference = Point.Temp - Temperature_C,
         DO.abs.difference = abs(DO.difference),
         Temp.abs.difference = abs(Temp.difference))

Point.logger <- Point.logger %>%
  left_join(fish.logger.dist %>% select(
    Field.Number,Serial_Number,Distance_m),
    by = c("Field.Number","Serial_Number"))

ggplot(Point.logger, aes(x = Distance_m, y = DO.difference, colour = Waterbody.Name)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.7) +
  facet_grid(Waterbody.Name~ Year) +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  labs(x = "Distance between fish site and logger (m)",
    y = expression("Point DO - logger DO (mg L"^{-1}*")"),
    colour = "Cell")

ggplot(Point.logger, aes(x = Distance_m, y = DO.abs.difference,colour = Waterbody.Name)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  stat_smooth(method='lm', se=F)+
  facet_grid(Waterbody.Name~ Year) +
  labs(x = "Distance between fish site and logger (m)",colour = "Cell",
       y = expression("Absolute difference in DO (mg L"^{-1}*")")) 

Point.logger %>%
  group_by(Year, Waterbody.Name) %>%
  summarise(
    n = n(),
    mean.diff = mean(DO.difference, na.rm = TRUE),
    median.diff = median(DO.difference, na.rm = TRUE),
    sd.diff = sd(DO.difference, na.rm = TRUE),
    mean.abs.diff = mean(DO.abs.difference, na.rm = TRUE),
    median.abs.diff = median(DO.abs.difference, na.rm = TRUE), .groups = "drop")

ggplot(Point.logger, aes(x = Distance_m, y = Temp.difference, colour = Waterbody.Name)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(alpha = 0.7) +
  facet_grid(Waterbody.Name~ Year) +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  labs(x = "Distance between fish site and logger (m)",
       y = "Point Temp - logger Temp",
       colour = "Cell")

ggplot(Point.logger, aes(x = Distance_m, y = Temp.abs.difference,colour = Waterbody.Name)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  facet_grid(Waterbody.Name~ Year) +
  stat_smooth(se=F)+
  labs(x = "Distance between fish site and logger (m)",colour = "Cell",
       y = "Absolute difference in temperature")

Point.logger %>%
  group_by(Year, Waterbody.Name) %>%
  summarise(
    n = n(),
    mean.diff = mean(Temp.difference, na.rm = TRUE),
    median.diff = median(Temp.difference, na.rm = TRUE),
    sd.diff = sd(Temp.difference, na.rm = TRUE),
    mean.abs.diff = mean(Temp.abs.difference, na.rm = TRUE),
    median.abs.diff = median(Temp.abs.difference, na.rm = TRUE), .groups = "drop")

################################################################################
################################################################################
# Air loggers
################################################################################
################################################################################
# West cell
Air.logger.West <- Air.logger.West %>%
  mutate(
    Local_Date_Time = with_tz(mdy_hm(Date.Time_UTC), "America/Toronto"),
    Date = as.Date(Local_Date_Time),
    Time = format(Local_Date_Time, "%H:%M:%S")
  )

# daily mean
Air.logger.West
Air.logger.West.daily <- Air.logger.West %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarize(TMean = mean(Temp...C., na.rm = TRUE), .groups = "drop")
Air.logger.West.daily$Cell <- "West cell"

# East cell
Air.logger.East <- Air.logger.East %>%
  mutate(
    Local_Date_Time = with_tz(mdy_hm(Date.Time_UTC), "America/Toronto"),
    Date = as.Date(Local_Date_Time),
    Time = format(Local_Date_Time, "%H:%M:%S")
  )

Air.logger.East.daily <- Air.logger.East %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date) %>%
  summarize(TMean = mean(Temp...C., na.rm = TRUE), .groups = "drop")
Air.logger.East.daily$Cell <- "East cell"

Daily.air.temp <- rbind(Air.logger.West.daily, Air.logger.East.daily)

# plot
daily.air.gg<-(ggplot(Daily.air.temp, aes(x = Date, y = TMean, group=Cell, color=Cell)) +
                 geom_line(lwd=1) +
                 labs(y = "Air temperature (C)") +
                 annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
                          ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
                 annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
                          ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
                 guides(color=guide_legend(position='inside'))+
                 scale_color_manual(values=c("#E66100", "#149A37"))+
                 theme(axis.title.x = element_blank(),
                       legend.title = element_blank(),
                       legend.key = element_blank(),
                       legend.background = element_blank(),
                       legend.position.inside = c(0.15,0.25)))
daily.air.gg
