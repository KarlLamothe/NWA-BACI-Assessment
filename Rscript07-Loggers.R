# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# read csv files
# Water loggers
Loggers <- read.csv("Data/RAW DATA - DO NOT EDIT/2023-2024-LCS-NWA_Master_DOTlongterm_Combined_Raw_2026-08-11.csv", header=T)
Logger.Hobo <- read.csv("Data/2023-LCS-NWA_MID-DEPTH_WEST.csv", header=T)

# Air loggers
Air.logger.West <- read.csv("Data/2023-LCS-NWA_AIR_WEST.csv", header=T)
Air.logger.East <- read.csv("Data/2023-LCS-NWA_AirTemp_EastCell_(2024-08-21).csv", header=T)
head(Air.logger.West)
head(Air.logger.East)

################################################################################
################################################################################
# Water loggers
################################################################################
################################################################################
# create a separate date and time column 
Loggers <- Loggers %>%
  mutate(
    Local_Date_Time = with_tz(ymd_hms(Local_Date_Time), "America/Toronto"),
    Date = as.Date(Local_Date_Time),
    Time = format(Local_Date_Time, "%H:%M:%S")
  )

# look at individual loggers
unique(Loggers$Serial_Number)
Log.7450_390571 <- Loggers[Loggers$Serial_Number=='7450-390571',]
Log.7450_400400 <- Loggers[Loggers$Serial_Number=='7450-400400',]
Log.7450_431525 <- Loggers[Loggers$Serial_Number=='7450-431525',]
Log.7450_439471 <- Loggers[Loggers$Serial_Number=='7450-439471',]
Log.7450_561235 <- Loggers[Loggers$Serial_Number=='7450-561235',]
Log.7450_571784 <- Loggers[Loggers$Serial_Number=='7450-571784',]
Log.7450_592323 <- Loggers[Loggers$Serial_Number=='7450-592323',]

Logger.Hobo <- Logger.Hobo %>%
  mutate(
    Local_Date_Time = with_tz(mdy_hm(Date.Time_UTC), "America/Toronto"),
    Date = as.Date(Local_Date_Time),
    Time = format(Local_Date_Time, "%H:%M:%S")
  )

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#                           inspect them individually
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
# Log.7450_390571 East Cell
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
    annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
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

# Log.7450_400400 East Cell
head(Log.7450_400400)
tail(Log.7450_400400, 50)

## checking for anomolies
## impossible values of temperature
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

### Log.7450_431525 ### Buried until April 18ish ### West cell
Log.7450_431525_rev <- Log.7450_431525 %>%
  filter( Local_Date_Time >= as.POSIXct("2024-04-18 14:00:00"))

## checking for anomolies
## impossible values of temperature
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
  
# Log.7450_439471 # West Cell
## checking for anomolies
## impossible values of temperature
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

# ~~~~~~~~~~~~~~~ #
# Log.7450_561235  East Cell
# ~~~~~~~~~~~~~~~ #
## checking for anomolies
## impossible values of temperature
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

# ~~~~~~~~~~~~~~~ #
# Log.7450_571784 # East Cell
# ~~~~~~~~~~~~~~~ #
## checking for anomolies
## impossible values of temperature
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

##### ~~~~~~~~~~~~~~~ #
##### Log.7450_592323 # Buried West Cell
##### ~~~~~~~~~~~~~~~ #
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

# Logger Hobo # West Cell
## checking for anomolies
## impossible values of temperature
#Logger.Hobo$flag <- Logger.Hobo$Temp...C. < -2 |
#  Logger.Hobo$Temp...C. > 40
#subset(Logger.Hobo, flag)
#
## repeated identical values
#rle_vals <- rle(Logger.Hobo$Temp...C.)
#which(rle_vals$lengths > 10)

(ggplot(Logger.Hobo, aes(x = Local_Date_Time, y = DO.conc..mg.L.)) +
    geom_line() +
    labs(y = "DO (mg/L)") +
    annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
             ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
    theme(axis.title.x = element_blank()))/
  (ggplot(Logger.Hobo, aes(x = Local_Date_Time, y = Temp...C.)) +
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
  filter(!(Loggers$Serial_Number=='7450_592323'),
         !(Loggers$Serial_Number=='7450_431525'))
head(Loggers1)
unique(Loggers1$Serial_Number)
Loggers1$Waterbody[Loggers1$Waterbody=="St Clair NWA - East Cell"] <- "East Cell"
Loggers1$Waterbody[Loggers1$Waterbody=="St Clair NWA - West Cell"] <- "West Cell"
Logger.Hobo$Waterbody <- "West cell"

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
    Serial_Number == '7450-571784' ~ "East cell"
  ))

Logger.Hobo.mean.daily <- Logger.Hobo %>%
  mutate(Date = as.Date(Local_Date_Time)) %>%
  group_by(Date) %>%
  summarize(
    Tmean = mean(Temp...C., na.rm = TRUE),
    .groups = "drop"
  )
head(Logger.Hobo.mean.daily)
Logger.Hobo.mean.daily$Waterbody <- "West cell"

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
  geom_line(data=Logger.Hobo, aes(x=Local_Date_Time, y=Temp...C.),
            inherit.aes = FALSE,color="#149A37") +
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
  geom_line(data=Logger.Hobo.mean.daily, aes(x=Date, y=Tmean),
            inherit.aes = FALSE,color="#149A37") +
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
aggregate(Loggers1$Temperature_C, list(Loggers1$Waterbody), mean)
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
    Serial_Number == '7450-390571' ~ "East Cell",
    Serial_Number == '7450-400400' ~ "East Cell",
    Serial_Number == '7450-431525' ~ "West Cell",
    Serial_Number == '7450-439471' ~ "West Cell",
    Serial_Number == '7450-561235' ~ "East Cell",
    Serial_Number == '7450-571784' ~ "East Cell",
    Serial_Number == '7450-592323' ~ "West Cell"))

# Daily across loggers
Logger_daily_allcomb_DO <- Loggers1 %>%
  mutate(Date = as.Date(Local_Date_Time)) %>%
  group_by(Date, Waterbody) %>%
  summarize(DOmean = mean(DO_mgL, na.rm = TRUE), .groups = "drop")

# all data plotted
ggplot(data=Loggers1, aes(x=Local_Date_Time, y=DO_mgL, 
                         group=Serial_Number, color=Waterbody))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line() +
  geom_line(data=Logger.Hobo, aes(x=Local_Date_Time, y=DO.conc..mg.L.),
            inherit.aes = FALSE,color="#149A37") +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  labs(y = "Dissolved oxygen (mg/L)") +
  theme(axis.title.x = element_blank())

Logger.Hobo.mean.daily.DO <- Logger.Hobo %>%
  mutate(Date = as.Date(Local_Date_Time)) %>%
  group_by(Date) %>%
  summarize(
    Tmean = mean(DO.conc..mg.L., na.rm = TRUE),
    .groups = "drop"
  )
head(Logger.Hobo.mean.daily.DO)
Logger.Hobo.mean.daily.DO$Waterbody <- "West cell"

# logger specific daily mean
ggplot(data=Logger_daily_DO, aes(x=Date, y=DOmean, 
                              group=Serial_Number, color=Waterbody))+
  annotate("rect",xmin = as.Date("2023-08-09"),xmax = as.Date("2023-08-22"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  annotate("rect",xmin = as.Date("2024-08-08"),xmax = as.Date("2024-09-05"),
           ymin = -Inf, ymax = Inf, fill = "grey70", alpha = 0.3) +
  geom_line() +
  geom_line(data=Logger.Hobo.mean.daily.DO, aes(x=Date, y=Tmean),
            inherit.aes = FALSE,color="#149A37") +
  scale_color_manual(values=c("#E66100", "#149A37"))+
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
  geom_line(data=Logger.Hobo.mean.daily.DO, aes(x=Date, y=Tmean),
            inherit.aes = FALSE,color="#149A37") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y") +
  labs(y = "Dissolved oxygen (mg/L)") +
  scale_color_manual(values=c("#E66100", "#149A37"))+
  theme(axis.title.x = element_blank())
DO.overall

temp.overall/DO.overall

# summarize for the periods
aggregate(Loggers.2023$DO_mgL, list(Loggers.2023$Waterbody), mean)
aggregate(Loggers.2023$DO_mgL, list(Loggers.2023$Waterbody), sd)

aggregate(Loggers.2024$DO_mgL, list(Loggers.2024$Waterbody), mean)
aggregate(Loggers.2024$DO_mgL, list(Loggers.2024$Waterbody), sd)

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
