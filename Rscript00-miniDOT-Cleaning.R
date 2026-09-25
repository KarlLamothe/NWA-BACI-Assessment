# miniDOT logger data
# compile the raw data files, match to deployment sites and drop rows outside of site times
# clean data to remove low quality readings (per PME miniDOT Q value), flag anomalies
# optional section to calculate % O2 saturation - finds local elevation based on site lat/longs

# Original CREATED BY R. GASPARDY WITH THE HELP OF COPILOT (FILE READ/COMBINE 
# AND MATCHING TO SITE DATA, ETC) 2026-08-07

##### REQUIRES #####
# 1) csv file site_metadata of "logger events": SN, project, site code, lat/long, start time (deployment), end time (lift/retrieval/end of site), any other columns to group the data later
# 2) raw logger files in their SN folders saved in one place

# load packages and set custom ggplot theme
source("Rscript00-Packages-Theme.R") 

# 1. SET PROJECT PATHS AND LOAD SITE METADATA
project_dir <- "I:/Biodiversity Science Section/SAR Database/LoggerData_RAW/miniDOT/2023-2024-LCS-NWA/"
raw_data_dir <- paste0(project_dir,"raw_data_longterm")

# Load metadata and parse the Start/End time strings, set local timezone
metadata <- read_csv(paste0(project_dir, "site_metadata.csv"), show_col_types = FALSE) %>%
  mutate(
    Start_Time = force_tz(ymd_hms(Start_Time), tzone = "America/Toronto"),
    End_Time   = force_tz(ymd_hms(End_Time), tzone = "America/Toronto")
  )

# 2. FIND ALL TEXT FILES
file_list <- list.files(path = raw_data_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)


# 3. MERGE FILES, RETAIN SERIAL NUMBER, AND REMOVE DUPLICATE ROWS
cat("Starting file compilation loop...\n")

raw_compiled <- file_list %>%
  map_df(
    function(file_path) {
      # Cross-platform safe serial number extraction from folder name
      serial_num <- basename(dirname(file_path))
      
      read_csv(
        file_path, 
        skip = 3, 
        col_names = c("Unix_Time", "Battery_V", "Temperature_C", "DO_mgL", "Q"),
        col_types = cols(
          Unix_Time     = col_double(), 
          Battery_V     = col_double(), 
          Temperature_C = col_double(), 
          DO_mgL         = col_double(), 
          Q             = col_character()
        ),
        show_col_types = FALSE
      ) %>%
        mutate(Serial_Number = serial_num)
    },
    # ENABLES NATIVE PROGRESS BAR: Labels it so you know what R is doing
    .progress = "Compiling miniDOT Logs"
  )

# Count rows BEFORE removing duplicates
initial_rows <- nrow(raw_compiled)

# Remove exact duplicates based on Serial Number and Timestamp
combined_master <- raw_compiled %>%
  distinct(Serial_Number, Unix_Time, .keep_all = TRUE)

# Count rows AFTER removing duplicates
final_rows <- nrow(combined_master)
duplicate_count <- initial_rows - final_rows

# Print the tracking message to the console
cat(paste0("--- Data Deduplication Summary ---\n",
           "Initial rows compiled: ", initial_rows, "\n",
           "Duplicate rows removed: ", duplicate_count, "\n",
           "Clean master rows remaining: ", final_rows, "\n",
           "----------------------------------\n"))

# 4. MATCH METADATA, JOIN, AND FLAG SPIKES USING LAG & LEAD
final_dataset <- combined_master %>%
  mutate(
    Unix_Time = as.numeric(Unix_Time), 
    Battery_V = as.numeric(Battery_V),
    Q = as.numeric(Q),
    Local_Date_Time = as_datetime(Unix_Time, tz = "America/Toronto") 
  ) %>%
  
  # A. Non-equi join using matching local time zones
  inner_join(
    metadata,
    by = join_by(
      Serial_Number == Serial_Number,
      Local_Date_Time >= Start_Time,
      Local_Date_Time <= End_Time
    )
  ) %>%
  
  # B. PME Quality filter https://www.pme.com/product-installs/q-measurement-found-in-minidot
  filter(Q >= 0.7) %>%
  
  # C. CHECK FOR AND FLAG SPIKES/DROPS WITHIN SITES (eg. logger out of water at start/end of net set, mid-set spikes/drops)
  # look if a record is significantly different than the one 10 minutes before or after it
  # add the flag to another column to decide later which to include/remove (i.e. filter out launch/retrieval flags where logger was out of water after start time/before end time but keep mid-set anomalies)
  group_by(Serial_Number, Site_Name) %>%
  arrange(Local_Date_Time, .by_group = TRUE) %>%
  mutate(
    # Calculate differences backward (lag) and forward (lead)
    Temp_Change_Back = Temperature_C - lag(Temperature_C),
    Temp_Change_Fwd  = Temperature_C - lead(Temperature_C),
    
    DO_Change_Back   = DO_mgL - lag(DO_mgL),
    DO_Change_Fwd    = DO_mgL - lead(DO_mgL),
    
    # Flag rows based on thresholds (using 1.5°C and 2.0 mg/L)
    Air_Exposure_Flag = case_when(
      # FIRST ROW CATCH: If first row drops drastically to the second row
      is.na(Temp_Change_Back) & abs(Temp_Change_Fwd) > 1.5  ~ "Launch Temp Flag",
      is.na(DO_Change_Back)   & abs(DO_Change_Fwd) > 2.0    ~ "Launch DO Flag",
      
      # LAST ROW CATCH: If last row spiked compared to the second-to-last
      is.na(Temp_Change_Fwd)  & abs(Temp_Change_Back) > 1.5 ~ "Retrieval Temp Flag",
      is.na(DO_Change_Fwd)    & abs(DO_Change_Back) > 2.0   ~ "Retrieval DO Flag",
      
      # MID-SET SPIKE CATCH: Significant jump from previous AND drop to the next
      abs(Temp_Change_Back) > 1.5 & abs(Temp_Change_Fwd) > 1.5 ~ "Mid-Set Temp Flag",
      abs(DO_Change_Back) > 2.0   & abs(DO_Change_Fwd) > 2.0   ~ "Mid-Set DO Flag",
      
      TRUE ~ "Clean"
    )
  ) %>%
  ungroup() %>% 
  
  # D. Organize final columns
  select(
    Serial_Number, Site_Name, Latitude, Longitude, 
    Local_Date_Time, Temperature_C, Temp_Change_Back, Temp_Change_Fwd, 
    DO_mgL, DO_Change_Back, DO_Change_Fwd, Air_Exposure_Flag, Battery_V, Q, everything()
  )

  # E. Remove launch and retrieval out-of-water artifacts (retain mid-set flags to decide later what to do with them)
final_dataset <- final_dataset %>%
  filter(!stringr::str_detect(Air_Exposure_Flag, "Launch|Retrieval")) %>%
  mutate(
    across(
      c(Waterbody, Site_Name, Group, Project_Code, Air_Exposure_Flag), 
      as.factor
    )
  )

# Write the file with saturation calculations
# edit filename as needed for specific project/date or export
write_csv(final_dataset, paste0(project_dir, "2023-2024-LCS-NWA_Master_DOTinfyke_Combined_Raw_2026-08-07.csv"))
