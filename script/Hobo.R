#Hobo data
library(lubridate)
library(dplyr)
#humidity
GH_humidity <- read.delim("E:/DRYER_greenhouse/resources/HOBO/T.H/serre/UB23-001_One_Serre_experimentation_temp_hum_29102021.csv", header=FALSE, comment.char="#") 

GH_humidity <- GH_humidity %>%
  drop_na() %>%
  dplyr::select(-V1)%>%
    rename(Date=V2, #GMT-03:00
           Temperature = V3, #Temp., °C (LGR S/N: 20020514, SEN S/N: 20020514, LBL: Serre_1)
           Relative_Humidity = V4, #HR, % (LGR S/N: 20020514, SEN S/N: 20020514, LBL: Serre_1)
           Dew_Point = V5, #PtRosée, °C (LGR S/N: 20020514, SEN S/N: 20020514)
           Battery = V6) #Pile, V (LGR S/N: 20020514, SEN S/N: 20020514)
head(GH_humidity)

#start:2021-10-24 22:05:00
#end: 2022-02-05 19:55:00


#remove one suspicious last line (before the battery died)
GH_humidity <- GH_humidity[-8157,]

# Convert non-standard date formats to a standardized format
# Replace "0021-" with "2021-" in the Date column using gsub
GH_humidity$Date <- as.POSIXct(GH_humidity$Date, format = "%m/%d/%Y %H:%M")
GH_humidity$Date <- gsub("0021-", "2021-", GH_humidity$Date)
GH_humidity$Date <- as.POSIXct(GH_humidity$Date, format = "%Y-%m-%d %H:%M")

# Extract hour from Date
GH_humidity$Hour <- as.numeric(format(GH_humidity$Date, "%H"))

# Filter data within the specified date range
start_date <- as.POSIXct("2021-10-24 22:05:00")
#end_date <- as.POSIXct("2022-02-05 19:55:00")
GH_humidity_filtered <- GH_humidity %>%
  filter(Date >= start_date)



# Filter data within the specified time range (6:00 AM to 6:00 PM)
GH_humidity_filtered <- GH_humidity_filtered %>%
  filter(Hour >= 6 & Hour <= 18)


# Calculate statistics
mean_temp <- mean(GH_humidity_filtered$Temperature) #29.48
max_temp <- max(GH_humidity_filtered$Temperature) #36.6
min_temp <-min(GH_humidity_filtered$Temperature) #25.477

mean_humidity <- mean(GH_humidity_filtered$Relative_Humidity) #82.929
max_humidity <- max(GH_humidity_filtered$Relative_Humidity) # 95.342
min_humidity <- min(GH_humidity_filtered$Relative_Humidity) #60.476

################################################
#Light
GH_light <- read.delim("E:/DRYER_greenhouse/resources/HOBO/Sab_pos6_Serre_experimentation02122021.csv", header=FALSE, comment.char="#", sep = ",") 

GH_light <- GH_light %>%
  dplyr::select(-V1)%>%
  rename(Date=V2, #GMT-03:00
         Temperature = V3, #Temp., °C (LGR S/N: 20020514, SEN S/N: 20020514, LBL: Serre_1)
         Lux = V4) #Intensité, Lux (LGR S/N: 20031507, SEN S/N: 20031507, LBL: serre)

head(GH_light)

#date format
GH_light$Date <- as.POSIXct(GH_light$Date, format = "%m/%d/%Y %H:%M")
GH_light$Date <- gsub("0021-", "2021-", GH_light$Date)
GH_light$Date <- as.POSIXct(GH_light$Date, format = "%Y-%m-%d %H:%M")

# Filter data within the specified date range
start_date <- as.POSIXct("2021-10-24 22:05:00")
#end_date <- as.POSIXct("2022-02-05 19:55:00")
GH_light_filtered <- GH_light %>%
  filter(Date >= start_date)

head(GH_light_filtered)
GH_light_daytime <- GH_light_filtered %>% filter(Lux !=0)

# Calculate statistics
mean_temp <- mean(GH_light_daytime$Temperature) #33.4
max_temp <- max(GH_light_daytime$Temperature) # 42.639
min_temp <-min(GH_light_daytime$Temperature) #24.738

# Group by date and calculate the sum of Lux
daily_lux_sum <- GH_light_daytime %>%
  group_by(Date = as.Date(Date)) %>%
  summarise(Lux_Sum = sum(Lux)) %>%
  filter(Date != '2021-10-28') %>%
  filter(Date != '2021-12-02')

# Calculate the percentage of Lux sum compared to external irradiance
external_irradiance <- read.csv("E:/DRYER_greenhouse/resources/HOBO/T.L/dehors/Dehors_30112021.csv", header=FALSE)

external_irradiance <- external_irradiance %>%
  dplyr::select(-V1)%>%
  rename(Date=V2, #GMT-03:00
         Temperature = V3, #Temp., °C (LGR S/N: 20020514, SEN S/N: 20020514, LBL: Serre_1)
         Lux = V4) #Intensité, Lux (LGR S/N: 20031507, SEN S/N: 20031507, LBL: serre)

#remove first two rows
external_irradiance <- external_irradiance[-c(1,2),]

#date format
external_irradiance$Date <- as.POSIXct(external_irradiance$Date, format = "%m/%d/%Y %H:%M")
external_irradiance$Date <- gsub("0021-", "2021-", external_irradiance$Date)
external_irradiance$Date <- as.POSIXct(external_irradiance$Date, format = "%Y-%m-%d %H:%M")

# Filter data within the specified date range
start_date <- as.POSIXct("2021-10-24 22:05:00")
#end_date <- as.POSIXct("2022-02-05 19:55:00")
external_irradiance <- external_irradiance %>%
  filter(Date >= start_date)
head(external_irradiance)

external_irradiance$Lux <- as.numeric(external_irradiance$Lux)
external_irradiance <- external_irradiance %>% filter(Lux !=0.0)

daily_lux_sum_ext <- external_irradiance %>%
  group_by(Date = as.Date(Date)) %>%
  summarise(Lux_Sum_ext = sum(Lux))

#leftjoin
light <- left_join(daily_lux_sum, daily_lux_sum_ext, by=c("Date")) %>% drop_na()

light <- light %>%
  mutate(light_percentage = (Lux_Sum/ Lux_Sum_ext)*100)

light <- light[-c(33,34),]

mean(light$light_percentage) #[1] 33.82818



