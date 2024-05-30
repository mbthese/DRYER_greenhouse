#Precipitation

library(readxl)
library(data.table)
library(dplyr)
Paracou <- read_excel("./resources/Precipitation/Gx_pluie-2004-2020 jour_Marion.xlsx", 
                                             sheet = "donnees jour")

Pluie <- read_excel("./resources/Precipitation/For_graph.xlsx", 
                    sheet = "Données") #sinnamary
names(Pluie)
dim(Pluie)
View(Pluie)

set.seed(123)  # Setting seed for reproducibility

# Filter data for the dry season (August to November)
dry_season_data <- Pluie %>%
  group_by(Annee) %>%
  mutate(Day_Count = row_number())

dry_season_data <- dry_season_data  %>%
  dplyr::filter(Mois %in% c("08", "09", "10", "11"))%>%
  dplyr::filter(1955 <= Annee & 2019 >= Annee)

dry_season_data <- dry_season_data %>%
  dplyr::filter(214 <= `Jour 365` & 335 >= `Jour 365`)
 # dplyr::filter(214 <= Day_Count & 335 >= Day_Count) #Bonal et al 2008

#Calculate the annual maximum number of consecutive days without rainfall for each dry season
result <- dry_season_data %>%
  group_by(Annee) %>%
  summarise(Max_Consecutive_Days = max(`Jour consecutifs pluie <= 0,2`), 
            sd_consecutive_days = sd(`Jour consecutifs pluie <= 0,2`))
            

#Calculate the mean and standard deviation of the maximum consecutive days for each year
mean_max_consecutive_days <- mean(result$Max_Consecutive_Days) #20.4
sd_max_consecutive_days <- mean(result$sd_consecutive_days)#5.2

ggplot(result) +
    aes(x = as.factor(Annee), y = Max_Consecutive_Days) +
    geom_bar(stat = "identity", fill = "#DAAF4B", alpha = 0.7) +
    ylab("Maximum Consecutive Days without rainfall") +
  xlab("Year")+
  geom_hline(yintercept = 21, col = "#8CB425") +
  geom_hline(yintercept = 27, col = "#ECE320") +
  geom_hline(yintercept = 71, col = "#DF5220") +
  geom_text(aes(label = "Current"), y = 21, x = 0, vjust = 1, hjust = -13, col = "#8CB425", fontface = "bold", size=5) +
  geom_text(aes(label = "Projected"), y = 27, x = 0, vjust = 1, hjust = -11, col = "#ECE320", fontface = "bold", size=5) +
  geom_text(aes(label = "Extreme"), y = 71, x = 0, vjust = 1, hjust = -13, col = "#DF5220", fontface = "bold", size=5) +
    scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 10 == 0, x, ""))+
  theme_classic(base_size = 20)

