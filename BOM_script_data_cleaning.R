#loading tidyverse

library(tidyverse)

#reading in data files

BOM_stations <- read_csv("Rawdata/BOM_stations.csv")
BOM_data <- read_csv("Rawdata/BOM_data.csv")
view(BOM_data)
view(BOM_stations)

#separate Temp_min_max into two separate columns

BOM_data_sep_temp <- 
separate(BOM_data,Temp_min_max, 
into=c("temp_min", "temp_max"), sep= "/")

#Converting - to NA in temp and Rainfall columns


Bom_data_sep_temp_withNA <- 
BOM_data_sep_temp %>% 
mutate(temp_max = na_if(temp_max,"-")) %>% 
mutate(temp_min = na_if(temp_min,"-")) %>% 
mutate(Rainfall = na_if(Rainfall,"-"))

Bom_data_sep_temp_withNA
BOM_data


#new file with grouped by Station_number

Bom_data_grouped <- 
  Bom_data_sep_temp_withNA %>% group_by(Station_number)

# create new variables removing NA values

Bom_data_remove_NA <- Bom_data_grouped %>% 
  filter(Rainfall!="NA") %>% 
  filter(temp_min!="NA") %>% 
  filter(temp_max!="NA")

#or I can use the following
Bom_data_remove_NA <- 
  filter(Bom_data_grouped, Rainfall!="NA",
         temp_min!="NA", temp_max!="NA")

#new file with grouped by Station_number
Bom_data_grouped<- 
  Bom_data_remove_NA %>% group_by(Station_number)

#Counting number of rows

Bom_data_number_per_station <- 
  summarise(Bom_data_grouped, Days_per_station=n())


view(Bom_data_number_per_station)
View(BOM_data)


#an alternate method from Bill



#bill also overwrites file name instead of








  