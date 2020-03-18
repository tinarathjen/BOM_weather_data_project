#loading tidyverse

library(tidyverse)

#reading in data files

BOM_stations <- read_csv("Rawdata/BOM_stations.csv")
BOM_data <- read_csv("Rawdata/BOM_data.csv")
view(BOM_data)
view(BOM_stations)


#Question 1

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



# create new variables removing NA values

Bom_data_remove_NA <- Bom_data_grouped %>% 
  filter(Rainfall!="NA") %>% 
  filter(temp_min!="NA") %>% 
  filter(temp_max!="NA")

#or I can use the following
Bom_data_remove_NA <- 
  filter(Bom_data_sep_temp_withNA, Rainfall!="NA",
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
#Very elegant
#Bill also overwrites file name instead of 
#creating new variable


BOM_Data <- read_csv("Data/Raw_Data/BOM_data.csv")

separate(BOM_Data, Temp_min_max, into = c("T_Min", "T_Max"), sep = "/") -> BOM_Data 
BOM_Data %>%
  mutate(T_Min = as.numeric(T_Min)) %>% 
  mutate(T_Max = as.numeric(T_Max)) %>% 
  mutate(Rainfall = as.numeric(Rainfall))-> BOM_Data

BOM_Data %>% 
  group_by(Station_number) %>%
  summarise(MinDays = sum(!is.na(T_Min)),
            MaxDays = sum(!is.na(T_Max)),
            RainyDays = sum(!is.na(Rainfall)))


#bill also overwrites file name instead of creating new variable


#Question 2
#Which month saw the lowest average daily 
#temperature difference?

#separate Temp_min_max into two separate columns
#(same as question 1)

#separating Min and max temps
Q2_answer


#An attempt at a more elegant solution (not working yet)
Answer_Q2 <- BOM_data %>%  
  separate(Temp_min_max, 
           into=c("temp_min", "temp_max"), sep= "/")%>% 
  c 
  mutate(temp_max = as.numeric(temp_max))  %>% 
  filter(temp_min!="NA") %>% 
  filter(temp_max!="NA") %>% 
  mutate(temp_diff =temp_max-temp_min) %>% 
  group_by(Month) %>% 
  summarise(mean_temp_diff = mean(temp_diff)) %>% 
arrange(BOM_data_mean_temp_diff, mean_temp_diff)



#Day6
# Question 3 Which state saw the lowest 
#average daily temperature difference?

#Two step process to get a file that has the stations as the first column
# Step 1 is to make a long file
#Step 2 is to amke a wide file but 
#use the station_id as the anchor
View(BOM_stations)

BOM_stations_long <- gather(BOM_stations, 
key="Station_number", value="value",2:21)

view(BOM_stations_long)

BOM_stations_new <- spread(BOM_stations_long,
                            key="info", value="value")
#Changing Station_number in file to numeric

BOM_stations_new <- 
  mutate(BOM_stations_new, Station_number=as.numeric(Station_number))

#merging
BOM_merge <- full_join(BOM_data_numeric_minus_NA,
                       BOM_stations_new, "Station_number")

view(BOM_merge)

#Daily change in temperature

BOM_merge2 <- mutate(BOM_merge,temp_diff =temp_max-temp_min)


BOM_merge3 <-  group_by(BOM_merge2,state)

BOM_merge4 <- summarise(BOM_merge3,mean_temp_diff = mean(temp_diff)) 

#trying the same using pipes

BOM_merge<- gather(BOM_stations, key="Station_number", 
                       value="value",2:21) %>% 
spread(key="info", value="value") %>% 
  mutate(Station_number=as.numeric(Station_number)) 

Answer3 <- full_join(BOM_data_numeric_minus_NA,BOM_stations_new, "Station_number") %>% 
 mutate(temp_diff =temp_max-temp_min) %>% 
  group_by(state) %>% 
summarise(mean_temp_diff = mean(temp_diff)) 

#Question4
#Does the westmost (lowest longitude) or eastmost (highest longitude) weather station 
#in our dataset have a higher average solar exposure?

  