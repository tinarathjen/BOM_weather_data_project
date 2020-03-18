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
BOM_data_sep_temp <- 
  separate(BOM_data,Temp_min_max, 
           into=c("temp_min", "temp_max"), sep= "/")

#changing every temp to a numeric character or NA
BOM_data_numeric <- BOM_data_sep_temp %>% 
  mutate(temp_min = as.numeric(temp_min)) %>% 
  mutate(temp_max = as.numeric(temp_max)) 

#removing rows with an NA in max or min temp
BOM_data_numeric_minus_NA <- BOM_data_numeric %>% 
  filter(temp_min!="NA") %>% 
  filter(temp_max!="NA")

BOM_data_daily_temp_diff <- BOM_data_numeric_minus_NA %>% 
  mutate(temp_diff =temp_max-temp_min)

BOM_grouped_month <- group_by(BOM_data_daily_temp_diff,Month)

BOM_data_mean_temp_diff <- 
  summarise(BOM_grouped_month,
            mean_temp_diff = mean(temp_diff))

Q2_answer <- arrange(BOM_data_mean_temp_diff, mean_temp_diff)
 



#An attempt at a more elegant solution (not working yet)
BOM_data2<- BOM_data %>%  
  separate(Temp_min_max, 
           into=c("temp_min", "temp_max"), sep= "/") %>% 
  mutate(temp_max = as.numeric(temp_max))  %>% 
  mutate(temp_min = as.numeric(temp_min)) %>% 
  filter(temp_min!="NA") %>% 
  filter(temp_max!="NA") %>% 
  mutate(temp_diff =temp_max-temp_min) %>% 
  group_by(Month) %>% 
  summarise(mean_temp_diff = mean(temp_diff)) %>% 
arrange(BOM_data_mean_temp_diff, mean_temp_diff)



BOM_data2

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
#Does the westmost (lowest longitude) or 
#eastmost (highest longitude) weather station 
#in our dataset have a higher average solar exposure?
#merge the data sets again
BOM_merge <- full_join(BOM_data_numeric_minus_NA,BOM_stations_new, "Station_number") 
  
#changes Solar_exposure to numeric form
BOM_merge2 <- 
  mutate(Solar_exposure=as.numeric(Solar_exposure))
#removing rows with solar exposure as NA
BOM_merge3 <- filter(BOM_merge2, Solar_exposure!="NA")
#group by longitude
BOM_merge4 <- group_by(BOM_merge3,lon)
#calculate mean Solar exposure
BOM_merge5 <- summarise(BOM_merge4, mean_Solar_exposure=mean(Solar_exposure))
#arrange according to longitude in ascending order
BOM_merge6 <- arrange(BOM_merge5, lon)
#view answers, lowest lon is West, highest lon is East
View(BOM_merge6)
head(BOM_merge6,1)
tail(BOM_merge6,1)

#or

Q4_answer <- full_join(BOM_data_numeric_minus_NA,
                       BOM_stations_new, "Station_number") %>% 
  mutate(Solar_exposure=as.numeric(Solar_exposure)) %>% 
filter( Solar_exposure!="NA") %>% group_by(lon) %>% 
summarise(mean_Solar_exposure=mean(Solar_exposure)) %>% 
arrange(lon)
Q4_answer
head(Q4_answer,1)
tail(Q4_answer,1)
