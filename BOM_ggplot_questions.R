library(tidyverse)


setwd("C:/Users/rat05a/DATA School FOCUS/Learning_ggplot/")
BOM_data <- read_csv('Rawdata/BOM_data.csv')
BOM_stations <- read_csv('Rawdata/BOM_stations.csv')
BOM_data
BOM_stations



#Tidying data



BOM_data_sep_temp <- 
  separate(BOM_data,Temp_min_max, 
           into=c("temp_min", "temp_max"), sep= "/")


Bomdata9225 <- filter(BOM_data_sep_temp, Station_number=="9225") %>%
  mutate(temp_min = as.numeric(temp_min)) %>% 
  mutate(temp_max = as.numeric(temp_max)) %>% 
  mutate(Rainfall = as.numeric(Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure))



#Question1
#mapping
ggplot(data=Bomdata9225, mapping = aes(x=temp_max, y=temp_min))+
  geom_point(size=0.5, colour="purple", alpha=.5)+
  geom_smooth(method="lm", colour="black")+
  labs(title="Relationship between maximum and minimum temperature date for Perth",
      x="Temperature maximum",
      y="Temperature minimum")
  
  
  roughplot1 <- ggplot(data=Bomdata9225, mapping = aes(x=temp_max, y=temp_min))+
    geom_point(size=0.5, colour="purple", alpha=.5)+
    geom_smooth(method="lm", colour="black")+
    labs(title="Relationship between maximum and minimum temperature for Perth",
         x="Temperature maximum (Degrees Celcius)",
         y="Temperature minimum (Degrees Celcius)")+theme(title = element_text(size=6))


roughplot2 <- ggplot(data=Bomdata9225, mapping = aes(x=temp_max, y=Solar_exposure))+
  geom_point(size=0.5, colour="red", alpha=.5)+
  geom_smooth(method="lm", colour="gray60")+
  labs(title="Relationship between maximum temperature and solar Exposure for Perth",
       x="Temperature maximum (Degrees Celcius)",
       y="Solar exposure")+theme(title = element_text(size=6))

roughplot3 <- ggplot(data=Bomdata9225, mapping = aes(x=temp_max, y=Rainfall))+
  geom_point(size=0.5, colour="green", alpha=.5)+
  geom_smooth(method="lm", colour="blue")+
  labs(title="Relationship between maximum temperature and Rainfall for Perth",
       x="Temperature maximum (Degrees Celcius)",
       y="Rainfall (mm)")+
  theme(title = element_text(size=6))


#Question2

roughplot4 <- ggplot(data=Bomdata9225, mapping = aes(x=temp_max, y=temp_min, 
                                                     size=Rainfall, colour=Solar_exposure))+
  geom_point(alpha=.2)+labs(title="Relationship between max. temp., min. temp., Solar Exposure and Rainfall for Perth",
                                                                   x="Temperature maximum (Degrees Celcius)",
                                                                   y="Temperature minimum (Degrees Celcius)")+
  theme(title = element_text(size=6),legend.text = element_text(size=8), legend.title = element_text(size=8))



#Question3


library(cowplot)
four_plot <- plot_grid(roughplot1, roughplot2, roughplot3, roughplot4)

ggsave(filename="Data/BOM_fig1.png", plot=four_plot, 
       width=36, height=30, dpi=300, units="cm")


#Question 4

#Making BOM_data tidy

Bom_data2 <- 
  BOM_data_sep_temp %>% 
  mutate(temp_max = na_if(temp_max,"-")) %>% 
  mutate(temp_min = na_if(temp_min,"-")) %>% 
  mutate(Rainfall = na_if(Rainfall,"-"))%>%
  filter(Rainfall!="NA") %>% 
  filter(temp_min!="NA") %>% 
  filter(temp_max!="NA")%>% 
  mutate(Rainfall = as.numeric(Rainfall))

#Making BOM_stations tidy

BOM_stations_tidy <- BOM_stations %>% 
  gather(key = Station_number, value = value, 2:21) %>%  
spread( key = info, value = value) %>%  mutate(Station_number = as.numeric(Station_number))

#joining data and stations

BOM <- left_join(Bom_data2, BOM_stations_tidy, by = "Station_number")

#mean rainfall

BOM_rainfall <- BOM %>% group_by(Station_number,name, Month, Year,state) %>% summarise(total_monthly_rainfall=sum(Rainfall))
BOM_mean_monthly_rainfall <- BOM_rainfall %>% group_by(Station_number,name, Month,state) %>% summarise(mean_monthly_rainfall=mean(total_monthly_rainfall))

BOM_text <- BOM_mean_monthly_rainfall %>% filter(Month==1)

#plots.



rough_rainfall_plot <- ggplot(data=BOM_mean_monthly_rainfall, mapping=aes(x=Month, y=mean_monthly_rainfall, colour=name))+
  geom_line(size=1)+scale_colour_discrete()+ 
  facet_wrap(~state)
  
  
 

rainfall_plot <- rough_rainfall_plot+labs(title="Average monthly rainfall per station", 
                         y="Average Monthly Rainfall (mm)",
                         x="Month",
                         colour="Station location",
                         caption="SOURCE:BOM meterological data")+
  theme_bw()+
  scale_x_continuous(breaks = c(1, 4, 7,10),
  label = c("Jan", "Apr", "July","Oct"))

ggsave(filename="Data/BOM_fig2.png", plot=rainfall_plot, 
       width=36, height=30, dpi=300, units="cm")





