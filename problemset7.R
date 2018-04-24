#Rohan Gupta
#April 26, 2018
#Pol Sci 4626 Problem Set 7

#2.
#Computed number of crimes per day by type of crime
March2018<-read.csv("/Users/rohangupta/Documents/WUSTL/SP2018/Pol Sci/PS7/March2018.CSV")
numberByType<-March2018%>%
  group_by(Description)%>% 
  summarise(count=n()
  )%>%
  filter(count>20)
View(numberByType)
#"Leaving scene of accident" was the type of crime that happened the most in March (464 times).

#3.
#Computed number of crimes per day by neighborhood
numberByNeighborhood<-March2018%>%
  group_by(Neighborhood)%>% 
  summarise(count=n()
  )%>%
  filter(count>20)
View(numberByNeighborhood)
#Neighborhood 35 has the most number of crimes (305).

#4.
#Computed proportion of crime related to robbery by district
March2018<-mutate(March2018, robbery = (grepl("robbery",Description,T)))
proportionByDistrict<-March2018%>%
  group_by(District,robbery)%>% 
  summarise(proportion=n()
  )%>%
  filter(proportion>0)
View(proportionByDistrict)
#District 5 has the largest proportion of crime related to robbery (0.03986711).

#5.
#Visualized changes of all types of crime over time using ggplot2
#Wrote appropriate labels and titles
March2018%>% 
  mutate(
    year = as.numeric(substr(DateOccur,7,10)),
    month = as.numeric(substr(DateOccur,1,2)),
    day = as.numeric(substr(DateOccur,4,5)),
    hour = as.numeric(substr(DateOccur,12,13)),
    minute = as.numeric(substr(DateOccur,15,16)),
    time = year + month / 12 + day / 365 + hour / 8760 + minute / 525600
  )%>% 
  ggplot(mapping = aes(time)) + 
  geom_freqpoly(mapping = aes(colour = Description), binwidth = 4)

#6.
#Visualized changes of all types of crime over time by district using ggplot2
#Chose different color to indicate each district
#Wrote appropriate legend, labels and titles
numberByDistrict<-March2018%>%
  group_by(District,robbery,DateOccur)%>% 
  summarise(count=n()
  )
numberByDistrict%>% 
  mutate(
    year = as.numeric(substr(DateOccur,7,10)),
    month = as.numeric(substr(DateOccur,1,2)),
    day = as.numeric(substr(DateOccur,4,5)),
    hour = as.numeric(substr(DateOccur,12,13)),
    minute = as.numeric(substr(DateOccur,15,16)),
    time = year + month / 12 + day / 365 + hour / 8760 + minute / 525600
  )%>% 
  ggplot(mapping = aes(time)) + 
  geom_freqpoly(mapping = aes(colour = robbery), binwidth = 4)