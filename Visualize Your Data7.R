library(tidyverse)
library("dplyr")

bill <- read.csv("500_Cities__Local_Data_for_Better_Health.csv")

print(bill)

State_description <- bill %>%
  select(Year, StateAbbr, Category, CityName, Measure, PopulationCount, Data_Value, Low_Confidence_Limit,High_Confidence_Limit)%>%
  filter(StateAbbr =="IL")%>%
  filter(Category =="Health Outcomes")%>%
  filter(Measure =="Current asthma among adults aged >=18 Years")%>%
  filter(Year == 2014)%>%
  mutate(PopulationCount, PopIncidence = Data_Value * PopulationCount * 0.01)%>%
  group_by(CityName, Measure, Year)%>%
  summarise(IncidenceRate=sum(PopIncidence, na.rm = TRUE), Incidence = 100*IncidenceRate/sum(PopulationCount))






View(State_description)
View(bill)
s <- ggplot(State_description,aes(x=CityName,y=Incidence,fill=CityName))
s + ggtitle("Current asthma among adults aged >=18 Years")+ coord_flip()+ geom_col() + scale_fill_manual(values=c("green", "green","purple","green","purple","green","green","purple","green","green","green","green","green","purple","purple","green","purple","green"))
