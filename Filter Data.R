library(tidyverse)
library("dplyr")

bill <- read.csv("500_Cities__Local_Data_for_Better_Health.csv")

print(bill)

State_description <- bill %>%
  select(Year, StateAbbr, StateDesc, CityName,PopulationCount, Category, Measure, Short_Question_Text, Data_Value_Type, Data_Value, Low_Confidence_Limit,High_Confidence_Limit)%>%
  filter(StateAbbr =="IL")%>%
  filter(Category =="Health Outcomes")%>%
  arrange(desc(PopulationCount))%>%
  group_by(Measure,CityName,Year)%>%
  summarise(sum=sum(PopulationCount)).
