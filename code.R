State_description <- bill %>%
  select(StateAbbr, Category, CityName, Measure, PopulationCount, Data_Value, Low_Confidence_Limit,High_Confidence_Limit)%>%
  filter(StateAbbr =="IL")%>%
  filter(Category =="Health Outcomes")%>%
  group_by(CityName, Measure)
