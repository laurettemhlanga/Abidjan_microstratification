#Merge relevant files for further analysis in Shiny App

source("load_path.R")

extracted_var <- read.csv(file.path(AbidjanDir, "Abidjan Data Variables.csv")) %>% 
  slice(-c(11:15))

abidjan_urban <- st_read(file.path(AbidjanDir, "Abidjan_urban_percentage.geojson")) %>% 
  dplyr::select(NOM, totalArea, urbanArea, urbanPercentage) %>% 
  st_drop_geometry()

#add real estate average price per square feet
avg_hd <- read.csv(file.path(AbidjanDir, "housing_prices.csv")) %>% 
  dplyr::select(-X)

#merge all

all_var <- extracted_var %>% 
  left_join(abidjan_urban, by = c("HealthDistrict" = "NOM")) %>% 
  left_join(avg_hd, by = c("HealthDistrict" = "NOM")) %>% 
  rename(WardName = HealthDistrict)

all_var <- all_var %>% 
  mutate(Urban = case_when(urbanPercentage > 30 ~ 'Urban', TRUE ~ 'Rural'))

#write.csv(all_var, file.path(AbidjanDir, "all_variables.csv"))

all_var_urban <- all_var %>% 
  filter(WardName != "ANYAMA" & WardName != "YOPOUGON-OUEST-SONGON")
write.csv(all_var_urban, file.path(AbidjanDir, "all_variables_urban.csv"))

#Add urban colum and rename columns for shapefile
urban <- all_var %>% 
  dplyr::select(WardName, Urban)

abidjan_shp2 <- abidjan_shp %>% 
  rename(WardName = NOM) %>% 
  left_join(urban, by = "WardName")

#st_write(abidjan_shp2, file.path(AbidjanDir, "Abidjan.shp"))

abidjan_shp3 <-abidjan_shp2 %>% 
  filter(WardName != "ANYAMA" & WardName != "YOPOUGON-OUEST-SONGON")
st_write(abidjan_shp3, file.path(AbidjanDir, "Abidjan_urban.shp"))


# run a regression model to see what drives risk score

all_var <- all_var %>% 
  mutate(rank = case_when(
    WardName == "TREICHVILLE-MARCORY" ~ 1,
    WardName == "ABOBO OUEST" ~ 2,
    WardName == "ABOBO EST" ~ 3,
    WardName == "COCODY BINGERVILLE" ~ 4,
    WardName == "PORT BOUET-VRIDI" ~ 5,
    WardName == "KOUMASSI" ~ 6,
    WardName == "ADJAME-PLATEAU-ATTECOUBE" ~ 7,
    WardName == "ANYAMA" ~ 8,
    WardName == "YOPOUGON-OUEST-SONGON" ~ 9,
    WardName == "YOPOUGON-EST" ~ 10,
    TRUE ~ NA_real_  
  )) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))



model <- lm(rank ~ avg_rainfall + avg_tpr  + meanEVI + mean_WF + mean_price, 
            data = all_var_urban_test)
summary(model)

## Water Frequency Analysis

#urban districts alone

ggplot(all_var_urban_test, aes(x = mean_WF, y = avg_tpr)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title = "Relationship between Water Frequency and TPR (Urban)", 
       x = "Mean WF", 
       y = "TPR") +
  theme_manuscript()


model <- lm(avg_tpr ~ mean_WF-1 , 
            data = all_var_urban_test) 
summary(model)

cor(all_var_urban_test$avg_tpr, all_var_urban_test$mean_WF)


##all districts
ggplot(all_var, aes(x = mean_WF, y = avg_tpr)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  labs(title = "Relationship between Water Frequency and TPR (All Districts)", 
       x = "Mean WF", 
       y = "TPR") +
  theme_manuscript()


model2 <- lm(avg_tpr ~ mean_WF-1 , 
            data = all_var) 
summary(model2)

cor(all_var$avg_tpr, all_var$mean_WF)


#variablilty in rainfall

sd(all_var$avg_rainfall, na.rm = TRUE)

cv <- sd(all_var_urban_test$avg_rainfall, na.rm = TRUE) / 
  mean(all_var_urban_test$avg_rainfall, na.rm = TRUE)
cv


hist(all_var$avg_rainfall, main = "Distribution of Avg Rainfall", 
     xlab = "Avg Rainfall", col = "lightblue", border = "black")

