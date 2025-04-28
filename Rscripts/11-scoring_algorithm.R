#### PRELIMINARY SCORING ALGORITHM

rm(list=ls())
source("load_path.R")

# Colour palettes for the map 
palettes_00 <- rev(RColorBrewer::brewer.pal(5, "OrRd"))
palettes <- list(rev(RColorBrewer::brewer.pal(5, "RdYlBu")))
class_colors <- c(
  "[0,0.2]" = "#fde0dd",  
  "(0.2,0.4]" = "#fa9fb5",
  "(0.4,0.6]" = "#f768a1",
  "(0.6,0.8]" = "#c51b8a",
  "(0.8,1]" = "#7a0177"   
)

# Data files 
scoring_dataset <- read.csv(file.path(AbidjanDir, "Abidjan Data Variables.csv")) %>% 
  dplyr::slice(-c(11:15))

# Abidjan 10 districts shapefile
abidjan_shp <- df_abidjan1 %>% 
  dplyr::filter(!NOM %in% c("JACQUEVILLE", "DABOU", "ALEPE", "GRAND-BASSAM", "AGBOVILLE"))

#st_write(abidjan_shp, file.path(AbidjanDir, "10_districts_shapefile", "Abidjan.shp"))

# Model permutations
model <- c("avg_tpr_normal_score + mean_WF_normal_score_normal_score",
                      "meanEVI_normal_score + mean_WF_normal_score",
                      "Slum_Count_normal_score + mean_WF_normal_score",
                      "avg_rainfall_normal_score + mean_WF_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score",
                      "avg_tpr_normal_score + Slum_Count_normal_score",
                      "avg_tpr_normal_score + avg_rainfall_normal_score",
                      "meanEVI_normal_score + Slum_Count_normal_score",
                      "meanEVI_normal_score + avg_rainfall_normal_score",
                      "Slum_Count_normal_score + avg_rainfall_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score + mean_WF_normal_score",
                      "avg_tpr_normal_score + Slum_Count_normal_score + mean_WF_normal_score",
                      "avg_tpr_normal_score + avg_rainfall_normal_score + mean_WF_normal_score",
                      "meanEVI_normal_score + Slum_Count_normal_score + mean_WF_normal_score",
                      "meanEVI_normal_score + avg_rainfall_normal_score + mean_WF_normal_score",
                      "Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score + avg_rainfall_normal_score",
                      "avg_tpr_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score",
                      "meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score + mean_WF_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score + avg_rainfall_normal_score + mean_WF_normal_score",
                      "avg_tpr_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score",
                      "meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score",
                      "avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score")

# Variable names 
new_names <- c(
  "meanEVI_normal_score" = "Enhanced Vegetation Index",
  "Slum_Count_normal_score" = "Slum Concentration",
  "avg_rainfall_normal_score" = "Rainfall",
  "avg_tpr_normal_score" = "Test Positivity Rate",
  "mean_WF_normal_score" = "Water Frequency"
)


# Assign mean values to cells with NA values (optional)
column_means <- colMeans(scoring_dataset [, -c(1, 2)], na.rm = TRUE)
print(column_means)

for (col in names(scoring_dataset)) {
  if (col %in% names(column_means)) {
    scoring_dataset[[col]] <- ifelse(is.na(scoring_dataset[[col]]), column_means[[col]], scoring_dataset[[col]])
  }
}

# Reshape non-normalized data for plotting (optional)
plotting_data1 <- scoring_dataset %>%
  dplyr::select(HealthDistrict,
                meanEVI,
                avg_rainfall,
                avg_tpr,
                Slum_Count,
                mean_WF) %>%
  reshape::melt(id.vars = c("HealthDistrict")) %>%
  #mutate(class = cut(value, seq(0,1, length.out = 6), include.lowest = T)) %>%
  mutate(class = cut(value, seq(0,5037.5968050, length.out =6), include.lowest = T)) %>%
  inner_join(abidjan_shp, by = c("HealthDistrict" = "NOM"))

# Reshape non-normalized data for plotting
plotting_data2 <- scoring_dataset %>%
  dplyr::select(HealthDistrict,
                meanEVI,
                avg_rainfall,
                avg_tpr,
                Slum_Count,
                mean_WF) %>%
  reshape::melt(id.vars = c("HealthDistrict")) %>%
  mutate(class = case_when(
    variable == "meanEVI" ~ cut(value, seq(610.5935, 5037.5968, length.out = 6), include.lowest = TRUE),
    variable == "avg_rainfall" ~ cut(value, seq(4.674972, 4.841463, length.out = 6), include.lowest = TRUE),
    variable == "avg_tpr" ~ cut(value, seq(0.4087708, 0.7311113, length.out = 6), include.lowest = TRUE),
    variable == "Slum_Count" ~ cut(value, seq(1, 23, length.out = 6), include.lowest = TRUE),
    variable == "mean_WF" ~ cut(value, seq(0, 13.0921409, length.out = 6), include.lowest = TRUE),
    TRUE ~ NA_character_)) %>%
  inner_join(abidjan_shp, by = c("HealthDistrict" = "NOM"))

plotting_data4 <- scoring_dataset %>%
  dplyr::select(HealthDistrict,
                meanEVI,
                avg_rainfall,
                avg_tpr,
                Slum_Count,
                mean_WF) %>%
  reshape::melt(id.vars = c("HealthDistrict")) %>%
  mutate(class = case_when(
    variable == "meanEVI" ~ cut(value, seq(0, 5038, length.out = 6), include.lowest = TRUE, include.highest = T),
    variable == "avg_rainfall" ~ cut(value, seq(0, 4.9, length.out = 6), include.lowest = TRUE, include.highest = T),
    variable == "avg_tpr" ~ cut(value, seq(0, 0.75, length.out = 6), include.lowest = TRUE, include.highest = T),
    variable == "Slum_Count" ~ cut(value, seq(0, 23, length.out = 6), include.lowest = TRUE, include.highest = T),
    variable == "mean_WF" ~ cut(value, seq(0, 13.1, length.out = 6), include.lowest = TRUE, include.highest = T),
    TRUE ~ NA_character_)) %>%
  inner_join(abidjan_shp, by = c("HealthDistrict" = "NOM"))


#############  NORMALIZATION ##################################################

# Normalize function
normalize <- function(x) {
  ifelse(is.na(x), NA, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# create dataframe with normalized values
df_normalized <- scoring_dataset %>%
  mutate(
    meanEVI_normal_score = normalize(meanEVI),
    avg_rainfall_normal_score = normalize(avg_rainfall),
    avg_tpr_normal_score = normalize(avg_tpr),
    Slum_Count_normal_score = normalize(Slum_Count),
    mean_WF_normal_score = normalize(mean_WF)
  )

# Individual Normalized Variable Maps

dfnormalized_sf <- left_join(df_normalized, abidjan_shp, by = c("HealthDistrict" = "NOM"))

variables <- c("meanEVI_normal_score", "avg_rainfall_normal_score", "avg_tpr_normal_score", "Slum_Count_normal_score", "mean_WF_normal_score")

# Colour scales
color_scales <- list(
  "meanEVI_normal_score" = scale_fill_gradient(name = "Enhanced Vegetation Index", low = "#F6E0B3", high = "darkgreen"),
  "avg_rainfall_normal_score" = scale_fill_gradient(name = "Rainfall", low = "lightblue", high = "darkblue"),
  "avg_tpr_normal_score" = scale_fill_gradient(name = "Malaria Test Positivity Rate (TPR)", low = "pink", high = "maroon"),
  "mean_WF_normal_score" = scale_fill_gradient(name = "Water Frequency", low = "grey", high = "blue"),
  "Slum_Count_normal_score" = scale_fill_gradient(name = "Slum Count", low = "lightyellow", high = "brown")
)

for (var in variables) {
  print(
    ggplot() +
      geom_sf(data = dfnormalized_sf, aes(geometry = geometry, fill = .data[[var]])) +
      color_scales[[var]] +
      labs(title = paste(new_names[var], "with Normalized Values"), x = "", y = "") +  # Use new descriptive names
      map_theme()
  )
}


# Scoring and Manipulations

# Data selection and model creation
scoring_dataset2 <- df_normalized %>%
  dplyr::select(ID,
                HealthDistrict,
                meanEVI_normal_score,
                avg_rainfall_normal_score,
                avg_tpr_normal_score,
                Slum_Count_normal_score,
                mean_WF_normal_score
                )%>%
  mutate (
  model01 = avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score,
  model02 = avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score,
  model03 = meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score,
  model04 = avg_tpr_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score,
  model05 = avg_tpr_normal_score + meanEVI_normal_score + avg_rainfall_normal_score + mean_WF_normal_score,
  model06 = avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score + mean_WF_normal_score,
  model07 = meanEVI_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score,
  model08 = avg_tpr_normal_score + Slum_Count_normal_score + avg_rainfall_normal_score,
  model09 = avg_tpr_normal_score + meanEVI_normal_score + avg_rainfall_normal_score,
  model10 = avg_tpr_normal_score + meanEVI_normal_score + Slum_Count_normal_score,
  model11 = Slum_Count_normal_score + avg_rainfall_normal_score + mean_WF_normal_score,
  model12 = meanEVI_normal_score + avg_rainfall_normal_score + mean_WF_normal_score,
  model13 = meanEVI_normal_score + Slum_Count_normal_score + mean_WF_normal_score,
  model14 = avg_tpr_normal_score + avg_rainfall_normal_score + mean_WF_normal_score,
  model15 = avg_tpr_normal_score + Slum_Count_normal_score + mean_WF_normal_score,
  model16 = avg_tpr_normal_score + meanEVI_normal_score + mean_WF_normal_score,
  model17 = Slum_Count_normal_score + avg_rainfall_normal_score,
  model18 = meanEVI_normal_score + avg_rainfall_normal_score,
  model19 = meanEVI_normal_score + Slum_Count_normal_score,
  model20 = avg_tpr_normal_score + avg_rainfall_normal_score,
  model21 = avg_tpr_normal_score + Slum_Count_normal_score,
  model22 = avg_tpr_normal_score + meanEVI_normal_score,
  model23 = avg_rainfall_normal_score + mean_WF_normal_score,
  model24 = Slum_Count_normal_score + mean_WF_normal_score,
  model25 = meanEVI_normal_score + mean_WF_normal_score,
  model26 = avg_tpr_normal_score + mean_WF_normal_score
)

############ RESHAPE AND PLOT##################

# Reshape normalized values for plotting
plotting_scoring_data <- scoring_dataset2 %>%
  dplyr::select(HealthDistrict,
         meanEVI_normal_score,
         avg_rainfall_normal_score,
         avg_tpr_normal_score,
         Slum_Count_normal_score,
         mean_WF_normal_score) %>%
  reshape::melt(id.vars = c("HealthDistrict")) %>%
  mutate(class = cut(value, seq(0,1, length.out = 6), include.lowest = T)) %>%
  inner_join(abidjan_shp, by = c("HealthDistrict" = "NOM"))

# Combined normalized variable maps
ggplot(data = abidjan_shp) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = plotting_scoring_data,
          aes(geometry = geometry, fill = value)) +
  facet_wrap(~variable, labeller = labeller(variable = new_names)) +
  #scale_fill_continuous(drop=FALSE, name="", type = gradient_color("red"))+ edit scale colour
  labs(subtitle = '', fill = "") +
  theme(panel.background = element_blank(), size = 20) +
  theme_void()+
  map_theme()

# Risk map by normalized covariates

ggplot(data = abidjan_shp) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = plotting_scoring_data,
          aes(geometry = geometry, fill = class), color = "gray") +
  facet_wrap(~variable, labeller = labeller(variable = new_names)) +
  #scale_fill_discrete(drop=FALSE, name="Class(Increasing risk)", type = palettes_00)+
  scale_fill_manual(drop=FALSE, 
                     name="Class (Increasing risk)", 
                     values = rev(palettes_00)) +  # Reverses the color scale
  labs(title = 'Malaria Risk Map by Normalized Variables', fill = "") +
  theme(panel.background = element_blank(), size = 20) +
  theme_void() +
  map_theme()


# Full Risk Map
plottingdata <- scoring_dataset2 %>%
  dplyr::select(HealthDistrict, model01:model26 ) %>% ##change to model01:model26
  reshape2::melt(id.vars = c("HealthDistrict")) %>%
  inner_join(abidjan_shp, by = c("HealthDistrict" = "NOM")) %>%
  group_by(variable) %>%
  mutate(new_value = (value - min(value))/(max(value) - min(value)),
         class = cut(new_value, seq(0, 1, 0.2), include.lowest = T)) %>%
  arrange(value) %>%
  mutate(rank = 1:n())

# Health district ranking, model
plottingdata_filtered <- plottingdata %>%
  filter(variable == "model01")

saveRDS(plottingdata_filtered, file = "plotting_data.rds")


class_labels <- c(
  "[0,0.2]" = "Low Risk",
  "(0.2,0.4]" = "Moderate-Low Risk",
  "(0.4,0.6]" = "Moderate Risk",
  "(0.6,0.8]" = "Moderate-High Risk",
  "(0.8,1]" = "High Risk"
)

ggplot() +
  geom_sf(data = plottingdata_filtered, aes(geometry = geometry, fill = class)) +
  geom_text_repel(data = plottingdata_filtered,
                  aes(label = rank, geometry = geometry), color = 'black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3, force = 1) +
  geom_text_repel(data = plottingdata_filtered %>%
                    filter(rank %in% c(1, 10)),
                  aes(label=str_to_sentence(HealthDistrict), geometry=geometry), color ='black',
                   stat = "sf_coordinates", min.segment.length = 0, size = 3, force = 1)+
  scale_fill_manual(values = class_colors, 
                    name = "Risk Classification", 
                    labels = class_labels) +
  labs(title = "Malaria Risk Profile in Abidjan", fill = "Risk Level",
       caption = "Numbers indicate rank in composite score") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  theme_void() +
  map_theme()

ggsave(paste0(PlotsDir,"/", "Full Risk Map_", Sys.Date(), '_.png'),  width = 8, height =5)





###PERFECT





###################PLOTS#####################################################

###### ECDF 
###pre normalization
ggplot(data = scoring_dataset, aes(x = meanEVI))+ ##change variable of interest
  stat_ecdf(geom = "step", color = "brown", linewidth = 1)+
  #facet_wrap(~variable, labeller = labeller(variable = new_names), scales = "free") + 
  labs(title = "EVI Before Normalization",
       x = "")+
  theme_manuscript() +
  theme(panel.border = element_blank())

### post normalization
# ggplot(data = plotting_scoring_data, aes(x = value))+
#   stat_ecdf(geom = "step", color = "brown", linewidth = 1)+
#   facet_wrap(~variable, labeller = labeller(variable = new_names), scales = "free") + 
#   labs(title = "After Normalization",
#        x = "")+
#   theme_manuscript() +
#   theme(panel.border = element_blank())

ggplot(data = df_normalized, aes(x = mean_WF_normal_score ))+ ##change variable of interest
  stat_ecdf(geom = "step", color = "brown", linewidth = 1)+
  #facet_wrap(~variable, labeller = labeller(variable = new_names), scales = "free") + 
  labs(title = "Water Frequency After Normalization",
       x = "normalized WF score")+
  theme_manuscript() +
  theme(panel.border = element_blank())


###side by side comparison (pre vs post normalization for each variable)
library(gridExtra)
plot1 <- ggplot(data = scoring_dataset, aes(x = mean_WF))+ ##change variable of interest
  stat_ecdf(geom = "step", color = "brown", linewidth = 1)+
  labs(title = "Water Frequency Before Normalization",
       x = "water frequency")+
  theme_manuscript() +
  theme(panel.border = element_blank())

filtered_data <- plotting_scoring_data %>%
  filter(variable == "mean_WF_normal_score") ##change variable of interest
plot2 <- ggplot(data = filtered_data, aes(x = value))+
  stat_ecdf(geom = "step", color = "brown", linewidth = 1)+
  labs(title = "WF After Normalization",
       x = "normalized WF")+
  theme_manuscript() +
  theme(panel.border = element_blank())

grid.arrange(plot1, plot2, ncol=2)


################ MODEL RISK MAPS #############################################
palettes <- list(RColorBrewer::brewer.pal(5, "YlOrRd"))

# ranking with normalized variables, ranking 2
ggplot(data = abidjan_shp)+
  geom_sf(color = "black", fill = "white")+
  geom_sf(data = plottingdata, aes(geometry = geometry, fill = class))+  #class is NA, when there are NA values
  geom_sf_text(data = plottingdata, aes(geometry = geometry, label =  rank), size = 3 )+ 
  facet_wrap(~variable, labeller = label_parsed, ncol = 5) +
  scale_fill_discrete(drop=FALSE, name="Rank(Increasing Risk)", type = palettes_00)+
  #scale_fill_manual(values = palettes)+
  labs(subtitle='', title='Health District Ranking of Malaria Risks',
       fill = "Rank(Increasing Risk)")+
  theme(panel.background = element_blank(), size = 20)+
  theme_void()+
  map_theme()


###ranking with normalized values- 3
#plot1 <-
  ggplot(data = abidjan_shp)+
  geom_sf(color = "black", fill = "white")+
  geom_sf(data = plottingdata, aes(geometry = geometry, fill = rank))+  #class is NA, when there are NA values
  geom_sf_text(data = plottingdata, aes(geometry = geometry, label =  rank), size = 3 )+ 
  facet_wrap(~variable, labeller = label_parsed, ncol = 3) +
  scale_fill_gradient(low = palettes_00[1], high = palettes_00[length(palettes_00)], 
                      name="Rank(Increasing Risk)")+
    # labs(title = "Health District Ranking of Malaria Risks",
    #      caption = "model01 = TPR + EVI + SC + RF + WF \nmodel02 = TPR + EVI + SC + RF \nmodel03 = EVI + SC + RF + WF \n model04 = TPR + SC + RF + WF \nmodel05 = TPR + EVI + RF + WF \nmodel06 = TPR + EVI + SC + WF \nmodel07 = EVI + SC + RF \nmodel08 = TPR + SC + RF \nmodel09 = TPR + EVI + RF \nmodel10 = TPR + EVI + SC \nWhere TPR= test positivity rate, EVI = mean enhanced vegetation index, RF = rainfall, WF= water frequency and SC = slum count
    #      ", 
    #      fill = "Rank (Increasing Risk)") +
#     labs(title = "Health District Ranking of Malaria Risks",
#          caption = "model11 = SC + RF + WF 
# model12 = EVI + RF + WF 
# model13 = EVI + SC + WF 
# model14 = TPR + RF + WF 
# model15 = TPR + SC + WF 
# model16 = TPR + EVI + WF 
# model17 = SC + RF
# model18 = EVI + RF
# model19 = EVI + SC
# model20 = TPR + RF
# Where TPR= test positivity rate, EVI = mean enhanced vegetation index, RF = rainfall, WF= water frequency and SC = slum count", 
#          fill = "Rank (Increasing Risk)") +
  labs(title = "Health District Ranking of Malaria Risks",
                caption = "model21 = TPR + SC
model22 = TPR + EVI
model23 = RF + WF 
model24 = SC + WF 
model25 = EVI + WF 
model26 = TPR + WF 
Where TPR= test positivity rate, EVI = mean enhanced vegetation index, RF = rainfall, WF= water frequency and SC = slum count",
                fill = "Rank (Increasing Risk)") +
  
    theme(panel.background = element_blank(), 
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  #theme(panel.background = element_blank(), size = 20)+
  theme_void()+
  map_theme()
  





######################## VARIABLE MODEL MAPS
###MAPS with NA values (no extrapolation), remember to run first, when mean hasn't been calculated
##non normalized
plot1 <- ggplot()+
  geom_sf(data = plotting_data1, aes(geometry=geometry, fill =class), color = "gray")+
  facet_wrap(~variable, labeller = labeller(variable = new_names))+
  scale_fill_discrete(drop = FALSE, name = "Class(Increased values)", type = palettes_00)+
  labs(title = "Malaria risk with non-normalized variables", fill = "")+
  theme(panel.background = element_blank(), size = 20) +
  theme_void()+
  map_theme()

## normalized variables
plot2 <- ggplot()+
  geom_sf(data = plotting_scoring_data, aes(geometry=geometry, fill=class), color = "gray")+
  facet_wrap(~variable, labeller = labeller(variable = new_names))+
  scale_fill_discrete(drop = FALSE, name = "Class(Increased values)", type = palettes_00)+
  labs(title = "Malaria risk with normalized variables", fill = "")+
  theme(panel.background = element_blank(), size = 20) +
  theme_void()+
  map_theme()


#### with extrapolated values
#non-normalized
plot3 <-  
  ggplot()+
  geom_sf(data = plotting_data1, aes(geometry=geometry, fill =value), color = "gray")+ #change to class
  facet_wrap(~variable, labeller = labeller(variable = new_names))+
  scale_fill_continuous(name="Value(Increasing Risk)", low = "#F6E0b3", high = "maroon")+
 # scale_fill_discrete(drop = FALSE, name = "Class(Increasing values)", type = palettes_00)+
  labs(title = "Malaria Risk Map with non-normalized variables", fill = "")+
  theme(panel.background = element_blank(), size = 20) +
  theme_void()+
  map_theme()
  
#normalized
plot4 <- 
  ggplot()+
  geom_sf(data = plotting_scoring_data, aes(geometry=geometry, fill=value), color = "gray")+
  facet_wrap(~variable, labeller = labeller(variable = new_names))+
  scale_fill_continuous(name="Value(Increasing Risk)", low = "#F6E0b3", high = "maroon")+
  #scale_fill_discrete(drop = FALSE, name = "Class(Increasing risk)", type = palettes_00)+
  labs(title = "Malaria Risk Map with normalized variables", fill = "")+
  theme(panel.background = element_blank(), size = 20) +
  theme_void()+
  map_theme()

library(gridExtra)
grid.arrange(plot3, plot4, ncol=2) 
  
