
source("load_path.R")

# To Estimate Cost of Nets and Total Cost Cost of Approaches


# calculate cost of procurement per net

net_prices <- read.csv(file.path(AbidjanDir, "gf_llin_referenceprices_table_en.csv"))

avg_net_costs <- net_prices %>%
  summarise(
    `PC Dual a.i (Standard)` = mean(PYRETHROID.CHLORFENAPYR..CFP.Dual.a.i...Standard),
    `Pyrethroid-PBO (ITN only)` = mean(PYRETHROID.PBO..ITN.only.),
    `Pyrethroid-PBO (Standard)` = mean(PYRETHROID.PBO..Standard.),
    `Pyrethroid-Only (ITN only)` = mean(PYRETHROID.ONLY..ITN.only.),
    `Pyrethroid-Only (Standard)` = mean(PYRETHROID.ONLY..Standard.)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Net_Type", values_to = "Average_Cost")

ggplot(avg_net_costs, aes(x = Net_Type, y = Average_Cost,  fill = Net_Type)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Average Cost of Net Types",
    x = "Type",
    y = "Average Cost (US$)",
    fill = "Net Type") +
  theme_manuscript()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


next_gen_avg_price <- mean(unlist(net_prices[, 3,5])) #standard ITN package only of new generation nets
overall_avg_price <- mean(unlist(net_prices[, 3:7]))


#define cost to distribute a net
dist_cost_per_net <- 2.67 #financial. cost of distributing net in a mass campaign


#total cost per net (procurement and distribution)
cost_per_net <- next_gen_avg_price + dist_cost_per_net

#define PLNP budget
budget <- 3000000


#get and estimate population and number of nets needed from previous campaign data

population <- campign_dat %>% 
  dplyr::select(district_name, total_population_enumerated, number_LLINs_needed, LLINs_distributed, theorectical_stock_LLINs_left.) %>% 
  mutate(across(-district_name, ~ as.numeric(gsub(",", "", .)))) %>% 
  rename(HealthDistrict = district_name)

growth_rate <- 0.025  #assuming an annual population growth rate of 2.5%

population <- population %>%
  mutate(est_population_2024 = ceiling(total_population_enumerated * (1 + growth_rate)^3 - 0.5)) %>% 
  mutate(LLINs_needed_2024 = ceiling(est_population_2024/2))


#add health district rankings

plottingdata_filtered <- readRDS("plotting_data.rds")

plottingdata2 <- plottingdata_filtered %>% 
  dplyr::select(-rank) %>% 
  mutate(rank = case_when( 
    #manually define rank
    HealthDistrict == "TREICHVILLE-MARCORY" ~ 1,
    HealthDistrict == "ABOBO OUEST" ~ 5,
    HealthDistrict == "ABOBO EST" ~ 4,
    HealthDistrict == "COCODY BINGERVILLE" ~ 2,
    HealthDistrict == "PORT BOUET-VRIDI" ~ 6,
    HealthDistrict == "KOUMASSI" ~ 3,
    HealthDistrict == "ADJAME-PLATEAU-ATTECOUBE" ~ 7,
    HealthDistrict == "YOPOUGON-EST" ~ 8,
    HealthDistrict == "ANYAMA" ~ 10,
    HealthDistrict == "YOPOUGON-OUEST-SONGON" ~ 10,
    TRUE ~ NA_real_  
  ))


# net_distribution <- population %>%
#   left_join(plottingdata_filtered %>%  
#               dplyr::select(HealthDistrict, rank) %>% 
#               rename(district_name = HealthDistrict),
#             by = "district_name")

net_distribution <- population %>%
  left_join(plottingdata2 %>%  
              dplyr::select(HealthDistrict, rank),
            by = "HealthDistrict")


############## NET DISTIRBUTION IN ALL APPROACHES ######

#calculate cost of nets for each district 
net_distribution_scenario <- net_distribution %>% 
  mutate(procurement_cost = LLINs_needed_2024 * next_gen_avg_price,  #overall_avg_price,
         distribution_cost = LLINs_needed_2024 * dist_cost_per_net,
         total_cost = LLINs_needed_2024 * cost_per_net) %>% 
  dplyr::select(HealthDistrict, est_population_2024, LLINs_needed_2024, rank, procurement_cost, distribution_cost, total_cost)


#create dataframe of costs for each approach, including estimated field study costs

universal_data <- net_distribution_scenario  #all districts
universal_procurement <- sum(universal_data$procurement_cost)
universal_distribution <- sum(universal_data$distribution_cost)
universal_field_study <- 0  

deprioritization_data <- net_distribution_scenario %>% filter(HealthDistrict != "TREICHVILLE-MARCORY")
deprioritization_procurement <- sum(deprioritization_data$procurement_cost)
deprioritization_distribution <- sum(deprioritization_data$distribution_cost)
deprioritization_field_study <- 326095  


prioritization_data <- net_distribution_scenario %>% 
  arrange(desc(rank)) %>%
  mutate(cumulative_pop = cumsum(est_population_2024)) %>%
  filter(cumulative_pop <= sum(est_population_2024) * 0.5) #0.5 %coverage or 

prioritization_procurement <- sum(prioritization_data$procurement_cost)
prioritization_distribution <- sum(prioritization_data$distribution_cost)
prioritization_field_study <- 850000 


approach_costs <- data.frame(
  scenario = rep(c("Universal Coverage", "Deprioritization", "Prioritization"), each = 4),
  cost_type = rep(c("Field Study", "Net Procurement", "Net Distribution", "Available Funding"), 3),
  amount = c(universal_field_study, universal_procurement, universal_distribution, budget,
             deprioritization_field_study, deprioritization_procurement, deprioritization_distribution, budget,  
             prioritization_field_study, prioritization_procurement, prioritization_distribution, budget)
)
approach_costs$amount <- approach_costs$amount / 1e6


#plot
ggplot(approach_costs, aes(x = scenario, y = amount, fill = cost_type)) +
  geom_bar(stat = "identity", position = "stack", 
           data = filter(approach_costs, cost_type != "Available Funding")) +
  geom_hline(aes(yintercept = unique(amount[cost_type == "Available Funding"])), 
             linetype = "dashed", color = "black", size = 1) +
  scale_fill_manual(values = c("Net Procurement" = "#1f78b4",
                               "Net Distribution" = "#33a02c",
                               "Field Study" = "#e31a1c")) +
  labs(
    title = "Estimated Cost of ITN Distribution in Abidjan",
    x = "Scenario)",
    y = "Cost (US$ millions)",
    fill = "Cost Type"
  )+
  theme_manuscript()


