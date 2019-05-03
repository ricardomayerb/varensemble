source('./R/combinations_functions.R')
library(ggthemes)
library(ggThemeAssist)
library(RColorBrewer)
library(jpeg)
library(imager)
library(openxlsx)


data_object_per_new <- readRDS("./data/VAR_data_Chile.rds")
# print(colnames(data_object_per_new))
all_transformations <- readRDS("./data/target_transformation/target_transformation_Chile.rds")
target_transformation <- all_transformations$target_transformation
raw_data <- readRDS("./data/raw_VAR_data/raw_VAR_data_Chile.rds")
var_data <- data_object_per_new

rgdp_level_ts <- raw_data[,"rgdp"]
rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts) %>% na.omit()


target_variable <- "rgdp"
# print(target_transformation)
n_cv <- 8
fc_horizon <- 8
training_length <- "per_cv_maxs" 
target_transform <- target_transformation
target_level_ts <- na.omit(raw_data[, target_variable])
threshold <- 1.65

# Extend exogenous variables
exogenous_variables <- c("ip_us", "ip_asia", "ip_ue")
exogenous_variables_with_rgc <- c("ip_us", "ip_asia", "ip_ue", "rgc")

names_exogenous <- exogenous_variables 
names_exogenous_with_rgc <- exogenous_variables_with_rgc 




# Forecast the exogenous variables with Arima models. These are used later on in the VAR forecasts and cv with exo variables
exodata_fullsample <- var_data[,exogenous_variables] # note that exogenous_variables is specified at the start of the scirpt and contains all exogenous variables to Uruguay's economic activity.
target_used_in_VAR <- var_data[, "rgdp"]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))

tic()
extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8,
                                        endo_end = end_target_in_VAR)
toc()

tic()
cv_extension_of_exo <- extending_exogenous_for_cv(
  exodata = exodata_fullsample, h = fc_horizon, endo_end = end_target_in_VAR,
  n_cv = n_cv, same_model_across_cv = FALSE)
toc()
saveRDS(extension_of_exo, file = "./data/extension_of_exo_us_ue_asia.rds")
saveRDS(cv_extension_of_exo, file = "./data/cv_extension_of_exo_us_ue_asia.rds")

extension_of_exo <- readRDS(file = "./data/extension_of_exo_us_ue_asia.rds")
cv_extension_of_exo <- readRDS(file = "./data/cv_extension_of_exo_us_ue_asia.rds")

# exo_var_transformations <- all_transformations$all_transformations %>% 
#   filter(variables %in% exogenous_variables) %>% 
#   dplyr::select(-c(level_recom, trend_recom)) %>% 
#   mutate(followed_recom = as.character(followed_recom))
# 
# exo_var_transformations


yta <- "current_and_next"

# tic()
# fcs_exo_for_tables <- get_fcs_tables_of_exo(
#   fcs_exo_ts = extension_of_exo$future_exo,
#   names_exogenous = names_exogenous,
#   all_transformations = all_transformations,
#   yta = yta) 
# toc()
# 

models_from_search_2 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_2.rds")
models_from_search_3 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_3.rds")
models_from_search_4 <- readRDS("./data/forecast_models/all_chile_models_new_data_all_variables_restricted_combos_t165_lag_4.rds")

models_from_search <- rbind(models_from_search_2$all_passing_models_2345,
                            models_from_search_3$all_passing_models_2345,
                            models_from_search_4$all_passing_models_2345)

models_per_h_to_work <- 50
working_models <- discard_by_rank(models_tbl = models_from_search, max_rank_h = models_per_h_to_work, is_wide = TRUE)
# working_models_y <- discard_by_rank(models_tbl = models_from_search, max_rank_h = models_per_h_to_work, is_wide = TRUE, is_yearly = TRUE)

saveRDS(working_models, "./wm_chl_50.rds")

rm(models_from_search_2)
rm(models_from_search_3)
rm(models_from_search_4)
rm(models_from_search)


tic()
new_cv <- cv_var_from_tbl_by_row(h = fc_horizon,
                                 n_cv = n_cv,
                                 training_length = training_length,
                                 models_tbl = working_models,
                                 var_data = var_data,
                                 target_transform = target_transform,
                                 target_level_ts = target_level_ts,
                                 names_exogenous = names_exogenous,
                                 future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()


saveRDS(new_cv, "./cv50yearly_chl.rds")

new_cv <- readRDS("./cv50yearly_chl.rds")

new_working_models <- new_cv$passing_models_tbl

# working_models_y_20 <- discard_by_rank(models_tbl = wii50, max_rank_h = 20, is_wide = TRUE, is_yearly = TRUE)

# old50 <- ensemble_fcs_list_50


tic()
ensemble_fcs_list_50 <- ensemble_fc_by_row(var_data = var_data, 
                                           raw_data = raw_data,
                                           models_tbl = new_working_models,
                                           extended_exo_list =  extension_of_exo,
                                           fc_horizon = fc_horizon,
                                           target_name = target_variable,
                                           all_transformations = all_transformations,
                                           names_exogenous = names_exogenous,
                                           max_rank_h = 50, 
                                           superset_fcs = NULL)
toc()


ensemble_fcs_list_40_ss <- ensemble_fc_by_row(var_data = var_data, 
                                             raw_data = raw_data,
                                             models_tbl = new_working_models,
                                             extended_exo_list=  extension_of_exo,
                                             fc_horizon = fc_horizon,
                                             target_name = target_variable,
                                             all_transformations = all_transformations,
                                             names_exogenous = names_exogenous,
                                             max_rank_h = 40, 
                                             superset_fcs = ensemble_fcs_list_50$basic_forecasts)



ensemble_fcs_list_30_ss <- ensemble_fc_by_row(var_data = var_data, 
                                             raw_data = raw_data,
                                             models_tbl = new_working_models,
                                             extended_exo_list =  extension_of_exo,
                                             fc_horizon = fc_horizon,
                                             target_name = target_variable,
                                             all_transformations = all_transformations,
                                             names_exogenous = names_exogenous,
                                             max_rank_h = 30, 
                                             superset_fcs = ensemble_fcs_list_50$basic_forecasts)



ensemble_fcs_list_20_ss <- ensemble_fc_by_row(var_data = var_data, 
                                             raw_data = raw_data,
                                             models_tbl = new_working_models,
                                             extended_exo_list =  extension_of_exo,
                                             fc_horizon = fc_horizon,
                                             target_name = target_variable,
                                             all_transformations = all_transformations,
                                             names_exogenous = names_exogenous,
                                             max_rank_h = 20, 
                                             superset_fcs = ensemble_fcs_list_50$basic_forecasts)



ensemble_fcs_list_10_ss <- ensemble_fc_by_row(var_data = var_data, 
                                             raw_data = raw_data,
                                             models_tbl = new_working_models,
                                             extended_exo_list =  extension_of_exo,
                                             fc_horizon = fc_horizon,
                                             target_name = target_variable,
                                             all_transformations = all_transformations,
                                             names_exogenous = names_exogenous,
                                             max_rank_h = 10, 
                                             superset_fcs = ensemble_fcs_list_50$basic_forecasts)



ensemble_fcs_list_5_ss <- ensemble_fc_by_row(var_data = var_data, 
                                            raw_data = raw_data,
                                            models_tbl = new_working_models,
                                            extended_exo_list =  extension_of_exo,
                                            fc_horizon = fc_horizon,
                                            target_name = target_variable,
                                            all_transformations = all_transformations,
                                            names_exogenous = names_exogenous,
                                            max_rank_h = 5, 
                                            superset_fcs = ensemble_fcs_list_50$basic_forecasts)


df_target_fcs <- get_annual_fcs(list(ensemble_fcs_list_10_ss,
                                     ensemble_fcs_list_20_ss, 
                                     ensemble_fcs_list_30_ss, 
                                     ensemble_fcs_list_40_ss,
                                     ensemble_fcs_list_50),
                                vbls = "rgdp") 


df_all_vbls_fcs <- get_annual_fcs(list(ensemble_fcs_list_10_ss,
                                       ensemble_fcs_list_20_ss, 
                                       ensemble_fcs_list_30_ss, 
                                       ensemble_fcs_list_40_ss,
                                       ensemble_fcs_list_50)) 

# current name inside .Rmd
df_forecasts_all_models <- dplyr::select(df_target_fcs,
                                         -c(max_rank_h, fc_name))

df_forecasts_all_models_rgdp <- dplyr::select(df_target_fcs,
                                         -c(max_rank_h))
df_forecasts_all_models_rgdp
df_forecasts_all_models


write.xlsx(df_forecasts_all_models , file = "./Excel_Output/Chile/forecasts_all_models.xlsx")




######################################### Summary Report ##############################################

chosen_ensemble <- ensemble_fcs_list_30_ss

variables_by_h_freq_rel <- chosen_ensemble$variables_freq_rel_qy 

write.xlsx(variables_by_h_freq_rel, file = "./Variable Count/Chile/variables_overall_freq_VAR30.xlsx")
write.xlsx(variables_by_h_freq_rel, file = "./Variable Count/Chile/variables_by_h_freq_VAR30.xlsx")

count_h1 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_1) %>% arrange(desc(n_1))
count_h2 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_2) %>% arrange(desc(n_2))
count_h3 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_3) %>% arrange(desc(n_3))
count_h4 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_4) %>% arrange(desc(n_4))

count_h5 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_5) %>% arrange(desc(n_5))
count_h6 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_6) %>% arrange(desc(n_6))
count_h7 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_7) %>% arrange(desc(n_7))
count_h8 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_8) %>% arrange(desc(n_8))

count_year1 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_1_y) %>% arrange(desc(n_1_y))
write.xlsx(count_year1, file = "./Variable Count/Chile/count_year1.xlsx")
count_year2 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_2_y) %>% arrange(desc(n_2_y))
write.xlsx(count_year2, file = "./Variable Count/Chile/count_year2.xlsx")

count_year1_and_rgdp <- add_row(count_year1, variable = "rgdp", n_1_y = 100, .before = TRUE)

# Create Data Summaries for the most important variables in year 1 and year 2

chosen_ensemble <- ensemble_fcs_list_10_ss

variables_by_h_freq_rel <- chosen_ensemble$variables_freq_rel_qy 

write.xlsx(variables_by_h_freq_rel, file = "./Variable Count/Chile/variables_overall_freq_VAR10.xlsx")
write.xlsx(variables_by_h_freq_rel, file = "./Variable Count/Chile/variables_by_h_freq_VAR10.xlsx")

count_h1 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_1) %>% arrange(desc(n_1))
count_h2 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_2) %>% arrange(desc(n_2))
count_h3 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_3) %>% arrange(desc(n_3))
count_h4 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_4) %>% arrange(desc(n_4))

count_h5 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_5) %>% arrange(desc(n_5))
count_h6 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_6) %>% arrange(desc(n_6))
count_h7 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_7) %>% arrange(desc(n_7))
count_h8 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_8) %>% arrange(desc(n_8))

count_year1 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_1_y) %>% arrange(desc(n_1_y))
write.xlsx(count_year1, file = "./Variable Count/Chile/count_year1.xlsx")
count_year2 <- variables_by_h_freq_rel %>% dplyr::select(variable, n_2_y) %>% arrange(desc(n_2_y))
write.xlsx(count_year2, file = "./Variable Count/Chile/count_year2.xlsx")

count_year1_and_rgdp <- add_row(count_year1, variable = "rgdp", n_1_y = 100, .before = TRUE)
count_year1_and_rgdp_non_zero <- count_year1_and_rgdp %>% filter(n_1_y > 0)
top_10_variables <- count_year1_and_rgdp_non_zero$variable
top_10_variables_tbl <- count_year1_and_rgdp_non_zero

country_name <- "Chile"
plots_path = paste("./Plots/", country_name, "/", sep = "")
filenames_top_variables_graphs <- c("Graph Variable 1", "Graph Variable 2", "Graph Variable 3", "Graph Variable 4",
                                    "Graph Variable 5", "Graph Variable 6", "Graph Variable 7", "Graph Variable 8",
                                    "Graph Variable 9", "Graph Variable 10")

# top_10_variables <- t(count_year1[1:10 , 1])
# top_10_variables_yr2 <- t(count_year2[1:10 , 1])

make_top_variables_graphs <- function(top_10_variables, country_data_level_ts){
  # print(top_10_variables)
  
  plot_list <- list()
  rgdp_label_for_plot <- "real GDP"
  # this_series_name <-  "tot"
  
  
  rgdp_level_ts <- country_data_level_ts[,"rgdp"]
  rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts) %>% na.omit()
  
  y_label_yoy <- "YoY variation (%)" 
  x_label <- ""
  
  df <- country_data_level_ts
  
  # filename_plot_tot <- paste(name_plot_tot, "png", sep = ".")
  
  for (i in 1:length(top_10_variables)) {
    name_variable <- top_10_variables[i]
    
    variable <- country_data_level_ts[,name_variable]
    mean_2015 <- paste("mean 2015q1 - now", name_variable, sep = " ")
    mean_2010 <- paste("mean 2010q1 - 2014q4", name_variable, sep = " ")
    # mean_2005 <- paste("mean 2005q1 - 2009q4", name_variable, sep = " ")
    # mean_2000 <- paste("mean 2000q1 - 2004q4", name_variable, sep = " ")
    
    variable_level_ts <- country_data_level_ts[,name_variable] %>% na.omit() 
    
    variable_yoy_ts <- window(make_yoy_ts(variable_level_ts), 
                              start = start(variable_level_ts), 
                              end = end(variable_level_ts))
    
    # print(start(variable_yoy_ts))
    
    # print(variable_yoy_ts)
    
    # variable_yoy_ts_2000 <- window(variable_yoy_ts, 
    #                                start = c(2000, 1), 
    #                                end = c(2004, 4))
    # print(variable_yoy_ts_2000)
    # 
    # variable_yoy_ts_2005 <- window(variable_yoy_ts, 
    #                                start = c(2005, 1), 
    #                                end = c(2009, 4))
    # print(variable_yoy_ts_2005)
    
    variable_yoy_ts_2010 <- window(variable_yoy_ts, 
                                   start = c(2010, 1), 
                                   end = c(2014, 4))
    
    variable_yoy_ts_2015 <- window(variable_yoy_ts, 
                                   start = c(2015, 1), 
                                   end = end(variable_yoy_ts))
    
    
    rgdp_yoy_ts <- window(rgdp_yoy_ts, 
                          start = start(variable_level_ts), 
                          end = end(variable_level_ts))
    
    # mean_variable_2000 <- mean(variable_yoy_ts_2000)
    # # print(mean_variable_2000)
    # mean_variable_2005 <- mean(variable_yoy_ts_2005)
    # print(mean_variable_2005)
    mean_variable_2010 <- mean(variable_yoy_ts_2010)
    # print(mean_variable_2010)
    mean_variable_2015 <- mean(variable_yoy_ts_2015)
    
    # mean_variable_series_2000 <- rep(mean_variable_2000, length(variable_yoy_ts_2000))
    # mean_variable_series_2005 <- rep(mean_variable_2005, length(variable_yoy_ts_2005))
    mean_variable_series_2010 <- rep(mean_variable_2010, length(variable_yoy_ts_2010))
    mean_variable_series_2015 <- rep(mean_variable_2015, length(variable_yoy_ts_2015))
    
    # mean_variable_series_2000_ts <- ts(data = mean_variable_series_2000, 
    #                                    start = c(2000, 1), 
    #                                    end = c(2004, 4), frequency = 4)
    # 
    # mean_variable_series_2005_ts <- ts(data = mean_variable_series_2005, 
    #                                    start = c(2005, 1), 
    #                                    end = c(2009, 4), frequency = 4)
    # 
    
    mean_variable_series_2010_ts <- ts(data = mean_variable_series_2010, 
                                       start = c(2010, 1), 
                                       end = c(2014, 4), frequency = 4)
    
    
    mean_variable_series_2015_ts <- ts(data = mean_variable_series_2015, 
                                       start = c(2015, 1), 
                                       end = end(variable_yoy_ts), frequency = 4)
    # print(mean_variable_series_2000_ts)
    # print(mean_variable_series_2010_ts)
    
    
    plot <- autoplot(100*variable_yoy_ts, series = name_variable) +
      autolayer(100*rgdp_yoy_ts, series=rgdp_label_for_plot, linetype = 2, size = 1) +
      # autolayer(100*mean_variable_series_2000_ts, series=mean_2000, linetype = 3, size = 1) +
      # autolayer(100*mean_variable_series_2005_ts, series=mean_2005, linetype = 3, size = 1) +
      autolayer(100*mean_variable_series_2010_ts, series=mean_2010, linetype = 3, size = 1) +
      autolayer(100*mean_variable_series_2015_ts, series=mean_2015, linetype = 3, size = 1) +
      xlab(x_label) + 
      ylab(y_label_yoy)
    
    plot_list[[i]] <- plot
    
  }
  return(plot_list)
  
}


list_top_variables_graphs <- make_top_variables_graphs(
  top_10_variables = top_10_variables[1:10], 
  country_data_level_ts = raw_data)

walk2(.x = list_top_variables_graphs, .y = filenames_top_variables_graphs, 
      ~ ggsave(filename = paste0(.y, ".png"), plot = .x, path = plots_path))


### Create Data yoy Frame best 10 Variables

make_yoy_table_top10 <- function(count_df, country_data_level_ts, start_table, end_table){
  variables <- t(count_df[ ,1])
  variables_with_rgdp <- c("rgdp", variables)
  
  data_level_ts <- country_data_level_ts[,variables_with_rgdp]
  colnames_data <- colnames(data_level_ts)
  yoy_ts <- window(make_yoy_ts(data_level_ts), 
                   start = start_table, 
                   end = end_table)
  colnames(yoy_ts) <- colnames_data
  yoy_ts <- yoy_ts*100
  
  return(yoy_ts)
}


generate_annual_means <- function(yoy_q_table, year1, year2){
  
  colnames_data <- colnames(yoy_q_table)
  yoy_q_table_2017 <- window(yoy_q_table, 
                             start = c(year1, 1), 
                             end = c(year1, 4))
  mean_yoy_q_table_2017 <- colMeans(yoy_q_table_2017)
  
  yoy_q_table_2018 <- window(yoy_q_table, 
                             start = c(year2, 1), 
                             end = c(year2, 4))
  mean_yoy_q_table_2018 <- colMeans(yoy_q_table_2018)
  yoy_annual_df <- data.frame(
    mean_2017 = mean_yoy_q_table_2017,
    mean_2018 = mean_yoy_q_table_2018)
  
  
  return(yoy_annual_df)
  
}

yoy_table_top10 <- make_yoy_table_top10(count_df = count_year1, country_data_level_ts = raw_data,
                                        start_table = c(2017, 1), end_table = c(2019, 1))


annual_mean_top10_table <-  generate_annual_means(yoy_q_table = yoy_table_top10, 
                                                  year1 = 2017, year2 = 2018)


yoy_table_top10_tbl <- as_tibble(t(yoy_table_top10), rownames = "fc_name")
annual_mean_top10_table_tbl <- as_tibble(annual_mean_top10_table, rownames = "fc_name")

annual_fcs_top_variables_unweighted <- left_join(
  ensemble_fcs_list_10_ss$simple_avg_all_wide, count_year1_and_rgdp,
  by = c("fc_name" = "variable")) %>%
  arrange(desc(n_1_y)) %>% 
  filter(n_1_y > 0) %>% 
  dplyr::select(-n_1_y)

annual_fcs_top_variables_n1y <- left_join(
  ensemble_fcs_list_10_ss$weighted_avg_all_wide, count_year1_and_rgdp,
  by = c("fc_name" = "variable")) %>% 
  filter(n_1_y > 0) %>% 
  arrange(desc(n_1_y)) 

annual_fcs_top_variables <- annual_fcs_top_variables_n1y %>% 
  dplyr::select(-n_1_y)

annual_fcs_top_variables <- annual_fcs_top_variables %>% mutate_if(is.numeric, ~ .x*100)
annual_fcs_top_variables_unweighted <- annual_fcs_top_variables_unweighted %>% mutate_if(is.numeric, ~ .x*100)

annual_mean_top10_table <- filter(
  annual_mean_top10_table_tbl, fc_name %in% annual_fcs_top_variables$fc_name) %>%
  left_join(annual_fcs_top_variables, by = "fc_name")

names(annual_mean_top10_table) <-  c("variables", seq(2017, length.out = ncol(annual_mean_top10_table)-1))

annual_mean_top10_table <- dplyr::select(annual_mean_top10_table, 1:4)

qy_means_y_fc_count_yr1 <- yoy_table_top10_tbl %>% 
  rename(variables = fc_name) %>% 
  filter(variables %in% annual_mean_top10_table$variables) %>% 
  left_join(annual_mean_top10_table, by = "variables")

# qy_means_y_fc_count_yr1

# Monthly variables: get the latest q data
write.xlsx(yoy_table_top10, file = "./Excel_Output/Chile/Forecast summary/quarterly yoy top 10 variables.xlsx")
write.xlsx(annual_mean_top10_table, file = "./Excel_Output/Chile/Forecast summary/annual mean top 10 variables.xlsx")
write.xlsx(qy_means_y_fc_count_yr1, file = "./Excel_Output/Chile/Forecast summary/q_y_means_y1_fcs.xlsx")

fcs_best_30 <- c(mean(ensemble_fcs_list_30_ss$weighted_avg_fc_yoy[1:4]), mean(ensemble_fcs_list_30_ss$weighted_avg_fc_yoy[5:8]))
# fcs_best_30
fcs_best_30_df <- data.frame(index = c(2019, 2020), 
                             value = fcs_best_30*100)

fcs_best_30_df$type <- rep("forecast", length(fcs_best_30_df$value))

rgdp_yoy_annual <- rgdp_yoy_ts %>% window(start = c(2010, 1), end = end(rgdp_yoy_ts)) %>% as.xts() %>% apply.yearly(FUN = mean) %>% tk_tbl() %>% mutate(index = year(index)) %>% as.data.frame()
rgdp_yoy_annual$value <- rgdp_yoy_annual$value*100
rgdp_yoy_annual$type <- rep("realization", length(rgdp_yoy_annual$value))

# row bind
data_bar_plot_long <- rbind(rgdp_yoy_annual, fcs_best_30_df)
only_fcs_tbl <- data_bar_plot_long %>% filter(type == "forecast")

# seq(from = 1992, to = 2020, by =1)


plot_annual_forecast_title <- paste("Annual GDP Forecasts", country_name, sep = " ")
filename_annual_forecast_plot <- paste(plot_annual_forecast_title, "png", sep = ".")
# position dodge is used so that the bars are next to each other and not on top of each other
# figure out how to control spacing between observations
# figure out how to get values on the last bars of the forecasts
annual_forecast_bar_plot <- ggplot(data_bar_plot_long, aes(x = index, y = value, fill = type)) + 
  geom_col(width=0.8, position = "dodge") +
  geom_text(data = only_fcs_tbl, aes(label = round(value, digits = 3), fontface = "bold"), 
            position = position_dodge(width=0.9), size=3, hjust = 0, vjust = 0.2, angle = 90) +
  scale_fill_brewer(palette = "Set2") + 
  ggtitle(plot_annual_forecast_title) +
  guides(fill=guide_legend(title="Time-Series:")) +
  scale_y_continuous(name = "Real GDP Growth YoY (%)", 
                     breaks=c(-3, -2, -1, 0, 1, 2, 3, 4, 
                              5, 6, 7, 8, 9, 10)) + 
  scale_x_yearmon(name = "Year", n = 15, format = "%Y")

annual_forecast_bar_plot

ggsave(filename = filename_annual_forecast_plot, path = plots_path)


# Correlation

make_correlations <- function(top_10_variables, country_data_level_ts){
  correlation_list <- list()
  
  rgdp_level_ts <- country_data_level_ts[,"rgdp"]
  rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts) %>% na.omit()
  
  df <- country_data_level_ts
  
  # filename_plot_tot <- paste(name_plot_tot, "png", sep = ".")
  
  for (i in 1:length(top_10_variables)) {
    name_variable <- top_10_variables[i]
    
    variable <- country_data_level_ts[,name_variable]
    
    variable_level_ts <- country_data_level_ts[,name_variable] %>% na.omit() 
    
    variable_yoy_ts <- window(make_yoy_ts(variable_level_ts), 
                              start = start(variable_level_ts), 
                              end = end(variable_level_ts))
    
    if(length(variable_yoy_ts) < length(rgdp_yoy_ts)){
      start_series <- start(variable_yoy_ts)
      
    }
    
    else{
      start_series <- start(rgdp_yoy_ts)
      
    }
    
    
    if(end(variable_yoy_ts) < end(rgdp_yoy_ts)){
      end_series <- end(variable_yoy_ts)
      
    }
    
    else{
      end_series <- end(rgdp_yoy_ts)
      
    }
    
    rgdp_yoy_ts <- window(rgdp_yoy_ts, 
                          start = start_series, 
                          end = end_series)
    
    variable_yoy_ts <- window(variable_yoy_ts, 
                              start = start_series, 
                              end = end_series)
    
    correlation <- cor( rgdp_yoy_ts, variable_yoy_ts)
    
    
    correlation_list[[i]] <- correlation
    
  }
  
  correlation_df <- data.frame(variables = t(top_10_variables), 
                               correlation_with_gdp = unlist(correlation_list))
  return(correlation_df)
  
}

correlations_df <- make_correlations(top_10_variables = top_10_variables[1:10], country_data_level_ts = raw_data)



############# Save all RDS's for in rmarkdown document ####################


# t_yoy_table_top10 <- as_tibble(cbind(variables = dimnames(yoy_table_top10)[[2]], as_tibble(t(yoy_table_top10))))
# names(t_yoy_table_top10)[2:10] <- 1:9
# 
# yoy_df_top10_variables <- left_join(t_yoy_table_top10, annual_mean_top10_table, by = "variables")

yoy_df_top10_variables <- qy_means_y_fc_count_yr1

saveRDS(object = list(forecast_df = df_forecasts_all_models,
                      top_10_year_1_count = count_year1,
                      yoy_df_top10_variables = yoy_df_top10_variables,
                      correlations_df = correlations_df),
        file = "./data/Forecast summary/Chile.rds")

####################### Componentes de la demanda ###############################

create_df_yoy <- function(data_level, 
                          variables_to_select,
                          start_table,
                          end_table){
  variables_with_rgdp <- c("rgdp", variables_to_select)
  
  data_level_ts_demanda <- data_level[,variables_with_rgdp]
  yoy_ts <- window(make_yoy_ts(data_level_ts_demanda), 
                   start = start_table, 
                   end = end_table)
  colnames(yoy_ts) <- variables_with_rgdp
  yoy_ts <- round(yoy_ts*100, digits = 2)
  
  return(yoy_ts)
}

componentes_demanda <- c("rpc", "rgc", "fbcf", "rx", "rm")
componentes_oferta <- c("copper_output", "manuf", "serv")

# df_componentes_demanda <- create_df_yoy(data_level = country_data_level_ts,
#                                         variables_to_select = componentes_demanda,
#                                         start_table = c(2017, 1), end_table = c(2018, 4))

df_componentes_demanda <- create_df_yoy(data_level = raw_data,
                                        variables_to_select = componentes_demanda,
                                        start_table = c(2017, 1), end_table = c(2018, 4))

# df_componentes_oferta <-create_df_yoy(data_level = country_data_level_ts,
#                                       variables_to_select = componentes_oferta,
#                                       start_table = c(2017, 1), end_table = c(2018, 4))

df_componentes_oferta <-create_df_yoy(data_level = raw_data,
                                      variables_to_select = componentes_oferta,
                                      start_table = c(2017, 1), end_table = c(2018, 4))


df_componentes_demanda_annual <- generate_annual_means(yoy_q_table = df_componentes_demanda, year1 = 2017, year2 = 2018)
df_componentes_oferta_annual <- generate_annual_means(yoy_q_table = df_componentes_oferta, year1 = 2017, year2 = 2018)

yoy_df_gdp_demanda <- cbind(t(df_componentes_demanda), df_componentes_demanda_annual); yoy_df_gdp_demanda
yoy_df_gdp_oferta <- cbind(t(df_componentes_oferta), df_componentes_oferta_annual); yoy_df_gdp_oferta

df_demanda_fcs <- get_annual_fcs(list(ensemble_fcs_list_10_ss,
                                      ensemble_fcs_list_20_ss, 
                                      ensemble_fcs_list_30_ss, 
                                      ensemble_fcs_list_40_ss,
                                      ensemble_fcs_list_50),
                                 vbls = c("rgdp", componentes_demanda)) 


df_oferta_fcs <- get_annual_fcs(list(ensemble_fcs_list_10_ss,
                                     ensemble_fcs_list_20_ss, 
                                     ensemble_fcs_list_30_ss, 
                                     ensemble_fcs_list_40_ss,
                                     ensemble_fcs_list_50),
                                vbls = c("rgdp",componentes_oferta)) 

df_demanda_fcs_50 <-  df_demanda_fcs %>% filter(max_rank_h == 50)
df_oferta_fcs_50 <-  df_oferta_fcs %>% filter(max_rank_h == 50)



yoy_df_gdp_demanda_b50 <- left_join(
  as_tibble(yoy_df_gdp_demanda, rownames = "fc_name"),
  df_demanda_fcs_50, by = "fc_name") 

yoy_df_gdp_demanda_b50 <- dplyr::select(
  yoy_df_gdp_demanda_b50, -c(Model, max_rank_h))

yoy_df_gdp_oferta_b50 <- left_join(as_tibble(yoy_df_gdp_oferta, rownames = "fc_name"), df_oferta_fcs_50, by = "fc_name")

yoy_df_gdp_oferta_b50 <- dplyr::select(
  yoy_df_gdp_oferta_b50, -c(Model, max_rank_h))


names(yoy_df_gdp_demanda_b50)[10:13] <- seq(2017,2020)
names(yoy_df_gdp_oferta_b50)[10:13] <- seq(2017,2020)

create_share_pib_df <- function(data_level, 
                                variables_to_select,
                                column_names,
                                start_table, end_table){
  
  data_level_ts_demanda <- data_level[,variables_to_select]
  pib <- data_level[,"rgdp"]
  share_pib <- data_level_ts_demanda / pib
  share_pib <- window(share_pib,
                      start = start_table,
                      end = end_table)
  
  colnames(share_pib) <- column_names
  
  return(share_pib)
}

df_componentes_demanda_y_shares <- create_share_pib_df(data_level = raw_data,
                                                       variables_to_select = componentes_demanda,
                                                       column_names = c("participacion_rpc_en_pib", "participacion_rgc_en_pib", 
                                                                        "participacion_ri_en_pib", "participacion_rx_en_pib", 
                                                                        "participacion_rm_en_pib"),
                                                       start_table = c(2017, 1), end_table = c(2018, 4))

df_componentes_demanda_y_shares_annual <- generate_annual_means(yoy_q_table = df_componentes_demanda_y_shares, 
                                                                year1 = 2017, year2 = 2018);df_componentes_demanda_y_shares_annual
df_share_gdp_demanda <- cbind(t(df_componentes_demanda_y_shares), df_componentes_demanda_y_shares_annual); df_share_gdp_demanda

df_componentes_demanda_y_shares <- rbind(yoy_df_gdp_demanda, df_share_gdp_demanda)

df_componentes_demanda_shares_y_contribucion <- as.data.frame(t(df_componentes_demanda_y_shares))

df_componentes_demanda_shares_y_contribucion <- df_componentes_demanda_shares_y_contribucion %>% mutate(contribucion_crecimiento_rpc_pib =  participacion_rpc_en_pib * rpc,
                                                                                                        contribucion_crecimiento_rgc_pib =  participacion_rgc_en_pib * rgc,
                                                                                                        contribucion_crecimiento_ri_pib =  participacion_ri_en_pib * fbcf,
                                                                                                        contribucion_crecimiento_rx_pib =  participacion_rx_en_pib * rx,
                                                                                                        contribucion_crecimiento_rm_pib =  participacion_rm_en_pib * rm,
                                                                                                        contribucion_rx_minus_rm = contribucion_crecimiento_rx_pib - contribucion_crecimiento_rm_pib)

df_componentes_demanda_shares_y_contribucion_table <- t(df_componentes_demanda_shares_y_contribucion)

df_componentes_oferta_y_shares <- create_share_pib_df(data_level = raw_data,
                                                      variables_to_select = componentes_oferta,
                                                      column_names = c("participacion_primario_en_pib", "participacion_manuf_en_pib", 
                                                                       "participacion_serv_en_pib"),
                                                      start_table = c(2017, 1), end_table = c(2018, 4))

df_componentes_oferta_y_shares_annual <- generate_annual_means(yoy_q_table = df_componentes_oferta_y_shares, 
                                                               year1 = 2017, year2 = 2018);df_componentes_oferta_y_shares_annual
df_share_gdp_oferta <- cbind(t(df_componentes_oferta_y_shares), df_componentes_oferta_y_shares_annual); df_share_gdp_oferta

df_componentes_oferta_y_shares <- rbind(yoy_df_gdp_oferta, df_share_gdp_oferta)

df_componentes_oferta_shares_y_contribucion <- as.data.frame(t(df_componentes_oferta_y_shares))

df_componentes_oferta_shares_y_contribucion <- df_componentes_oferta_shares_y_contribucion %>% mutate(contribucion_crecimiento_primario_pib =  participacion_primario_en_pib * copper_output,
                                                                                                      contribucion_crecimiento_manuf_pib =  participacion_manuf_en_pib * manuf,
                                                                                                      contribucion_crecimiento_serv_pib =  participacion_serv_en_pib * serv)
df_componentes_oferta_shares_y_contribucion_table <- t(df_componentes_oferta_shares_y_contribucion)




