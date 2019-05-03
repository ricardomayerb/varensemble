source('./R/combinations_functions.R')
library(openxlsx)

# data_object_ury <- readRDS("./data/examples/example_data_ury.rds")
data_object_per_new <- readRDS("./data/VAR_data_Ecuador.rds")
print(colnames(data_object_per_new))
target_transformation <- readRDS("./data/target_transformation/target_transformation_Ecuador.rds")
target_transformation <- target_transformation$target_transformation
# country <- data_object_ury$country_name
# target_transformation <- data_object_ury$target_transformation
# raw_data <- data_object_ury$raw_data
# var_data <- data_object_ury$transformed_data
raw_data <- readRDS("./data/raw_VAR_data/raw_VAR_data_Ecuador.rds")
var_data <- data_object_per_new

# # exclude "exp_tradicional" and keep "exp" and "exp_notradicional" 
# var_data <- var_data[, ! colnames(var_data) == "exp_tradicional"]

target_variable <- "rgdp"
print(target_transformation)
n_cv <- 8
fc_horizon <- 8
training_length <- "per_cv_maxs" 
target_transform <- target_transformation
target_level_ts <- na.omit(raw_data[, target_variable])
threshold <- 1.65

# Extend exogenous variables
exogenous_variables <- c("ip_us", "ip_asia", "ip_ue")

names_exogenous <- exogenous_variables 

# Forecast the exogenous variables with Arima models. These are used later on in the VAR forecasts and cv with exo variables
exodata_fullsample <- var_data[,exogenous_variables] # note that exogenous_variables is specified at the start of the scirpt and contains all exogenous variables to Uruguay's economic activity.
target_used_in_VAR <- var_data[, "rgdp"]
start_target_in_VAR <- start(na.omit(target_used_in_VAR))
end_target_in_VAR <- end(na.omit(target_used_in_VAR))


# tic()
# extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8, 
#                                         endo_end = end_target_in_VAR)
# toc()
# tic()
# cv_extension_of_exo <- extending_exogenous_for_cv(
#   exodata = exodata_fullsample, h = fc_horizon, endo_end = end_target_in_VAR, 
#   n_cv = n_cv, same_model_across_cv = FALSE)
# toc()
# saveRDS(extension_of_exo, file = "./data/extension_of_exo_us_ue_asia.rds")
# saveRDS(cv_extension_of_exo, file = "./data/cv_extension_of_exo_us_ue_asia.rds")

extension_of_exo <- readRDS(file = "./data/extension_of_exo_us_ue_asia.rds")
cv_extension_of_exo <- readRDS(file = "./data/cv_extension_of_exo_us_ue_asia.rds")

names_all <- colnames(var_data)
names_all
lag_choices <- c(3)

accumulated_tried_models <- tibble()
accumulated_passing_models <- tibble()

tic()
specs_size_2 <- all_specifications(
  var_size = 2,
  all_variables = names_all,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data,
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)
toc()

tic()
cv_size_2_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                              training_length = training_length, 
                                              models_tbl = specs_size_2, 
                                              var_data = var_data,
                                              target_transform = target_transform, 
                                              target_level_ts = target_level_ts, 
                                              names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()

tried_models_size_2 <- cv_size_2_per_row$tried_models_tbl
passing_models_size_2 <- cv_size_2_per_row$passing_models_tbl

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_2)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_2)

### generate all size 3 and do cv
tic()
specs_size_3 <- all_specifications(
  var_size = 3,
  all_variables = names_all,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data,
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)
toc()


tic()
cv_size_3_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = specs_size_3, 
                                            var_data = var_data,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()

tried_models_size_3 <- cv_size_3_per_row$tried_models_tbl
passing_models_size_3 <- cv_size_3_per_row$passing_models_tbl



# Add all new passing models to the passing models of size 2
accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_3)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_3)

# # Make sure that we only retain unique models


###### start size 4 choosing specs and variables
# We know from print(ntable_31) that the number of specifications gets really large from size 4 on
# Therefore, we tried to come up with strategies to reduce the number of recommendations
# We combine two strategies: 
# (1) At each h we collect the best 10 models in the accumulated models tibble. Then at each h for each of these 10 models
# we augment it by increasing its size by any combination possible given the model specification and the variables not used
# in that specification yet. 
# (2) try all models but with reduced number of variables. We try all models at the new size for only those variables 
# that are among the best 10 ranked in terms of frequency in the accumulated models tibble.

# Strategy 1
in_best_10_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 10, is_wide = TRUE)

# All combination of the unique best 10 models at each h extended with all the other variables
in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_all)

# Elminate models that have been already tried
not_already_tried_models <- !in_best_10_augmented$short_name %in% accumulated_tried_models$short_name

in_best_10_augmented_not_tried <- in_best_10_augmented[not_already_tried_models, ]

# Stategy 2: try all models but with reduced number of variables. max_small_rank = 3 means the top 3 models per h
f_vbls <- variable_freq_by_n(accumulated_passing_models, 
                             h_max = fc_horizon,
                             max_rank = 10,
                             n_freq = 10, 
                             is_wide = TRUE, 
                             max_small_rank = 3)

f_vbls$vbl_freqs_by_h

new_names_by_freq <- sort(f_vbls[["variables_in_top_small"]])

print(new_names_by_freq)

specs_size_4_freq <- all_specifications(
  var_size = 4,
  all_variables = new_names_by_freq,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data[, new_names_by_freq],
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)

specs_size_4_freq <- mutate(specs_size_4_freq,
                            short_name = pmap(list(variables, lags, t_threshold),
                                              ~ make_model_name(variables = ..1, 
                                                                lags = ..2,
                                                                t_threshold = ..3)),
                            short_name = unlist(short_name))

not_in_best_10 <- !specs_size_4_freq$short_name %in% in_best_10_augmented_not_tried$short_name
sum(not_in_best_10)
specs_size_4_freq_not_in_best_10 <- specs_size_4_freq[not_in_best_10, ]
not_tried <- !specs_size_4_freq_not_in_best_10$short_name %in% accumulated_tried_models$short_name
specs_size_4_freq_proposed <- specs_size_4_freq_not_in_best_10[not_tried, ]


proposed_specs_s4 <- rbind(dplyr::select(in_best_10_augmented_not_tried, 
                                         names(specs_size_4_freq_proposed)),
                           specs_size_4_freq_proposed)

nrow(distinct(proposed_specs_s4, short_name))

names_of_res <- proposed_specs_s4 %>% filter(t_threshold != 0) %>% 
  mutate(name_wo_thresh = map_chr(short_name, ~ substr(.x, 1, nchar(.x)-5))) %>% 
  dplyr::select(name_wo_thresh)

unrepeated_u <- proposed_specs_s4 %>% filter(t_threshold == 0) %>% 
  mutate(name_wo_thresh = map_chr(short_name, ~ substr(.x, 1, nchar(.x)-5))) %>% 
  mutate(redundant = map_lgl(name_wo_thresh, ~ .x %in% names_of_res$name_wo_thresh)) %>% 
  filter(!redundant) %>% 
  dplyr::select(names(proposed_specs_s4))

specs_size_4 <-  rbind(unrepeated_u, filter(proposed_specs_s4, t_threshold != 0))

print(nrow(distinct(specs_size_4, short_name)))



tic()
cv_size_4_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = specs_size_4, 
                                            var_data = var_data,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()


## update passing and tried models
tried_models_size_4_pr <- cv_size_4_per_row$tried_models_tbl
passing_models_size_4_pr <- cv_size_4_per_row$passing_models_tbl

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_4_pr)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_4_pr)



# tried_models_size_4 <- cv_size_4$tried_models
# passing_models_size_4 <- cv_size_4$passing_models


# saveRDS(list(cv_size_4 = cv_proposed_size_4),
#              file = "./data/forecast_models/all_ury_models_25_variables_new_data_all_variables_restricted_combos_s4.rds")

######  start size 5 choosing specs and variables
in_best_10_some_h <- discard_by_rank(accumulated_passing_models, max_rank_h = 10, is_wide = TRUE)

# All combination of the unique best 10 models at each h extended with all the other variables
in_best_10_augmented <- augment_with_variable(in_best_10_some_h, names_all)

# Elminate models that have been already tried
not_already_tried_models <- !in_best_10_augmented$short_name %in% accumulated_tried_models$short_name

in_best_10_augmented_not_tried <- in_best_10_augmented[not_already_tried_models, ]

# Stategy 2: try all models but with reduced number of variables
f_vbls <- variable_freq_by_n(accumulated_passing_models, 
                             h_max = fc_horizon,
                             max_rank = 10,
                             n_freq = 10, 
                             is_wide = TRUE, 
                             max_small_rank = 3)

f_vbls$vbl_freqs_by_h

new_names_by_freq <- sort(f_vbls[["variables_in_top_small"]])

print(new_names_by_freq)


specs_size_5_freq <- all_specifications(
  var_size = 5,
  all_variables = new_names_by_freq,
  lag_choices = lag_choices, 
  use_info_lags = FALSE,
  var_data = var_data_20[, new_names_by_freq],
  t_thresholds = threshold,
  names_exogenous = exogenous_variables)

specs_size_5_freq <- mutate(specs_size_5_freq,
                            short_name = pmap(list(variables, lags, t_threshold),
                                              ~ make_model_name(variables = ..1, 
                                                                lags = ..2,
                                                                t_threshold = ..3)),
                            short_name = unlist(short_name))

not_in_best_10 <- !specs_size_5_freq$short_name %in% in_best_10_augmented_not_tried$short_name
sum(not_in_best_10)
specs_size_5_freq_not_in_best_10 <- specs_size_5_freq[not_in_best_10, ]
not_tried <- !specs_size_5_freq_not_in_best_10$short_name %in% accumulated_tried_models$short_name
specs_size_5_freq_proposed <- specs_size_5_freq_not_in_best_10[not_tried, ]


proposed_specs_s5 <- rbind(dplyr::select(in_best_10_augmented_not_tried, 
                                         names(specs_size_5_freq_proposed)),
                           specs_size_5_freq_proposed)

nrow(distinct(proposed_specs_s5, short_name))


names_of_res <- proposed_specs_s5 %>% filter(t_threshold != 0) %>% 
  mutate(name_wo_thresh = map_chr(short_name, ~ substr(.x, 1, nchar(.x)-5))) %>% 
  dplyr::select(name_wo_thresh)

unrepeated_u <- proposed_specs_s5 %>% filter(t_threshold == 0) %>% 
  mutate(name_wo_thresh = map_chr(short_name, ~ substr(.x, 1, nchar(.x)-5))) %>% 
  mutate(redundant = map_lgl(name_wo_thresh, ~ .x %in% names_of_res$name_wo_thresh)) %>% 
  filter(!redundant) %>% 
  dplyr::select(names(proposed_specs_s5))

specs_size_5 <-  rbind(unrepeated_u, filter(proposed_specs_s5, t_threshold != 0))
print(nrow(distinct(specs_size_5, short_name)))


tic()
cv_size_5_per_row <- cv_var_from_tbl_by_row(h = fc_horizon,
                                            n_cv = n_cv, 
                                            training_length = training_length, 
                                            models_tbl = specs_size_5, 
                                            var_data = var_data,
                                            target_transform = target_transform, 
                                            target_level_ts = target_level_ts, 
                                            names_exogenous = names_exogenous, 
                                            future_exo_cv = cv_extension_of_exo$future_exo_cv)
toc()


## update passing and tried models
tried_models_size_5_pr <- cv_size_5_per_row$tried_models_tbl
passing_models_size_5_pr <- cv_size_5_per_row$passing_models_tbl

accumulated_tried_models <- rbind(accumulated_tried_models, tried_models_size_5_pr)
accumulated_passing_models <- rbind(accumulated_passing_models, passing_models_size_5_pr)


accumulated_passing_models <- distinct(accumulated_passing_models, short_name, .keep_all = TRUE)
accumulated_tried_models <- distinct(accumulated_tried_models, short_name, .keep_all = TRUE)

write.xlsx(accumulated_passing_models, file = "./Excel_Output/Ecuador/All_accumulated_passing_models_ecuador_t165_lag_3.xlsx")

saveRDS(list(cv_size_2 = cv_size_2_per_row, cv_size_3 = cv_size_3_per_row, 
             cv_size_4 = cv_size_4_per_row, cv_size_5 = cv_size_5_per_row, 
             all_tried_models_2345 = accumulated_tried_models,
             all_passing_models_2345 = accumulated_passing_models),
        file = "./data/forecast_models/all_ecuador_models_new_data_all_variables_restricted_combos_t165_lag_3.rds")

