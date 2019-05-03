library(tibbletime)
library(xts)
library(readxl)
library(timetk)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(forecast)
library(tictoc)
library(tidyselect)
library(ggplot2)
library(ggthemes)

yoy_to_level <- function(yoy_series, level_series){
  
  yoy_start <- tsp(yoy_series)[1]
  init_levels_start_date <- yoy_start - 1
  init_levels_end_date <- yoy_start - 0.25
  
  init_values_series <- window(level_series, start = init_levels_start_date,
                               end = init_levels_end_date)
  
  this_freq <- frequency(init_values_series)
  ivs_as_vec <- init_values_series[1:this_freq]
  init_and_new <- ivs_as_vec
  
  for (t in seq(1,length(yoy_series)) ) {
    
    this_yoy <- yoy_series[t]
    this_previous_level <- init_and_new[t]
    this_level <- this_previous_level*(1+this_yoy)
    init_and_new[t+this_freq] <- this_level
  }
  
  level_from_yoy <- ts(data = init_and_new, start = init_levels_start_date, frequency = this_freq)
  
  level_from_yoy <- window(level_from_yoy, start = yoy_start)
  
  return(level_from_yoy)
  
}



any_fc_2_fc_yoy <- function(current_fc, rgdp_transformation, rgdp_level_ts) {
  
  if (is.null(current_fc)) {
    yoy_fc <- NULL
    return(yoy_fc)
  }
  
  if (all(is.na(current_fc))) {
    yoy_fc <- NA
    return(yoy_fc)
  }
  
  yq_pre_fc <- as.yearqtr(min(time(current_fc)) - 0.25)
  
  end_adjusted <- c(year(yq_pre_fc), quarter(yq_pre_fc))
  
  rgdp_level_end_adjusted  <- window(rgdp_level_ts, end = end_adjusted )
  
  
  if (rgdp_transformation == "yoy") {
    yoy_fc <- current_fc
  }
  
  
  if (rgdp_transformation == "log") {
    level_fc <- exp(current_fc)
    fc_and_data <- ts(c(rgdp_level_end_adjusted, level_fc), frequency = 4,
                      start = start(rgdp_level_end_adjusted))
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
  }
  
  
  if (rgdp_transformation == "none") {
    rgdp_data_transformed <- rgdp_level_end_adjusted 
    fc_and_data <- ts(c(rgdp_level_end_adjusted, current_fc), frequency = 4,
                      start = start(rgdp_level_end_adjusted))
    
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
  }
  
  
  if (rgdp_transformation == "diff_yoy") {
    
    rgdp_yoy_end_adjusted <- make_yoy_ts(rgdp_level_end_adjusted)
    
    last_data_undiff <- window(rgdp_yoy_end_adjusted, start = end_adjusted, 
                               end = end_adjusted)
    
    # print("last_data_undiff")
    # print(last_data_undiff)
    # 
    # print("current_fc")
    # print(current_fc)
    
    yoy_fc <- un_diff_ts(last_undiffed = last_data_undiff, diffed_ts = current_fc)
    
    # print("yoy_fc")
    # print(yoy_fc)
    
  }
  
  
  if (rgdp_transformation == "diff") {
    
    last_data_undiff <- window(rgdp_level_end_adjusted, start = end_adjusted, 
                               end = end_adjusted)
    
    level_fc <- un_diff_ts(last_undiffed = last_data_undiff, diffed_ts = current_fc)
    
    fc_and_data <- ts(c(rgdp_level_end_adjusted, level_fc), frequency = 4,
                      start = start(rgdp_level_end_adjusted))
    
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
    
  }
  
  return(yoy_fc)
  
}


any_fc_2_fc_yoy_and_level <- function(current_fc, current_transformation, level_ts,
                                      just_yoy_fc = FALSE) {
  

  if (current_transformation == "level") {
    level_fc <- current_fc 
  }
  
  if (is.null(current_fc)) {
    yoy_fc <- NULL
    level_fc <- NULL
    
    if (!just_yoy_fc) {
      return(list(level_fc = level_fc, yoy_fc = yoy_fc))
    }
    
    if (just_yoy_fc) {
      return(yoy_fc)
    }
  }
  
  if (all(is.na(current_fc))) {
    yoy_fc <- NA
    level_fc <- NA
    
    if (!just_yoy_fc) {
      return(list(level_fc = level_fc, yoy_fc = yoy_fc))
    }
    
    if (just_yoy_fc) {
      return(yoy_fc)
    }
  }
  
  yq_pre_fc <- as.yearqtr(min(time(current_fc)) - 0.25)
  
  end_adjusted <- c(year(yq_pre_fc), quarter(yq_pre_fc))
  
  series_level_end_adjusted  <- window(level_ts, end = end_adjusted )
  
  
  if (current_transformation == "yoy") {
    # print("doing yoy")
    
    yoy_fc <- current_fc
    # code from yoy to level
    level_fc <- yoy_to_level(yoy_fc, level_ts)
  }
  
  
  if (current_transformation == "log") {
    level_fc <- exp(current_fc)
    fc_and_data <- ts(c(series_level_end_adjusted, level_fc), frequency = 4,
                      start = start(series_level_end_adjusted))
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
  }

  if (current_transformation == "none" | current_transformation == "level" ) {
    # print("doing level")
    series_data_transformed <- series_level_end_adjusted 
    fc_and_data <- ts(c(series_level_end_adjusted, current_fc), frequency = 4,
                      start = start(series_level_end_adjusted))
    
    level_fc <- current_fc
    
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
  }
  
  if (current_transformation == "diff_yoy") {
    
    # print("doing diff_yoy")
    
    series_yoy_end_adjusted <- make_yoy_ts(series_level_end_adjusted)
    
    last_data_undiff <- window(series_yoy_end_adjusted, start = end_adjusted, 
                               end = end_adjusted)
    
    yoy_fc <- un_diff_ts(last_undiffed = last_data_undiff, diffed_ts = current_fc)
    
    level_fc <- yoy_to_level(yoy_fc, level_ts)
  }
  

  if (current_transformation == "diff") {
    # print("doing diff")
    
    
    last_data_undiff <- window(series_level_end_adjusted, start = end_adjusted, 
                               end = end_adjusted)
    
    level_fc <- un_diff_ts(last_undiffed = last_data_undiff, diffed_ts = current_fc)
    
    fc_and_data <- ts(c(series_level_end_adjusted, level_fc), frequency = 4,
                      start = start(series_level_end_adjusted))
    
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)
    
    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
    
  }
  
  if (current_transformation == "diff_diff") {
    # print("doing diff_diff")
    
    # print("level_ts")
    # print(level_ts)
    
    
    aux_diff_series <- diff(level_ts)
    
    # print("aux_diff_series")
    # print(aux_diff_series)
    
    # print("current_fc")
    # print(current_fc)
    
    
    aux_diff_end_adjusted  <- window(aux_diff_series, end = end_adjusted )
    
    # print("aux_diff_end_adjusted")
    # print(aux_diff_end_adjusted)
    
    
    last_data_undiff_aux <- window(aux_diff_end_adjusted, start = end_adjusted, 
                                   end = end_adjusted)
    
    diff_fc <- un_diff_ts(last_undiffed = last_data_undiff_aux, diffed_ts = current_fc)
    
    # print("diff_fc")
    # print(diff_fc)
    
    # second_end <- tsp(aux_diff_end_adjusted)[2] + 0.25
    second_end <- tsp(aux_diff_end_adjusted)[2] 
    
    # print("second_end")
    # print(second_end)
    
    series_level_end_adjusted  <- window(level_ts, end = second_end)
    
    last_data_undiff <- window(series_level_end_adjusted, start = second_end, 
                               end = second_end)
    
    level_fc <- un_diff_ts(last_undiffed = last_data_undiff, diffed_ts = diff_fc)
    
    fc_and_data <- ts(c(series_level_end_adjusted, level_fc), frequency = 4,
                      start = start(series_level_end_adjusted))
    
    fc_and_data_transformed <- make_yoy_ts(fc_and_data)

    yoy_fc <- window(fc_and_data_transformed, start = start(current_fc))
    
  } 
  
  # print("level_fc")
  # print(level_fc)
  # 
  # print("yoy_fc")
  # print(yoy_fc)

  if (!just_yoy_fc) {
    return(list(level_fc = level_fc, yoy_fc = yoy_fc))
  }
  
  if (just_yoy_fc) {
    return(yoy_fc)
  }
}


comb_ndiffs <- function(this_series, return_4_seas = FALSE, 
                        do_other_seas = FALSE, seas_test = "seas",
                        tests_alpha = c(0.01, 0.05, 0.1)) {
  
  tests_names <- c("kpss", "pp", "adf")
  tests_season_names <- c("seas", "ocsb", "hegy", "ch")
  
  tests_type <- c("level", "trend")
  
  
  # tests_of_stationarity <- as_tibble(
  #   expand.grid(tests_names, tests_type, tests_alpha,
  #               stringsAsFactors = FALSE)) %>% 
  #   rename(test = Var1, deter_part = Var2, alpha = Var3) %>% 
  #   mutate(seas_result = map_dbl(alpha,
  #                                ~ nsdiffs(x = this_series, alpha = ., 
  #                                          test = seas_test)),
  #          seas_test = seas_test,
  #          sta_result = pmap_dbl(list(test, alpha, deter_part),
  #                                ~ ndiffs(x = this_series, alpha = ..2,
  #                                         test = ..1, type = ..3)),
  #          sta_result_after_seas = pmap_dbl(
  #            list(test, alpha, deter_part, seas_result),
  #            ~ ndiffs(x = my_diff(this_series, lag = 4, differences = ..4), 
  #                     alpha = ..2, test = ..1, type = ..3)),
  #          recommendation = pmap_chr(
  #            list(seas_result, sta_result, sta_result_after_seas),
  #            ~ make_recommendation(seas = ..1, sta = ..2, sta_after_seas = ..3)
  #          )
  #   ) %>% 
  #   dplyr::select(test, deter_part, alpha, sta_result, seas_test,
  #                 seas_result, sta_result_after_seas, recommendation)
  
  
  tests_of_stationarity <- as_tibble(
    expand.grid(tests_names, tests_type, tests_alpha,
                stringsAsFactors = FALSE))
  
  # print(1)
  # print(tests_of_stationarity)
  
  tests_of_stationarity <- tests_of_stationarity  %>% 
    rename(test = Var1, deter_part = Var2, alpha = Var3) %>% 
    mutate(seas_result = map_dbl(alpha,
                                 ~ nsdiffs(x = this_series, alpha = ., 
                                           test = seas_test))
    )
  
  # print(2)
  # print(tests_of_stationarity)
  
  
  tests_of_stationarity <- tests_of_stationarity  %>% 
    mutate(
      seas_test = seas_test
    )
  
  # print(3)
  # print(tests_of_stationarity)
  
  tests_of_stationarity <- tests_of_stationarity  %>% 
    mutate(sta_result_after_seas = pmap_dbl(
      list(test, alpha, deter_part, seas_result),
      ~ ndiffs(x = my_diff(this_series, lag = 4, differences = ..4), 
               alpha = ..2, test = ..1, type = ..3)))
  
  # print(4)
  # print(tests_of_stationarity)
  
  tests_of_stationarity <- tests_of_stationarity  %>% 
    mutate(recommendation = pmap_chr(
      list(seas_result, sta_result_after_seas),
      ~ make_recommendation(seas = ..1, sta_after_seas = ..2)
    ))
  #        
  # print(5)
  # print(tests_of_stationarity)
  
  
  
  if (do_other_seas) {
    print("doing other seas")
    tests_of_seasonality <- as_tibble(
      expand.grid(tests_season_names, tests_alpha, stringsAsFactors = FALSE)) %>% 
      rename(test = Var1, alpha = Var2) %>% 
      mutate(seas_result = map2_dbl(test, alpha,
                                    suppressWarnings(
                                      ~ nsdiffs(x = this_series, alpha = .y,
                                                test = .x)))
      )
  }
  # 
  # print(6)
  # print(tests_of_seasonality)
  
  
  
  if (return_4_seas) {
    return(list(stationarity = tests_of_stationarity, 
                seas = tests_of_seasonality))
  } else {
    return(tests_of_stationarity)
  }
  
}


drop_this_vars <- function(df, vars_to_drop) {
  new_df <- df[,!(names(df) %in% vars_to_drop)]
}



each_plot_rmse_all_h <- function(selected_one, selected_two, extra_models = NULL,
                                 is_wide = FALSE, h_max = 7, 
                                 rank_h_max = 30) {
  
  rmse_names <- paste("rmse", seq(h_max), sep = "_")
  
  if (is_wide) {
    selected_one <-  selected_one %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse),
             horizon = as.numeric(substr(rmse_h, 6, 6))) %>% 
      filter(rank_h < rank_h_max +1) %>% 
      filter(!is.na(rmse)) %>% 
      ungroup()
    
    selected_two <-  selected_two %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse),
             horizon = as.numeric(substr(rmse_h, 6, 6))) %>% 
      filter(rank_h < rank_h_max +1) %>% 
      filter(!is.na(rmse)) %>% 
      ungroup()
  }
  
  
  
  rmse_table_single_h <- selected_one %>% rbind(selected_two) %>%  
    dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
  
  if (!is.null(extra_models)) {
    extra_models <- extra_models %>% 
      dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
    
    rmse_table_single_h <- rbind(rmse_table_single_h, 
                                 extra_models)
  }
  
  rmse_table_single_h <- rmse_table_single_h %>%
    arrange(rmse_h, rmse, model_function) %>% 
    mutate(idx = 1:n())
  
  max_horizon <- max(rmse_table_single_h$horizon)
  
  n_models_h <- nrow(rmse_table_single_h %>% filter(horizon == 1))
  
  max_rmse <- max(rmse_table_single_h$rmse)
  v_ticks <- 1 + n_models_h * (0:(max_horizon - 1))
  
  p <- ggplot(rmse_table_single_h, aes(x = idx, y = rmse)) + 
    geom_point(aes(color = model_function),
               size = 2.2, alpha = 0.5) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    geom_vline(xintercept = v_ticks , alpha = 0.3, 
               linetype = "dashed") +
    annotate("text", x = v_ticks[1] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 1", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[2] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 2", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[3] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 3", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[4] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 4", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[5] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 5", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[6] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 6", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[7] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 7", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[8] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 8", fontface = "bold", colour = "royalblue4") +
    theme_tufte() + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank()) +
    theme(axis.title = element_text(face = "bold"))
  
  # p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
  
  return(p)
}


extending_exogenous <- function(exodata, h, endo_end=NULL, 
                                list_of_models = NULL, freq = 4) {
  
  
  endo_end_decimal <- endo_end[1] + (endo_end[2]-1)/freq
    
  endo_end_of_fc_decimal <- endo_end_decimal + h/freq
  
  
  if(is.null(dim(exodata))) {
    nexo <- 1
  } else {
    nexo <- ncol(exodata)
  }
  
  extended_exo_list <- list_along(seq(1, nexo))
  future_exo_list <- list_along(seq(1, nexo))
  arima_models_list <- list_along(seq(1, nexo))
  
  for (i in seq(1, nexo)) {
    
    print(paste0("exo_variable = ", i))
    
    if (nexo == 1) {
      this_series <- exodata
    } else {
      this_series <- exodata[,i]
    }
    
    
    # print("in inner fun")
    # print(endo_end)
    this_series <- na.omit(this_series)
    
    
    # this_series <- window(this_series, end = endo_end)
 
    
    # print(this_series)
    
    if (is.null(list_of_models)) {
      this_ari <- auto.arima(y = this_series, stepwise = FALSE, 
                             approximation = FALSE, allowdrift = TRUE)
    } else {
      this_ari <- list_of_models[[i]]
    }
    
    this_fc <- forecast(this_ari, h = h)

    future_exo <-  this_fc$mean
    
    # print("first future_exo")
    # print(future_exo)
    
    future_exo <- window(future_exo, end = endo_end_of_fc_decimal)
    
    # print("second future_exo")
    # print(future_exo)
    
    extended_exo <- ts(data = c(this_series, future_exo), 
                       frequency = frequency(this_series),
                       start = start(this_series))
    
    
    future_exo <- window(extended_exo, start = (endo_end_decimal+1/freq),
                         end = endo_end_of_fc_decimal)
    
    # print("third future_exo")
    # print(future_exo)
    
    future_exo_list[[i]] <- future_exo
    extended_exo_list[[i]] <- extended_exo
    arima_models_list[[i]] <- this_ari
  }
  
  extended_exo_mts <- reduce(extended_exo_list, ts.union)
  future_exo_mts <- reduce(future_exo_list, ts.union)
  colnames(extended_exo_mts) <- colnames(exodata)
  colnames(future_exo_mts) <- colnames(exodata)
  
  return(list(extended_exo = extended_exo_mts,
              future_exo = future_exo_mts,
              arima_models = arima_models_list))
}



extending_exogenous_for_cv <- function(exodata, h, endo_end, n_cv, 
                                       list_of_models = NULL, 
                                       same_model_across_cv = TRUE,
                                       fixed_window = FALSE,
                                       training_length = NULL) {
  
  exodata <- window(exodata, end = endo_end)
  
  if (is.null(dim(exodata))) {
    full_sample_length <- length(exodata)
    names_exogenous <- "exo_vbl"
  } else {
    full_sample_length <- nrow(exodata)
    names_exogenous <- colnames(exodata)
  }
  
  # print(paste0("full_sample_length = ", full_sample_length))
  
  if (is.null(list_of_models)) {
    if (same_model_across_cv) {
      full_sample_results <- extending_exogenous(exodata = exodata, h = h, 
                                                 endo_end = endo_end)
      full_sample_models <- full_sample_results[["arima_models"]]
    }
  }
  
  fcs_per_cv <- list_along(seq(1, n_cv))
  models_per_cv <- list_along(seq(1, n_cv))
  
  for (i  in seq(1, n_cv)) {
    print(paste0("cv = ", i))
    test_offset <- h + i - 1
    end_index <- full_sample_length - test_offset
    # print(paste0("end_index = ", end_index))
    exodata_train <- subset(exodata, end = end_index)
    
    if (fixed_window) {
      print("it is fixed window")
      start_index <- end_index - training_length + 1
      exodata_train <- subset(exodata_train, start = start_index)
    }
    
    # print(exodata_train)
    
    cv_endo_date = end(exodata_train)
    # print(cv_endo_date)
    
    if (same_model_across_cv) {
      this_cv_results <-  extending_exogenous(exodata = exodata_train, h = h, 
                                              endo_end = cv_endo_date, 
                                              list_of_models = full_sample_models)
      this_cv_models <- this_cv_results[["arima_models"]]
    } else {
      this_cv_results <-  extending_exogenous(exodata = exodata_train, h = h, 
                                              endo_end = cv_endo_date, 
                                              list_of_models = NULL)
      this_cv_models <- this_cv_results[["arima_models"]]
    }
    
    # print(this_cv_results[["future_exo"]])
    
    this_cv_fcs_mts <- this_cv_results[["future_exo"]]
    colnames(this_cv_fcs_mts) <- names_exogenous
    # print("names_exogenous")
    # print(names_exogenous)
    # print("this_cv_fcs_mts")
    # print(this_cv_fcs_mts)
    
    fcs_per_cv[[i]] <- this_cv_fcs_mts
    models_per_cv[[i]] <- this_cv_models
    
    
  }
  
  return(list(future_exo_cv = fcs_per_cv,
              arima_models_cv = models_per_cv))
}




facet_rmse_all_h <- function(selected_models_tbl, extra_models = NULL) {
  
  rmse_table_single_h <- selected_models_tbl %>% 
    dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon)
  
  if (!is.null(extra_models)) {
    extra_models <- extra_models %>% 
      dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
    
    rmse_table_single_h <- rbind(rmse_table_single_h, 
                                 extra_models)
  }
  
  rmse_table_single_h <- rmse_table_single_h %>% 
    arrange(rmse_h, model_function, rmse) %>% 
    mutate(idx = 1:n()) %>% 
    group_by(horizon) %>% 
    mutate(id_in_h = 1:n())
  
  max_horizon <- max(rmse_table_single_h$horizon)
  
  n_models_h <- nrow(rmse_table_single_h %>% filter(horizon == 1))
  
  max_rmse <- max(rmse_table_single_h$rmse)
  
  labels <- c(rmse_1 = "RMSE h = 1", rmse_2 = "RMSE h = 2", 
              rmse_3 = "RMSE h = 3", rmse_4 = "RMSE h = 4",
              rmse_5 = "RMSE h = 5", rmse_6 = "RMSE h = 6",
              rmse_7 = "RMSE h = 7", rmse_8 = "RMSE h = 8")
  
  # labels <- paste0("RMSE h = ", 1:max_horizon)
  # print(labels)
  
  p <- ggplot(rmse_table_single_h, aes(x = id_in_h, y = rmse)) + 
    geom_point(aes(color = model_function), size = 2.2, alpha = 0.8) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    facet_wrap(~ rmse_h, labeller=labeller(rmse_h = labels)) + 
    theme_bw()  + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank())
  
  
  return(p)
}



fcs_accu <- function(fc_mat, test_data_mat) {
  
  errors_mat <- test_data_mat - fc_mat
  rmse_vec <- sqrt(colMeans(errors_mat^2))
  mean_rmse <- mean(rmse_vec)
  return(mean_rmse)
}

find_statio_diffs <- function(data_ts, id = "this_country",  return_4_seas = FALSE, 
                              do_other_seas = FALSE, seas_test = "seas",
                              tests_alpha = c(0.01, 0.05, 0.1),
                              this_alpha = 0.05,
                              this_deterministic = "level", this_sta_test = "kpss") {
  
  
  names_of_variables <- colnames(data_ts)
  sta_reco_list <- list_along(names_of_variables)
  stationarity_list <- list_along(names_of_variables)
  
  
  for (j in seq_along(names_of_variables)) {
    this_variable_name <- names_of_variables[j]
    this_variable_ts <- data_ts[ , this_variable_name]
    this_variable_ts <- na.omit(this_variable_ts)
    tests_of_stationarity <- suppressWarnings(comb_ndiffs(
      this_variable_ts, return_4_seas = return_4_seas, 
      do_other_seas = do_other_seas, seas_test = seas_test, tests_alpha = tests_alpha))
    # print("after comb")
    tests_of_stationarity$id<- id
    # print("after id")
    tests_of_stationarity$variable <- this_variable_name
    # print("after variable")
    reco <- get_reco_from_sta(stdata = tests_of_stationarity, variable_name = this_variable_name,
                              this_alpha = this_alpha,
                              this_deterministic = this_deterministic, this_sta_test = this_sta_test)
    
    stationarity_list[[j]] <- tests_of_stationarity
    sta_reco_list[[j]] <- reco
    
  }
  
  names(stationarity_list) <- names_of_variables
  names(sta_reco_list) <- names_of_variables
  
  reco_all_variables <- reduce(sta_reco_list, rbind)
  
  return(reco_all_variables)
}

find_statio_diffs_old <- function(data_ts, country = "this_country",  return_4_seas = FALSE, 
                              do_other_seas = FALSE, seas_test = "seas") {
  
  names_of_variables <- colnames(data_ts)
  sta_reco_list <- list_along(names_of_variables)
  stationarity_list <- list_along(names_of_variables)
  
  
  for (j in seq_along(names_of_variables)) {
    this_variable_name <- names_of_variables[j]
    this_variable_ts <- data_ts[ , this_variable_name]
    this_variable_ts <- na.omit(this_variable_ts)
    tests_of_stationarity <- suppressWarnings(comb_ndiffs(this_variable_ts))
    tests_of_stationarity$country <- country
    tests_of_stationarity$variable <- this_variable_name
    
    reco <- get_reco_from_sta(tests_of_stationarity, this_variable_name)
    
    stationarity_list[[j]] <- tests_of_stationarity
    sta_reco_list[[j]] <- reco
    
  }
  
  names(stationarity_list) <- names_of_variables
  names(sta_reco_list) <- names_of_variables
  
  reco_all_variables <- reduce(sta_reco_list, rbind)
  
  return(reco_all_variables)
}



new_find_statio_diffs <- function(data_ts, id = "this_country",  return_4_seas = FALSE, 
                                  do_other_seas = FALSE, seas_test = "seas") {
  
  
  names_of_variables <- colnames(data_ts)
  sta_reco_list <- list_along(names_of_variables)
  stationarity_list <- list_along(names_of_variables)
  
  
  for (j in seq_along(names_of_variables)) {
    this_variable_name <- names_of_variables[j]
    this_variable_ts <- data_ts[ , this_variable_name]
    this_variable_ts <- na.omit(this_variable_ts)
    tests_of_stationarity <- suppressWarnings(comb_ndiffs(
      this_variable_ts, return_4_seas = return_4_seas, 
      do_other_seas = do_other_seas, seas_test = seas_test))
    print("after comb")
    tests_of_stationarity$id<- id
    print("after id")
    tests_of_stationarity$variable <- this_variable_name
    print("after variable")
    reco <- get_reco_from_sta(tests_of_stationarity, this_variable_name)
    
    stationarity_list[[j]] <- tests_of_stationarity
    sta_reco_list[[j]] <- reco
    
  }
  
  names(stationarity_list) <- names_of_variables
  names(sta_reco_list) <- names_of_variables
  
  reco_all_variables <- reduce(sta_reco_list, rbind)
  
  return(reco_all_variables)
}


follow_rec <- function(data_tbl_ts, table_of_recommendations) {
  
  rec_rows <- nrow(table_of_recommendations)
  
  rec_column <- "this_test_alpha_deter"
  
  new_variables_list <- list_along(1:rec_rows)
  
  for (i in seq_len(rec_rows)) {
    
    this_rec <- table_of_recommendations[[i, rec_column]]
    this_variable <- table_of_recommendations[[i, "variable"]]
    this_variable_ts <- data_tbl_ts[, this_variable] 
    
    
    
    if (this_rec == "level") {
      new_variable_ts <- this_variable_ts
    }
    
    if (this_rec == "yoy") {
      new_variable_ts <- make_yoy_ts(this_variable_ts)
    }
    
    if (this_rec == "diff") {
      new_variable_ts <- base::diff(this_variable_ts)
    }
    
    if (this_rec == "diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts))
    }
    
    if (this_rec == "diff_diff") {
      new_variable_ts <- base::diff(this_variable_ts, differences = 2)
    }
    
    if (this_rec == "diff_diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts),
                                    differences = 2)
    }
    
    new_variables_list[[i]] <- new_variable_ts
    
    
  }
  
  new_data_ts <- reduce(new_variables_list, ts.union)
  colnames(new_data_ts) <- colnames(data_tbl_ts)
  
  return(new_data_ts)
  
}

follow_rec_old <- function(data_tbl_ts, table_of_recommendations) {
  
  rec_rows <- nrow(table_of_recommendations)
  
  rec_column <- "kpss_05_level"
  
  new_variables_list <- list_along(1:rec_rows)
  
  for (i in seq_len(rec_rows)) {
    
    this_rec <- table_of_recommendations[[i, rec_column]]
    this_variable <- table_of_recommendations[[i, "variable"]]
    this_variable_ts <- data_tbl_ts[, this_variable] 
    
    
    
    if (this_rec == "level") {
      new_variable_ts <- this_variable_ts
    }
    
    if (this_rec == "yoy") {
      new_variable_ts <- make_yoy_ts(this_variable_ts)
    }
    
    if (this_rec == "diff") {
      new_variable_ts <- base::diff(this_variable_ts)
    }
    
    if (this_rec == "diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts))
    }
    
    if (this_rec == "diff_diff") {
      new_variable_ts <- base::diff(this_variable_ts, differences = 2)
    }
    
    if (this_rec == "diff_diff_yoy") {
      new_variable_ts <- base::diff(make_yoy_ts(this_variable_ts),
                                    differences = 2)
    }
    
    new_variables_list[[i]] <- new_variable_ts
    
    
  }
  
  new_data_ts <- reduce(new_variables_list, ts.union)
  colnames(new_data_ts) <- colnames(data_tbl_ts)
  
  return(new_data_ts)
  
}

get_raw_data_ts <- function(id, data_path, 
                            general_variables_to_drop,
                            sheet_q_variables,
                            sheet_m_variables, 
                            column_number_date_q = 1,
                            column_number_date_m = 1){
  
  variables_to_drop <- general_variables_to_drop
  
  this_q <- read_excel(data_path, sheet = sheet_q_variables, na = c("", "NaN"))
  this_q <- drop_this_vars(this_q, variables_to_drop)
  
  # This code makes sure that the column name is called date, even if the modeller chooses a different name. 
  colnames(this_q)[column_number_date_q] <- "date"
  this_q <- as_tbl_time(this_q, index = date)
  this_q[is.nan(as.matrix(this_q))] <- NA
  
  
  if(id == "Uruguay") {
    this_q[, "rm"] <- - this_q[, "rm"]
  }
  
  this_m <- read_excel(data_path, sheet = sheet_m_variables, na = c("", "NaN"))
  this_m <- drop_this_vars(this_m, variables_to_drop)
  
  # this_m <- replace_na(data = this_m, replace = "NaN")
  this_m[is.nan(as.matrix(this_m))] <- NA
  # print(this_m, n = 320)
  colnames(this_m)[column_number_date_m] <- "date"
  this_m <- as_tbl_time(this_m, index = date)
  
  this_m_q <- this_m  %>%
    collapse_by(period = "quarterly") %>%
    group_by(date) %>% transmute_all(mean, na.rm = TRUE) %>%
    distinct(date, .keep_all = TRUE) %>% 
    ungroup() 
  
  # this_m_q[is.nan(as.matrix(as_tibble(this_m_q)))] <- NA
  
  # print(this_m_q, n = 320)
  
  
  # this_m_q <- drop_this_vars(this_m_q, variables_to_drop)
  
  # print("this_q")
  # print(this_q, n = 120)
  # print("this_m_q")
  # print(this_m_q, n = 120)
  
  
  m_and_q <- left_join(this_q, this_m_q, by = "date")
  
  maq_start <- first(tk_index(m_and_q))
  m_and_q_ts <- suppressWarnings(
    tk_ts(m_and_q, frequency = 4, start = c(year(maq_start),
                                            quarter(maq_start)))
  )
  
  m_and_q_ts[is.nan(m_and_q_ts)] <- NA
  
  return(m_and_q_ts)
}
get_raw_data_ts_old <- function(country, data_path = "./data/excel/"){
  
  this_file_path <- paste0(data_path, country, ".xlsx")
  
  general_variables_to_drop <- list(c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                                      "month", "conf_emp", "conf_ibre", "ip_ine", 
                                      "vta_auto", "exist"))
  # to make the data work we have to delete "m2" for argentina, "imp_int", "imp_k" for Ecuador and 
  # "imp_consumer", "imp_intermediate", "imp_capital" for Mexico
  extra_vars_to_drop <- list(Argentina = c("emae", "", "", "", "", "", "", "", "", "", ""), 
                             Bolivia = c("igae", "hidrocarburo", "", "", "", "", "", "", "", "", "", ""), 
                             Brasil = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Chile = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Colombia = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Ecuador = c("confianza_con", "confianza_emp", "m1", "rm", "", "", "", "", "", ""), 
                             Mexico = c("imp_consumer", "imp_intermediate", "imp_capital", "", "", "", "", "", "", "", "", ""), 
                             Paraguay = c("", "", "", "", "", "", "", "", "", "", "", ""), 
                             Peru = c("expec_demand", "", "", "", "", "", "", "", "", "", "", ""),
                             Uruguay = c("cred", "imp_nonpetro", "", "", "", "", "", "", "", ""))
  
  variables_to_drop <- map2(extra_vars_to_drop, general_variables_to_drop, c)
  
  this_variables_to_drop <- variables_to_drop[[country]]
  
  this_q <- read_excel(this_file_path, sheet = "quarterly", na = c("", "NaN"))
  this_q <- as_tbl_time(this_q, index = date)
  this_q <- dplyr::select(this_q, -c(year, hlookup))
  this_q[is.nan(as.matrix(this_q))] <- NA
  
  
  if(country == "Uruguay") {
    this_q[, "rm"] <- - this_q[, "rm"]
  }
  
  this_m <- read_excel(this_file_path, sheet = "monthly", na = c("", "NaN"))
  # this_m <- replace_na(data = this_m, replace = "NaN")
  this_m[is.nan(as.matrix(this_m))] <- NA
  # print(this_m, n = 320)
  this_m <- as_tbl_time(this_m, index = date)
  
  this_m_q <- this_m  %>%
    collapse_by(period = "quarterly") %>%
    group_by(date) %>% transmute_all(mean, na.rm = TRUE) %>%
    distinct(date, .keep_all = TRUE) %>% 
    ungroup() 
  
  # this_m_q[is.nan(as.matrix(as_tibble(this_m_q)))] <- NA
  
  # print(this_m_q, n = 320)
  
  this_q <- drop_this_vars(this_q, this_variables_to_drop)
  this_m_q <- drop_this_vars(this_m_q, this_variables_to_drop)
  
  # print("this_q")
  # print(this_q, n = 120)
  # print("this_m_q")
  # print(this_m_q, n = 120)
  
  m_and_q <- left_join(this_q, this_m_q, by = "date")
  
  maq_start <- first(tk_index(m_and_q))
  m_and_q_ts <- suppressWarnings(
    tk_ts(m_and_q, frequency = 4, start = c(year(maq_start),
                                            quarter(maq_start)))
  )
  
  m_and_q_ts[is.nan(m_and_q_ts)] <- NA
  
  return(m_and_q_ts)
}

get_raw_external_data_ts <- function(data_path, variables_to_drop_external,
                                     sheet_m_variables,
                                     column_number_date_m = 1){
  
  variables_to_drop <- variables_to_drop_external
  
  external_m <- read_excel(data_path, sheet = sheet_m_variables, na = c("", "NaN"))
  external_m[is.nan(as.matrix(external_m))] <- NA
  colnames(external_m)[column_number_date_m] <- "date"
  external_m <- as_tbl_time(external_m, index = date)
  external_m_q <- external_m  %>%
    collapse_by(period = "quarterly") %>%
    group_by(date) %>% transmute_all(mean, na.rm = TRUE) %>%
    distinct(date, .keep_all = TRUE) %>% 
    ungroup() 
  
  external_m_q <- external_m_q %>% dplyr::select(- variables_to_drop)
  
  external_start <- first(tk_index(external_m_q))
  external_m_q_ts <- suppressWarnings(tk_ts(external_m_q, frequency = 4,
                                            start = c(year(external_start), 
                                                      quarter(external_start))))
  
  external_m_q_ts[is.nan(external_m_q_ts)] <- NA
  
  return(external_m_q_ts)
}

get_raw_external_data_ts_old <- function(data_path = "./data/excel/"){
  
  external_path <- paste0(data_path,  "external.xlsx")
  
  variables_to_drop <- c("year", "month", "hlookup")
  
  external_m <- read_excel(external_path, sheet = "monthly", na = c("", "NaN"))
  external_m[is.nan(as.matrix(external_m))] <- NA
  
  external_m <- as_tbl_time(external_m, index = date)
  external_m_q <- external_m  %>%
    collapse_by(period = "quarterly") %>%
    group_by(date) %>% transmute_all(mean, na.rm = TRUE) %>%
    distinct(date, .keep_all = TRUE) %>% 
    ungroup() 
  
  external_m_q <- external_m_q %>% dplyr::select(- variables_to_drop)
  
  external_start <- first(tk_index(external_m_q))
  external_m_q_ts <- suppressWarnings(tk_ts(external_m_q, frequency = 4,
                                            start = c(year(external_start), 
                                                      quarter(external_start))))
  
  external_m_q_ts[is.nan(external_m_q_ts)] <- NA
  
  return(external_m_q_ts)
}



get_reco_from_sta <- function(stdata, variable_name, this_alpha = 0.05, 
                              this_deterministic = "level", this_sta_test = "kpss") {
  
  
  
  # print("inside get reco from sta")
  # print(stdata)
  
  unanim <- stdata %>% 
    mutate(unanimity = min(recommendation) == max(recommendation),
           unanimity = ifelse(unanimity, recommendation, NA)) %>% 
    dplyr::select(id, unanimity) %>% 
    unique()
  
  
  unanim_deterministic <- stdata %>%
    filter(deter_part == this_deterministic ) %>% 
    mutate(unan_deter = min(recommendation) == max(recommendation),
           unan_deter = ifelse(unan_deter, recommendation, NA)) %>% 
    dplyr::select(id, unan_deter) %>% 
    unique()
  
  unanim_alpha_deterministic <- stdata %>%
    filter(deter_part == this_deterministic, alpha == this_alpha ) %>% 
    mutate(unan_alpha_deter = min(recommendation) == max(recommendation),
           unan_alpha_deter = ifelse(unan_alpha_deter, recommendation, NA)) %>% 
    dplyr::select(id, unan_alpha_deter) %>% 
    unique()
  
  unanim_test <- stdata %>% 
    filter(test == this_sta_test) %>% 
    mutate(unan_test = min(recommendation) == max(recommendation),
           unan_test = ifelse(unan_test, recommendation, NA)) %>% 
    dplyr::select(id, unan_test) %>% 
    unique()
  
  unanim_test_deterministic <- stdata %>% 
    filter(test == this_sta_test, deter_part == this_deterministic) %>% 
    mutate(unan_test_deter = min(recommendation) == max(recommendation),
           unan_test_deter = ifelse(unan_test_deter, recommendation, NA)) %>% 
    dplyr::select(id, unan_test_deter) %>% 
    unique()
  
  this_test_alpha_deter_reco <- stdata %>% 
    filter(test == this_sta_test, deter_part == this_deterministic, alpha == this_alpha) %>%
    dplyr::select(id, recommendation) %>% 
    rename(this_test_alpha_deter = recommendation)
  
  id_recos <- left_join(unanim, unanim_deterministic, by = "id") %>% 
    left_join(unanim_alpha_deterministic, by = "id") %>% 
    left_join(unanim_test, by = "id") %>% 
    left_join(unanim_test_deterministic, by = "id") %>% 
    left_join(this_test_alpha_deter_reco, by = "id")
  
  id_recos$variable <- variable_name
  
  id_recos$test_if_fixed <- this_sta_test
  id_recos$alpha_if_fixed <- this_alpha
  id_recos$deter_if_fixed <- this_deterministic
  
  # yoy_reco <- stdata %>% 
  #   filter(recommendation == "yoy")
  # 
  # diff_yoy_reco <- stdata %>% 
  #   filter(recommendation == "diff_yoy")
  
  # print("id_recos")
  # print(id_recos)
  
  return(id_recos)
}


get_variable_diff_stationarity_recom <- function(reco_level, reco_trend){
  
  trend_rec <- reco_trend$this_test_alpha_deter
  level_rec <- reco_level$this_test_alpha_deter
  level_and_trend_rec <- cbind(trend_rec , level_rec, trend_rec==level_rec)
  colnames(level_and_trend_rec) <- c("level", "trend", "equal")
  level_and_trend_rec[, "equal"]
  as.logical(level_and_trend_rec[, "equal"])
  level_and_trend_rec_same <- reco_trend$variable[as.logical(level_and_trend_rec[, "equal"])]; 
  level_and_trend_rec_diff <- reco_trend$variable[!as.logical(level_and_trend_rec[, "equal"])]
  
  return(level_and_trend_rec_diff)
}

make_final_VAR_data_for_estimation_old <- function(VAR_data_level, VAR_data_trend, variables_with_trend_recom){
  
  column_names_level <- colnames(VAR_data_level)
  column_names_trend <- colnames(VAR_data_trend)
  
  common_span_ts <- ts.union(VAR_data_level[,1], VAR_data_trend[,1])
  # make sure level and trend transformed series have the same length
  VAR_data_level <- ts.union(common_span_ts, VAR_data_level)
  VAR_data_trend <- ts.union(common_span_ts, VAR_data_trend)
  VAR_data_level <- VAR_data_level[, -c(1,2)]
  VAR_data_trend <- VAR_data_trend[, -c(1,2)]
  
  colnames(VAR_data_level) <- column_names_level
  colnames(VAR_data_trend) <- column_names_trend
  
  
  for (i in 1:length(variables_with_trend_recom)) {
    VAR_data_level[,variables_with_trend_recom[i]] <- VAR_data_trend[,variables_with_trend_recom[i]]
  }
  return(VAR_data_level)
}



make_final_VAR_data_for_estimation <- function(VAR_data_level, VAR_data_trend, variables_with_trend_recom){
  
  column_names_level <- colnames(VAR_data_level)
  column_names_trend <- colnames(VAR_data_trend)
  
  common_span_ts <- ts.union(VAR_data_level[,1], VAR_data_trend[,1])
  # make sure level and trend transformed series have the same length
  VAR_data_level <- ts.union(common_span_ts, VAR_data_level)
  VAR_data_trend <- ts.union(common_span_ts, VAR_data_trend)
  VAR_data_level <- VAR_data_level[, -c(1,2)]
  VAR_data_trend <- VAR_data_trend[, -c(1,2)]
  
  colnames(VAR_data_level) <- column_names_level
  colnames(VAR_data_trend) <- column_names_trend
  
  for (i in 1:length(variables_with_trend_recom)) {
    VAR_data_level[,variables_with_trend_recom[i]] <- VAR_data_trend[,variables_with_trend_recom[i]]
  }
  
  return(VAR_data_level)
}

make_model_name_old <- function(variables, lags, t_threshold = NULL, model_function = NULL, 
                                base_variable = "rgdp", remove_base = FALSE) {
  
  if(is.null(t_threshold) | t_threshold == 0 | !is.numeric(t_threshold)){
    threshold_string <-  "000"
  } else {
    threshold_string <- as.character(100*t_threshold)
  }
  
  
  variables <- sort(variables)
  
  colap_variables <- paste(variables, collapse = "___")
  # print(colap_variables)
  
  if (!remove_base) {
    if (is.null(model_function)) {
      short_name <- paste(colap_variables, lags, threshold_string, 
                          sep = "__")
      model_name <- short_name
    } else {
      long_name <- paste(model_function, colap_variables, lags, 
                         threshold_string,  sep = "__")
      model_name <- long_name
    }
  } else {
    if (is.null(model_function)) {
      short_name <- paste(colap_variables, lags, threshold_string,
                          sep = "__")
      short_name <- str_remove(short_name, "rgdp_")
      model_name <- short_name
    } else {
      long_name <- paste(model_function, colap_variables, lags,
                         threshold_string,  sep = "__")
      long_name <- str_remove(long_name, "rgdp_")
      model_name <- long_name
    }
  }
  
  # print(model_name)
  # 
  # print("model_name")
  
  return(model_name)
}



make_model_name <- function(variables, 
                            lags, 
                            t_threshold = NULL) {
  
  if(is.null(t_threshold) | t_threshold == 0 | !is.numeric(t_threshold)){
    threshold_string <-  "000"
  } else {
    threshold_string <- as.character(100*t_threshold)
  }
  
  variables <- sort(variables)
  
  colap_variables <- paste(variables, collapse = "___")
  # print(colap_variables)
  
  short_name <- paste(colap_variables, lags, threshold_string, 
                      sep = "__")
  model_name <- short_name
  
  return(model_name)
}



make_models_tbl <- function(arima_res, var_models_and_rmse, VAR_data, h_max, 
                            force.constant, ave_rmse_sel = FALSE, pval_arima = 0.05) {
  
  
  rmse_yoy_sarimax <- arima_res$compare_rmse_yoy %>% mutate(id = 1:n())
  rmse_level_sarimax <- arima_res$compare_rmse %>% mutate(id = 1:n())
  v_lags_order_season <- arima_res$var_lag_order_season 
  extended_x_data_ts <- arima_res$mdata_ext_ts
  rgdp_ts_in_arima <- arima_res$rgdp_ts_in_arima
  
  
  rmse_yoy_sarimax <- rmse_yoy_sarimax %>% 
    left_join(v_lags_order_season, by = c("variable", "lag"))
  
  
  each_h_just_model_and_ave_rmse_var <- var_models_and_rmse %>% 
    mutate(arima_order = NA, arima_seasonal = NA, model_function = "VAR") %>% 
    dplyr::select(- starts_with("rank"))
  
  
  each_h_just_model_and_ave_rmse_sarimax <- rmse_yoy_sarimax %>%
    mutate(model_function = "Arima") %>% 
    dplyr::select(variable, lag, id, starts_with("yoy"), arima_order, arima_seasonal, 
                  model_function) %>% 
    rename(variables = variable, lags = lag) %>% 
    rename_at(vars(starts_with("yoy_rmse")), funs(sub("yoy_rmse", "rmse", .)))
  
  
  if (ave_rmse_sel) {
    models_rmse_at_each_h_arima  <- as_tibble(
      each_h_just_model_and_ave_rmse_sarimax) %>% 
      mutate(ave_rmse = rowMeans(select(., starts_with("rmse")))) %>% 
      group_by(variables) %>%
      mutate(min_ave_per_variable = min(ave_rmse)) %>% 
      filter(ave_rmse == min_ave_per_variable) %>% 
      ungroup() %>% 
      gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
      ungroup() %>% 
      group_by(rmse_h) %>% 
      mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
      filter(rmse <= rgdp_rmse) %>% 
      ungroup() %>% 
      select(-c(ave_rmse, rgdp_rmse, min_ave_per_variable)) %>% 
      arrange(rmse_h, variables)
    
  } else {
    models_rmse_at_each_h_arima <- as_tibble(
      each_h_just_model_and_ave_rmse_sarimax) %>% 
      gather(key = "rmse_h", value = "rmse", starts_with("rmse")) %>% 
      arrange(variables) %>% 
      group_by(rmse_h, variables) %>% 
      mutate(min_per_variable_and_h = min(rmse)) %>% 
      filter(rmse == min_per_variable_and_h) %>% 
      select(-min_per_variable_and_h ) %>%  
      ungroup() %>% 
      group_by(rmse_h) %>% 
      mutate(rgdp_rmse = rmse[variables == "rgdp"] ) %>% 
      filter(rmse <= rgdp_rmse) %>% 
      ungroup() %>% 
      select(-rgdp_rmse) %>% 
      arrange(rmse_h, rmse)
  }
  
  
  models_rmse_at_each_h_var <- as_tibble(each_h_just_model_and_ave_rmse_var) %>% 
    gather(key = "rmse_h", value = "rmse", starts_with("rmse"))
  
  models_rmse_at_each_h <- rbind(models_rmse_at_each_h_var, 
                                 models_rmse_at_each_h_arima) %>% 
    mutate(inv_mse = 1/rmse^2) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) %>% 
    arrange(rmse_h, rank_h)
  
  
  models_rmse_at_each_h <- models_rmse_at_each_h %>%
    mutate(short_name = map2(variables, lags,
                             ~ make_model_name(variables = .x, lags = .y)),
           long_name = pmap(list(variables, lags, model_function),
                            ~ make_model_name(variables = ..1, lags = ..2,
                                              model_function = ..3)),
           short_name = as_factor(unlist(short_name)),
           long_name = as_factor(unlist(long_name))
    ) 
  
  my_stability_fun <- function(model_type, model_object) {
    
    # print(model_type)
    # print(model_object)
    
    if (model_type == "Arima") {
      is.stable <- TRUE
      
    }
    if (model_type == "VAR"){
      is.stable <- all(roots(model_object) < 1)
    }
    
    if(!is.stable) {
      print("Ooops, not stable")
    }
    
    return(is.stable)
  }
  
  tic()
  models_rmse_at_each_h <- models_rmse_at_each_h %>%
    dplyr::distinct(long_name, .keep_all = TRUE) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse),
           fit = pmap(list(model_function, variables, lags, arima_order, 
                           arima_seasonal),
                      ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                      lags = ..3, order = ..4, seasonal = ..5,
                                      extended_x_data_ts = extended_x_data_ts,
                                      arima_rgdp_ts = rgdp_ts_in_arima,
                                      force.constant = force.constant,
                                      var_data = VAR_data)),
           is_stable = map2(model_function, fit, 
                            ~my_stability_fun(model_type = .x, model_object = .y)),
           is_white_noise = map2(model_function, fit, 
                                 ~check_resid_VAR_Arima(model_function = .x, 
                                                        fit = .y,
                                                        pval_arima = pval_arima))
    ) %>% 
    ungroup() %>% filter(is_stable == TRUE, is_white_noise == TRUE) %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) %>% 
    select(-fit)
  toc()
  
  return(models_rmse_at_each_h)
}



make_recommendation <- function(seas, sta_after_seas) {
  
  if (seas == 1 & sta_after_seas == 0) {
    recommendation <- "yoy"
  } 
  if (seas == 0 & sta_after_seas == 0) {
    recommendation <- "level"
  } 
  if (seas == 1 & sta_after_seas == 1) {
    recommendation <- "diff_yoy"
  } 
  if (seas == 0 & sta_after_seas == 1) {
    recommendation <- "diff"
  } 
  if (seas == 0 & sta_after_seas == 2) {
    recommendation <- "diff_diff"
  } 
  if (seas == 1 & sta_after_seas == 2) {
    recommendation <- "diff_diff_yoy"
  } 
  return(recommendation)
}

make_stationarity_plots <- function(vector_variables_with_diff_sta_rec, 
                                    VAR_data_for_estimation_level,
                                    VAR_data_for_estimation_trend){
  plot_list <- list()
  
  for (i in 1:length(vector_variables_with_diff_sta_rec)) {
    name_variable <- vector_variables_with_diff_sta_rec[i]
    variable_level <- na.omit(VAR_data_for_estimation_level[,vector_variables_with_diff_sta_rec[i]])
    variable_trend <- na.omit(VAR_data_for_estimation_trend[,vector_variables_with_diff_sta_rec[i]])
    
    variable_transformed_level <- paste(name_variable, "level", sep = "_")
    variable_transformed_trend <- paste(name_variable, "trend", sep = "_")
    y_label_yoy <- name_variable
    x_label <- ""
    name_plot <- paste(variable_transformed_level, "vs", variable_transformed_trend, sep = " ")
    
    
    plot <- autoplot(variable_level, series = variable_transformed_level) +
      autolayer(variable_trend, series=variable_transformed_trend, linetype = 2, size = 1) + 
      xlab(x_label) + 
      ylab(y_label_yoy) +
      ggtitle(name_plot)
    
    plot_list[[i]] <- plot
    
  }
  return(plot_list)
}

make_test_dates_list <- function(ts_data, type = "tscv", n = 8, h_max = 6,
                                 timetk_idx = TRUE, training_length = 25,
                                 external_idx = NULL) {
  
  # print("this is make test dates list")
  # print("training_length")
  # print(training_length)
  
  
  data_length <- nrow(ts_data)
  
  date_time_index <- as.yearqtr(time(ts_data))
  
  list_of_positions <- list_along(seq(1:n))
  list_of_dates <- list_along(seq(1:n))
  list_of_year_quarter <- list_along(seq(1:n))
  
  training_length_choice <- training_length
  
  if (type == "tscv") {
    
    for (i in seq.int(1:n)) {
      
      # print(paste0("i = ", i))
      
      if (training_length_choice == "per_cv_maxs") {
        training_length <- data_length - h_max - (i - 1)
        # print("training_length")
        # print(training_length)
      }
      
      from_the_right <-  i - 1
      
      end_test_pos <- data_length - from_the_right 
      start_test_pos <- end_test_pos - h_max + 1
      end_training_pos <- start_test_pos - 1
      start_training_pos <- end_training_pos - training_length + 1
      
      # print("start_training_pos")
      # print(start_training_pos)
      # print("end_training_pos")
      # print(end_training_pos)
      
      end_test_date <- date_time_index[end_test_pos]
      start_test_date <- date_time_index[start_test_pos] 
      end_training_date <- date_time_index[end_training_pos]
      start_training_date <- date_time_index[start_training_pos]
      
      end_test_year <- year(end_test_date)
      start_test_year <- year(start_test_date) 
      end_training_year <- year(end_training_date) 
      start_training_year <- year(start_training_date)
      
      end_test_quarter <- quarter(end_test_date)
      start_test_quarter <- quarter(start_test_date) 
      end_training_quarter <- quarter(end_training_date) 
      start_training_quarter <- quarter(start_training_date)
      
      this_pos <- list(
        tra_s = start_training_pos, 
        tra_e = end_training_pos,
        tes_s = start_test_pos, 
        tes_e = end_test_pos)
      
      this_date <- list(
        tra_s = start_training_date, 
        tra_e = end_training_date,
        tes_s = start_test_date, 
        tes_e = end_test_date)
      
      this_yq <- list(
        tra_s = c(start_training_year, start_training_quarter),
        tra_e = c(end_training_year, end_training_quarter),
        tes_s = c(start_test_year, start_test_quarter),
        tes_e = c(end_test_year, end_test_quarter)
      )
      
      list_of_positions[[i]] <- this_pos
      list_of_dates[[i]] <- this_date
      list_of_year_quarter[[i]] <- this_yq
      
    }
    
    return(list(
      list_of_year_quarter = list_of_year_quarter,
      list_of_dates = list_of_dates,
      list_of_positions = list_of_positions)
    )
    
  }
  
}


make_yoy_ts <- function(df_ts, freq = 4, is_log = FALSE) {
  
  if (is_log) {
    df_ts <- exp(df_ts)
  }
  
  new_ts <- base::diff(df_ts, lag = freq)/stats::lag(df_ts, k = -freq)
  
  return(new_ts)
}


my_diff <- function(series, lag = 1, differences = 1) {
  if (differences == 0) {
    x_diff <- series
  } else {
    x_diff <- diff(series, lag = lag, differences = differences)
  }
  return(x_diff)
}

single_plot_rmse_all_h <- function(selected_models_tbl, extra_models = NULL,
                                   is_wide = FALSE, h_max = 7, 
                                   rank_h_max = 30) {
  
  rmse_names <- paste("rmse", seq(h_max), sep = "_")
  
  if (is_wide) {
    selected_models_tbl <-  selected_models_tbl %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
      group_by(rmse_h) %>% 
      arrange(rmse_h, rmse) %>% 
      mutate(rank_h = rank(rmse),
             horizon = as.numeric(substr(rmse_h, 6, 6))) %>% 
      filter(rank_h < rank_h_max +1) %>% 
      filter(!is.na(rmse)) %>% 
      ungroup()
  }
  
  
  
  rmse_table_single_h <- selected_models_tbl %>% 
    dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
  
  if (!is.null(extra_models)) {
    extra_models <- extra_models %>% 
      dplyr::select(variables, lags, model_function, rmse_h, rmse, horizon) 
    
    rmse_table_single_h <- rbind(rmse_table_single_h, 
                                 extra_models)
  }
  
  
  # rmse_table_single_h <- rmse_table_single_h %>%
  #   arrange(rmse_h, model_function, rmse) %>% 
  #   mutate(idx = 1:n())
  
  rmse_table_single_h <- rmse_table_single_h %>%
    arrange(rmse_h, rmse, model_function) %>% 
    mutate(idx = 1:n())
  
  max_horizon <- max(rmse_table_single_h$horizon)
  
  n_models_h <- nrow(rmse_table_single_h %>% filter(horizon == 1))
  
  max_rmse <- max(rmse_table_single_h$rmse)
  v_ticks <- 1 + n_models_h * (0:(max_horizon - 1))
  
  p <- ggplot(rmse_table_single_h, aes(x = idx, y = rmse)) + 
    geom_point(aes(color = model_function),
               size = 2.2, alpha = 0.5) + 
    coord_cartesian(ylim = c(0, 1.1*max_rmse)) + 
    geom_vline(xintercept = v_ticks , alpha = 0.3, 
               linetype = "dashed") +
    annotate("text", x = v_ticks[1] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 1", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[2] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 2", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[3] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 3", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[4] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 4", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[5] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 5", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[6] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 6", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[7] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 7", fontface = "bold", colour = "royalblue4") +
    annotate("text", x = v_ticks[8] + 0.5*n_models_h, y = 1.1*max_rmse, label = "h = 8", fontface = "bold", colour = "royalblue4") +
    theme_tufte() + 
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.title = element_blank()) +
    theme(axis.title = element_text(face = "bold"))
  
  # p + annotate("text", x = 2:3, y = 20:21, label = c("my label", "label 2"))
  
  return(p)
}





transform_cv <- function(list_series, series_name, current_form,
                         auxiliary_ts, n_cv) {
  
  # print("list_series")
  # print(list_series)
  # print("unlist(list_series)")
  # print(unlist(list_series))
  # print("is.null(unlist(list_series))")
  # print(is.null(unlist(list_series)))
  
  if (is.null(unlist(list_series))) {
    new_series_list <- list_series
    return(new_series_list)
  }
  
  
  series_name <- series_name
  new_series_list <- list_along(1:n_cv)
  
  if (current_form == "diff_yoy") {
    len_initial_cond <- 1
    
    for (td in seq_along(1:n_cv)) {
      
      this_test_data <- list_series[[td]]
      test_time <- time(this_test_data)
      start_test <- min(test_time)
      end_initial_cond <- start_test - 0.25
      start_initial_cond <- start_test - 0.25*len_initial_cond
      end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                                quarter(as.yearqtr(end_initial_cond))
      )
      start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                  quarter(as.yearqtr(start_initial_cond))
      )
      initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                                end = end_initial_cond_y_q)
      
      new_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
      
      
      new_series_list[[td]] <- new_test_data
      
    }
    
  }
  
  if (current_form == "diff") {
    len_initial_cond <- 1
    
    for (td in seq_along(1:n_cv)) {
      
      this_test_data <- list_series[[td]]
      test_time <- time(this_test_data)
      start_test <- min(test_time)
      end_initial_cond <- start_test - 0.25
      start_initial_cond <- start_test - 0.25*len_initial_cond
      end_initial_cond_y_q <- c(year(as.yearqtr(end_initial_cond)),
                                quarter(as.yearqtr(end_initial_cond))
      )
      start_initial_cond_y_q <- c(year(as.yearqtr(start_initial_cond)),
                                  quarter(as.yearqtr(start_initial_cond))
      )
      initial_cond_ts <- window(auxiliary_ts, start = start_initial_cond_y_q,
                                end = end_initial_cond_y_q)
      level_test_data <- un_diff_ts(initial_cond_ts, this_test_data)
      pre_test_level_data <- window(auxiliary_ts, end = end_initial_cond_y_q)
      data_and_test_level <- ts(c(pre_test_level_data, level_test_data),
                                frequency = 4, start = start(auxiliary_ts))
      
      data_and_test_yoy <- make_yoy_ts(data_and_test_level, freq = 4, 
                                       is_log = FALSE)
      
      new_test_data <- window(data_and_test_yoy, start = start(this_test_data),
                              end = end(this_test_data))
      
      new_series_list[[td]] <- new_test_data
    }
    
  }
  
  
  return(new_series_list)
  
}


un_diff_ts <- function(last_undiffed, diffed_ts) {
  undiffed <- as.numeric(last_undiffed) + cumsum(diffed_ts)
  
  this_year <- as.integer(floor(time(diffed_ts)))
  this_quarter <- as.integer(4 * (time(diffed_ts) - this_year + 0.25))
  undiffed_ts <- ts(undiffed, start = c(first(this_year), first(this_quarter)),
                    end = c(last(this_year), last(this_quarter)), frequency = 4)
  
  return(undiffed_ts)
}



yr_averages <- function(series_to_average) {
  
  number_of_years <- ceiling(length(series_to_average)/frequency(series_to_average))
  yearly_averages <- vector(mode = "numeric", length = number_of_years) 
  first_year <- floor(tsp(series_to_average)[1])
  current_year <- first_year
  
  # print("in yr averages")
  # print("series_to_average")
  # print(series_to_average)
  # print(number_of_years)
  
  for (i in seq(1, number_of_years)) {
    is_from_current_year <- floor(time(series_to_average)) == current_year
    this_ts_current_year <- series_to_average[is_from_current_year]
    current_year_average <- mean(this_ts_current_year)
    yearly_averages[i] <-  current_year_average
    current_year <- current_year + 1
  }
  
  return(yearly_averages)
}