source(file = "./R/utils.R")
library(MTS)
library(vars)


old_freq_n_of_variables <- function(tbl_of_models, tbl_models_only = FALSE, 
                                h_max = 8) {
  
  
  variable_n_tbl <- tbl_of_models %>% 
    dplyr::select(variables, rmse_h, ranking) %>% 
    group_by(rmse_h) %>% 
    summarise(unique_variables = list(unique(unlist(variables))),
              non_unique_variables = list(unlist(variables)),
              n = length(unlist(unique_variables)) - 1)
  
  # print(1)
  
  all_variables <- unlist(tbl_of_models$variables)
  
  # print(2)
  
  all_variables_freq_table <- tibble::as.tibble(table(all_variables)) %>% 
    arrange(desc(n))
  
  # print(3)
  
  all_variables_h_freqs <- tbl_of_models %>% 
    dplyr::select(variables, rmse_h, ranking) %>% 
    group_by(rmse_h) 
  
  all_variables_h_freqs <- all_variables_h_freqs %>% 
    summarise(freq = list(as.data.frame(table(unlist(variables)))) ) 
  
  # print(4)
  
  
  tbl_with_freqs_per_h <- reduce(all_variables_h_freqs$freq,
                                 full_join, by = "Var1") 
  
  # print(41)
  
  names(tbl_with_freqs_per_h) <- c("variable", paste0("n_", 1:h_max))
  
  
  # print(5)
  
  tbl_with_freqs_per_h <- tbl_with_freqs_per_h %>% 
    mutate(ave = rowSums(.[2:(h_max+1)], na.rm = TRUE)/h_max) %>% 
    arrange(desc(ave), desc(n_1), desc(n_2), desc(n_3), desc(n_4) ) 
  
  return(list(variable_n_tbl = variable_n_tbl,
              all_variables_freq_table = all_variables_freq_table,
              tbl_with_freqs_per_h = tbl_with_freqs_per_h))
  
}




freq_n_of_variables <- function(best_long) {
  
  
  
  if ("rmse_y" %in% names(best_long)) {
    best_long <- rename(best_long, 
                        this_period_ranking = rank_y,
                        rmse = yearly_rmse,
                        this_period_rmse = rmse_y)
  } else {
    best_long <- rename(best_long, 
                        this_period_ranking = rank_h,
                        this_period_rmse = rmse_h)
  }
  
  rmse_names <- sort(unique(best_long$this_period_rmse))
  n_names <- paste0("n_", seq_along(rmse_names))
  
  # print(rmse_names)
  
  
  # print(best_long)
  
  
  models_list <- list_along(rmse_names)
  lags_list <- list_along(rmse_names)
  threshold_list <- list_along(rmse_names)
  variables_list <- list_along(rmse_names)
  n_vbls_list <- list_along(rmse_names)
  non_unique_variables_list <- list_along(rmse_names)
  non_unique_n_vbls_list <- list_along(rmse_names)
  table_lags_list <- list_along(rmse_names)
  table_variables_list <- list_along(rmse_names)
  table_thresholds_list <- list_along(rmse_names)
  
  for (i in seq(1, length(rmse_names))) {
    this_h_tbl <- filter(best_long, this_period_rmse == rmse_names[[i]])
    
    best_model_h <- this_h_tbl$short_name[1]
    h_vbls <- unlist(this_h_tbl$variables)
    h_lags <- this_h_tbl$lags
    # print(i)
    # print("h_lags")
    # print(h_lags)
    # print("table(h_lags)")
    # print(table(h_lags))
    
    h_threshold <- this_h_tbl$t_threshold
    lags_table <- table(h_lags)
    vbls_table <- sort(table(h_vbls), decreasing = TRUE)
    threshold_table <- sort(table(h_threshold), decreasing = TRUE)
    n_vbls <- length(unique(h_vbls))
    non_unique_n_vbls <- length(h_vbls)
    
    
    models_list[[i]] <- this_h_tbl
    lags_list[[i]] <- h_lags
    threshold_list[[i]] <- h_threshold
    variables_list[[i]] <- unique(h_vbls)
    non_unique_variables_list[[i]] <- h_vbls
    n_vbls_list[[i]] <- n_vbls
    non_unique_n_vbls_list[[i]] <- non_unique_n_vbls
    table_lags_list[[i]] <- as_tibble(lags_table)
    table_variables_list[[i]] <- as_tibble(vbls_table)
    table_thresholds_list[[i]] <- as_tibble(threshold_table)
  }

  tbl_with_freqs_per_h <- reduce(table_variables_list, full_join, by = "h_vbls")
  names(tbl_with_freqs_per_h) <- c("variable", paste0("n_", seq_along(rmse_names)))
  tbl_with_freqs_per_h[is.na(tbl_with_freqs_per_h)] <- 0
  
  tbl_with_freqs_per_h <- tbl_with_freqs_per_h %>% 
    mutate(ave = rowMeans(dplyr::select(., n_names), na.rm = TRUE)) %>% 
    arrange(desc(ave, starts_with("n_")))
  
  # print(table_lags_list)
  
  
  tbl_with_lags_per_h <- reduce(table_lags_list, full_join, by = "h_lags")
  names(tbl_with_lags_per_h) <- c("variable", paste0("n_", seq_along(rmse_names)))
  tbl_with_lags_per_h[is.na(tbl_with_lags_per_h)] <- 0

  tbl_with_lags_per_h <- tbl_with_lags_per_h %>%
    mutate(ave = rowMeans(dplyr::select(., n_names), na.rm = TRUE)) %>%
    arrange(desc(ave, starts_with("n_")))
   
  # print("tbl_with_freqs_per_h")
  # print(tbl_with_freqs_per_h)
  
  
  # tbl_with_freqs_per_h 
  
  tbl_with_freqs_overall <- tbl_with_freqs_per_h %>% 
    mutate(total = rowSums(dplyr::select(., n_names), na.rm = TRUE)) %>% 
    dplyr::select(variable, total)
  
  # tbl_with_freqs_overall
  
  variable_n_tbl <- tibble(rmse_h = rmse_names, 
                           unique_variables = variables_list, 
                           non_unique_n_vbls = non_unique_variables_list,
                           n = unlist(n_vbls_list) - 1)
  
  # variable_n_tbl
  
  
  return(list(variable_n_tbl = variable_n_tbl,
              all_variables_freq_table = tbl_with_freqs_overall,
              tbl_with_freqs_per_h = tbl_with_freqs_per_h,
              table_lags_list = tbl_with_lags_per_h))
  
}




get_fcs_tables_of_exo <- function(fcs_exo_ts,
                                  names_exogenous,
                                  all_transformations, 
                                  yta="current_and_next") {
  
  # fcs_exo_ts <- extension_of_exo$future_exo 
  
  exo_var_transformations <- all_transformations$all_transformations %>% 
    filter(variables %in% names_exogenous) %>% 
    dplyr::select(-c(level_recom, trend_recom)) %>% 
    mutate(followed_recom = as.character(followed_recom))
  
  names_in_exo <- colnames(fcs_exo_ts)
  
  raw_fc_list <- list_along(names_in_exo)
  yoy_fc_list <- list_along(names_in_exo)
  level_fc_list <- list_along(names_in_exo)
  raw_fc_tbl_list <- list_along(names_in_exo)
  yoy_fc_tbl_list <- list_along(names_in_exo)
  level_fc_tbl_list <- list_along(names_in_exo)
  yr_yoy_fc_list <- list_along(names_in_exo)
  yr_yoy_fc_tbl_list <- list_along(names_in_exo)
  
  for (k in seq(1:length(names_in_exo))) {
    this_name <- names_in_exo[k]
    this_recom_row <- filter(exo_var_transformations, variables == this_name)
    this_followed_recom <-  this_recom_row$followed_recom
    this_mean_from_var <- fcs_exo_ts[, this_name]
    this_level_series <- raw_data[ , this_name]
    
    this_mean_fc_yoy_and_level <- any_fc_2_fc_yoy_and_level(
      current_fc = this_mean_from_var, 
      current_transformation = this_followed_recom,
      level_ts = this_level_series)
    
    this_mean_fc  <- this_mean_from_var
    this_mean_fc_yoy <- this_mean_fc_yoy_and_level[["yoy_fc"]]
    this_mean_fc_level <- this_mean_fc_yoy_and_level[["level_fc"]]
    
    qrts_in_fc <- time(this_mean_fc_yoy) - floor(time(this_mean_fc_yoy))
    qrts_in_fc <- 4*(as.numeric(qrts_in_fc) + 0.25)
    names_yq_in_fc_yoy <- paste0(floor(time(this_mean_fc_yoy)), "_q", qrts_in_fc)
    
    this_q_raw_wide <- this_mean_fc %>% 
      matrix(data = ., nrow = 1, dimnames = list(NULL, names_yq_in_fc_yoy)) %>% 
      as_tibble()
    this_q_yoy_wide <- this_mean_fc_yoy %>% 
      matrix(data = ., nrow = 1, dimnames = list(NULL, names_yq_in_fc_yoy)) %>% 
      as_tibble()
    this_q_level_wide <- this_mean_fc_level %>% 
      matrix(data = ., nrow = 1, dimnames = list(NULL, names_yq_in_fc_yoy)) %>% 
      as_tibble()
    
    this_yr_avg <- yr_averages_of_fcs(this_mean_fc_yoy,
                                      level_series = this_level_series,
                                      yta = yta)
    first_fc_year <- floor(min(time(this_mean_fc_yoy)))
    seq_of_yrs <- seq(first_fc_year, (first_fc_year+length(this_yr_avg)-1))
    # yr_col_names <- paste0("Year_", 1:length(this_yr_avg))
    yr_col_names <- seq_of_yrs
    this_yr_avg_tbl <- this_yr_avg %>% 
      matrix(data = ., nrow = 1, dimnames = list(NULL, yr_col_names)) %>% 
      as_tibble()
    
    raw_fc_list[[k]] <- this_mean_fc
    yoy_fc_list[[k]] <- this_mean_fc_yoy
    level_fc_list[[k]] <- this_mean_fc_level
    raw_fc_tbl_list[[k]] <- this_q_raw_wide 
    yoy_fc_tbl_list[[k]] <- this_q_yoy_wide 
    level_fc_tbl_list[[k]] <- this_q_level_wide 
    yr_yoy_fc_list[[k]] <- this_yr_avg
    yr_yoy_fc_tbl_list[[k]] <- this_yr_avg_tbl
  }
  
  fc_tbl <- tibble(fc_name = names_in_exo, 
                   fc_raw = raw_fc_list, 
                   fc_yoy = yoy_fc_list,
                   fc_level = level_fc_list,
                   fc_year_yoy = yr_yoy_fc_tbl_list)
  
  yr_avg_all <- fc_tbl[, "fc_name"] %>% 
    cbind(reduce(yr_yoy_fc_tbl_list, rbind))
  
  yoy_fc_all_wide <- fc_tbl[, "fc_name"] %>% 
    cbind(reduce(yoy_fc_tbl_list, rbind))
  
  raw_fc_all_wide <- fc_tbl[, "fc_name"] %>% 
    cbind(reduce(raw_fc_tbl_list, rbind))
  
  level_fc_all_wide <- fc_tbl[, "fc_name"] %>% 
    cbind(reduce(level_fc_tbl_list, rbind))
  
  return(list(fc_q_yoy_wide = yoy_fc_all_wide,
              fc_q_level_wide = level_fc_all_wide,
              fc_q_raw_wide = raw_fc_all_wide,
              fc_y_yoy_wide = yr_avg_all)
  )
  
}


get_annual_fcs <- function(list_of_ensemble_fcs, vbls = NULL,
                           weighted_average_fc = TRUE) {
  
  n_ef <- length(list_of_ensemble_fcs)
  df_fcs <- list_along(1:n_ef)
  this_row <- list
  
  for (i in 1:n_ef) {
    this_eforecst <- list_of_ensemble_fcs[[i]]
    maxrank <- this_eforecst$max_rank_h
    model_name <- paste0("VAR_Best_", maxrank)
    if (weighted_average_fc) {
      yr_fcs <- this_eforecst$weighted_avg_all_wide
    } else {
      yr_fcs <- ensemble_fcs_list_50$simple_avg_all_wide
    }
    this_tibble <- yr_fcs
    # print(this_tibble)
    # print(maxrank)
    # print(model_name)
    
    if (!is.null(vbls)) {
      this_tibble <- filter(this_tibble,
                            fc_name %in% vbls)
    }
    
    this_tibble <- mutate(this_tibble,
                          max_rank_h = maxrank, 
                          Model = model_name)
    this_tibble <- dplyr::select(this_tibble,
                                 max_rank_h, Model, everything())
    
    df_fcs[[i]] <- this_tibble 
  }
  
  df_fcs <- reduce(df_fcs, rbind) %>% arrange(fc_name, max_rank_h)
  
  return(df_fcs)
  
}


predict_conditional <-
  function(object, cond_name, Z_cond_future, ..., 
           n.ahead = 10, ci = 0.95, dumvar = NULL){
    K <- object$K
    p <- object$p
    obs <- object$obs
    type <- object$type
    data.all <- object$datamat
    ynames <- colnames(object$y)
    n.ahead <- as.integer(n.ahead)
    Z <- object$datamat[, -c(1 : K)]
    B <- Bcoef(object)
    
    # print("ynames")
    # print(ynames)
    # 
    # print("head(Z, n = 8)")
    # print(head(Z, n = 8))
    # 
    # print("B")
    # print(B)
    
    cond_name_number <-  which(ynames == cond_name)
    
    # print("cond_name_number")
    # print(cond_name_number)
    
    cond_colindices <- seq(cond_name_number, ncol(Z), by=K) 
    
    # print("cond_colindices")
    # print(cond_colindices)
    
    Z_endov_cond <- Z[,cond_colindices]  
    
    # print("head(Z_endov_cond , n = 8)")
    # print(head(Z_endov_cond , n = 8))
    
    
    Z_no_cond <- Z[, -cond_colindices]  
    
    # print("head(Z_no_cond, n = 8)")
    # print(head(Z_no_cond , n = 8))
    
    ##
    ## Deterministic and lagged y's
    ## Retrieval of A in matrix (whole)
    ## Deterministic variables in Zdet
    ##
    if(type == "const"){
      Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
      colnames(Zdet) <- "const"
    }else if(type == "trend"){
      trdstart <- nrow(Z) + 1 + p
      Zdet <- matrix(seq(trdstart, length = n.ahead), nrow = n.ahead, ncol = 1)
      colnames(Zdet) <- "trend"
    }else if(type == "both"){
      trdstart <- nrow(Z) + 1 + p
      Zdet <- matrix(c(rep(1, n.ahead), seq(trdstart, length = n.ahead)), nrow = n.ahead, ncol = 2)
      colnames(Zdet) <- c("const", "trend")
    }else if(type == "none"){
      Zdet <- NULL
    }
    ## Include seasonal if applicable
    if(!is.null(eval(object$call$season))){
      season <- eval(object$call$season)
      seas.names <- paste("sd", 1:(season-1), sep = "")
      cycle <- tail(data.all[, seas.names], season)
      seasonal <- as.matrix(cycle, nrow = season, ncol = season - 1)
      if(nrow(seasonal) >= n.ahead){
        seasonal <- as.matrix(cycle[1:n.ahead, ], nrow = n.ahead, ncol = season -1 )
      } else {
        while(nrow(seasonal) < n.ahead){
          seasonal <- rbind(seasonal, cycle)
        }
        seasonal <- seasonal[1:n.ahead, ]
      }
      rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
      if(!is.null(Zdet)){
        Zdet <- as.matrix(cbind(Zdet, seasonal))
      } else {
        Zdet <- as.matrix(seasonal)
      }
    }
    ## Include exogenous variables if applicable
    if(!is.null(eval(object$call$exogen))){
      if(is.null(dumvar)){
        stop("\nNo matrix for dumvar supplied, but object varest contains exogenous variables.\n")
      }
      if(!all(colnames(dumvar) %in% colnames(data.all))){
        stop("\nColumn names of dumvar do not coincide with exogen.\n")
      }
      if(!identical(nrow(dumvar), n.ahead)){
        stop("\nRow number of dumvar is unequal to n.ahead.\n")
      }
      if(!is.null(Zdet)){
        Zdet <- as.matrix(cbind(Zdet, dumvar))
      } else {
        Zdet <- as.matrix(dumvar)
      }
    }
    ## Retrieving predetermined y variables
    Zy <- as.matrix(object$datamat[, 1:(K * (p + 1))])
    
    # print("head(Zy, n=8)")
    # print(head(Zy, n=8))
    
    # print("colnames(Zy)")
    # print(colnames(Zy))
    
    cond_indices_in_Zy <- seq(cond_name_number, ncol(Zy), by=K) 
    # print("cond_indices_in_Zy")
    # print(cond_indices_in_Zy)
    # 
    # print("names of cond_indices_in_Zy")
    # print(colnames(Zy)[cond_indices_in_Zy])
    
    yse <- matrix(NA, nrow = n.ahead, ncol = K)
    # sig.y <- .fecov(x = object, n.ahead = n.ahead)
    sig.y <- vars:::.fecov(x = object, n.ahead = n.ahead)
    for(i in 1 : n.ahead){
      yse[i, ] <- sqrt(diag(sig.y[, , i]))
    }
    yse <- -1 * qnorm((1 - ci) / 2) * yse
    colnames(yse) <- paste(ci, "of", ynames)
    ## forecast recursion
    forecast <- matrix(NA, ncol = K, nrow = n.ahead)
    lasty <- c(Zy[nrow(Zy), ])
    # print("before forecast recursion")
    # print("lasty")
    # print(lasty)
    # print("inside the recursion:")
    for(i in 1 : n.ahead){
      # print("i")
      # print(i)
      
      lasty <- lasty[1 : (K * p)]
      # print("lasty[1 : (K * p)]")
      # print(lasty[1 : (K * p)])
      # 
      # print("columns to be replaced")
      # print(colnames(lasty)[cond_indices_in_Zy])
      
      # print("values to be replaced")
      # print(lasty[cond_colindices])
      
      # print("Replaced with")
      # print(Z_cond_future[i, 2:(p+1)])
      
      lasty[cond_colindices] <- Z_cond_future[i, 2:(p+1)]
      
      Z <- c(lasty, Zdet[i, ])
      # print("c(lasty, Zdet[i, ])")
      # print(c(lasty, Zdet[i, ]))
      
      forecast[i, ] <- B %*% Z
      
      # print("rgc in big Z")
      # print(Z[cond_colindices])
      
      temp <- forecast[i, ]
      lasty <- c(temp, lasty)
    }
    colnames(forecast) <- paste(ynames, ".fcst", sep="")
    lower <- forecast - yse
    colnames(lower) <- paste(ynames, ".lower", sep="")
    upper <- forecast + yse
    colnames(upper) <- paste(ynames, ".upper", sep="")
    forecasts <- list()
    for(i in 1 : K){
      forecasts[[i]] <- cbind(forecast[, i], lower[, i], upper[, i], yse[, i])
      colnames(forecasts[[i]]) <- c("fcst", "lower", "upper", "CI")
    }
    names(forecasts) <- ynames
    result <- list(fcst = forecasts, endog = object$y, model = object, exo.fcst = dumvar)
    class(result) <- "varprd"
    return(result)
  }



get_raw_yoy_level_fc_mean <- function(fc_object, transformations, raw_data){
  
  this_forecast <- fc_object[["forecast"]]
  names_in_forecast <- names(this_forecast)
  transformations_in_forecast <- as_tibble(all_transformations[["all_transformations"]])
  transformations_in_forecast <- filter(transformations_in_forecast, variables %in% names_in_forecast)
  transformations_in_forecast <- mutate(transformations_in_forecast, followed_recom = as.character(followed_recom))
  
  raw_fc_list <- list_along(names_in_forecast)
  yoy_fc_list <- list_along(names_in_forecast)
  level_fc_list <- list_along(names_in_forecast)
  
  for (k in seq(1:length(names_in_forecast))) {
    this_name <- names_in_forecast[k]
    this_recom_row <- filter(transformations_in_forecast, variables == this_name)
    this_followed_recom <-  this_recom_row$followed_recom
    this_orig_fc <- this_forecast[[this_name]]
    this_mean_from_var <- this_orig_fc[["mean"]]
    this_level_series <- raw_data[ , this_name]
    
    # print("this_name")
    # print(this_name)
    # 
    # print("this_followed_recom")
    # print(this_followed_recom)
    
    # print("about to call anyyl")
    this_mean_fc_yoy_and_level <- any_fc_2_fc_yoy_and_level(
      current_fc = this_mean_from_var, 
      current_transformation = this_followed_recom,
      level_ts = this_level_series)
    # print("done calling anyyl")
    
    this_mean_fc  <- this_mean_from_var
    this_mean_fc_yoy <- this_mean_fc_yoy_and_level[["yoy_fc"]]
    this_mean_fc_level <- this_mean_fc_yoy_and_level[["level_fc"]]
    
    raw_fc_list[[k]] <- this_mean_fc
    yoy_fc_list[[k]] <- this_mean_fc_yoy
    level_fc_list[[k]] <- this_mean_fc_level
    
    if (this_name == target_variable) {
      target_mean_fc_yoy <- this_mean_fc_yoy
      target_mean_fc_level <- this_mean_fc_level
      target_mean_fc <- this_mean_fc
    }
    
  }
  
  fc_tbl <- tibble(fc_name = names_in_forecast, 
                   fc_raw = raw_fc_list, 
                   fc_yoy = yoy_fc_list,
                   fc_level = level_fc_list)
  
  return(fc_tbl)
  
  # extract target mean yoy fc as follow:
  # foo <- filter(fc_means_tbl, fc_name == target_variable) %>% dplyr::select(fc_yoy) %>% .[[1,1]]
  
  
} 


specs_to_fc <- function(var_data, 
                        raw_data,
                        variables, 
                        lags,
                        h, 
                        target_name, 
                        all_transformations, 
                        extended_exo_mts,
                        t_thresholds = 0, 
                        do_tests = FALSE,
                        names_exogenous = c("")){
  pass_tests <- TRUE
  
  # print("in specs to fc, names exogenous is")
  # print(names_exogenous)
  
  # print("In specs to fc")
  
  
  if (length(t_thresholds) == 1) {
    if (t_thresholds == 0) {
      is_unrestricted <- TRUE
    } else {
      is_unrestricted <- FALSE
    }
  } else {
    is_unrestricted <- FALSE
  }
  
  fit <- try(fit_VAR_rest(var_data = var_data, variables = variables,
                          p = lags, t_thresh = t_thresholds,
                          names_exogenous = names_exogenous),
             silent = TRUE)
  
  if (is_unrestricted) {
    thresh_fit_tbl <- tibble(t_threshold = t_thresholds, fit = list(fit))
    
  } else {
    thresh_fit_tbl <- fit
  }
  
  nfits <- nrow(thresh_fit_tbl)
  # print(nfits)
  if (is.null(nfits)) {
    print("fit:")
    print(fit)
    print(variables)
    print(lags)
    print(t_thresholds)
  }
  all_fits_list <- list_along(seq(1, nfits))
  
  for (f in seq(1, nfits)) {
    this_row <- thresh_fit_tbl[f, ]
    this_fit <- this_row[["fit"]][[1]]
    this_thresh <- this_row[["t_threshold"]]
    fit_class <- class(this_fit)[[1]]
    
    if (fit_class != "varest") {
      do_tests <- FALSE
      pass_tests <- FALSE
      tested <- FALSE
    }
    
    msg <- "ok"
    
    if (this_thresh > 0 & fit_class != "varest") {
      msg <- "restr_fail"
    }
    
    if (this_thresh == 0 & fit_class != "varest") {
      msg <- "unrestr_fail"
    }
    
    tested <- FALSE
    
    if (do_tests) {
      tested <- TRUE
      is_stable <-  try(all(vars::roots(this_fit) < 1))
      if(class(is_stable) == "try-error") {
        print("problem with var roots. Current variables are")
        print(variables)
        is_stable <- FALSE 
      }
      is_white_noise <-  check_resid_VAR(this_fit)
      pass_tests <- is_stable & is_white_noise
      
    }
    
    if (tested) {
      if (!is_stable) {
        msg <- "unstable"
      } 
      
      if (is_stable & !is_white_noise) {
        msg <- "not_white_noise"
      }
    }
    
    if (!tested) {
      is_stable <- NA
      is_white_noise <- NA
      pass_tests <- NA
    }
    
    if (is.na(pass_tests)) {
      do_fc <- fit_class == "varest"
    } else {
      do_fc <- pass_tests
    }
    
    this_fc <- forecast_VAR_one_row(this_fit, h, variables, extended_exo_mts, 
                                    names_exogenous = names_exogenous, exo_lag = NULL) 
    
    raw_yoy_level_fc_tbl <- get_raw_yoy_level_fc_mean(
      fc_object = this_fc, 
      transformations = all_transformations, 
      raw_data = raw_data)
    
    target_mean_fc  <- filter(raw_yoy_level_fc_tbl, fc_name == target_name) %>% 
        dplyr::select(fc_raw) %>% .[[1,1]]
      
    target_mean_fc_yoy <- filter(raw_yoy_level_fc_tbl, fc_name == target_name) %>%
        dplyr::select(fc_yoy) %>% .[[1,1]]
    
    # print("target_mean_fc_yoy")
    # print(target_mean_fc_yoy)
    
    target_mean_fc_level <- filter(raw_yoy_level_fc_tbl, fc_name == target_name) %>%
        dplyr::select(fc_level) %>% .[[1,1]]
        
      
    tibble_to_return <- tibble(msg=msg, tested=tested, pass_tests=pass_tests,
                               t_threshold=this_thresh, variables=list(variables),
                               fc_obj=list(this_fc), target_mean_fc=list(target_mean_fc), 
                               target_mean_fc_yoy=list(target_mean_fc_yoy),
                               target_mean_fc_level=list(target_mean_fc_level),
                               fcs_all_tbl = list(raw_yoy_level_fc_tbl))
    
    # tibble_to_return <- cbind(tibble_to_return, raw_yoy_level_fc_tbl)
    
    # tibble_to_return <- as_tibble(c(tibble_to_return, rmse_yoy_all_h))
    
    # print("raw_yoy_level_fc_tbl")
    # print(raw_yoy_level_fc_tbl)
    # print("tibble_to_return")
    # print(tibble_to_return)
    
    all_fits_list[[f]] <- tibble_to_return
  }
  
  # print("all_fits_list")
  # print(all_fits_list)
  
  all_fits_list <- reduce(all_fits_list, rbind)
  
  return(all_fits_list)
  
}


basic_forecast <- function(var_data,
                           raw_data,
                           models_tbl, 
                           extended_exo_list, 
                           fc_horizon, 
                           target_name, 
                           all_transformations, 
                           names_exogenous,
                           max_rank_h = NULL,
                           training_length = NULL,
                           n_cv = NULL,
                           cv_extension_of_exo = NULL,
                           recompute_rmse = FALSE,
                           do_yearly_calculations = FALSE){
  
  
  extended_exo_mts <- extended_exo_list$extended_exo
  
  
  if (!is.null(max_rank_h)) {
    models_tbl <- discard_by_rank(models_tbl, max_rank_h)
  }
  
  variables <- models_tbl$variables
  lags <- models_tbl$lags
  t_threshold <- models_tbl$t_threshold
  
  model_and_fcs <- mutate(
    models_tbl,
    fc_tbl = pmap(list(variables, lags, t_threshold),
                  ~ specs_to_fc(var_data = var_data, 
                                raw_data = raw_data,
                                variables = ..1,
                                lags = ..2, 
                                t_thresholds = ..3,
                                h = fc_horizon,
                                extended_exo_mts = extended_exo_mts,
                                all_transformations = all_transformations, 
                                target_name = target_name, 
                                do_tests = FALSE,
                                names_exogenous = names_exogenous)
    )
  )
  
  return(model_and_fcs)
}


ensemble_fc_by_row <- function(var_data,
                               raw_data,
                               models_tbl, 
                               extended_exo_list, 
                               fc_horizon, 
                               target_name, 
                               all_transformations, 
                               names_exogenous,
                               max_rank_h = NULL,
                               superset_fcs = NULL,
                               training_length = NULL,
                               n_cv = NULL,
                               cv_extension_of_exo = NULL,
                               recompute_rmse = FALSE,
                               do_yearly_calculations = TRUE,
                               yta = "current_and_next") {
  
  
  fcs_exo_for_tables <- get_fcs_tables_of_exo(
    fcs_exo_ts = extended_exo_list$future_exo,
    names_exogenous = names_exogenous,
    all_transformations = all_transformations,
    yta = yta) 
  
  if(is.null(superset_fcs)) {
    basic_fcs <- basic_forecast(
      var_data = var_data, 
      raw_data = raw_data,
      models_tbl = models_tbl,
      extended_exo_list =  extended_exo_list,
      fc_horizon = fc_horizon,
      target_name = target_name,
      all_transformations = all_transformations,
      names_exogenous = names_exogenous,
      max_rank_h = max_rank_h)
  }
  
  if(!is.null(superset_fcs)){
    basic_fcs <- superset_fcs
  }
  
  ensemble_h_list <- ensemble_fc_by_row_by_h(
    var_data = var_data, 
    raw_data = raw_data,
    models_tbl = models_tbl,
    extended_exo_list =  extended_exo_list,
    fc_horizon = fc_horizon,
    target_name = target_name,
    all_transformations = all_transformations,
    names_exogenous = names_exogenous,
    max_rank_h = max_rank_h,
    superset_fcs = basic_fcs)
  
  
  ensemble_y_list <- ensemble_fc_by_row_by_year(
    var_data = var_data, 
    raw_data = raw_data,
    models_tbl = models_tbl,
    extended_exo_list =  extended_exo_list,
    fc_horizon = fc_horizon,
    target_name = target_name,
    all_transformations = all_transformations,
    names_exogenous = names_exogenous,
    max_rank_h = max_rank_h,
    superset_fcs = basic_fcs,
    yta = yta)
  
  if (!is.null(max_rank_h)) {
    models_tbl <- discard_by_rank(models_tbl, max_rank_h)
  }


  variables_info_h <- freq_n_of_variables(ensemble_h_list$model_and_fcs)

  variables_info_y <- freq_n_of_variables(ensemble_y_list$model_and_fcs)

  variables_count <- variables_info_h$variable_n_tbl 
  variables_overall_freq <- variables_info_h$all_variables_freq_table
  variables_by_h_freq <- variables_info_h$tbl_with_freqs_per_h 
  
  variables_count_y <- variables_info_y$variable_n_tbl
  variables_overall_freq_y <- variables_info_y$all_variables_freq_table
  variables_by_h_freq_y <- variables_info_y$tbl_with_freqs_per_h 
  
  variables_by_h_freq_qy <- full_join(
    variables_by_h_freq, variables_by_h_freq_y,
    by = "variable", suffix = c("", "_y"))
  
  variables_freq_rel_qy <- variables_by_h_freq_qy %>% 
    mutate_if(is.numeric, ~ round(100*(./.[1]))) %>% 
    arrange(desc(n_1_y)) %>% 
    filter(variable != variable[1])
  
  variables_overall_freq_qy <- full_join(
    variables_overall_freq, variables_overall_freq_y,
    by = "variable", suffix = c("", "_y"))
  
  # print(variables_overall_freq_qy)
  
  variables_overall_freq_rel_qy <- variables_overall_freq_qy  %>% 
    mutate_if(is.numeric, ~ round(100*(./.[1]))) %>% 
    arrange(desc(total_y)) %>% 
    filter(variable != variable[1])
  
  names(variables_overall_freq_rel_qy) <- c("percent", "percent_y")
  
  # print(variables_overall_freq_rel_qy)
  
  
  weighted_avg_all_wide <- rbind(ensemble_y_list$weighted_avg_all_wide, 
                                 fcs_exo_for_tables$fc_y_yoy_wide)
  
  simple_avg_all_wide <- rbind(ensemble_y_list$simple_avg_all_wide, 
                                 fcs_exo_for_tables$fc_y_yoy_wide)
  
  
  
  return(
    list(  model_and_fcs = ensemble_h_list$model_and_fcs,
           basic_forecasts = basic_fcs,
           ensemble_tbl = ensemble_h_list$ensemble_tbl,
           weighted_avg_fc_yoy = ensemble_h_list$weighted_avg_fc_yoy,
           simple_avg_fc_yoy = ensemble_h_list$simple_avg_fc_yoy,
           ensemble_all_vbls = ensemble_h_list$ensemble_all_vbls,
           other_fcs = ensemble_h_list$other_fcs,
           model_and_fcs_y = ensemble_y_list$model_and_fcs,
           ensemble_tbl_y = ensemble_y_list$ensemble_tbl,
           weighted_avg_fc_yoy_y = ensemble_y_list$weighted_avg_fc_yoy,
           simple_avg_fc_yoy_y = ensemble_y_list$simple_avg_fc_yoy,
           ensemble_all_vbls_y = ensemble_y_list$ensemble_all_vbls,
           weighted_avg_all_wide = weighted_avg_all_wide, 
           simple_avg_all_wide = simple_avg_all_wide, 
           other_fcs_y = ensemble_y_list$other_fcs,
           max_rank_h = max_rank_h,
           fcs_exo_for_tables = fcs_exo_for_tables,
           variables_info_h = variables_info_h,
           variables_info_y = variables_info_y,
           variables_freq_rel_qy = variables_freq_rel_qy)
  )
}





ensemble_fc_by_row_by_h <- function(var_data,
                               raw_data,
                               models_tbl, 
                               extended_exo_list, 
                               fc_horizon, 
                               target_name, 
                               all_transformations, 
                               names_exogenous,
                               max_rank_h = NULL,
                               superset_fcs = NULL,
                               training_length = NULL,
                               n_cv = NULL,
                               cv_extension_of_exo = NULL,
                               recompute_rmse = FALSE,
                               do_yearly_calculations = FALSE){
  
  extended_exo_mts <- extended_exo_list$extended_exo
  
  if (!is.null(max_rank_h)) {
    models_tbl <- discard_by_rank(models_tbl, max_rank_h)
  }
  
  variables <- models_tbl$variables
  lags <- models_tbl$lags
  t_threshold <- models_tbl$t_threshold
  
  if (!is.null(superset_fcs)) {
    model_and_fcs <- superset_fcs 
    
    if (!is.null(max_rank_h)) {
      model_and_fcs <- discard_by_rank(model_and_fcs, max_rank_h = max_rank_h)
    }
  }
  
  
  if (is.null(superset_fcs)) {
      model_and_fcs <- mutate(
        models_tbl,
        fc_tbl = pmap(list(variables, lags, t_threshold),
                      ~ specs_to_fc(var_data = var_data, 
                                    raw_data = raw_data,
                                    variables = ..1,
                                    lags = ..2, 
                                    t_thresholds = ..3,
                                    h = fc_horizon,
                                    extended_exo_mts = extended_exo_mts,
                                    all_transformations = all_transformations, 
                                    target_name = target_name, 
                                    do_tests = FALSE,
                                    names_exogenous = names_exogenous)
        )
      )
  }
    
    
  rmse_names <- paste0("rmse_", seq(1, fc_horizon))
  
  is_yr <- str_detect(names(model_and_fcs), "rmse_yr")
  rmse_names_y <- paste0("rmse_yr_", 1:sum(is_yr))

  model_and_fcs <- dplyr::select(model_and_fcs, c(short_name, rmse_names, lags, fc_tbl))
  
  model_and_fcs <- model_and_fcs %>% 
    mutate(fc_tbl_with_0 = fc_tbl, 
           fc_tbl = map(fc_tbl_with_0, ~ .x[nrow(.x), ])) %>% 
    dplyr::select(-fc_tbl_with_0)
  
  
  model_and_fcs <- unnest(model_and_fcs, fc_tbl)
  model_and_fcs <- dplyr::select(model_and_fcs, -fc_obj)
  model_and_fcs <- mutate(
    model_and_fcs, 
    short_name = pmap(list(variables, lags, t_threshold),
                      ~ make_model_name(variables = ..1,
                                        lags = ..2,
                                        t_threshold = ..3)),
    short_name = unlist(short_name))
  model_and_fcs <- dplyr::select(model_and_fcs, short_name, everything())
  model_and_fcs_long <- gather(model_and_fcs, key = "rmse_h", value = "rmse", 
                               rmse_names)
  
  model_and_fcs_long <-  group_by(model_and_fcs_long, rmse_h) %>%
    mutate(rank_h = rank(rmse)) 

  if (!is.null(max_rank_h)) {
    model_and_fcs_long <- filter(model_and_fcs_long, rank_h <= max_rank_h)
  }
  
  model_and_fcs_long <- mutate(model_and_fcs_long,
                               inv_mse = 1/(rmse*rmse),
                               model_weight = inv_mse/sum(inv_mse))
  
  model_and_fcs_long <- mutate(model_and_fcs_long,
                               horizon = as.numeric(substr(rmse_h, 6, 6))) 
  
  model_and_fcs_long <- mutate(model_and_fcs_long, 
                               this_h_fc_yoy = map2_dbl(target_mean_fc_yoy,
                                                        horizon,
                                                        ~ .x[.y])) 
  
  model_and_fcs_long <- mutate(model_and_fcs_long, 
                               weighted_this_h_fc_yoy = map2_dbl(this_h_fc_yoy, 
                                                                 model_weight, 
                                                                 ~ .x*.y)) 
  
  model_and_fcs_long <- mutate(model_and_fcs_long,
                               waverage_fc_yoy_h = sum(weighted_this_h_fc_yoy)) 

  
  model_and_fcs_long <- mutate(model_and_fcs_long,
                               average_fc_yoy_h = mean(this_h_fc_yoy)) 
  
  waverage_tbl <- model_and_fcs_long %>% 
    dplyr::select(rmse_h, waverage_fc_yoy_h, average_fc_yoy_h) %>% 
    summarise(waverage_fc_yoy_h = unique(waverage_fc_yoy_h),
              average_fc_yoy_h = unique(average_fc_yoy_h))
  
  fc_start <- start(model_and_fcs_long$target_mean_fc_yoy[[1]])
  
  fc_freq <- frequency(model_and_fcs_long$target_mean_fc_yoy[[1]])
  
  weighted_avg_fc_yoy <- ts(waverage_tbl$waverage_fc_yoy_h, 
                            start = fc_start, frequency = fc_freq)
  
  simple_avg_fc_yoy <- ts(waverage_tbl$average_fc_yoy_h, 
                          start = fc_start, frequency = fc_freq)
  
  model_and_fcs_long <- ungroup(model_and_fcs_long)
  
  ensemble_tbl <- tibble(variables = list(table(unlist(model_and_fcs_long$variables))),
                         lags = list(table(unlist(model_and_fcs_long$lags))),
                         short_name = "ensemble",
                         horizon = sort(unique(model_and_fcs_long$horizon)),
                         this_h_fc_yoy = as.numeric(weighted_avg_fc_yoy),
                         this_h_fc_yoy_ew = as.numeric(simple_avg_fc_yoy)
  )
  
  
  u_long <- unnest(model_and_fcs_long, fcs_all_tbl,
                .preserve = c(variables, target_mean_fc, target_mean_fc_yoy))
  
  other_fcs <- dplyr::select(u_long, 
                             c(short_name, fc_name, rank_h, horizon, 
                               model_weight, fc_yoy, fc_raw, fc_level)) %>% 
    mutate(this_fc_yoy = map2_dbl(fc_yoy, horizon, ~ .x[.y]),
           this_fc_level = map2_dbl(fc_level, horizon, ~ .x[.y]),
           this_fc_raw = map2_dbl(fc_raw, horizon, ~ .x[.y]))
  
  other_fcs <- other_fcs %>%  
    group_by(horizon, fc_name) %>% 
    mutate(total_w_this_vbl = sum(model_weight),
           indiv_w_this_h_vbl = model_weight/total_w_this_vbl,
           n_appears = n(),
           w_this_h_fc_yoy = this_fc_yoy*indiv_w_this_h_vbl,
           vbl_ensemble_h_fc_yoy = sum(w_this_h_fc_yoy),
           vbl_ensemble_h_fc_yoy_eqw = mean(this_fc_yoy)
    )
  
  ensemble_fcs_all_vbls <- other_fcs %>% 
    dplyr::select(fc_name, horizon, vbl_ensemble_h_fc_yoy, vbl_ensemble_h_fc_yoy_eqw) 
  
  ensemble_fcs_all_vbls <- ensemble_fcs_all_vbls %>% 
    distinct(fc_name, .keep_all = TRUE) 
  
  return(list(model_and_fcs = model_and_fcs_long,
              ensemble_tbl = ensemble_tbl,
              weighted_avg_fc_yoy = weighted_avg_fc_yoy,
              simple_avg_fc_yoy = simple_avg_fc_yoy,
              ensemble_all_vbls = ensemble_fcs_all_vbls,
              other_fcs = other_fcs))
}




ensemble_fc_by_row_by_year <- function(var_data,
                               raw_data,
                               models_tbl, 
                               extended_exo_list, 
                               fc_horizon, 
                               target_name, 
                               all_transformations, 
                               names_exogenous,
                               max_rank_h = NULL,
                               superset_fcs = NULL,
                               training_length = NULL,
                               n_cv = NULL,
                               cv_extension_of_exo = NULL,
                               recompute_rmse = FALSE,
                               do_yearly_calculations = FALSE,
                               yta = "current_and_next"){
  
  extended_exo_mts <- extended_exo_list$extended_exo
  
  target_level_ts <- na.omit(raw_data[,target_name])
  
  
  if (!is.null(max_rank_h)) {
    models_tbl <- discard_by_rank(models_tbl, max_rank_h, is_yearly = TRUE)
  }
  
  variables <- models_tbl$variables
  lags <- models_tbl$lags
  t_threshold <- models_tbl$t_threshold
  
  if (!is.null(superset_fcs)) {
    model_and_fcs <- superset_fcs 
    
    if (!is.null(max_rank_h)) {
      model_and_fcs <- discard_by_rank(model_and_fcs, max_rank_h = max_rank_h,
                                       is_yearly = TRUE)
    }
  }
  
  if (is.null(superset_fcs)) {
      model_and_fcs <- mutate(
        models_tbl,
        fc_tbl = pmap(list(variables, lags, t_threshold),
                      ~ specs_to_fc(var_data = var_data, 
                                    raw_data = raw_data,
                                    variables = ..1,
                                    lags = ..2, 
                                    t_thresholds = ..3,
                                    h = fc_horizon,
                                    extended_exo_mts = extended_exo_mts,
                                    all_transformations = all_transformations, 
                                    target_name = target_name, 
                                    do_tests = FALSE,
                                    names_exogenous = names_exogenous)
        )
      )
  }
  
  
  rmse_h_names <- paste0("rmse_", seq(1, fc_horizon))
  
  is_yr <- str_detect(names(model_and_fcs), "rmse_yr")
  rmse_names <- paste0("rmse_yr_", 1:sum(is_yr))
  
  model_and_fcs <- dplyr::select(model_and_fcs, c(short_name, rmse_names, lags, fc_tbl))
  
  
  model_and_fcs <- model_and_fcs %>% 
    mutate(fc_tbl_with_0 = fc_tbl, 
           fc_tbl = map(fc_tbl_with_0, ~ .x[nrow(.x), ])) %>% 
    dplyr::select(-fc_tbl_with_0)
  
  model_and_fcs <- unnest(model_and_fcs, fc_tbl)
  model_and_fcs <- dplyr::select(model_and_fcs, -fc_obj)

  model_and_fcs <- dplyr::select(model_and_fcs, short_name, everything())
  
  model_and_fcs_long <- gather(model_and_fcs,
                               key = "rmse_y",
                               value = "yearly_rmse",
                               rmse_names)
  
  model_and_fcs_long <- mutate(
    model_and_fcs_long,
    horizon = as.numeric(str_extract(rmse_y, "\\d{1,9}"))) 
  
  model_and_fcs_long <-  group_by(model_and_fcs_long, rmse_y) %>%
    mutate(rank_y = rank(yearly_rmse)) 

  if (!is.null(max_rank_h)) {
    model_and_fcs_long <- filter(model_and_fcs_long, rank_y <= max_rank_h)
  }
  
  model_and_fcs_long <- mutate(model_and_fcs_long,
                               inv_mse = 1/(yearly_rmse*yearly_rmse),
                               model_weight = inv_mse/sum(inv_mse))
  
  # cambiar yr_averages por nueva funcion
  
  model_and_fcs_long <- mutate(model_and_fcs_long, 
                               target_y_avg_yoy = map(
                                 target_mean_fc_yoy,
                                 ~ yr_averages_of_fcs(.x, 
                                   level_series = target_level_ts,
                                   yta = yta))) 
  
  model_and_fcs_long <- mutate(model_and_fcs_long, 
                               this_h_fc_yoy = map2_dbl(target_y_avg_yoy,
                                                        horizon,
                                                        ~ .x[.y])) 
  
  model_and_fcs_long <- mutate(model_and_fcs_long, 
                               weighted_this_h_fc_yoy = map2_dbl(this_h_fc_yoy, 
                                                                 model_weight, 
                                                                 ~ .x*.y)) 
  
  model_and_fcs_long <- mutate(model_and_fcs_long,
                               waverage_fc_yoy_h = sum(weighted_this_h_fc_yoy)) 
  
  
  model_and_fcs_long <- mutate(model_and_fcs_long,
                               average_fc_yoy_h = mean(this_h_fc_yoy)) 
  
  waverage_tbl <- model_and_fcs_long %>% 
    dplyr::select(rmse_y, waverage_fc_yoy_h, average_fc_yoy_h) %>% 
    summarise(waverage_fc_yoy_h = unique(waverage_fc_yoy_h),
              average_fc_yoy_h = unique(average_fc_yoy_h))
  
  fc_start <- start(model_and_fcs_long$target_mean_fc_yoy[[1]])
  
  fc_freq <- 1
  
  weighted_avg_fc_yoy <- ts(waverage_tbl$waverage_fc_yoy_h, 
                            start = fc_start, frequency = fc_freq)
  
  simple_avg_fc_yoy <- ts(waverage_tbl$average_fc_yoy_h, 
                            start = fc_start, frequency = fc_freq)
  
  model_and_fcs_long <- ungroup(model_and_fcs_long)
  
  ensemble_tbl <- tibble(variables = list(table(unlist(model_and_fcs_long$variables))),
                         lags = list(table(unlist(model_and_fcs_long$lags))),
                         short_name = "ensemble",
                         horizon = sort(unique(model_and_fcs_long$horizon)),
                         this_h_fc_yoy = as.numeric(weighted_avg_fc_yoy),
                         this_h_fc_yoy_ew = as.numeric(simple_avg_fc_yoy)
  )
  
  u_long <- unnest(model_and_fcs_long, fcs_all_tbl,
                   .preserve = c(variables, target_mean_fc, target_mean_fc_yoy))
  

  # other_fcs <- dplyr::select(u_long, 
  #                            c(short_name, fc_name, rank_y, horizon, 
  #                              model_weight, fc_yoy)) %>% 
  #   mutate(yoy_year_averages = map(fc_yoy,  ~ yr_averages(.x)),
  #          this_fc_yoy = map2_dbl(yoy_year_averages, horizon, ~ .x[.y]))
  
  other_fcs <- dplyr::select(u_long, 
                             c(short_name, fc_name, rank_y, horizon, 
                               model_weight, fc_yoy)) %>% 
    mutate(yoy_year_averages = map2(fc_yoy, fc_name,
                                    ~ yr_averages_of_fcs(
                                      yoy_fcs_to_average = .x,
                                      level_series = na.omit(raw_data[,.y]), 
                                      yta = yta)),
           this_fc_yoy = map2_dbl(yoy_year_averages, horizon, ~ .x[.y]))
  
  other_fcs <- other_fcs %>%  
    group_by(horizon, fc_name) %>% 
    mutate(total_w_this_vbl = sum(model_weight),
           indiv_w_this_h_vbl = model_weight/total_w_this_vbl,
           n_appears = n(),
           w_this_h_fc_yoy = this_fc_yoy*indiv_w_this_h_vbl,
           vbl_ensemble_h_fc_yoy = sum(w_this_h_fc_yoy),
           vbl_ensemble_h_fc_yoy_eqw = mean(this_fc_yoy)
    )
  
  ensemble_fcs_all_vbls <- other_fcs %>% 
    dplyr::select(fc_name, horizon, vbl_ensemble_h_fc_yoy, vbl_ensemble_h_fc_yoy_eqw) 
  
  ensemble_fcs_all_vbls <- ensemble_fcs_all_vbls %>% 
    distinct(fc_name, .keep_all = TRUE) 
  
  starting_year_of_fcs <- other_fcs[1, "fc_yoy"][[1]][[1]] %>% time() %>% min()

  
  yr_weighted_avg_wide <- ensemble_fcs_all_vbls %>% 
    dplyr::select(-vbl_ensemble_h_fc_yoy_eqw) %>%  
    tidyr::spread(key = horizon, value = vbl_ensemble_h_fc_yoy)
  
  yr_simple_avg_wide <- ensemble_fcs_all_vbls %>% 
    dplyr::select(-vbl_ensemble_h_fc_yoy) %>% 
    tidyr::spread(key = horizon, value = vbl_ensemble_h_fc_yoy_eqw)
  
  ensemble_fcs_all_vbls <- ungroup(ensemble_fcs_all_vbls)
  yr_weighted_avg_wide <- ungroup(yr_weighted_avg_wide)
  yr_simple_avg_wide <- ungroup(yr_simple_avg_wide)
  
  ncl <- ncol(yr_weighted_avg_wide)
  names_dates_yrs_fcs <- seq(starting_year_of_fcs, length.out = (ncl-1))
  names_counting_yrs_fcs <- paste0("Year_", 1:(ncl-1))

  names(yr_weighted_avg_wide)[2:ncl] <- names_dates_yrs_fcs
  names(yr_simple_avg_wide)[2:ncl] <- names_dates_yrs_fcs
  
  return(list(model_and_fcs = model_and_fcs_long,
              ensemble_tbl = ensemble_tbl,
              weighted_avg_fc_yoy = weighted_avg_fc_yoy,
              simple_avg_fc_yoy = simple_avg_fc_yoy,
              ensemble_all_vbls = ensemble_fcs_all_vbls,
              weighted_avg_all_wide = yr_weighted_avg_wide, 
              simple_avg_all_wide = yr_simple_avg_wide, 
              other_fcs = other_fcs))
}



cv_of_VAR_ensemble_by_row <- function(var_data,
                                      used_cv_models,
                                      fc_horizon,
                                      n_cv,
                                      training_length,
                                      cv_extension_of_exo,
                                      extension_of_exo,
                                      names_exogenous, 
                                      target_transform, 
                                      target_level_ts, 
                                      max_rank_h = NULL, 
                                      full_cv_output = FALSE,
                                      return_models_rmse = FALSE,
                                      superset_cv_fcs = NULL) {
  
  if (!is.null(max_rank_h)) {
    print(paste0("N initial models:", nrow(used_cv_models)))
    used_cv_models <- discard_by_rank(used_cv_models, max_rank_h)
    print(paste0("N models after max_rank_h:", nrow(used_cv_models)))
  }
  
  
  variables_used <- used_cv_models %>% 
    dplyr::select(variables) %>% unlist() %>% unique()
  
  if (training_length == "common_max") {
    total_obs <- nrow( na.omit(var_data[, variables_used]))
    training_length <- total_obs - h_max - (n_cv - 1)
    print(paste0("common_max = ", training_length))
  }
  
  train_test_dates <- make_test_dates_list(
    ts_data = na.omit(var_data[, variables_used]), 
    type = "tscv",
    n = n_cv, 
    h_max = fc_horizon, 
    training_length = training_length)
  
  cv_ensemble_fcs <- list_along(seq(1, n_cv))
  cv_ensemble_test_data <- list_along(seq(1, n_cv))
  cv_ensemble_errors <- list_along(seq(1, n_cv))
  cv_indiv_models_rmse <- list_along(seq(1, n_cv))
  
  
  for (i in seq(1, n_cv)) {
    
    print(paste0("This is cv round ", i))
    
    train_test_yq <- train_test_dates[["list_of_year_quarter"]]
    
    this_tra_s <- train_test_yq[[i]]$tra_s
    this_tra_e <- train_test_yq[[i]]$tra_e
    
    this_tes_s <- train_test_yq[[i]]$tes_s
    this_tes_e <- train_test_yq[[i]]$tes_e
    
    var_data_train <- window(var_data, start = this_tra_s, end = this_tra_e)
    
    future_exo_in_cv <- cv_extension_of_exo[[i]]
    sample_exo_in_cv <- window(extension_of_exo, end = start(future_exo_in_cv))
    
    nsample <- nrow(sample_exo_in_cv)
    
    reduced_rows <- nsample-1
    
    sample_exo_in_cv <- window(sample_exo_in_cv, 
                               end =  last(time(sample_exo_in_cv))-0.25 )
    
    this_extened_exo_mts <- ts(
      rbind(sample_exo_in_cv, future_exo_in_cv),
      start = start(sample_exo_in_cv), frequency = frequency(sample_exo_in_cv))
    
    if (is.null(superset_cv_fcs)) {
      ensemble_fc_list <- ensemble_fc_by_row(var_data = var_data_train,
                                             models_tbl = used_cv_models, 
                                             extended_exo_mts =  this_extened_exo_mts,
                                             fc_horizon = fc_horizon,
                                             target_level_ts = target_level_ts,
                                             target_transform = target_transform,
                                             names_exogenous = names_exogenous, 
                                             max_rank_h = max_rank_h)
    }
    
    if (!is.null(superset_cv_fcs)) {
      ensemble_fc_list <- ensemble_fc_by_row(var_data = var_data_train,
                                             models_tbl = used_cv_models, 
                                             extended_exo_mts =  this_extened_exo_mts,
                                             fc_horizon = fc_horizon,
                                             target_level_ts = target_level_ts,
                                             target_transform = target_transform,
                                             names_exogenous = names_exogenous, 
                                             max_rank_h = max_rank_h, 
                                             superset_fcs = superset_cv_fcs[[i]])
    }
    
    if (return_models_rmse) {
      cv_indiv_models_rmse[[i]] <- ensemble_fc_list$model_and_fcs
    }
    
    ensemble_fc_ts <- ensemble_fc_list$weighted_avg_fc_yoy
    
    test_target_yoy <- window(make_yoy_ts(target_level_ts), 
                              start = this_tes_s,
                              end = this_tes_e)
    
    ensemble_cv_error_yoy <- test_target_yoy - ensemble_fc_ts 
    
    cv_ensemble_fcs[[i]] <- ensemble_fc_ts
    cv_ensemble_test_data[[i]] <- test_target_yoy
    cv_ensemble_errors[[i]] <- ensemble_cv_error_yoy
  }
  
  mat_cv_errors_ensemble <- matrix(
    reduce(cv_ensemble_errors, rbind), 
    nrow = n_cv)
  
  rownames(mat_cv_errors_ensemble) <- NULL
  
  ensemble_rmse <- sqrt(colMeans(mat_cv_errors_ensemble^2, na.rm = TRUE))
  
  if (!full_cv_output){
    cv_ensemble_fcs <- NULL 
    cv_ensemble_test_data <- NULL
    cv_ensemble_errors <- NULL
  }
  
  if (!return_models_rmse) {
    cv_indiv_models_rmse <- NULL
  }
  
  return(list(ensemble_rmse = ensemble_rmse, 
              cv_ensemble_fcs = cv_ensemble_fcs, 
              cv_ensemble_test_data = cv_ensemble_test_data,
              cv_ensemble_errors = cv_ensemble_errors,
              cv_indiv_models_rmse = cv_indiv_models_rmse)
         )
}



fc_models_and_ensemble_by_row <- function(var_data, working_models,
                                          extension_of_exo, 
                                          cv_extension_of_exo,
                                          fc_horizon,
                                          target_level_ts,
                                          target_transform,
                                          n_cv, training_length, 
                                          names_exogenous = c(""), 
                                          max_rank_h = NULL){
  
  print("Forecasts of individual models and ensemble model")
  
  ensemble_fc_list <- ensemble_fc_by_row(var_data = var_data,
                                         models_tbl = working_models, 
                                         extended_exo_mts =  extension_of_exo$extended_exo,
                                         fc_horizon = fc_horizon,
                                         target_level_ts = target_level_ts,
                                         target_transform = target_transform,
                                         names_exogenous = names_exogenous, 
                                         max_rank_h = max_rank_h)
  
  print("Computing accuracy of ensemble model")
  
  cv_of_ensamble_list <- cv_of_VAR_ensemble_by_row(var_data = var_data,
                                                   used_cv_models = working_models,
                                                   fc_horizon = fc_horizon,
                                                   n_cv = n_cv,
                                                   training_length = training_length,
                                                   cv_extension_of_exo = cv_extension_of_exo$future_exo_cv,
                                                   extension_of_exo = extension_of_exo$extended_exo,
                                                   names_exogenous = names_exogenous, 
                                                   target_transform = target_transform, 
                                                   target_level_ts = target_level_ts, 
                                                   max_rank_h = max_rank_h, 
                                                   full_cv_output = FALSE)
  
  
  print("Building final table")
  
  ensemble_fc_and_rmse <- ensemble_fc_list$ensemble_tbl %>% 
    mutate(rmse_y = paste0("rmse_", 1:n()),
           rmse = cv_of_ensamble_list$ensemble_rmse)
  
  fcs_models_to_bind <- ensemble_fc_list$model_and_fcs %>% 
    mutate(lags = list(lags)) %>% 
    dplyr::select(names(ensemble_fc_and_rmse))
  
  
  models_and_ensemble_fcs <- rbind(ensemble_fc_and_rmse, 
                                   fcs_models_to_bind)
  
  models_and_ensemble_fcs <- models_and_ensemble_fcs %>% 
    group_by(rmse_y) %>% 
    mutate(ranking = rank(rmse)) %>% 
    ungroup()
  
  return(models_and_ensemble_fcs)
}




add_one_variable <- function(current_vbls, extra_vbl) {
  
  if (extra_vbl %in% current_vbls) {
    next_vbls <- "repeated_variables" 
    return(next_vbls)
  }
  
  next_vbls <- c(current_vbls, extra_vbl)
  return(next_vbls)
}


augment_with_variable <- function(models_tbl_to_aug, vec_of_extra_variables) {
  
  
  names_of_cols <- names(models_tbl_to_aug)
  fsr_in_tbl <- "full_sample_resmat" %in% names_of_cols
  if (!fsr_in_tbl) {
    print("full sample resmat not found")
  }
  
  if (fsr_in_tbl) {
    models_tbl_to_aug <- models_tbl_to_aug %>% 
      dplyr::select(variables, lags, t_threshold, is_unrestricted, full_sample_resmat) %>% 
      rename(previous_variables = variables) 
  } else {
    models_tbl_to_aug <- models_tbl_to_aug %>% 
      dplyr::select(variables, lags, t_threshold, is_unrestricted) %>% 
      rename(previous_variables = variables) 
  }
  
  
  
  list_of_tbl <- list_along(vec_of_extra_variables)
  
  for (i in seq(1, length(vec_of_extra_variables))) {
    this_variable <- vec_of_extra_variables[i]
    this_augmented_tbl <-  mutate(models_tbl_to_aug, 
                                  variables = map(
                                    previous_variables,
                                    ~ add_one_variable(.x, this_variable)))
    this_augmented_tbl <-  filter(this_augmented_tbl, 
                                  !variables == "repeated_variables")
    
    
    if (fsr_in_tbl) {
      this_augmented_tbl <-  dplyr::select(this_augmented_tbl, variables, lags,
                                           t_threshold, is_unrestricted, 
                                           full_sample_resmat)
    } else {
      this_augmented_tbl <-  dplyr::select(this_augmented_tbl, variables, lags,
                                           t_threshold, is_unrestricted)
    }
    
    list_of_tbl[[i]] <- this_augmented_tbl
  }
  
  augmented_tbl <- reduce(list_of_tbl, rbind)
  augmented_tbl <- mutate(augmented_tbl,
                          short_name = pmap(list(variables, lags, t_threshold),
                                            ~ make_model_name(variables = ..1, 
                                                              lags = ..2,
                                                              t_threshold = ..3)),
                          short_name = unlist(short_name),
                          size = map_dbl(variables, length)
  )
  
  augmented_tbl <- distinct(augmented_tbl, short_name, .keep_all = TRUE)
  
  if (fsr_in_tbl) {
    augmented_tbl <-  dplyr::select(augmented_tbl, short_name, variables, 
                                    size, lags, t_threshold, is_unrestricted,
                                    full_sample_resmat)
  } else {
    augmented_tbl <-  dplyr::select(augmented_tbl, short_name, variables, 
                                    size, lags, t_threshold, is_unrestricted)
  }
  
  return(augmented_tbl)
}



discard_by_rank <- function(models_tbl, max_rank_h, is_yearly = FALSE, 
                            is_wide = TRUE) {
  
  is_rmse <- str_detect(names(models_tbl), "rmse")
  names_with_rmse <- names(models_tbl)[is_rmse]
  
  is_rmse_yearly <- str_detect(names_with_rmse, "yr")
  
  names_yearly_rmse <- names_with_rmse[is_rmse_yearly]
  names_rmse_h <- names_with_rmse[!is_rmse_yearly]

  if (!is_yearly) {
    rmse_to_use <- names_rmse_h
  }
  
  if (is_yearly) {
    rmse_to_use <- names_yearly_rmse
  }

  if (is_wide) {
    if (is_yearly) {
      # print("IS YEARLY")
      long_filtered <- models_tbl %>%
        gather(key = "rmse_h", value = "rmse", rmse_to_use) %>%
        group_by(rmse_h) %>%
        mutate(rank_y = rank(rmse)) %>%
        filter(rank_y <= max_rank_h) %>%
        ungroup()
    }
    
    if (!is_yearly) {
      # print("is not yearly")
      long_filtered <- models_tbl %>%
        gather(key = "rmse_h", value = "rmse", rmse_to_use) %>%
        group_by(rmse_h) %>%
        mutate(rank_h = rank(rmse)) %>%
        filter(rank_h <= max_rank_h) %>%
        ungroup()
    }

    
    surviving_names <- long_filtered %>%
      dplyr::select(short_name) %>%
      distinct()
    
    models_tbl <- semi_join(models_tbl, surviving_names, by = "short_name")
  }
  
  if (!is_wide) {
    models_tbl <- models_tbl %>% 
      group_by(rmse_h) %>%
      mutate(rank = rank(rmse)) %>%
      filter(rank <= max_rank_h) %>%
      ungroup()
  }
  
  return(models_tbl)
}



fc_mean_of_VAR_ensemble <- function(models_tbl, max_rank_h = NULL) {
  # print(5)
  
  if (!any(c("rmse", "rmse_h", "rmse_1") %in% names(models_tbl))){
    print("models_tbl does not have rmse information, please provide an appropriate tibble")
  }
  
  if (!"rmse_h" %in% names(models_tbl)) {
    # i.e. is still in wide form with rmse_1, rmse_2 etc
    
    # if (!is.null(max_rank_h)) {
    #   models_tbl <- discard_by_rank(models_tbl, max_rank_h = max_rank_h, is_wide = TRUE)
    # }
    
    models_tbl <- models_tbl %>% 
      gather(key = "rmse_h", value = "rmse", rmse_names) 
  }
  
  models_tbl <- models_tbl %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) 
  
  
  if (!is.null(max_rank_h)) {
    models_tbl <- models_tbl %>% 
      filter(rank_h <= max_rank_h)
  }
  
  models_tbl <- models_tbl %>% 
    mutate(inv_mse = 1/(rmse*rmse),
           model_weight = inv_mse/sum(inv_mse)
    )
  
  models_tbl <- models_tbl %>% 
    mutate(horizon = as.numeric(substr(rmse_h, 6, 6))
    ) 
  
  models_tbl <- models_tbl %>% 
    mutate(this_h_fc_yoy = map2_dbl(target_mean_fc_yoy, horizon, ~ .x[.y])
    ) 
  
  models_tbl <- models_tbl %>% 
    mutate(weighted_this_h_fc_yoy = map2_dbl(this_h_fc_yoy, model_weight, ~ .x*.y)
    ) 
  
  models_tbl <- models_tbl %>% 
    mutate(waverage_fc_yoy_h = sum(weighted_this_h_fc_yoy)
    ) 
  
  waverage_tbl <- models_tbl %>% 
    dplyr::select(rmse_h, waverage_fc_yoy_h) %>% 
    summarise(waverage_fc_yoy_h = unique(waverage_fc_yoy_h))
  
  fc_start <- start(models_tbl$target_mean_fc_yoy[[1]])
  
  fc_freq <- frequency(models_tbl$target_mean_fc_yoy[[1]])
  
  weighted_avg_fc_yoy <- ts(waverage_tbl$waverage_fc_yoy_h, start = fc_start, frequency = fc_freq)
  
  models_tbl <- ungroup(models_tbl)
  
  ensemble_tbl <- tibble(variables = list(table(unlist(models_tbl$variables))),
                         lags = list(table(unlist(models_tbl$lags))),
                         short_name = "ensemble",
                         horizon = sort(unique(models_tbl$horizon)),
                         this_h_fc_yoy = as.numeric(weighted_avg_fc_yoy)
  )
  
  return(list(ensemble_tbl = ensemble_tbl, 
              weighted_avg_fc_yoy = weighted_avg_fc_yoy)
  )
  
}


cv_of_VAR_ensemble <- function(var_data,
                               used_cv_models,
                               fc_horizon,
                               n_cv,
                               training_length,
                               cv_extension_of_exo,
                               names_exogenous, 
                               target_transform, 
                               target_level_ts, 
                               max_rank_h = NULL, 
                               full_cv_output = FALSE) {

  # print("inside cv of VAR ensemble")
  # 
  # print("used_cv_models")
  # print(used_cv_models)
  
  if (! "rmse_h" %in% names(used_cv_models)) {
    # i.e. is still in wide form with rmse_1, rmse_2 etc
    
    if (!is.null(max_rank_h)) {
      used_cv_models <- discard_by_rank(used_cv_models, max_rank_h = max_rank_h,
                                        is_wide = TRUE)
    }
    
  }
  
  variables_used <- used_cv_models %>% 
    dplyr::select(variables) %>% unlist() %>% unique()
  
  if (training_length == "common_max") {
    total_obs <- nrow( na.omit(var_data[, variables_used]))
    training_length <- total_obs - h_max - (n_cv - 1)
    print(paste0("common_max = ", training_length))
  }
  
  train_test_dates <- make_test_dates_list(
    ts_data = na.omit(var_data[, variables_used]), 
    type = "tscv",
    n = n_cv, 
    h_max = fc_horizon, 
    training_length = training_length)
  
  cv_ensemble_fcs <- list_along(seq(1, n_cv))
  cv_ensemble_test_data <- list_along(seq(1, n_cv))
  cv_ensemble_errors <- list_along(seq(1, n_cv))
  
  
  for (i in seq(1, n_cv)) {
    
    print(paste0("This is cv round ", i))
    
    train_test_yq <- train_test_dates[["list_of_year_quarter"]]
    
    this_tra_s <- train_test_yq[[i]]$tra_s
    this_tra_e <- train_test_yq[[i]]$tra_e
    
    this_tes_s <- train_test_yq[[i]]$tes_s
    this_tes_e <- train_test_yq[[i]]$tes_e
    
    var_data_train <- window(var_data, start = this_tra_s, end = this_tra_e)

        future_exo_in_cv <- cv_extension_of_exo[[i]]
    sample_exo_in_cv <- window(extension_of_exo, end = start(future_exo_in_cv))
    sample_exo_in_cv <- subset(sample_exo_in_cv, end = nrow(sample_exo_in_cv)-1)
    this_extened_exo_mts <- ts(
      rbind(sample_exo_in_cv, future_exo_in_cv),
      start = start(sample_exo_in_cv), frequency = frequency(sample_exo_in_cv))
    
    # print()
    
    this_fc_list <- forecast_var_from_model_tbl(
      models_tbl = used_cv_models, 
      fc_horizon = fc_horizon, 
      var_data = var_data_train,
      target_transform = target_transform,
      target_level_ts = target_level_ts,
      names_exogenous = names_exogenous, 
      extended_exo_mts = this_extened_exo_mts,
      use_resmat = TRUE,
      keep_wide_tbl = FALSE,
      max_rank_h = max_rank_h) 
    
    this_fc_tbl <- this_fc_list$models_tbl
    
    ensemble_fc_list <- fc_mean_of_VAR_ensemble(models_tbl = this_fc_tbl, 
                                                max_rank_h = max_rank_h)
    
    ensemble_fc_ts <- ensemble_fc_list$weighted_avg_fc_yoy
    
    test_target_yoy <- window(make_yoy_ts(target_level_ts), 
                              start = this_tes_s,
                              end = this_tes_e)
    
    ensemble_cv_error_yoy <- test_target_yoy - ensemble_fc_ts 
    
    
    cv_ensemble_fcs[[i]] <- ensemble_fc_ts
    cv_ensemble_test_data[[i]] <- test_target_yoy
    cv_ensemble_errors[[i]] <- ensemble_cv_error_yoy
  }
  
  mat_cv_errors_ensemble <- matrix(
    reduce(cv_ensemble_errors, rbind), 
    nrow = n_cv)
  
  rownames(mat_cv_errors_ensemble) <- NULL
  
  ensemble_rmse <- sqrt(colMeans(mat_cv_errors_ensemble^2, na.rm = TRUE))
  
  if (!full_cv_output){
    cv_ensemble_fcs <- NULL 
    cv_ensemble_test_data <- NULL
    cv_ensemble_errors <- NULL
  }
  
  return(list(ensemble_rmse = ensemble_rmse, 
              cv_ensemble_fcs = cv_ensemble_fcs, 
              cv_ensemble_test_data = cv_ensemble_test_data,
              cv_ensemble_errors = cv_ensemble_errors))
  
}


ensemble_fc_from_models_rmse <- function(models_tbl_with_rmse, 
                                         var_data,
                                         n_cv, 
                                         training_length,
                                         max_rank_h,
                                         fc_horizon,
                                         names_exogenous,
                                         target_transform, 
                                         target_level_ts, 
                                         extension_of_exo, 
                                         cv_extension_of_exo, 
                                         fit_column = NULL,
                                         keep_wide_tbl = TRUE, 
                                         full_cv_output = FALSE) {
  
  fcs_list <- forecast_var_from_model_tbl(
    models_tbl = models_tbl_with_rmse,
    fit_column = fit_column,
    var_data = var_data, 
    fc_horizon = fc_horizon, 
    target_transform = target_transform,
    target_level_ts = target_level_ts,
    names_exogenous = names_exogenous, 
    extended_exo_mts = extension_of_exo, 
    keep_wide_tbl = keep_wide_tbl, 
    max_rank_h = max_rank_h
  )
  
  ensemble_fc_list <- fc_mean_of_VAR_ensemble(models_tbl = fcs_list$models_tbl,
                                              max_rank_h = max_rank_h)
  
  ensemble_cv <- cv_of_VAR_ensemble(var_data = var_data,
                                    used_cv_models = fcs_list$models_tbl_wide,
                                    fc_horizon = fc_horizon,
                                    n_cv = n_cv,
                                    training_length = training_length,
                                    cv_extension_of_exo = cv_extension_of_exo,
                                    names_exogenous = names_exogenous,
                                    max_rank_h = max_rank_h,
                                    full_cv_output =  full_cv_output,
                                    target_transform = target_transform,
                                    target_level_ts = target_level_ts)
  
  ensemble_fc_and_rmse <- ensemble_fc_list$ensemble_tbl %>% 
    mutate(rmse_h = paste0("rmse_", 1:n()),
           rmse = ensemble_cv$ensemble_rmse,
           rank = -1)
  
  fcs_models_to_bind <- fcs_list$models_tbl %>% 
    mutate(lags = list(lags)) %>% 
    dplyr::select(names(ensemble_fc_and_rmse))
  
  
  models_and_ensemble_fcs <- rbind(ensemble_fc_and_rmse, 
                                   fcs_models_to_bind)
  
  return(models_and_ensemble_fcs)
  
}


add_one_variable <- function(current_vbls, extra_vbl) {
  
  if (extra_vbl %in% current_vbls) {
    next_vbls <- "repeated_variables" 
    return(next_vbls)
  }
  
  next_vbls <- c(current_vbls, extra_vbl)
  return(next_vbls)
}


augment_with_variable <- function(models_tbl_to_aug, vec_of_extra_variables) {
  
  
  names_of_cols <- names(models_tbl_to_aug)
  fsr_in_tbl <- "full_sample_resmat" %in% names_of_cols
  if (!fsr_in_tbl) {
    print("full sample resmat not found")
  }
  
  if (fsr_in_tbl) {
    models_tbl_to_aug <- models_tbl_to_aug %>% 
      dplyr::select(variables, lags, t_threshold, is_unrestricted, full_sample_resmat) %>% 
      rename(previous_variables = variables) 
  } else {
    models_tbl_to_aug <- models_tbl_to_aug %>% 
      dplyr::select(variables, lags, t_threshold, is_unrestricted) %>% 
      rename(previous_variables = variables) 
  }
  
  
  
  list_of_tbl <- list_along(vec_of_extra_variables)
  
  for (i in seq(1, length(vec_of_extra_variables))) {
    this_variable <- vec_of_extra_variables[i]
    this_augmented_tbl <-  mutate(models_tbl_to_aug, 
                                  variables = map(
                                    previous_variables,
                                    ~ add_one_variable(.x, this_variable)))
    this_augmented_tbl <-  filter(this_augmented_tbl, 
                                  !variables == "repeated_variables")
    
    
    if (fsr_in_tbl) {
      this_augmented_tbl <-  dplyr::select(this_augmented_tbl, variables, lags,
                                           t_threshold, is_unrestricted, 
                                           full_sample_resmat)
    } else {
      this_augmented_tbl <-  dplyr::select(this_augmented_tbl, variables, lags,
                                           t_threshold, is_unrestricted)
    }
    
    list_of_tbl[[i]] <- this_augmented_tbl
  }
  
  augmented_tbl <- reduce(list_of_tbl, rbind)
  augmented_tbl <- mutate(augmented_tbl,
                          short_name = pmap(list(variables, lags, t_threshold),
                                            ~ make_model_name(variables = ..1, 
                                                              lags = ..2,
                                                              t_threshold = ..3)),
                          short_name = unlist(short_name),
                          size = map_dbl(variables, length)
  )
  
  augmented_tbl <- distinct(augmented_tbl, short_name, .keep_all = TRUE)
  
  if (fsr_in_tbl) {
    augmented_tbl <-  dplyr::select(augmented_tbl, short_name, variables, 
                                    size, lags, t_threshold, is_unrestricted,
                                    full_sample_resmat)
  } else {
    augmented_tbl <-  dplyr::select(augmented_tbl, short_name, variables, 
                                    size, lags, t_threshold, is_unrestricted)
  }
  
  return(augmented_tbl)
  
}


fc_mean_of_VAR_ensemble <- function(models_tbl, max_rank_h = NULL) {
  # print(5)
  
  if(!any(c("rmse", "rmse_h", "rmse_1") %in% names(models_tbl))){
    print("models_tbl does not have rmse information, please provide an appropriate tibble")
  }
  
  if (! "rmse_h" %in% names(models_tbl)) {
    # i.e. is still in wide form with rmse_1, rmse_2 etc
    
    # if (!is.null(max_rank_h)) {
    #   models_tbl <- discard_by_rank(models_tbl, max_rank_h = max_rank_h, is_wide = TRUE)
    # }
    
    models_tbl <- models_tbl %>% 
      gather(key = "rmse_h", value = "rmse", rmse_names) 
  }
  
  models_tbl <- models_tbl %>% 
    group_by(rmse_h) %>% 
    mutate(rank_h = rank(rmse)) 
  
  
  if (!is.null(max_rank_h)) {
    models_tbl <- models_tbl %>% 
      filter(rank_h <= max_rank_h)
  }
  
  models_tbl <- models_tbl %>% 
    mutate(inv_mse = 1/(rmse*rmse),
           model_weight = inv_mse/sum(inv_mse)
    )
  
  models_tbl <- models_tbl %>% 
    mutate(horizon = as.numeric(substr(rmse_h, 6, 6))
    ) 
  
  models_tbl <- models_tbl %>% 
    mutate(this_h_fc_yoy = map2_dbl(target_mean_fc_yoy, horizon, ~ .x[.y])
    ) 
  
  models_tbl <- models_tbl %>% 
    mutate(weighted_this_h_fc_yoy = map2_dbl(this_h_fc_yoy, model_weight, ~ .x*.y)
    ) 
  
  models_tbl <- models_tbl %>% 
    mutate(waverage_fc_yoy_h = sum(weighted_this_h_fc_yoy)
    ) 
  
  waverage_tbl <- models_tbl %>% 
    dplyr::select(rmse_h, waverage_fc_yoy_h) %>% 
    summarise(waverage_fc_yoy_h = unique(waverage_fc_yoy_h))
  
  fc_start <- start(models_tbl$target_mean_fc_yoy[[1]])
  
  fc_freq <- frequency(models_tbl$target_mean_fc_yoy[[1]])
  
  weighted_avg_fc_yoy <- ts(waverage_tbl$waverage_fc_yoy_h, start = fc_start, frequency = fc_freq)
  
  models_tbl <- ungroup(models_tbl)
  
  ensemble_tbl <- tibble(variables = list(table(unlist(models_tbl$variables))),
                         lags = list(table(unlist(models_tbl$lags))),
                         short_name = "ensemble",
                         horizon = sort(unique(models_tbl$horizon)),
                         this_h_fc_yoy = as.numeric(weighted_avg_fc_yoy)
  )
  
  return(list(ensemble_tbl = ensemble_tbl, 
              weighted_avg_fc_yoy = weighted_avg_fc_yoy)
  )
  
}


cv_of_VAR_ensemble <- function(var_data,
                               used_cv_models,
                               fc_horizon,
                               n_cv,
                               training_length,
                               cv_extension_of_exo,
                               names_exogenous, 
                               target_transform, 
                               target_level_ts, 
                               max_rank_h = NULL, 
                               full_cv_output = FALSE) {

  # print("inside cv of VAR ensemble")
  # 
  # print("used_cv_models")
  # print(used_cv_models)
  
  if (! "rmse_h" %in% names(used_cv_models)) {
    # i.e. is still in wide form with rmse_1, rmse_2 etc
    
    if (!is.null(max_rank_h)) {
      used_cv_models <- discard_by_rank(used_cv_models, max_rank_h = max_rank_h,
                                        is_wide = TRUE)
    }
    
  }
  
  variables_used <- used_cv_models %>% 
    dplyr::select(variables) %>% unlist() %>% unique()
  
  if (training_length == "common_max") {
    total_obs <- nrow( na.omit(var_data[, variables_used]))
    training_length <- total_obs - h_max - (n_cv - 1)
    print(paste0("common_max = ", training_length))
  }
  
  train_test_dates <- make_test_dates_list(
    ts_data = na.omit(var_data[, variables_used]), 
    type = "tscv",
    n = n_cv, 
    h_max = fc_horizon, 
    training_length = training_length)
  
  cv_ensemble_fcs <- list_along(seq(1, n_cv))
  cv_ensemble_test_data <- list_along(seq(1, n_cv))
  cv_ensemble_errors <- list_along(seq(1, n_cv))
  
  
  for (i in seq(1, n_cv)) {
    
    print(paste0("This is cv round ", i))
    
    train_test_yq <- train_test_dates[["list_of_year_quarter"]]
    
    this_tra_s <- train_test_yq[[i]]$tra_s
    this_tra_e <- train_test_yq[[i]]$tra_e
    
    this_tes_s <- train_test_yq[[i]]$tes_s
    this_tes_e <- train_test_yq[[i]]$tes_e
    
    var_data_train <- window(var_data, start = this_tra_s, end = this_tra_e)

        future_exo_in_cv <- cv_extension_of_exo[[i]]
    sample_exo_in_cv <- window(extension_of_exo, end = start(future_exo_in_cv))
    sample_exo_in_cv <- subset(sample_exo_in_cv, end = nrow(sample_exo_in_cv)-1)
    this_extened_exo_mts <- ts(
      rbind(sample_exo_in_cv, future_exo_in_cv),
      start = start(sample_exo_in_cv), frequency = frequency(sample_exo_in_cv))
    
    # print()
    
    this_fc_list <- forecast_var_from_model_tbl(
      models_tbl = used_cv_models, 
      fc_horizon = fc_horizon, 
      var_data = var_data_train,
      target_transform = target_transform,
      target_level_ts = target_level_ts,
      names_exogenous = names_exogenous, 
      extended_exo_mts = this_extened_exo_mts,
      use_resmat = TRUE,
      keep_wide_tbl = FALSE,
      max_rank_h = max_rank_h) 
    
    this_fc_tbl <- this_fc_list$models_tbl
    
    ensemble_fc_list <- fc_mean_of_VAR_ensemble(models_tbl = this_fc_tbl, 
                                                max_rank_h = max_rank_h)
    
    ensemble_fc_ts <- ensemble_fc_list$weighted_avg_fc_yoy
    
    test_target_yoy <- window(make_yoy_ts(target_level_ts), 
                              start = this_tes_s,
                              end = this_tes_e)
    
    ensemble_cv_error_yoy <- test_target_yoy - ensemble_fc_ts 
    
    
    cv_ensemble_fcs[[i]] <- ensemble_fc_ts
    cv_ensemble_test_data[[i]] <- test_target_yoy
    cv_ensemble_errors[[i]] <- ensemble_cv_error_yoy
  }
  
  mat_cv_errors_ensemble <- matrix(
    reduce(cv_ensemble_errors, rbind), 
    nrow = n_cv)
  
  rownames(mat_cv_errors_ensemble) <- NULL
  
  ensemble_rmse <- sqrt(colMeans(mat_cv_errors_ensemble^2, na.rm = TRUE))
  
  if (!full_cv_output){
    cv_ensemble_fcs <- NULL 
    cv_ensemble_test_data <- NULL
    cv_ensemble_errors <- NULL
  }
  
  return(list(ensemble_rmse = ensemble_rmse, 
              cv_ensemble_fcs = cv_ensemble_fcs, 
              cv_ensemble_test_data = cv_ensemble_test_data,
              cv_ensemble_errors = cv_ensemble_errors))
  
}


ensemble_fc_from_models_rmse <- function(models_tbl_with_rmse, 
                                         var_data,
                                         n_cv, 
                                         training_length,
                                         max_rank_h,
                                         fc_horizon,
                                         names_exogenous,
                                         target_transform, 
                                         target_level_ts, 
                                         extension_of_exo, 
                                         cv_extension_of_exo, 
                                         fit_column = NULL,
                                         keep_wide_tbl = TRUE, 
                                         full_cv_output = FALSE) {
  
  fcs_list <- forecast_var_from_model_tbl(
    models_tbl = models_tbl_with_rmse,
    fit_column = fit_column,
    var_data = var_data, 
    fc_horizon = fc_horizon, 
    target_transform = target_transform,
    target_level_ts = target_level_ts,
    names_exogenous = names_exogenous, 
    extended_exo_mts = extension_of_exo, 
    keep_wide_tbl = keep_wide_tbl, 
    max_rank_h = max_rank_h
  )
  
  ensemble_fc_list <- fc_mean_of_VAR_ensemble(models_tbl = fcs_list$models_tbl,
                                              max_rank_h = max_rank_h)
  
  ensemble_cv <- cv_of_VAR_ensemble(var_data = var_data,
                                    used_cv_models = fcs_list$models_tbl_wide,
                                    fc_horizon = fc_horizon,
                                    n_cv = n_cv,
                                    training_length = training_length,
                                    cv_extension_of_exo = cv_extension_of_exo,
                                    names_exogenous = names_exogenous,
                                    max_rank_h = max_rank_h,
                                    full_cv_output =  full_cv_output,
                                    target_transform = target_transform,
                                    target_level_ts = target_level_ts)
  
  ensemble_fc_and_rmse <- ensemble_fc_list$ensemble_tbl %>% 
    mutate(rmse_h = paste0("rmse_", 1:n()),
           rmse = ensemble_cv$ensemble_rmse,
           rank = -1)
  
  fcs_models_to_bind <- fcs_list$models_tbl %>% 
    mutate(lags = list(lags)) %>% 
    dplyr::select(names(ensemble_fc_and_rmse))
  
  
  models_and_ensemble_fcs <- rbind(ensemble_fc_and_rmse, 
                                   fcs_models_to_bind)
  
  return(models_and_ensemble_fcs)
  
}


add_prechosen_for_this_step <- function(search_plan, step_index, prechosen_so_far, 
                                        models_table, 
                                        max_rank_some_h_for_freq = 50,
                                        discard_previous = FALSE,
                                        best_n_VAR_for_preselecting = 10) {
  
  this_search_step <- search_plan[[step_index]]
  # print("In addprechosen, this_search_step:")
  # print(this_search_step)
  
  this_size <- this_search_step[["size"]]
  
  # print("Preparing possible pre-chosen variables (other than the target variable)")
  
  if (is.null(this_search_step$manually_prechosen)) {
    this_manually_prechosen <- c("")
  } else {
    this_manually_prechosen <- this_search_step[["manually_prechosen"]]
  }
  
  if (this_manually_prechosen == c("")) {
    # print("No manually pre-chosen variables for this step")
  }
  
  all_prechosen_previous_step <- prechosen_so_far[[step_index - 1]]
  
  if (is.null(all_prechosen_previous_step)) {
    # print("No pre-chosen variables from previous step")
    all_prechosen_previous_step <- c("")
  }
  
  previous_and_manual <- c(this_manually_prechosen, all_prechosen_previous_step)

  
  if (all(previous_and_manual == "")) {
    previous_and_manual <- c("")
  } else {
    previous_and_manual <- unique(previous_and_manual)
    previous_and_manual <- previous_and_manual[previous_and_manual != ""]
  }
  
  if ((!is.null(all_prechosen_previous_step))  & (this_manually_prechosen != c("")) ) {
    all_prechosen_previous_step <- map(all_prechosen_previous_step, ~ c(.x, this_manually_prechosen))
  }
  
  # print("Prechosen, including manually specified and from previous steps:")
  # print(all_prechosen_previous_step)
  
  
  if (is.null(this_search_step$n_new_prechosen)) {
    n_new_prechosen <- 0
    auto_prechosen_at_this_step <- c("")
    all_prechosen_this_step <- this_manually_prechosen
    updated_prechosen_so_far <- prechosen_so_far
    updated_prechosen_so_far[[step_index]] <- all_prechosen_this_step 
    return(updated_prechosen_so_far)
  } else {
    n_new_prechosen <- this_search_step[["n_new_prechosen"]]
  }
  
  n_sets_of_previous_prechosen <- length(all_prechosen_previous_step)
  
  updated_prechosen_so_far <- prechosen_so_far
  
  apc <- 1
  
  # print("unlist version of all_prechosen_previous_step")
  # print(unlist(all_prechosen_previous_step))
  
  vec_all_prechosen_previous_step <- all_prechosen_previous_step
  
  
  for (ppc in seq(1, n_sets_of_previous_prechosen)) {
    
    this_previous_prechosen <- all_prechosen_previous_step[ppc]
    this_previous_prechosen <- this_previous_prechosen[this_previous_prechosen != ""]
    
    new_prechosen_list <- list_along(seq(1, n_new_prechosen))
    
    for (new_pc in seq(1, n_new_prechosen)) {
      
      n_freq_for_preselecting <- 2*this_size
      
      print(paste0("step = ", step_index ,", previous = ", ppc, ", new = ", new_pc, " and this_previous_prechosen:"))
      print(this_previous_prechosen)
      
      position_of_new_prechosen <- 1 + new_pc + length(this_previous_prechosen)
      
      # print("position_of_new_prechosen")
      # print(position_of_new_prechosen)
      
      # print("models_table")
      # print(models_table)
      
      f_vbls <- variable_freq_by_n(models_table,
                                   h_max = fc_horizon,
                                   max_rank = max_rank_some_h_for_freq,
                                   n_freq = n_freq_for_preselecting,
                                   is_wide = TRUE,
                                   max_small_rank = best_n_VAR_for_preselecting)
      
      vbl_table <- f_vbls$vbl_freqs_by_h %>% arrange(desc(total_n))
      
      print("vbl_table")
      print(vbl_table)
      
      vbl_table_by_total <- vbl_table %>% 
        arrange(desc(total_n)) %>% 
        dplyr::select(vbl) %>% 
        dplyr::filter(row_number() <= n_freq_for_preselecting)
      
      vbl_by_total <-  vbl_table_by_total$vbl
      
      is_vbl_by_total_in_pc <- vbl_by_total %in% vec_all_prechosen_previous_step
      vbl_by_total_not_in_pc <- vbl_by_total[!is_vbl_by_total_in_pc]
      
      new_prechosen <- vbl_by_total_not_in_pc[new_pc + 1] 
      
      # print("vbl_by_total")
      # print(vbl_by_total)
      # print("vbl_by_total_not_in_pc")
      # print(vbl_by_total_not_in_pc)
      # print("new_prechosen")
      # print(new_prechosen)
      
      new_prechosen_list[[new_pc]] <- new_prechosen
      
      this_prechosen_variables <- c(this_previous_prechosen, new_prechosen)
      this_prechosen_variables <- this_prechosen_variables[this_prechosen_variables != ""]
      
      # print("this_prechosen_variables")
      # print(this_prechosen_variables)
      
      updated_prechosen_so_far[[step_index]][[apc]] <- this_prechosen_variables
      
      apc <- apc + 1
      
    }
    
  }
  
  return(updated_prechosen_so_far)
}


add_rmse_rankings <- function(tbl_with_rmses) {
  rmse_names <- names(tbl_with_rmses)
  rmse_names <- vars_select(names(tbl_with_rmses), starts_with("rmse"))
  rmse_names <- unname(rmse_names)
  
  rankings_as_list <- list_along(rmse_names)
  
  for (i in seq_along(rmse_names)) {
    this_rmse <- paste0("rmse_", i)
    this_rmse_data <- as.matrix(tbl_with_rmses[, this_rmse])
    this_rank <- rank(this_rmse_data)
    rankings_as_list[[i]] <- this_rank
  }
  
  rankings <- as_tibble(reduce(rankings_as_list, cbind))
  names(rankings) <- paste0("rank_", seq_along(rmse_names))
  new_tbl <- as_tibble(cbind(tbl_with_rmses, rankings))
  return(new_tbl)
}

all_rmse_from_cv_obj <- function(cv_obj) {

  if (length(cv_obj) == 1) {
    if (is.na(cv_obj[[1]])) {
      return(NA)
    }
  }  
  
  cv_errors <- cv_obj[["cv_errors"]]
  # print(cv_errors)
  n_cv <- length(cv_errors)
  # print(n_cv)
  # print(cv_errors[[1]])
  t_periods <-  length(cv_errors[[1]])
  # print(t_periods)
  
  # matrix is n_cv x t_periods, i.e. a column represent fixed period, varying cv
  matrix_errors <- reduce(cv_errors, rbind) 
  rownames(matrix_errors) <- NULL
  rmse <- sqrt(colMeans(matrix_errors^2))
  
  return(rmse)
}



all_yearly_rmse_from_cv_obj <- function(cv_obj) {
  
  if (length(cv_obj) == 1) {
    if (is.na(cv_obj[[1]])) {
      return(NA)
    }
  }  
  
  cv_errors <- cv_obj[["cv_errors_from_yr_avg"]]
  # print("in all_yearly_rmse_from_cv_obj")
  # print("cv_errors")
  # print(cv_errors)
  
  
  n_cv <- length(cv_errors)
  # print(n_cv)
  # print(cv_errors[[1]])
  t_periods <-  length(cv_errors[[1]])
  # print(t_periods)
  
  # matrix is n_cv x t_periods, i.e. a column represent fixed period, varying cv
  matrix_errors <- reduce(cv_errors, rbind) 
  # print("matrix_errors" )
  # print(matrix_errors )
  rownames(matrix_errors) <- NULL
  rmse <- sqrt(colMeans(matrix_errors^2))
  
  return(rmse)
}


all_mae_from_cv_obj <- function(cv_obj) {
  
  if (length(cv_obj) == 1) {
    if (is.na(cv_obj[[1]])) {
      return(NA)
    }
  }  
  
  cv_errors <- cv_obj[["cv_errors"]]
  # print(cv_errors)
  n_cv <- length(cv_errors)
  # print(n_cv)
  # print(cv_errors[[1]])
  t_periods <-  length(cv_errors[[1]])
  # print(t_periods)
  
  # matrix is n_cv x t_periods, i.e. a column represent fixed period, varying cv
  matrix_errors <- reduce(cv_errors, rbind) 
  rownames(matrix_errors) <- NULL
  mae <- colMeans(abs(matrix_errors))
  
  return(mae)
}



all_specifications <- function(var_size, all_variables, 
                               target_variable = "rgdp",
                               non_target_fixed = c(""), lag_choices = 1, 
                               use_info_lags = FALSE, maxlag = 7, 
                               t_thresholds = 0, names_exogenous = c(""),
                               var_data = NULL, silent = TRUE) {
  
  non_target_fixed <- c("")
  if (non_target_fixed %in% c(c(""), c(" "), c("  "))) {
    fixed_variables <- target_variable
  } else {
    fixed_variables <- c(target_variable, non_target_fixed) 
  }
  
  vbls_to_choose_from <- all_variables[! all_variables %in% fixed_variables]
  
  free_slots <- var_size - length(fixed_variables)
  
  n_exo <- length(names_exogenous)
  
  ncombinations <- count_combn(var_size = var_size, n_total = length(all_variables),
                               n_fixed = length(fixed_variables), n_exo = 0)
  
  all_nontarget_combn <- combn(x = vbls_to_choose_from, m = free_slots)
  
  all_combn_list <- map(array_tree(all_nontarget_combn, 2), ~ c(fixed_variables, .x))
  
  all_combn_tbl <- tibble(variables = all_combn_list, size = var_size)
  
  do_manual_lags <- is.numeric(lag_choices)
  
  if (do_manual_lags) {
    manual_lags <- lag_choices
  } else {
    manual_lags <- NULL
  }
  
  if(use_info_lags) {
    do_info_lags <- TRUE
    this_info_lags <- map(all_combn_tbl$variables, 
                          ~ lags_for_var(var_data, .x, vec_lags = "info", 
                                         max_p_for_estimation = maxlag+2,
                                         silent = silent))
  } else {
    # print("not using info lags")
    do_info_lags <- FALSE
    this_info_lags <- list_along(all_combn_tbl$variables)
  }
  
  if (length(t_thresholds) > 1) {
    t_thresholds <- t_thresholds[!t_thresholds == 0]
  }
  
  keep_unrestricted <- TRUE
  
  if(length(t_thresholds) == 1) {
    if(t_thresholds == 0 | !t_thresholds | is.null(t_thresholds)) {
      is_unrestricted <- TRUE
    } else {
      is_unrestricted <- FALSE
    }
  } else {
    is_unrestricted <- FALSE
  }
  
  
  all_specifications_tbl <-  all_combn_tbl %>% 
    mutate(manual_lags = list(manual_lags),
           info_lags = this_info_lags,
           lags = map2(manual_lags, info_lags, ~ sort(unique(c(.x, .y)))),
           lags = map(lags, ~ .x[.x <= maxlag ])
    ) %>% 
    dplyr::select(-c(manual_lags, info_lags)) %>% 
    unnest(lags, .drop = FALSE) %>% 
    mutate(t_threshold = list(t_thresholds))
  
  # print("is_unrestricted")
  # print(is_unrestricted)
  
  return(all_specifications_tbl)
  
}


ave_fc_from_cv <- function(cv_tbl, best_n_to_keep = "all", is_wide = TRUE) {
  
  cv_tbl_na <- filter(cv_tbl, is.na(target_mean_fc_yoy))
  cv_tbl <- filter(cv_tbl, !is.na(target_mean_fc_yoy))
  
  print("cases where the forecast resulted in NA:")
  print(cv_tbl_na[["short_name"]])
  
  if (is_wide) {
    rmse_names <- names(cv_tbl)[str_detect(names(cv_tbl), "rmse")]
    cv_tbl <- cv_tbl %>%
      gather(key = "rmse_h", value = "rmse", rmse_names) %>%
      dplyr::select(vars_select(names(.), -starts_with("rank"))) %>%
      group_by(rmse_h) %>%
      arrange(rmse_h, rmse) %>%
      mutate(rank_h = rank(rmse)) %>%
      ungroup() %>%
      mutate(lags = unlist(lags),
             model_type = "VAR")
  } else {
    rmse_names <- unique(cv_tbl$rmse_h)
  }
  
  if (best_n_to_keep == "all") {
    cv_tbl <- cv_tbl
  }
  
  if (is.numeric(best_n_to_keep)) {
    cv_tbl <- cv_tbl %>%
      arrange(rmse_h, rmse) %>%
      group_by(rmse_h) %>%
      mutate(rank_h = rank(rmse)) %>%
      filter(rank_h <= best_n_to_keep) %>%
      mutate(inv_mse = 1/(rmse*rmse),
             model_weight_h = inv_mse/sum(inv_mse),
             weighted_fc_h = map2(target_mean_fc_yoy, model_weight_h, ~ .x * .y)
      ) %>%
      ungroup()
  }
  
  a_fc <- cv_tbl$weighted_fc_h[[1]]
  
  ave_by_h_fc <- vector(mode = "numeric", length = length(rmse_names))
  
  for (r in seq(1, length(rmse_names))) {
    this_rmse <- rmse_names[r]
    
    this_h_fc <- cv_tbl %>% 
      dplyr::filter(rmse_h == this_rmse) %>% 
      dplyr::select(weighted_fc_h) 
    
    this_h_fc <- reduce(this_h_fc[[1]], rbind)
    this_h_fc <- colSums(this_h_fc)
    this_h_fc <- this_h_fc[r]
    
    ave_by_h_fc[r] <- this_h_fc
  }
  
  ave_by_h_fc <- ts(data = ave_by_h_fc, frequency = frequency(a_fc),
                    start = start(a_fc))

  return(list(cv_tbl = cv_tbl, ave_by_h_fc = ave_by_h_fc))
}


check_resid_VAR <- function(fit_VAR, type = "PT.asymptotic", lags.pt = 16,
                            pval_ref = 0.05) {
  
  test_object <- try(serial.test(fit_VAR, type = type, lags.pt = lags.pt),
                     silent = TRUE)
  # print(class(test_object))
  
  if (class(test_object) == "try-error") {
    # print("Running serial.test threw an error.")
    is_white_noise <- FALSE
  } else {
    pval <- test_object[["serial"]][["p.value"]]
    pval <- unname(pval)
    is_white_noise <- pval > pval_ref
  }
  
  return(is_white_noise)
}


count_combn <- function(var_size, n_total, n_exo, n_fixed = 1) {
  
  n_free <- n_total - n_fixed
  
  k_free <- var_size - n_fixed
  
  ncomb_simple <- choose(n = n_free, k = k_free)
  
  ncomb_fixed_and_exo <- choose(n = n_exo, k = k_free)
  
  ncomb_notpureexo <- ncomb_simple - ncomb_fixed_and_exo 
  
  return(c(ncomb = ncomb_simple, ncomb_x = ncomb_fixed_and_exo,
           ncombn_adjusted = ncomb_notpureexo))
}


cv_var_from_model_tbl <- function(h, n_cv, 
                                  training_length, 
                                  models_tbl, 
                                  var_data, 
                                  new_t_threshold = NULL, 
                                  fit_column = NULL, 
                                  target_transform = "yoy", 
                                  target_level_ts = NULL,
                                  keep_varest_obj = FALSE,
                                  keep_cv_objects = FALSE,
                                  keep_fc_objects = FALSE,
                                  names_exogenous = c(""),
                                  exo_lag = NULL,
                                  future_exo = NULL,
                                  future_exo_cv = NULL,
                                  do_full_sample_fcs = FALSE,
                                  extended_exo_mts = NULL,
                                  do_tests = FALSE,
                                  silent = TRUE) { 
  
  print("in cv_var_from_model_tbl names_exogenous is")
  print(names_exogenous)
  # print("and future_exo_cv is")
  # print(future_exo_cv)
  # print("while extended_exo_mts is")
  # print(extended_exo_mts)
  # print("and extended_exo_mts$future_exo_cv is")
  # print(extended_exo_mts$future_exo_cv)
  
  
  if ("fit" %in% names(models_tbl)) {
    table_of_tried_specifications <- models_tbl %>% dplyr::select(-fit) 
  } else {
    table_of_tried_specifications <- models_tbl
  }
  
  if (!do_tests) {
    passing_models <-  models_tbl
    n_lost_to_threshold <-  NULL
    n_lost_to_roots <-  NULL
    n_lost_to_white <-  NULL
  }

  starting_names <- names(models_tbl)
  has_short_name <- "short_name" %in% starting_names
  has_t_threshold <- "t_threshold" %in% starting_names
  
  if (!has_t_threshold) {
    models_tbl <- models_tbl %>% mutate(t_threshold = 0)
  }
  
  if (!has_short_name) {
    models_tbl <- models_tbl %>% 
      mutate(short_name = pmap(list(variables, lags, t_threshold),
                               ~ make_model_name(variables = ..1, 
                                                 lags = ..2,
                                                 t_threshold = ..3)),
             short_name = unlist(short_name))
    
    models_tbl <- models_tbl %>% dplyr::select(short_name, everything())
  }
  
  if (is.null(fit_column)) {
    print("There is no column with fit varest objects, thus we will estimate all VARs now")
    # tic()
    # models_tbl <- estimate_var_from_model_tbl(
    #   models_tbl = models_tbl, var_data = var_data, new_t_threshold = new_t_threshold, 
    #   names_exogenous = names_exogenous)
    
    # print("still in cvvarfrommodeltbl, before fit_tests_models_table, models_tbl:")
    # print("models_tbl")
    # print(models_tbl)
    # print("models_tbl$t_threshold")
    # print(models_tbl$t_threshold)
    
    ftmt <- fit_tests_models_table(models_tbl = models_tbl,
                                   do_tests = do_tests, 
                                   var_data = var_data,
                                   names_exogenous = names_exogenous,
                                   exo_lag = exo_lag)
    
    models_tbl <- ftmt[["passing_models"]]
    tried_models <- ftmt[["tried_models"]]
    n_lost_to_threshold <- ftmt[["n_lost_to_threshold"]]
    n_lost_to_roots <- ftmt[["n_lost_to_roots"]]
    n_lost_to_white <- ftmt[["n_lost_to_white"]]
    # toc()
    
    print("Done estimating VARs, now we will compute the forecasts")
    
  } else {
    print("Using previously estimates varest objects")
  }
  
  if (do_full_sample_fcs) {
    print("Start forecasts of the estimated models")
    
    tic()
    models_tbl <- forecast_var_from_model_tbl(
      models_tbl = models_tbl, 
      var_data = var_data,
      fc_horizon = fc_horizon, 
      new_t_threshold = new_t_threshold,
      target_transform = target_transform, 
      target_level_ts = target_level_ts, 
      names_exogenous = names_exogenous, 
      extended_exo_mts = extended_exo_mts, 
      fit_column = "fit", 
      keep_varest_obj = TRUE)
    toc()
  }

  
  print("Starting cv")
  # print("but before, print future exo cv")
  # print(future_exo_cv)
  # print(paste0("Number of specifications to cv: ", nrow(models_tbl)))
  
  # tic()
  models_tbl <-  models_tbl %>%
    mutate(cv_obj = pmap(list(fit, variables, lags, t_threshold),
                         ~ cv_var_from_one_row(var_data = var_data,
                                               fit = ..1,
                                               variables = ..2, lags = ..3,
                                               this_thresh = ..4,
                                               h = h, 
                                               n_cv = n_cv,
                                               names_exogenous = names_exogenous,
                                               training_length = training_length,
                                               this_type = "const",
                                               future_exo_cv = future_exo_cv)
                         ),
           full_sample_resmat = map(cv_obj, "full_sample_resmat")
    )
  # toc()
  
  print("transform to yoy")
  
  if (target_transform != "yoy") {
    
    if (target_transform == "diff_yoy") {
      
      print("from diff_yoy to yoy")
      
      models_tbl <- models_tbl %>%
        rename(cv_obj_diff_yoy = cv_obj)
      
      models_tbl <- models_tbl %>%
        mutate(cv_obj_yoy = map(cv_obj_diff_yoy,
                                ~ transform_all_cv( .,
                                                    current_form = target_transform,
                                                    target_level_ts =  target_level_ts,
                                                    n_cv = n_cv)
        )
        )
    }
    
    if (target_transform == "diff") {
      print("from diff to yoy")
      auxiliary_ts <-  target_level_ts
      
      models_tbl <- models_tbl %>%
        rename(cv_obj_diff = cv_obj)
      
      results_all_models <- results_all_models %>%
        mutate(cv_obj_yoy = map(cv_obj_diff,
                                ~ transform_all_cv(cv_object  = .,
                                                   current_form = target_transformation,
                                                   auxiliary_ts = target_level_ts,
                                                   n_cv = n_cv)
        )
        )
    }
    
  }
  
  if (target_transform == "yoy") {
    print("Already in yoy form")
    models_tbl <- models_tbl %>%
      rename(cv_obj_yoy = cv_obj)
  }
  
  print("done transforming")

  models_tbl <- models_tbl %>%
    mutate(rmse_yoy_all_h = map(cv_obj_yoy, all_rmse_from_cv_obj))

  rmse_tibble <- as_tibble(reduce(models_tbl$rmse_yoy_all_h, rbind))
  names(rmse_tibble) <- paste0("rmse_", seq(1, ncol(rmse_tibble)))
  
  models_tbl <- models_tbl %>%
    dplyr::select(-rmse_yoy_all_h) %>%
    cbind(rmse_tibble)

  
  if (!keep_varest_obj) {
    models_tbl <- models_tbl %>%
      dplyr::select(-fit)
  }
  
  if (!keep_fc_objects) {
    models_tbl <- models_tbl %>%
      dplyr::select(vars_select(names(.), -starts_with("fc_ob")))
  } else {
    print("keeping forecast list-columns (they are pretty big ...)")
  }
  
  if (!keep_cv_objects) {
    models_tbl <- models_tbl %>%
      dplyr::select(vars_select(names(.), -starts_with("cv_ob")))
  }
  
  models_tbl <- as_tibble(models_tbl)

  return(list(passing_models = models_tbl, 
              tried_models = table_of_tried_specifications,
              n_lost_to_threshold = n_lost_to_threshold,
              n_lost_to_roots = n_lost_to_roots,
              n_lost_to_white = n_lost_to_white))
} 



cv_var_from_one_row <- function(var_data, 
                                fit, 
                                variables, 
                                lags, 
                                h, 
                                training_length, 
                                n_cv,
                                number_of_qrt_in_year_1 = 4,
                                names_exogenous = c(""),
                                this_type = "const", 
                                future_exo_cv = NULL,
                                this_thresh = 0) {

  this_restriction_mat <- try(fit$restrictions, silent = TRUE) 
  
  if (class(this_restriction_mat) == "try-error") {
    this_restriction_mat <-  NULL
  }
  
  sub_data <- na.omit(var_data[, variables])

  sub_data_tk_index <- tk_index(sub_data, timetk_idx = TRUE, silent = TRUE)
  
  # print("in var cv from one row")
  # print("names in subdata")
  # print(colnames(sub_data))
  # print("names_exogenous")
  # print(names_exogenous)
  # print("this_thresh")
  # print(this_thresh)
  # print("future_exo_cv")
  # print(future_exo_cv)

  this_cv <- var_cv(var_data = sub_data,
                    h_max = h,
                    n_cv = n_cv, this_p = lags,  
                    external_idx = sub_data_tk_index,
                    full_sample_resmat = this_restriction_mat,
                    names_exogenous = names_exogenous,
                    training_length = training_length,
                    this_type = this_type,
                    future_exo_cv = future_exo_cv,
                    this_thresh = this_thresh,
                    number_of_qrt_in_year_1 = number_of_qrt_in_year_1)
  
  # print("just did this_cv")
  
  return(this_cv)
}


# 
# 
# 
# cv_of_VAR_ensemble <- function(var_training_length, 
#                            n_cv, 
#                            tbl_of_models_and_rmse, 
#                            extended_x_data_ts, 
#                            var_data, 
#                            rgdp_level_ts,
#                            max_rank_h = NULL,
#                            chosen_rmse_h = NULL,
#                            h_var = NULL,
#                            ensemble_name = NULL) {
#   
#   if (is.null(ensemble_name)) {
#     ensemble_name <- "ensemble"
#   }
#   
#   
#   if (is.null(ensemble_name)) {
#     model_function_name <- "Ensemble"
#   }
#   
#   cv_ticks_lists_var <- make_test_dates_list(var_data, n = n_cv, h_max = h_var,
#                                              training_length = var_training_length)
#   cv_yq_lists_var <- cv_ticks_lists_var[["list_of_year_quarter"]]
#   cv_dates_lists_var <- cv_ticks_lists_var[["list_of_dates"]]
#   
#   cv_test_data_list <- list()
#   cv_w_fcs_list <- list()
#   cv_error_yoy_list <- list()
#   
#   for (i in 1:n_cv) {
#     this_cv_yq_list_var <- cv_yq_lists_var[[i]]
#     this_training_s_var <- this_cv_yq_list_var$tra_s
#     this_training_e_var <- this_cv_yq_list_var$tra_e
#     this_test_s_var <- this_cv_yq_list_var$tes_s
#     this_test_e_var <- this_cv_yq_list_var$tes_e
#     
#     print("this_training_s_var")
#     print(this_training_s_var)
#     print("this_training_e_var")
#     print(this_training_e_var)
#     print("this_test_s_var")
#     print(this_test_s_var)
#     print("this_test_e_var")
#     print(this_test_e_var)
#     
#     
#     # fcs_and_models <- forecast_var_from_model_tbl(foooooo)
#     # 
#     # w_ave_fc_yoy <- fcs_and_models$w_fc_yoy_ts
#     # 
#     # rgdp_test_yoy_data <- window(make_yoy_ts(rgdp_level_ts, is_log = FALSE),
#     #                              start = this_test_s_arima, 
#     #                              end = this_test_e_arima)
#     # 
#     # cv_error_yoy <- rgdp_test_yoy_data - w_ave_fc_yoy
#     # 
#     # cv_test_data_list[[i]] <- rgdp_test_yoy_data
#     # cv_w_fcs_list[[i]] <- w_ave_fc_yoy
#     # cv_error_yoy_list[[i]] <- cv_error_yoy
#     # 
#     # this_cv_dates_list_arima <- cv_dates_lists_arima[[i]]
#     # date_start_training_arima <- this_cv_dates_list_arima$tra_s
#     # 
#     # this_cv_dates_list_var <- cv_dates_lists_var[[i]]
#     # date_start_training_var <- this_cv_dates_list_var$tra_s
#     # 
#     # 
#     # if (date_start_training_var == as.yearqtr(min(time(var_data)))) {
#     #   print(paste("Training, for VAR, cannot start before this date. Current cv is",
#     #               i))
#     #   n_cv = i
#     #   break
#     # }
#     # 
#     # if (date_start_training_arima == as.yearqtr(min(time(rgdp_ts_in_arima)))) {
#     #   print(paste("Training, for Arima, cannot start before this date. Current cv is",
#     #               i))
#     #   n_cv = i
#     #   break
#     # }
#     
#   }
#   
#   
#   mat_cv_error_yoy <- matrix(
#     reduce(cv_error_yoy_list, rbind), 
#     nrow = n_cv, byrow = TRUE)
#   
#   ensemble_rmse <-   sqrt(colMeans(mat_cv_error_yoy^2))
#   
#   this_name <- ensemble_name
#   this_rmse <- ensemble_rmse
#   this_rmse_h <- paste0("rmse_", 1:length(this_rmse))
#   
#   ensemble_rmse_tbl <- tibble(variables = this_name, model_function = model_function_name, 
#                               rmse_h = this_rmse_h, rmse = this_rmse, 
#                               horizon = 1:length(this_rmse), lags = NA)
#   
#   
#   return(list(ensemble_rmse = ensemble_rmse_tbl,
#               cv_error_yoy_list = cv_error_yoy_list,
#               cv_test_data_list = cv_test_data_list,
#               cv_w_fcs_list = cv_w_fcs_list))
#   
# }




cv_var_from_tbl_by_row <- function(h, 
                                   n_cv, 
                                   training_length, 
                                   models_tbl, 
                                   var_data, 
                                   fit_column = NULL, 
                                   target_transform = "yoy", 
                                   target_level_ts = NULL,
                                   keep_varest_obj = FALSE,
                                   keep_cv_objects = FALSE,
                                   names_exogenous = c(""),
                                   exo_lag = NULL,
                                   future_exo_cv = NULL,
                                   do_tests = TRUE,
                                   silent = TRUE) {
  
  # print("in cv_var_from_model_tbl_by_row, names_exogenous is")
  # print(names_exogenous)
  # print("and future_exo_cv is")
  # print(future_exo_cv)
 
  
  variables <- models_tbl$variables
  lags <- models_tbl$lags
  t_threshold <- models_tbl$t_threshold

  model_and_rmse <- mutate(
    models_tbl,
    tests_and_rmses = pmap(list(variables, lags, t_threshold),
                           ~ specs_to_rmse(var_data = var_data, 
                                           variables = ..1,
                                           lags = ..2, 
                                           t_thresholds = ..3,
                                           future_exo_cv = future_exo_cv,
                                           training_length = training_length,
                                           h = fc_horizon, n_cv = n_cv,
                                           target_transform = target_transform, 
                                           target_level_ts = target_level_ts, 
                                           names_exogenous = names_exogenous)
    )
  )
  
  # print("salimos de specs to rmse")
  rmse_names_h <- paste0("rmse_", seq(1, h))
  nested_rmse <- model_and_rmse
  nested_rmse <- dplyr::select(model_and_rmse, - rmse_names_h) %>% 
    dplyr::select(-c(t_threshold, msg, tested, pass_tests))
  
  model_and_rmse <- unnest(nested_rmse, tests_and_rmses) 
  
  print("model_and_rmse")
  print(model_and_rmse)
  
  model_and_rmse <- mutate(model_and_rmse, 
                           short_name = pmap_chr(list(variables, lags, t_threshold),
                                             ~ make_model_name(..1, ..2, ..3)
                                             ),
                           is_unrestricted = t_threshold == 0
                           )
  
  

  model_and_rmse <- dplyr::select(model_and_rmse, short_name, variables, size, lags, t_threshold, everything()) 
  
  names_tried_models <- model_and_rmse$short_name
  
  passing_models <- filter(model_and_rmse, pass_tests)
  names_passing_models <- passing_models$short_name
  tried_models <- dplyr::select(model_and_rmse, variables, size, lags, t_threshold, short_name)
  
  return(list(passing_models_tbl = passing_models,
              tried_models_tbl = tried_models, 
              tried_models_names = names_tried_models, 
              nested_rmse = nested_rmse))
}



estimate_var_from_model_tbl_old <- function(models_tbl, 
                                        var_data, 
                                        new_t_threshold = NULL, 
                                        names_exogenous = c(""),
                                        exo_lag = NULL,
                                        remove_ranks = TRUE) {
  
  # print("in estimate var from model tbl")
  # print(names_exogenous)
  
  starting_names <- names(models_tbl)
  # print("starting_names in estimate var from")
  # print(starting_names)
  
  has_short_name <- "short_name" %in% starting_names
  has_t_threshold <- "t_threshold" %in% starting_names
  
  if (!has_t_threshold) {
    models_tbl <- models_tbl %>% mutate(t_threshold = FALSE)
  }
  
  
  models_tbl <- models_tbl %>% 
        mutate(model_type = "VAR",
               )

  
  if (!has_short_name) {
    models_tbl <- models_tbl %>% 
      mutate(short_name = pmap(list(variables, lags, t_threshold),
                               ~ make_model_name(variables = ..1, lags = ..2,
                                                 t_threshold = ..3)),
             short_name = unlist(short_name))
    
    models_tbl <- models_tbl %>% dplyr::select(short_name, everything())
  }
  
  one_model_per_row <- models_tbl %>% 
     distinct(short_name, .keep_all = TRUE)
  
  
  if (is.null(new_t_threshold)) {
    one_model_per_row <- one_model_per_row %>%
      mutate(fit = pmap(list(variables, lags, t_threshold),
                        ~ fit_VAR_rest(var_data = var_data, variables = ..1,
                                       p = ..2, t_thresh = ..3, 
                                       names_exogenous = names_exogenous, 
                                       exo_lag = exo_lag))
      )
  } 
  
  if (!is.null(new_t_threshold)) {
    
    all_one_model_per_row <- list_along( seq(1, length(new_t_threshold))  )
    
    for (i in seq(1, length(new_t_threshold))) {
      
      this_thresh <- new_t_threshold[i]
      
      this_one_model_per_row <- one_model_per_row %>%
        mutate(t_threshold = this_thresh,
               short_name_t = map_chr(short_name, ~ paste0(.x, "_t", this_thresh*100)),
               fit = pmap(list(variables, lags, t_threshold),
                          ~ fit_VAR_rest(var_data = var_data, variables = ..1,
                                         p = ..2, t_thresh = ..3, 
                                         names_exogenous = names_exogenous, 
                                         exo_lag = exo_lag))
        )
      
      all_one_model_per_row[[i]] <- this_one_model_per_row
    }
    one_model_per_row <- reduce(all_one_model_per_row, rbind)
  }
  
  return(one_model_per_row)
}


fit_tests_models_table <- function(models_tbl,
                                   var_data,
                                   keep_fit = TRUE, 
                                   do_tests = TRUE,
                                   names_exogenous = c(""),
                                   exo_lag = NULL,
                                   silent = TRUE,
                                   remove_aux_unrest = FALSE,
                                   use_resmat = FALSE) {
  
  # print("in fit_tests_models_table:")
  # print("names_exogenous")
  # print(names_exogenous)
  # 
  # print("use_resmat")
  # print(use_resmat)
  
  models_tbl <- models_tbl %>% 
    mutate(is_unrestricted = map_lgl(t_threshold, 
                                     ~ length(.) == 1 & (all(. == 0) )
                                     ))

  # print("first models_tbl in fit test models table")
  # print(models_tbl)
  
  premod_rest <- models_tbl %>% filter(!is_unrestricted)
  
  if (nrow(premod_rest) > 0){
    premod_rest <- premod_rest %>% 
      unnest(t_threshold, .drop = FALSE)
  }
   
  premod_unrest <- models_tbl %>% filter(is_unrestricted)

  premod <- rbind(premod_rest, premod_unrest) %>% 
    mutate(short_name = pmap(list(variables, lags, t_threshold), 
                             ~ make_model_name(variables = ..1, 
                                               lags = ..2, 
                                               t_threshold = ..3)),
           short_name = unlist(short_name))

  original_short_names <- dplyr::select(premod, short_name)

  n_pure_unrestricted <- sum(models_tbl$is_unrestricted)
  n_pure_restricted <- length(unlist(
    models_tbl$t_threshold[!models_tbl$is_unrestricted]))
  n_auxiliar_unrestricted <- sum(!(models_tbl$is_unrestricted))

  if (!silent) {
    print(paste0("Number of intended unrestricted models to fit: ", n_pure_unrestricted))
    print(paste0("Number of restricted models to fit: ", n_pure_restricted))
    print(paste0("Number of purely auxiliar unrestricted models to fit: ", n_auxiliar_unrestricted - n_pure_unrestricted))
    print(paste0("Total number of models to fit: ", 
                 n_pure_restricted + n_auxiliar_unrestricted))
  }
  
  # some of these unrestricted models could be duplicated as auxiliar estimations
  # of restricted models. To avoid double work, estimate first restricted models
  # return also auxiliar unrestricted ones and, then, only estimate unnrestricted 
  # models that are not among the set of auxilar models
  
  models_tbl_pure_unrestricted <- models_tbl %>%
    filter(is_unrestricted)
  
  if (nrow(models_tbl_pure_unrestricted) > 0) {
    models_tbl_pure_unrestricted <- models_tbl_pure_unrestricted %>% 
      mutate(short_name = pmap(list(variables, lags, t_threshold), 
                               ~ make_model_name(variables = ..1, 
                                                 lags = ..2, 
                                                 t_threshold = ..3)),
             short_name = unlist(short_name))
  } 

  models_tbl <- models_tbl %>% filter(!is_unrestricted)
  


  if (n_pure_restricted > 0) {
   
    if (use_resmat) {
      models_tbl <- models_tbl %>% 
        mutate(fit = pmap(list(variables, lags, t_threshold, full_sample_resmat),
                          ~ fit_VAR_rest(var_data, variables = ..1, 
                                         p = ..2, t_thresh = ..3,
                                         resmat = ..4,
                                         names_exogenous = names_exogenous))
        )
    } else {
      
      models_tbl <- models_tbl %>% 
        mutate(fit = pmap(list(variables, lags, t_threshold),
                          ~ fit_VAR_rest(var_data, variables = ..1, 
                                         p = ..2, t_thresh = ..3,
                                         resmat = NULL,
                                         names_exogenous = names_exogenous))
        )
    }
    
    # print("Right after fit_VAR_rest models_tbl is")
    # print(models_tbl, n = 40)
    
    models_tbl <- models_tbl  %>% 
      dplyr::select(-t_threshold) %>% 
      unnest(fit, .drop = FALSE) %>% 
      mutate(short_name = pmap(list(variables, lags, t_threshold), 
                                ~ make_model_name(variables = ..1, 
                                                  lags = ..2, 
                                                  t_threshold = ..3)),
             short_name = unlist(short_name),
             is_auxiliary = map_lgl(t_threshold, ~ . == 0),
             is_unrestricted = map_lgl(t_threshold, 
                                       ~ length(.) == 1 & (all(. == 0))
                                       )
             ) %>% 
      distinct(short_name, .keep_all = TRUE)
    
    # print("restricted and unrestricted models_tbl")
    # print(models_tbl)
    # 
    # print("original_short_names")
    # print(original_short_names)
    
    auxiliary_also_original <-  models_tbl %>% 
      filter(is_auxiliary) %>% 
      semi_join(original_short_names, by = "short_name") %>% 
      dplyr::select(-is_auxiliary)
    
    # print("auxiliary_also_original")
    # print(auxiliary_also_original)
    
    auxiliary_but_not_original <-  models_tbl %>% 
      filter(is_auxiliary) %>% 
      anti_join(original_short_names, by = "short_name") %>% 
      dplyr::select(-is_auxiliary)
    
    # print("auxiliary_but_not_original")
    # print(auxiliary_but_not_original)
    
    models_tbl <- models_tbl  %>% filter(!is_auxiliary) %>% 
      dplyr::select(-is_auxiliary)
  } else {
    # zero row tibbles
    auxiliary_but_not_original <- models_tbl
    auxiliary_also_original <- models_tbl
  }

  if (n_pure_unrestricted > 0) {
    # elimate repeated unrestricted
    # but only if there are auxiliary models
    if (n_pure_restricted > 0) {
      unrestricted_not_auxiliary  <- anti_join(models_tbl_pure_unrestricted, 
                                               auxiliary_also_original,
                                               by = "short_name")
    } else {
      unrestricted_not_auxiliary  <- models_tbl_pure_unrestricted
    }

    if (nrow(unrestricted_not_auxiliary) > 0){
      unrestricted_not_auxiliary  <- unrestricted_not_auxiliary  %>% 
        mutate(fit = pmap(list(variables, lags, t_threshold),
                          ~ fit_VAR_rest(var_data, variables = ..1, 
                                         p = ..2, t_thresh = ..3, 
                                         names_exogenous = names_exogenous)))
    }
  } else {
    # is an zero row tibble
    unrestricted_not_auxiliary <- models_tbl_pure_unrestricted
  }
  
  # print("before rbind models tbl")
  # print(models_tbl)
  
  if (!remove_aux_unrest) {
    print("NOT removing aux")
    models_tbl <- rbind(models_tbl, unrestricted_not_auxiliary, 
                        auxiliary_also_original, auxiliary_but_not_original) %>% 
      distinct(short_name, .keep_all = TRUE)

  } else {
    print("removing aux")
    models_tbl <- rbind(models_tbl, unrestricted_not_auxiliary, 
                        auxiliary_also_original) %>% 
      distinct(short_name, .keep_all = TRUE)
    
    # print(models_tbl)
  }

  # print("models ready for testing")
  # print(models_tbl)

  table_of_tried_specifications <- models_tbl %>% dplyr::select(-fit) 
  
  n_before_varestfilter <- nrow(models_tbl)

  if (do_tests) {
    
    models_tbl <- models_tbl %>% 
      mutate(cf = map(fit, ~class(.x))) %>% 
      filter(cf == "varest")
    
    n_post_varestfilter <- nrow(models_tbl)
    n_non_varest <- n_before_varestfilter - n_post_varestfilter
    
    models_tbl <- models_tbl %>% 
      mutate(is_stable = map_lgl(fit, ~ all(vars::roots(.x) < 1))
      ) %>% 
      filter(is_stable)
    
    n_post_stable <- nrow(models_tbl)
    n_non_stable <- n_post_varestfilter - n_post_stable
    
    
    models_tbl <- models_tbl %>% 
      mutate(is_white_noise = map_lgl(fit, ~ check_resid_VAR(.x))
      ) %>% 
      filter(is_white_noise)
    
    n_post_checkresid <- nrow(models_tbl)
    n_non_white_noise <- n_post_stable - n_post_checkresid
    
    if (!silent) {
      print("doing tests")
      print(paste0("Number of models with non-surving equations: ", n_non_varest))
      print(paste0("Number of models to be tested for stability: ", n_post_varestfilter))
      print(paste0("Number of models with unstable roots: ", n_non_stable))
      print(paste0("Number of models for portmanteau testing: ", n_post_stable))
      print(paste0("Number of models with non-white-noise residuals : ", n_non_white_noise))
      print(paste0("Number of models to be (ts)cross-validated or forecasted: ", n_post_checkresid))
    }
    
    models_tbl <- models_tbl %>% dplyr::select(-c(is_stable, is_white_noise, cf))
    if(!keep_fit) {
      models_tbl <- models_tbl %>% dplyr::select(-fit)
    }
    
    return(list(passing_models = models_tbl, 
                tried_models = table_of_tried_specifications,
                n_lost_to_threshold = n_non_varest,
                n_lost_to_roots = n_non_stable,
                n_lost_to_white = n_non_white_noise))
    
  } else {
    
    print("not doing tests")
    
    print(paste0("Number of models to be (ts)cross-validated or forecasted: ", n_before_varestfilter))
    if (!keep_fit) {
      print("Not keeping fit column")
      models_tbl <- models_tbl %>% dplyr::select(-fit)
    }
    
    # print("right before returning output in fit test models table")
    # print(models_tbl)
    
    return(list(passing_models = models_tbl, 
                tried_models = table_of_tried_specifications,
                n_lost_to_threshold = NULL,
                n_lost_to_roots = NULL,
                n_lost_to_white = NULL)
           )
  }
}


fit_VAR_rest <- function(var_data, 
                         variables,
                         p,
                         t_thresh = FALSE, 
                         type = "const",
                         names_exogenous = c(""),
                         exo_lag = NULL,
                         resmat = NULL)  {
  
  
  # print("in fit VAR rest")
  # print("variables")
  # print(variables)
  # print("p")
  # print(p)
  # print("t_thresh")
  # print(t_thresh)
  # print("names_exogenous")
  # print(names_exogenous)
  # print("resmat")
  # print(resmat)
  # print("colnames(var_data)")
  # print(colnames(var_data))
  
  
  if (length(t_thresh) == 1) {
    if (t_thresh == 0 | is.null(t_thresh)) {
      t_thresh <- FALSE
    }
  }
  # print(11)
  this_var_data <- var_data[, variables]
  # print(12)
  this_var_data <- na.omit(this_var_data)
  
  vbls_for_var <- colnames(this_var_data)
  endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
  exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 
  

  
  if (length(endov) == 1) {
    this_fit <- NA
    # this_fit <- "one endog"
    
    print("only one endogenous variable, not a real VAR, returning NA")
    
    
    if (t_thresh) {
      thresholds_and_fits <- tibble(t_threshold = c(0, t_thresh),
                                    fit = this_fit)
      
    } else {
      thresholds_and_fits <- tibble(t_threshold = c(0),
                                    fit = this_fit)
      
    }
    
    return(thresholds_and_fits)
  }
  
  endodata <- this_var_data[ , endov]
  exodata <- this_var_data[ , exov]
  
  if (is.null(dim(endodata))) {
    names(endodata) <- endov
  } else {
    colnames(endodata) <- endov
  }
  
  if (is.null(exo_lag)) {
    exo_lag <- p
  }
  
  exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
  n <- nrow(var_data)
  
  if (is.null(exo_and_lags)) {
    unrestricted_fit <- vars::VAR(y = endodata, p = p, type = type) 
    # print("unrestricted_fit")
    # print(unrestricted_fit)
    
  } else {
    unrestricted_fit <- vars::VAR(y = endodata, p = p, type = type, 
                          exogen = exo_and_lags)
    # print("unrestricted_fit")
    # print(unrestricted_fit)
    
  }

  # print("unrestricted_fit")
  # print(unrestricted_fit)
  
  if (is.numeric(t_thresh)) {
    nrest <- length(t_thresh)
    list_of_varests <- list_along(seq(1, nrest+1)) 
    list_of_varests[[1]] <- unrestricted_fit
    
    # print("inside IF is.numeric(t_thresh) ")
    # print("t_thresh")
    # print(t_thresh)
    # print("nrest")
    # print(nrest)
    
    for (i in seq(1, nrest)) {
      this_thresh <- t_thresh[i]
      # print(this_thresh)
      
      if(is.null(resmat)) {
        this_fit <- try(vars::restrict(unrestricted_fit, method = "ser", 
                                       thresh = this_thresh), silent = TRUE)
      } else {
        # print("in fit var rest, using full sample resmat")
        # print(resmat)
        this_fit <- try(vars::restrict(unrestricted_fit, method = "manual", 
                                       resmat = resmat), silent = TRUE)
      }
      
      # print("with resmat:")
      # print(resmat)
      # 
      # print("this_fit")
      # print(this_fit)
      
      if (class(this_fit) == "try-error") {
        this_fit <- "one_or_more_eqn_drops"
      }
      
      list_of_varests[[i+1]] <- this_fit
    }
    
    # print("in fitvarrest")
    # print("variables")
    # print(variables)
    # print("p")
    # print(p)
    # print("t_thresh")
    # print(t_thresh)
    # print("this_fit")
    # print(this_fit)
    # print("-------------------------------")
    
    thresholds_and_fits <- tibble(t_threshold = c(0, t_thresh),
                                  fit = list_of_varests)
    
    return(thresholds_and_fits)
    
  } else {
    return(unrestricted_fit)
  }
  
}



forecast_VAR_one_row <- function(fit,
                                 h, 
                                 variables,
                                 extended_exo_mts, 
                                 names_exogenous = c(""), 
                                 exo_lag = NULL,
                                 use_vars_predict = FALSE,
                                 future_cond_endo = NULL, 
                                 cond_name = NULL)  {
  
  are_there_exo <- any(names_exogenous %in% variables)
  
  # print("are_there_exo")
  # print(are_there_exo)
  # print("variables")
  # print(variables)
  # print("names_exo")
  # print(names_exogenous)
  # print("fit")
  # print(fit)
  
  if(class(fit)[[1]] == "tbl_df") {
    # print("fit")
    # print(fit)
  }

  if (class(fit) == "varest") {

    
    this_var_data <- fit$y
    endov <- variables[!variables %in% names_exogenous] 
    exov <- variables[variables %in% names_exogenous] 
    
    # print("endov")
    # print(endov)
    # 
    # print("exov")
    # print(exov)
    # print( "extended_exo_mts")
    # print(extended_exo_mts)
    
    if (!are_there_exo) {
      
      
      # print("there is no exo variables used")
      exo_and_lags <- NULL
      exo_and_lags_extended <- NULL
    } else {
      
      # print("there is at least one exo variables used")
      exodata <- extended_exo_mts[, exov]
      # print("in particular: ")
      # print(exodata)

      if (is.null(exo_lag)) {
        exo_lag <- fit$p
      }

      
      exo_and_lags_extended <- make_exomat(exodata = exodata, 
                                           exov = exov,
                                           exo_lag = exo_lag)
     
      # print("thisvardata")
      # print(this_var_data)
      # print("exodata")
      # print(exodata)
      # print("exo_and_lags_extended")
      # print(exo_and_lags_extended)
      # print("exov")
      # print(exov)
      # 
      # print(1)
      
      exo_and_lags <- window(exo_and_lags_extended,
                             end = end(this_var_data))

      exo_and_lags_for_fc <- subset(exo_and_lags_extended, 
                                    start = nrow(exo_and_lags) + 1)
      
      # print("doing assing")
      
      assign("exo_and_lags", exo_and_lags,
             envir = .GlobalEnv)
      
    }
    
    # print(2)
    # print("exo_and_lags")
    # print(exo_and_lags)
    
  
    if (is.null(exo_and_lags_extended)) {
      # print("forecasting without exovars")
      this_fc <- forecast(fit, h = h)
    } else {
      # print("forecasting with exogenous variables")
      
      if (use_vars_predict) {
        this_fc <- predict_conditional(object = fit, 
                                n.ahead = h, 
                                dumvar = exo_and_lags_for_fc, 
                                Z_cond_future = future_cond_endo, 
                                cond_name = cond_name)
      }
      
      if (!use_vars_predict) {
        this_fc <- forecast(fit, h = h, dumvar = exo_and_lags_for_fc,
                            exogen = exo_and_lags)
      }
      
    }
    
  }
  
  if (!class(fit) == "varest") {
    this_fc <- list(forecast = list(rgdp = list(mean = NA)))
    
  }
  
  # print("class(this_fc)")
  # print(class(this_fc))
  
  
  if(class(this_fc) == "varprd") {
    out <- list(model = fit, forecast = vector("list", fit$K))
    
    names(out[["forecast"]]) <- colnames(fit$y)
    
    
    tspx <- tsp(fit$y)
    j <- 1
    for (fcast in out$forecast) {
      fcast$mean <- ts(this_fc$fcst[[j]][, "fcst"], frequency = tspx[3], start = tspx[2] + 1 / tspx[3])
      fcast$x <- fit$y[, j]
      fcast$series <- colnames(fit$y)[j]
      fcast <- structure(fcast, class = "forecast")
      out$forecast[[j]] <- fcast
      # out$forecast[[colnames(fit$y)[j]]] <- fcast
      # names(out$forecast[[j]]) <- colnames(fit$y)[j]
      j <- j + 1
    }
    this_fc <- out
  }

    return(this_fc)
}





forecast_var_from_model_tbl <- function(models_tbl, 
                                        var_data,
                                        fc_horizon, 
                                        new_t_threshold = NULL, 
                                        fit_column = NULL,
                                        target_transform = "yoy",
                                        target_level_ts = NULL,
                                        keep_fc_obj = FALSE,
                                        keep_varest_obj = FALSE,
                                        names_exogenous = c(""),
                                        extended_exo_mts = NULL,
                                        do_tests = FALSE,
                                        remove_aux_unrest = TRUE,
                                        use_resmat = FALSE,
                                        keep_wide_tbl = FALSE, 
                                        max_rank_h = NULL) {
  
  
  
  # print("In fc from mdel tibble")
  # print("names_exogenous")
  # print(names_exogenous)
  # print("models_tbl")
  # print(models_tbl)
  
  if (!is.null(max_rank_h)) {
    models_tbl <- discard_by_rank(models_tbl, max_rank_h = max_rank_h, is_wide = TRUE)
  }


  starting_names <- names(models_tbl)
  has_short_name <- "short_name" %in% starting_names | "model_name"  %in% starting_names 
  has_t_threshold <- "t_threshold" %in% starting_names
  
  if (!has_t_threshold) {
    models_tbl <- models_tbl %>% mutate(t_threshold = 0)
  }
  
  if (!has_short_name) {
    models_tbl <- models_tbl %>% 
      mutate(short_name = pmap(list(variables, lags, t_threshold),
                               ~ make_model_name(variables = ..1, lags = ..2, t_threshold = ..3)),
             short_name = unlist(short_name))
    
    models_tbl <- models_tbl %>% dplyr::select(short_name, everything())
  }
  
  if (is.null(fit_column)) {
    
    print("There is no column with fit varest objects, so we will estimate all VARs now")
    # print("pre rubvwecd")
    # print(models_tbl)
    if (use_resmat) {
      print("Using pre-existent restriction matrices for restricted models")
      ftmt <- fit_tests_models_table(models_tbl = models_tbl,
                                     var_data = var_data,
                                     do_tests = do_tests,
                                     remove_aux_unrest = remove_aux_unrest,
                                     silent = FALSE, 
                                     use_resmat = TRUE, 
                                     names_exogenous = names_exogenous)
      # print("ftmt[[passing_models]]$fit")
      # print(ftmt[["passing_models"]]$fit)
    } else {
      print("Using t-threshold values for restricted models")
      
      ftmt <- fit_tests_models_table(models_tbl = models_tbl, 
                                     var_data = var_data,
                                     do_tests = do_tests, 
                                     remove_aux_unrest = remove_aux_unrest,
                                     silent = FALSE, 
                                     use_resmat = FALSE, 
                                     names_exogenous = names_exogenous)
    } 
    
    models_tbl <- ftmt[["passing_models"]]
    
    
    models_tbl <- models_tbl %>% 
      mutate(is_unrestricted = map_lgl(t_threshold, 
                                       ~ length(.) == 1 & (all(. == 0))
                                       ),
             short_name = pmap(list(variables, lags, t_threshold),
                                ~ make_model_name(variables = ..1, lags = ..2,
                                                  t_threshold = ..3)),
             short_name = unlist(short_name))

    print("Done estimating VARs, now we will compute the forecasts")
    
  } else {
    print("Using previously estimates varest objects")
  }

  # print(1)
  # print("models_tbl")
  # print(models_tbl)
  # print(models_tbl$fit)
  # print(models_tbl$variables)
  
  models_tbl <- models_tbl %>% 
    mutate(fc_object_raw = map2(fit, variables,
                                ~ forecast_VAR_one_row(
                                  fit = .x, variables = .y, h = fc_horizon, 
                                  names_exogenous = names_exogenous,
                                  extended_exo_mts = extended_exo_mts)
                                )
           )

  if (target_transform == "yoy") {
    print("Target variable already in YoY form, so no transformation is needed")
    models_tbl <- models_tbl %>% 
      mutate(target_mean_fc_yoy = map(fc_object_raw,
                                      ~ .x[["forecast"]][["rgdp"]][["mean"]]))
  }
  
  if (target_transform != "yoy") {
    
    print(paste0("Target variable is in ", target_transform,
                 " form. Forecasts will be transformed to YoY."))

    models_tbl <- models_tbl %>% 
      mutate(target_mean_fc = map(fc_object_raw,
                                  ~ .x[["forecast"]][["rgdp"]][["mean"]])
      )

    models_tbl <- models_tbl %>% 
      mutate(target_mean_fc_yoy = map(target_mean_fc, 
                                      ~ any_fc_2_fc_yoy(
                                        current_fc = .x, 
                                        rgdp_transformation = target_transform,
                                        rgdp_level_ts = target_level_ts)
                                      )
      )

  }
  
  print("Done transforming")

  if (!keep_varest_obj) {
    models_tbl <- models_tbl %>% 
      dplyr::select(-fit)
  }
  
  if (!keep_fc_obj) {
    models_tbl <- models_tbl %>% 
      dplyr::select(-fc_object_raw)
  }
  
  if(keep_wide_tbl) {
    models_tbl_wide <- models_tbl
  } 
  
  rmse_names <- paste0("rmse_", 1:fc_horizon)

  models_tbl <- models_tbl %>% 
    gather(key = "rmse_h", value = "rmse", rmse_names)
  
  models_tbl <- models_tbl %>% 
    mutate(horizon = as.numeric(substr(rmse_h, 6, 6))
    ) 
  
  models_tbl <- models_tbl %>% 
    mutate(this_h_fc_yoy = map2_dbl(target_mean_fc_yoy, horizon, ~ .x[.y])
    ) 
  
  models_tbl <- models_tbl %>% 
    group_by(horizon) %>% 
    mutate(rank = rank(rmse))  
  
  if (!is.null(max_rank_h)) {
    models_tbl <- discard_by_rank(models_tbl, max_rank_h = max_rank_h,
                                  is_wide =FALSE)
  }
  
  models_tbl <- ungroup(models_tbl)
  

  models_info_per_h <- models_tbl %>% group_by(rmse_h) %>% 
    summarise(unique_vbl_per_h = list(unique(unlist(variables))),
              freq_vbl_per_h = list(table(unlist(variables))),
              unique_maxlag_per_h = list(unique(unlist(lags))),
              freq_lags_per_h = list(table(unlist(lags)))
    )
  
  if(!keep_wide_tbl) {
    models_tbl_wide <- NULL
  } 
  
  
  return(list(models_tbl = models_tbl, 
              models_info_per_h = models_info_per_h, 
              models_tbl_wide = models_tbl_wide))
  
}


get_rmses_h_rankings_h <- function(data = cv_objects, h_max = 6){
  cv_errors <- data[["cv_errors"]]
  
  all_rmses <- map(cv_errors, function(x) sqrt(colMeans( (reduce(x, rbind))^2))  )
  print(is.null(all_rmses))
  all_rmses_tbl <- reduce(all_rmses, rbind)
  rmse_names <- paste0("rmse_", 1:h_max)
  colnames(all_rmses_tbl) <- rmse_names
  row.names(all_rmses_tbl) <- NULL
  
  
  for (r in seq_along(rmse_names)) {
    this_rmse <- rmse_names[r]
    rmse_vec <- all_rmses_tbl[, this_rmse]
    this_rank <- rank(rmse_vec)
    all_rmses_tbl <- cbind(all_rmses_tbl, this_rank)
    
  }
  
  ranking_names <- paste0("rank_", 1:h_max)
  rmse_and_rank_names <- c(rmse_names, ranking_names)
  colnames(all_rmses_tbl) <- rmse_and_rank_names
  
  rmse_each_h <- cbind(data, all_rmses_tbl)
  
  return(rmse_each_h)
  
}


get_sets_of_variables <- function(df, this_size, all_variables, already_chosen){
  
  len_already_chosen <- length(already_chosen)
  len_other_vbls <- this_size - len_already_chosen
  
  passing_variables <- all_variables
  
  passing_not_alr_chosen <- passing_variables[!passing_variables %in% already_chosen]
  
  n_passing_vbls <- length(passing_not_alr_chosen)
  
  # print(paste("We have", n_passing_vbls, "free variables, to fill the remaining", len_other_vbls,
  #             "variables in the VAR.Total possible combinations :",
  #             choose(n_passing_vbls, len_other_vbls)))
  
  combinations <- combn(passing_not_alr_chosen, len_other_vbls)
}




indiv_weigthed_fcs_VAR <- function(tbl_of_models_and_rmse, extended_x_data_ts, 
                               rgdp_ts_in_arima, var_data, max_rank_h = NULL,
                               model_type = NULL, chosen_rmse_h = NULL,
                               force.constant = FALSE,
                               h_arima = NULL, h_var = NULL,
                               var_start =  NULL, var_end = NULL,
                               arima_start = NULL, arima_end = NULL) {
  
  # print("in indiv new")
  # print("in indiv new, h_arima")
  # print(h_arima)
  
  
  if (!is.null(model_type)) {
    tbl_of_models_and_rmse <- tbl_of_models_and_rmse %>% 
      filter(model_function == model_type) %>% 
      group_by(rmse_h) %>% 
      mutate(rank_h = rank(rmse)) %>% 
      arrange(rmse_h, rank_h)
  }
  
  if (!is.null(chosen_rmse_h)) {
    tbl_of_models_and_rmse <- tbl_of_models_and_rmse %>% 
      filter(rmse_h == chosen_rmse_h) %>% 
      mutate(rank_h = rank(rmse))
  }
  
  if (!is.null(max_rank_h)) {
    tbl_of_models_and_rmse <- tbl_of_models_and_rmse %>% 
      filter(rank_h <= max_rank_h)
  }
  
  if (!is.null(var_start)) {
    var_data <- window(var_data, start = var_start)
  }
  
  if (!is.null(var_end)) {
    var_data <- window(var_data, end = var_end)
  }
  
  if (!is.null(arima_start)) {
    rgdp_ts_in_arima <- window(rgdp_ts_in_arima, start = arima_start)
  }
  
  if (!is.null(arima_end)) {
    rgdp_ts_in_arima <- window(rgdp_ts_in_arima, end = arima_end)
  }
  
  
  my_stability_fun <- function(model_type, model_object) {
    
    # print(model_type)
    # print(model_object)
    
    if (model_type == "Arima") {
      is.stable <- TRUE
      
    }
    if (model_type == "VAR"){
      is.stable <- all(roots(model_object) < 1)
    }
    
    return(is.stable)
  }
  
  
  # print("in indiv new, before the tibble ")
  
  
  tibble_fit_and_fcs <- tbl_of_models_and_rmse %>% 
    group_by(rmse_h) %>% 
    mutate(sum_invmse_h = sum(inv_mse),
           model_weight_h = inv_mse/sum_invmse_h,
           horizon = as.numeric(substr(rmse_h, 6, 6)),
           fit = pmap(list(model_function, variables, lags, arima_order, 
                           arima_seasonal),
                      ~ fit_VAR_Arima(model_function = ..1, variables = ..2, 
                                      lags = ..3, order = ..4, seasonal = ..5,
                                      extended_x_data_ts = extended_x_data_ts,
                                      arima_rgdp_ts = rgdp_ts_in_arima,
                                      force.constant = force.constant,
                                      var_data = var_data)),
           fc_obj = pmap(list(model_function, variables, lags, fit),
                         ~ forecast_VAR_Arima(model_function = ..1, 
                                              variables = ..2, lags = ..3,
                                              fit = ..4, h_arima = h_arima, 
                                              h_var = h_var,
                                              mat_x_ext = extended_x_data_ts,
                                              force.constant = force.constant)),
           fc_mean = map2(model_function, fc_obj, ~ fc_mean_var_arima(.x, .y)),
           rgdp_transformation = map(
             model_function, ~ what_rgdp_transformation(country_name = country_name,
                                                        model_type = .)),
           fc_yoy = map2(fc_mean, rgdp_transformation,
                         ~ any_fc_2_fc_yoy(current_fc = .x,
                                           rgdp_transformation = .y,
                                           rgdp_level_ts = rgdp_level_ts)),
           weighted_fc_at_h = pmap(list(model_weight_h, fc_yoy, horizon),
                                   ~ subset(..1 * ..2, start = ..3, end = ..3)),
           fc_at_h = pmap(list(model_weight_h, fc_yoy, horizon),
                          ~ subset(..2, start = ..3, end = ..3)),
           is_stable = map2(model_function, fit, ~my_stability_fun(model_type = .x, model_object = .y))
    ) %>% 
    ungroup() %>% filter(is_stable == TRUE)
  
  w_ave_fc_tbl <- tibble_fit_and_fcs %>% 
    group_by(horizon) %>%
    summarise(sum_one_h = reduce(weighted_fc_at_h, sum))
  
  # print(" w_ave_fc_tbl ")
  # print( w_ave_fc_tbl )
  
  
  if (is.null(model_type)) {
    w_fc_yoy_ts <- fc_summ_to_ts(w_ave_fc_tbl, var_data = var_data)
  } else {
    if (model_type == "Arima") {
      w_fc_yoy_ts <- fc_summ_to_ts(w_ave_fc_tbl, var_data = rgdp_ts_in_arima)
    }
    
    
    if (model_type == "VAR") {
      w_fc_yoy_ts <- fc_summ_to_ts(w_ave_fc_tbl, var_data = var_data)
    }
  }
  
  w_fc_yoy_ts <- na.omit(w_fc_yoy_ts)
  
  return(list(info_fit_ifcs = tibble_fit_and_fcs,
              w_fc_yoy_ts = w_fc_yoy_ts))
  
  # fc_for_plot <- tibble_fit_and_fcs %>% 
  #   select(short_name, model_function, fc_yoy, fc_at_h, rmse_h, rmse, 
  #          model_weight_h)
  # 
  # ensemble_model_tbl <- tibble(short_name = "ensemble", 
  #                              model_function = "weighted_average",
  #                              fc_yoy = w_fc_yoy_ts, fc_at_h = NA, rmse_h = "rmse_1",
  #                              rmse = 0.00001, model_weight_h = 1)
  
  # fc_for_plot <- rbind(fc_for_plot, ensemble_model_tbl)
  
  
  
  
  # return(list(info_fit_ifcs = tibble_fit_and_fcs,
  #             w_fc_yoy_ts = w_fc_yoy_ts,
  #             fc_for_plot = fc_for_plot))
}


make_exomat <- function(exodata, exov, exo_lag) {
  
  n_exo <- length(exov)
  
  if (n_exo == 0) {
    exodata <- NULL
    return(exodata)
  }
  
  if (n_exo > 0) {
    exo_and_lags_list <- list_along(seq(1, n_exo))
    names_exo_and_lags_list <- list_along(seq(1, n_exo))
    for (ex in 1:n_exo) {
      this_exoname <- exov[ex]
      if (n_exo == 1) {
        this_exovar <- exodata
      } else {
        this_exovar <- exodata[, this_exoname]
      }
      # print(this_exoname)
      # print(this_exovar)
      
      one_exo_with_lags_list <- list_along(seq(0, exo_lag))
      # print("one_exo_with_lags_list")
      # print(one_exo_with_lags_list)
      for (exlag  in seq(0, exo_lag)) {
        # print(paste0("ex"))
        this_lag_exo <- lag.xts(this_exovar, k = exlag)
        one_exo_with_lags_list[[exlag + 1]] <- this_lag_exo
      }
      one_exo_with_lags <- reduce(one_exo_with_lags_list, ts.union)
      if (!is.null(dim(one_exo_with_lags))) {
        this_exolags_names <- paste(this_exoname, seq(0, exo_lag), sep = ".l")
        this_exolags_names[1] <- this_exoname
        colnames(one_exo_with_lags) <- this_exolags_names
      } else {
        this_exolags_names <- paste(this_exoname, seq(0, exo_lag), sep = ".l")
        this_exolags_names[1] <- this_exoname
        names(one_exo_with_lags) <- this_exolags_names
      }
      exo_and_lags_list[[ex]] <- one_exo_with_lags
      names_exo_and_lags_list[[ex]] <- this_exolags_names
    }
    exo_and_lags <- reduce(exo_and_lags_list, ts.union)
    names_exo_and_lags <- reduce(names_exo_and_lags_list, c)
    if(is.null(dim(exo_and_lags))){
      names(exo_and_lags) <- names_exo_and_lags
    } else {
      colnames(exo_and_lags) <- names_exo_and_lags
    }
  }
  
  
  
  return(exo_and_lags)
  
}


max_effective_lag <- function(var_obj) {
  
  vres <- var_obj$restrictions
  
  if (is.null(vres)) {
    # print("VAR does nor have restriction matrix")
    nominal_lag <- var_obj$p
    return(nominal_lag)
  }
  
  
  csum <- colSums(vres[,1:ncol(vres)])
  names_unrest <- names(csum[csum > 0])
  names_unrest_num <-  as.numeric(map_chr(str_extract_all(names_unrest, "\\d"),
                                          ~ paste(.x, collapse = "")))
  max_lag_unrest <- max(names_unrest_num, na.rm = TRUE)
  return(max_lag_unrest)
}

read_compare_var_res <- function(var_res_new, var_res_old, h_max = 8, 
                                 rank_h_max = 30) {
  
  var_res_new <- var_res_new %>% 
    mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
           model_function = "new") %>% 
    dplyr::select(-lag_sel_method) 
  
  var_res_old$t_treshold <- 0 
  var_res_old <- var_res_old %>% 
    mutate(short_name = map2(variables, lags, ~make_model_name(.x, .y, remove_base = FALSE)),
           model_function = "old",
           var_size = map_dbl(variables, length)) 
  
  old_and_new <- stack_models(list(var_res_new, var_res_old))  
  
  plot_best_consolidated <- single_plot_rmse_all_h(old_and_new, is_wide = TRUE, 
                                                   h_max = h_max, rank_h_max = rank_h_max)
  
  plot_best_each <- each_plot_rmse_all_h(selected_one = var_res_new,
                                         selected_two = var_res_old,
                                         is_wide = TRUE, 
                                         h_max = h_max,
                                         rank_h_max = rank_h_max)
  
  size4_vbls_new <-  var_res_new %>% 
    filter(var_size == 4) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique() %>% sort()
  
  size4_vbls_old <-  var_res_old %>% 
    filter(var_size == 4) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique() %>% sort()
  
  size5_vbls_new <-  var_res_new %>% 
    filter(var_size == 5) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique() %>% sort()
  
  size5_vbls_old <-  var_res_old %>% 
    filter(var_size == 5) %>% 
    dplyr::select(variables) %>% 
    unlist() %>% 
    unique() %>% sort()
  
  return(list(size4_vbls_new = size4_vbls_new, size4_vbls_old = size4_vbls_old,
              size5_vbls_new = size5_vbls_new, size5_vbls_old = size5_vbls_old,
              var_res_old_and_new = old_and_new, var_res_new = var_res_new,
              var_res_old = var_res_old, 
              plot_best_consolidated  = plot_best_consolidated,
              plot_best_each = plot_best_each))
  
} 


# search var one size formerly known as try_sizes_vbls_lags
# then it was modified to work on single size choice
search_var_one_size_old <- function(var_data,
                                rgdp_yoy_ts,
                                rgdp_level_ts,
                                target_v, 
                                var_size, 
                                vec_lags = c(1,2,3,4),
                                names_exogenous = c(""),
                                exo_lag = NULL,
                                pre_selected_v = "",
                                is_cv = FALSE, 
                                h_max = 5, 
                                n_cv = 8,
                                training_length = 24,
                                return_cv = TRUE,
                                max_rank = 30,
                                rgdp_current_form = "yoy",
                                check_residuals_full_sample = TRUE,
                                check_residuals_cv = TRUE,
                                white_noise_target_ratio = 1,
                                keep_only_white_noise_fs = TRUE,
                                max_p_for_estimation = 7,
                                restrict_by_signif = TRUE,
                                t_tresh = 1.65,
                                keep_varest = FALSE,
                                add_info_based_lags = TRUE) {
  
  print("names_exogenous")
  print(names_exogenous)

  all_names <- colnames(var_data)
  models_with_cv_excercises <- 0
  models_with_eqn_dropping <- 0
  binding_max_p <- 0
  
  
  if (!restrict_by_signif) {
    t_tresh <- NA
  }
  
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags
  
  model_number <- 0
  models_unstable <- 0
  models_non_white_fs <- 0
  
  n_pre_selected_v <- length(pre_selected_v[pre_selected_v != ""])
  
  already_chosen <- c(target_v, pre_selected_v)
  already_chosen <- already_chosen[already_chosen != ""]
  len_already_chosen <- length(already_chosen)
  len_other_vbls <- var_size - len_already_chosen
  
  sets_of_other_variables <- get_sets_of_variables(
    df = var_data, this_size = var_size, all_variables = all_names, 
    already_chosen = already_chosen)
  
  len_sets_of_vars <- ncol(sets_of_other_variables)
  
  var_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
  
  combinations_of_variables_considered <- list_along(seq.int(1, len_sets_of_vars))
  
  messa1 <- paste0("This search: VARs with ", target_v, " as target variable, ",
                   n_pre_selected_v, " variables as pre-chosen variables and ",
                   len_other_vbls, " other free variables chosen among the ",
                   ncol(var_data) - len_already_chosen, " available variables.")
  
  
  messa2 <- paste0("That amounts to ", len_sets_of_vars, " different combinations of 
                   variables, each of them paired with ", length(vec_lags), 
                   " choices of max. lag to form " ,
                   len_sets_of_vars*length(vec_lags), " concrete unrestricted VARs.")
  
  messa3 <- paste0("Furthermore each unrestricted VAR will produce ", length(t_tresh), 
                   " more restricted version(s) to be evaluated alonside the unrestricted one.")
  
  print("")
  print(messa1)
  print(messa2)
  print(messa3)
  
  if (n_pre_selected_v > 0) {
    print("Prechosen variables (other than the target variables) for this search:")
    print(pre_selected_v)
    
    print("it should match already_chosen vector:")
    print("already_chosen" )
    print(already_chosen )
    
  }
  
  one_endog_count <- 0
  
  for (j in seq.int(1, len_sets_of_vars)) {

    vec_of_other_vbls <- sets_of_other_variables[,j]

    vbls_for_var <- c(already_chosen, vec_of_other_vbls)
    combinations_of_variables_considered[[j]] <- vbls_for_var

    sub_data = var_data[, vbls_for_var]

    sub_data = na.omit(sub_data)

    sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)

    endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
    exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 

    if (length(endov) == 1) {
      # print("Only one endogenous variable. Skip to next selection")
      one_endog_count <- one_endog_count + 1
      next
    }
    endodata <- sub_data[ , endov]
    exodata <- sub_data[ , exov]
    
    if (is.null(dim(endodata))) {
      names(endodata) <- endov
    } else {
      colnames(endodata) <- endov
    }
    
    if (is.character(vec_lags)) {
      lag_sel_method <- "info"
      info_lag_max <- 8

      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
      
      sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max,
                             exogen = exo_and_lags)
      sel_criteria <- sel$selection
      cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                     fpe = "FPE(n)")
      this_cri <- cri_names[vec_lags]
      named_lags <- sel_criteria[this_cri]
      p_for_estimation <- unique(unname(named_lags))
      max_found_p <- max(p_for_estimation)
      too_high_p <- p_for_estimation > max_p_for_estimation
      p_for_estimation[too_high_p] <- max_p_for_estimation 

      if (any(too_high_p)) {
        binding_max_p <- binding_max_p + 1
      }
    }
    
    if (is.numeric(vec_lags)) {
      lag_sel_method <- "manual"
      p_for_estimation <- unique(vec_lags)
      
      if (add_info_based_lags) {
        lag_sel_method <- "manual_and_info"
        info_lag_max <- 8
        
        exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
        
        sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                               exogen = exo_and_lags)
        sel_criteria <- sel$selection
        print(sel_criteria)
        cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                       fpe = "FPE(n)")
        this_cri <- cri_names
        named_lags <- sel_criteria[this_cri]
        info_based_p_for_estimation <- unique(unname(named_lags))
        
        too_high_p <- info_based_p_for_estimation > max_p_for_estimation
        
        info_based_p_for_estimation[too_high_p] <- max_p_for_estimation
        
        p_for_estimation <- unique(c(p_for_estimation, 
                                     info_based_p_for_estimation)
        )
      }
      
    }
    
    
    len_lag <- length(p_for_estimation)
    var_fixed_vset_all_lags <- list_along(seq(len_lag))
    fcs_fixed_vset_all_lags <- list_along(seq(len_lag))
    
    for (k in seq.int(1, len_lag)) {
      this_cv <- list()
      this_lag <- p_for_estimation[k]
      
      if (is.null(exo_lag)) {
        exo_lag <- this_lag 
      }
      
      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
      
      full_sample_var <- vars::VAR(y = endodata, type = "const", p = this_lag, 
                                   exogen = exo_and_lags)

      model_number <- model_number + 1
      
      if (restrict_by_signif) {
        full_sample_var <- try(vars::restrict(full_sample_var, method = "ser", 
                                              thresh = t_tresh), silent = TRUE)
      }
      
      if (class(full_sample_var) == "try-error") {
        # print(paste("One or more equations in", paste(colnames(sub_data), collapse = " "),  
        #             ",have no coefficients passing t-treshold =", t_tresh))
        some_eqn_drop <- TRUE
        models_with_eqn_dropping <- models_with_eqn_dropping + 1
        is_stable <- FALSE
        is_white_noise_fs <- FALSE
        this_cv[["t_treshold"]] <-  t_tresh
        this_cv[["lag_sel_method"]] <- lag_sel_method
        if (keep_varest) {
          this_cv[["full_sample_varest"]] <- full_sample_var
        }
        
      } 
      
      if (!class(full_sample_var) == "try-error") {
        var_restrictions <- full_sample_var$restrictions
        some_eqn_drop <- FALSE
        
        this_root <- try(vars::roots(full_sample_var))
        
        if (class(this_root) == "try-error") {
          print("error computing roots. Possible NAs or Inf in x")
          print(paste0("current variables: "))
          print(colnames(sub_data))
          print(paste0("current max lag: ", this_lag))
          is_stable <- FALSE
        } else {
          is_stable <- all(this_root < 1)
        }
        
        if (!is_stable) {
          # print("Current VAR not stable. No CV analysis will be done")
          # print(paste("Roots are", paste(this_root, collapse = ", ")))
          models_unstable <- models_unstable + 1 
          # print(paste("Unstable models so far:", models_unstable))
        }
        if (is_stable & check_residuals_full_sample) {
          is_white_noise_fs <- check_resid_VAR(full_sample_var)
          # print("is_white_noise_fs")
          # print(is_white_noise_fs)
          if (!is_white_noise_fs) {
            # print("foo")
            models_non_white_fs <- models_non_white_fs + 1
          }
        } else {
          is_white_noise_fs <- TRUE
        }
        
        if (is_white_noise_fs & is_stable) {
          models_with_cv_excercises <- models_with_cv_excercises + 1
          # print("paso!")
          # print(models_with_cv_excercises)
          # print("var_restrictions")
          # print(var_restrictions)
          
          if (is.null(exo_lag)) {
            exo_lag <- this_lag 
          }
          
          # print("Sub_data before var_cv")
          # print(sub_data)
          
          this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                            external_idx = sub_data_tk_index, this_p = this_lag,
                            this_type = "const", h_max = h_max,
                            n_cv = n_cv, training_length = training_length, 
                            test_residuals = check_residuals_cv,
                            full_sample_resmat = var_restrictions, 
                            names_exogenous = names_exogenous, exo_lag = exo_lag)
          cv_num_of_white_noises <- sum(this_cv[["cv_is_white_noise"]])
          ratio_of_white_noises <- cv_num_of_white_noises/n_cv
          overall_cv_white_noise <- ratio_of_white_noises >= white_noise_target_ratio
          this_cv[["overall_cv_white_noise"]] <- overall_cv_white_noise
          this_cv[["is_white_noise_fse"]] <- TRUE
          this_cv[["is_stable"]] <- TRUE
          this_cv[["t_treshold"]] <- t_tresh
          this_cv[["lag_sel_method"]] <- lag_sel_method
          if (keep_varest) {
            this_cv[["full_sample_varest"]] <- full_sample_var
          }
          
          if (keep_varest) {
            this_cv[["full_sample_varest"]] <- full_sample_var
          }
          
          
        }
      }
      
      if ( (!is_white_noise_fs) | (!is_stable) | some_eqn_drop) {
        
        this_cv <- list(cv_errors = list(NULL),
                        cv_test_data = list(NULL),
                        cv_fcs = list(NULL),
                        mean_cv_rmse = list(NULL),
                        cv_vbl_names = list(colnames(sub_data)),
                        cv_lag = list(this_lag),
                        cv_is_white_noise = list(NULL))
        
        this_cv[["overall_cv_white_noise"]] <- list(NULL)
        this_cv[["is_white_noise_fse"]] <- list(FALSE)
        this_cv[["is_stable"]] <- is_stable
        this_cv[["t_treshold"]] <- t_tresh
        this_cv[["lag_sel_method"]] <- lag_sel_method
        if (keep_varest) {
          this_cv[["full_sample_varest"]] <- full_sample_var
        }
        
      }
      
      this_cv[["some_eqn_drop"]] <- some_eqn_drop

      var_fixed_vset_all_lags[[k]] <- this_cv
    }
    
    var_all_vset_all_lags[[j]] <- var_fixed_vset_all_lags
  }
  
  # print("combinations_of_variables_considered")
  # print(combinations_of_variables_considered)
  
  results_all_models <- flatten(var_all_vset_all_lags)

  results_all_models <- discard(results_all_models, 
                                ~ is.null(.x[["cv_test_data"]][[1]]))
  
  if (length(results_all_models) == 0) {
    print("No model passed all tests")
    
    print(paste("Number of models analyzed:", model_number))
    print(paste("Total models dropped after significance restrictions applied:", 
                models_with_eqn_dropping, "out of", model_number))
    print(paste("Total significant models unstable:", 
                models_unstable, "out of", model_number - models_with_eqn_dropping))
    print(paste("Total significant stable models, but with non-white residuals:", 
                models_non_white_fs, "out of", model_number -
                  models_with_eqn_dropping - models_unstable ))
    
    return(list(accu_rankings_models = list(),
                cv_objects = list(),
                combinations_of_variables_considered = combinations_of_variables_considered
    )
    )
  }
  
  column_names <- names(results_all_models[[1]])
  
  # transitory names to allow conversion to tibble (columns must be names)
  names(results_all_models) <- seq_along(results_all_models)
  
  # transpose tibble, ensure result is still a tibble
  results_all_models <- as_tibble(t(as_tibble(results_all_models)))
  
  
  names(results_all_models) <- column_names
  
  if (rgdp_current_form != "yoy") {
    if (rgdp_current_form == "diff_yoy") {
      
      auxiliary_ts <-  rgdp_yoy_ts
      
      results_all_models <- results_all_models %>% 
        rename(cv_test_data_diff_yoy = cv_test_data,
               cv_fcs_diff_yoy = cv_fcs)
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff_yoy, ~ transform_cv(list_series  = ., 
                                                series_name = "cv_test_data",
                                                current_form = rgdp_current_form,
                                                auxiliary_ts = auxiliary_ts,
                                                n_cv = n_cv) ),
          cv_fcs = map(
            cv_fcs_diff_yoy,  ~ transform_cv(list_series  = .,
                                             series_name = "cv_fcs",
                                             current_form = rgdp_current_form,
                                             auxiliary_ts = auxiliary_ts,
                                             n_cv = n_cv) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
    }
    ##### ESTA PARTE HAY QUE CAMBIAR: DIFF
    if (rgdp_current_form == "diff") {
      auxiliary_ts <-  rgdp_level_ts
      
      results_all_models <- results_all_models %>% 
        rename(cv_test_data_diff = cv_test_data,
               cv_fcs_diff = cv_fcs)
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff, ~ transform_cv(list_series  = ., 
                                            series_name = "cv_test_data",
                                            current_form = rgdp_current_form,
                                            auxiliary_ts = auxiliary_ts,
                                            n_cv = n_cv) ),
          cv_fcs = map(
            cv_fcs_diff,  ~ transform_cv(list_series  = .,
                                         series_name = "cv_fcs",
                                         current_form = rgdp_current_form,
                                         auxiliary_ts = auxiliary_ts,
                                         n_cv = n_cv) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
      
    }
    
  }
  
  if (keep_only_white_noise_fs){
    
    results_all_models <- results_all_models %>% 
      filter(unlist(is_white_noise_fse))
    
  }
  
  print(paste("Number of models analyzed:", model_number))
  print(paste("Total models dropped after significance restrictions applied:", 
              models_with_eqn_dropping, "out of", model_number))
  print(paste("Total significant models unstable:", 
              models_unstable, "out of", model_number - models_with_eqn_dropping))
  print(paste("Total significant stable models, but with non-white residuals:", 
              models_non_white_fs, "out of", model_number -
                models_with_eqn_dropping - models_unstable ))
  print(paste("As a result,  performed CV on", models_with_cv_excercises, "of them"))
  print(paste("CV repetitions:", n_cv))
  print(paste("Total estimations (full sample + cv rounds):", 
              n_cv*models_with_cv_excercises + model_number))
  print(paste("Total times p exceeded max_p_for_e:", binding_max_p))
  
  
  if (nrow(results_all_models) > 0) {
    
    results_all_models <- get_rmses_h_rankings_h(data = results_all_models,
                                                 h_max = h_max)
    
    results_all_models <- results_all_models %>% 
      filter_at( vars(starts_with("rank")), any_vars(. <= max_rank)) %>% 
      mutate(cv_vbl_names = map(cv_vbl_names, 1),
             cv_lag = map(cv_lag, 1))
    
    
    cv_objects <- results_all_models %>% 
      dplyr::select(cv_vbl_names, cv_lag, 
                    cv_errors, cv_test_data, cv_fcs) %>% 
      rename(variables = cv_vbl_names, lags = cv_lag)
    
    
    if (keep_varest) {
      accu_rankings_models <- results_all_models %>% 
        dplyr::select(cv_vbl_names, cv_lag, lag_sel_method, t_treshold,
                      starts_with("rmse"), starts_with("rank"), 
                      overall_cv_white_noise, is_white_noise_fse,
                      full_sample_varest) %>% 
        rename(variables = cv_vbl_names, lags = cv_lag, 
               wn_cv = overall_cv_white_noise, wn_fs = is_white_noise_fse)
    } else {
      accu_rankings_models <- results_all_models %>% 
        dplyr::select(cv_vbl_names, cv_lag, lag_sel_method, t_treshold,
                      starts_with("rmse"), starts_with("rank"), 
                      overall_cv_white_noise, is_white_noise_fse) %>% 
        rename(variables = cv_vbl_names, lags = cv_lag, 
               wn_cv = overall_cv_white_noise, wn_fs = is_white_noise_fse)
    }
    
    accu_rankings_models <- accu_rankings_models %>% 
      mutate(short_name = map2(variables, lags,
                               ~ make_model_name(variables = .x, lags = .y))
      )
    
    accu_rankings_models <- accu_rankings_models %>% 
      dplyr::select(short_name, everything())
  }
  
  accu_rankings_models <- as_tibble(accu_rankings_models)
  accu_rankings_models$model_function <- "VAR"
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects,
                combinations_of_variables_considered = combinations_of_variables_considered))
  } else {
    return(list(accu_rankings_models = accu_rankings_models,
                combinations_of_variables_considered = combinations_of_variables_considered))
  }
}



#' Title provides a set of maximum lag values
#'
#' @param vec_lags 
#' @param max_p_for_estimation 
#' @param add_info_based_lags 
#' @param endodata 
#' @param exodata 
#' @param exov 
#' @param discard_negative 
#'
#' @return
#' @export
#'
#' @examples
lags_for_var_old <- function(vec_lags,
                         max_p_for_estimation,
                         add_info_based_lags = FALSE,
                         endodata,
                         exodata = NULL,
                         exov = NULL,
                         discard_negative = FALSE, 
                         ret_info_results = FALSE) {
  
  info_lag_max <- max_p_for_estimation
  
  if (is.character(vec_lags)) {
    lag_sel_method <- "info"
    info_lag_max <-  max_p_for_estimation
    
    exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)

    sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max,
                           exogen = exo_and_lags)

    sel_criteria <- sel$selection

    cleaned_criteria <- t(sel$criteria)
    cleaned_criteria <- cleaned_criteria[is.finite(cleaned_criteria[,2]), ]
    
    if (nrow(cleaned_criteria) < nrow(t(sel$criteria))) {
      print("Caution: NaNs or -Inf values in some of the info criteria")
    }

    info_based_p_for_estimation <- c(which.min(cleaned_criteria[, 1]), which.min(cleaned_criteria[, 2]),
                 which.min(cleaned_criteria[, 3]), which.min(cleaned_criteria[, 4]))
    names(info_based_p_for_estimation) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")

    p_for_estimation <- unique(info_based_p_for_estimation)
    max_found_p <- max(p_for_estimation)
    too_high_p <- p_for_estimation > max_p_for_estimation
    p_for_estimation[too_high_p] <- max_p_for_estimation 
  }
  
  if (is.numeric(vec_lags)) {
    lag_sel_method <- "manual"
    p_for_estimation <- unique(vec_lags)
    
    if (add_info_based_lags) {
      lag_sel_method <- "manual_and_info"
      info_lag_max <- max_p_for_estimation
      
      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
      
      sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                             exogen = exo_and_lags)
      sel_criteria <- sel$selection
      cleaned_criteria <- t(sel$criteria)
      cleaned_criteria <- cleaned_criteria[is.finite(cleaned_criteria[,2]), ]
      
      if (nrow(cleaned_criteria) < nrow(t(sel$criteria))) {
        print("Caution: NaNs or -Inf values in some of the info criteria")
      }
      
      info_based_p_for_estimation <- c(which.min(cleaned_criteria[, 1]), which.min(cleaned_criteria[, 2]),
                   which.min(cleaned_criteria[, 3]), which.min(cleaned_criteria[, 4]))
      names(info_based_p_for_estimation) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")

      too_high_p <- info_based_p_for_estimation > max_p_for_estimation
      
      info_based_p_for_estimation[too_high_p] <- max_p_for_estimation
      
      p_for_estimation <- unique(c(p_for_estimation, 
                                   info_based_p_for_estimation)
      )
    }
    
  }
  
  if (ret_info_results) {
    return(list(p_for_estimation = p_for_estimation, 
                info_criteria = info_based_p_for_estimation))
  } else {
    return(p_for_estimation)
  }
  
  
}




#' Title provides a set of maximum lag values
#'
#' @param vec_lags 
#' @param max_p_for_estimation 
#' @param add_info_based_lags 
#' @param variables 
#' @param exonames
#' @param discard_negative 
#'
#' @return
#' @export
#'
#' @examples
lags_for_var <- function(var_data,
                         variables,
                         vec_lags,
                         max_p_for_estimation,
                         add_info_based_lags = FALSE,
                         exov = NULL,
                         discard_negative = FALSE, 
                         ret_info_results = FALSE,
                         silent = FALSE) {
  
  
  this_data <- var_data[, variables]
  this_data <- na.omit(this_data)
  
  endonames <- variables[!variables %in% exov]
  endodata <- this_data[, endonames]
  
  if (is.null(exov)) {
    exodata <- NULL
  } else {
    exodata <- this_data[, exov]
  }
  
  info_lag_max <- max_p_for_estimation
  
  if (is.character(vec_lags)) {
    lag_sel_method <- "info"
    info_lag_max <-  max_p_for_estimation
    
    exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
    
    if (silent) {
      suppressWarnings(
        sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                               exogen = exo_and_lags)
      )
    } else {
      sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                             exogen = exo_and_lags)
    }
    
    sel_criteria <- sel$selection
    
    cleaned_criteria <- t(sel$criteria)
    cleaned_criteria <- cleaned_criteria[is.finite(cleaned_criteria[,2]), ]
    
    if (!silent) {
      if (nrow(cleaned_criteria) < nrow(t(sel$criteria))) {
        print("Caution: NaNs or -Inf values in some of the info criteria")
      }
    }
    
    info_based_p_for_estimation <- c(which.min(cleaned_criteria[, 1]), which.min(cleaned_criteria[, 2]),
                                     which.min(cleaned_criteria[, 3]), which.min(cleaned_criteria[, 4]))
    names(info_based_p_for_estimation) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
    
    p_for_estimation <- unique(info_based_p_for_estimation)
    max_found_p <- max(p_for_estimation)
    too_high_p <- p_for_estimation > max_p_for_estimation
    p_for_estimation[too_high_p] <- max_p_for_estimation 
  }
  
  if (is.numeric(vec_lags)) {
    lag_sel_method <- "manual"
    p_for_estimation <- unique(vec_lags)
    
    if (add_info_based_lags) {
      lag_sel_method <- "manual_and_info"
      info_lag_max <- max_p_for_estimation
      
      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
      
      if (silent) {
        # print("being silent")
        suppressMessages(
          sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                                                 exogen = exo_and_lags)
          )
      } else {
        sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                               exogen = exo_and_lags)
      }
      
     
      sel_criteria <- sel$selection
      cleaned_criteria <- t(sel$criteria)
      cleaned_criteria <- cleaned_criteria[is.finite(cleaned_criteria[,2]), ]
      
      if (!silent) {
        if (nrow(cleaned_criteria) < nrow(t(sel$criteria))) {
          print("Caution: NaNs or -Inf values in some of the info criteria")
        }
      }
      
      
      info_based_p_for_estimation <- c(which.min(cleaned_criteria[, 1]), which.min(cleaned_criteria[, 2]),
                                       which.min(cleaned_criteria[, 3]), which.min(cleaned_criteria[, 4]))
      names(info_based_p_for_estimation) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
      
      too_high_p <- info_based_p_for_estimation > max_p_for_estimation
      
      info_based_p_for_estimation[too_high_p] <- max_p_for_estimation
      
      p_for_estimation <- unique(c(p_for_estimation, 
                                   info_based_p_for_estimation)
      )
    }
    
  }
  
  if (ret_info_results) {
    return(list(p_for_estimation = p_for_estimation, 
                info_criteria = info_based_p_for_estimation))
  } else {
    return(p_for_estimation)
  }
  
}



search_var_one_size <- function(var_data,
                                rgdp_yoy_ts,
                                rgdp_level_ts,
                                target_v, 
                                var_size, 
                                vec_lags = c(1,2,3,4),
                                names_exogenous = c(""),
                                exo_lag = NULL,
                                pre_selected_v = "",
                                is_cv = FALSE, 
                                h_max = 5, 
                                n_cv = 8,
                                training_length = 24,
                                return_cv = TRUE,
                                max_rank = 30,
                                rgdp_current_form = "yoy",
                                check_residuals_full_sample = TRUE,
                                check_residuals_cv = TRUE,
                                white_noise_target_ratio = 1,
                                keep_only_white_noise_fs = TRUE,
                                max_p_for_estimation = 7,
                                restrict_by_signif = TRUE,
                                t_tresh = 1.65,
                                keep_varest = FALSE,
                                add_info_based_lags = TRUE) {
  
  print("names_exogenous")
  print(names_exogenous)
  
  all_names <- colnames(var_data)
  models_with_cv_excercises <- 0
  models_with_eqn_dropping <- 0
  binding_max_p <- 0
  
  
  # if (!restrict_by_signif) {
  #   t_tresh <- NA
  # }
  
  ## j, loop through the combination of variables of a fixed size, e.g. all sets of 5 variables
  ### k, loop through values of lags
  
  model_number <- 0
  models_unstable <- 0
  models_non_white_fs <- 0
  
  n_pre_selected_v <- length(pre_selected_v[pre_selected_v != ""])
  
  already_chosen <- c(target_v, pre_selected_v)
  already_chosen <- already_chosen[already_chosen != ""]
  len_already_chosen <- length(already_chosen)
  len_other_vbls <- var_size - len_already_chosen
  
  sets_of_other_variables <- get_sets_of_variables(
    df = var_data, this_size = var_size, all_variables = all_names, 
    already_chosen = already_chosen)
  
  len_sets_of_vars <- ncol(sets_of_other_variables)
  
  var_all_vset_all_lags <- list_along(seq.int(1, len_sets_of_vars))
  
  combinations_of_variables_considered <- list_along(seq.int(1, len_sets_of_vars))
  
  messa1 <- paste0("This search: VARs with ", target_v, " as target variable, ",
                   n_pre_selected_v, " variables as pre-chosen variables and ",
                   len_other_vbls, " other free variables chosen among the ",
                   ncol(var_data) - len_already_chosen, " available variables.")
  
  
  messa2 <- paste0("That amounts to ", len_sets_of_vars, " different combinations of 
                   variables, each of them paired with ", length(vec_lags), 
                   " choices of max. lag to form " ,
                   len_sets_of_vars*length(vec_lags), " concrete unrestricted VARs.")
  
  messa3 <- paste0("Furthermore each unrestricted VAR will produce ", length(t_tresh), 
                   " more restricted version(s) to be evaluated alonside the unrestricted one.")
  
  print("")
  print(messa1)
  print(messa2)
  print(messa3)
  
  if (n_pre_selected_v > 0) {
    print("Prechosen variables (other than the target variables) for this search:")
    print(pre_selected_v)
    
    print("it should match already_chosen vector:")
    print("already_chosen" )
    print(already_chosen )
    
  }
  
  one_endog_count <- 0
  
  for (j in seq.int(1, len_sets_of_vars)) {
    
    vec_of_other_vbls <- sets_of_other_variables[,j]
    
    vbls_for_var <- c(already_chosen, vec_of_other_vbls)
    combinations_of_variables_considered[[j]] <- vbls_for_var
    
    sub_data = var_data[, vbls_for_var]
    
    sub_data = na.omit(sub_data)
    
    sub_data_tk_index <- tk_index(var_data, timetk_idx = TRUE)
    
    endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
    exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 
    
    if (length(endov) == 1) {
      # print("Only one endogenous variable. Skip to next selection")
      one_endog_count <- one_endog_count + 1
      next
    }
    endodata <- sub_data[ , endov]
    exodata <- sub_data[ , exov]
    
    if (is.null(dim(endodata))) {
      names(endodata) <- endov
    } else {
      colnames(endodata) <- endov
    }
    
    if (is.character(vec_lags)) {
      lag_sel_method <- "info"
      info_lag_max <- 8
      
      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
      
      sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max,
                             exogen = exo_and_lags)
      sel_criteria <- sel$selection
      cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                     fpe = "FPE(n)")
      this_cri <- cri_names[vec_lags]
      named_lags <- sel_criteria[this_cri]
      p_for_estimation <- unique(unname(named_lags))
      max_found_p <- max(p_for_estimation)
      too_high_p <- p_for_estimation > max_p_for_estimation
      p_for_estimation[too_high_p] <- max_p_for_estimation 
      
      if (any(too_high_p)) {
        binding_max_p <- binding_max_p + 1
      }
    }
    
    if (is.numeric(vec_lags)) {
      lag_sel_method <- "manual"
      p_for_estimation <- unique(vec_lags)
      
      if (add_info_based_lags) {
        lag_sel_method <- "manual_and_info"
        info_lag_max <- 8
        
        exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = info_lag_max)
        
        sel <- vars::VARselect(y = endodata, type = "const", lag.max = info_lag_max, 
                               exogen = exo_and_lags)
        sel_criteria <- sel$selection
        print(sel_criteria)
        cri_names <- c(aic = "AIC(n)", hq = "HQ(n)", sc = "SC(n)",
                       fpe = "FPE(n)")
        this_cri <- cri_names
        named_lags <- sel_criteria[this_cri]
        info_based_p_for_estimation <- unique(unname(named_lags))
        
        too_high_p <- info_based_p_for_estimation > max_p_for_estimation
        
        info_based_p_for_estimation[too_high_p] <- max_p_for_estimation
        
        p_for_estimation <- unique(c(p_for_estimation, 
                                     info_based_p_for_estimation)
        )
      }
      
    }
    
    
    len_lag <- length(p_for_estimation)
    var_fixed_vset_all_lags <- list_along(seq(len_lag))
    fcs_fixed_vset_all_lags <- list_along(seq(len_lag))
    
    for (k in seq.int(1, len_lag)) {
      this_cv <- list()
      this_lag <- p_for_estimation[k]
      
      if (is.null(exo_lag)) {
        exo_lag <- this_lag 
      }
      
      exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
      
      full_sample_var <- vars::VAR(y = endodata, type = "const", p = this_lag, 
                                   exogen = exo_and_lags)
      
      model_number <- model_number + 1
      
      if (restrict_by_signif) {
        full_sample_var <- try(vars::restrict(full_sample_var, method = "ser", 
                                              thresh = t_tresh), silent = TRUE)
      }
      
      if (class(full_sample_var) == "try-error") {
        # print(paste("One or more equations in", paste(colnames(sub_data), collapse = " "),  
        #             ",have no coefficients passing t-treshold =", t_tresh))
        some_eqn_drop <- TRUE
        models_with_eqn_dropping <- models_with_eqn_dropping + 1
        is_stable <- FALSE
        is_white_noise_fs <- FALSE
        this_cv[["t_treshold"]] <-  t_tresh
        this_cv[["lag_sel_method"]] <- lag_sel_method
        if (keep_varest) {
          this_cv[["full_sample_varest"]] <- full_sample_var
        }
        
      } 
      
      if (!class(full_sample_var) == "try-error") {
        var_restrictions <- full_sample_var$restrictions
        some_eqn_drop <- FALSE
        
        this_root <- try(vars::roots(full_sample_var))
        
        if (class(this_root) == "try-error") {
          print("error computing roots. Possible NAs or Inf in x")
          print(paste0("current variables: "))
          print(colnames(sub_data))
          print(paste0("current max lag: ", this_lag))
          is_stable <- FALSE
        } else {
          is_stable <- all(this_root < 1)
        }
        
        if (!is_stable) {
          # print("Current VAR not stable. No CV analysis will be done")
          # print(paste("Roots are", paste(this_root, collapse = ", ")))
          models_unstable <- models_unstable + 1 
          # print(paste("Unstable models so far:", models_unstable))
        }
        if (is_stable & check_residuals_full_sample) {
          is_white_noise_fs <- check_resid_VAR(full_sample_var)
          # print("is_white_noise_fs")
          # print(is_white_noise_fs)
          if (!is_white_noise_fs) {
            # print("foo")
            models_non_white_fs <- models_non_white_fs + 1
          }
        } else {
          is_white_noise_fs <- TRUE
        }
        
        if (is_white_noise_fs & is_stable) {
          models_with_cv_excercises <- models_with_cv_excercises + 1
          # print("paso!")
          # print(models_with_cv_excercises)
          # print("var_restrictions")
          # print(var_restrictions)
          
          if (is.null(exo_lag)) {
            exo_lag <- this_lag 
          }
          
          # print("Sub_data before var_cv")
          # print(sub_data)
          
          this_cv <- var_cv(var_data = sub_data, timetk_idx = FALSE,
                            external_idx = sub_data_tk_index, this_p = this_lag,
                            this_type = "const", h_max = h_max,
                            n_cv = n_cv, training_length = training_length, 
                            test_residuals = check_residuals_cv,
                            full_sample_resmat = var_restrictions, 
                            names_exogenous = names_exogenous, exo_lag = exo_lag)
          cv_num_of_white_noises <- sum(this_cv[["cv_is_white_noise"]])
          ratio_of_white_noises <- cv_num_of_white_noises/n_cv
          overall_cv_white_noise <- ratio_of_white_noises >= white_noise_target_ratio
          this_cv[["overall_cv_white_noise"]] <- overall_cv_white_noise
          this_cv[["is_white_noise_fse"]] <- TRUE
          this_cv[["is_stable"]] <- TRUE
          this_cv[["t_treshold"]] <- t_tresh
          this_cv[["lag_sel_method"]] <- lag_sel_method
          if (keep_varest) {
            this_cv[["full_sample_varest"]] <- full_sample_var
          }
          
          if (keep_varest) {
            this_cv[["full_sample_varest"]] <- full_sample_var
          }
          
          
        }
      }
      
      if ( (!is_white_noise_fs) | (!is_stable) | some_eqn_drop) {
        
        this_cv <- list(cv_errors = list(NULL),
                        cv_test_data = list(NULL),
                        cv_fcs = list(NULL),
                        mean_cv_rmse = list(NULL),
                        cv_vbl_names = list(colnames(sub_data)),
                        cv_lag = list(this_lag),
                        cv_is_white_noise = list(NULL))
        
        this_cv[["overall_cv_white_noise"]] <- list(NULL)
        this_cv[["is_white_noise_fse"]] <- list(FALSE)
        this_cv[["is_stable"]] <- is_stable
        this_cv[["t_treshold"]] <- t_tresh
        this_cv[["lag_sel_method"]] <- lag_sel_method
        if (keep_varest) {
          this_cv[["full_sample_varest"]] <- full_sample_var
        }
        
      }
      
      this_cv[["some_eqn_drop"]] <- some_eqn_drop
      
      var_fixed_vset_all_lags[[k]] <- this_cv
    }
    
    var_all_vset_all_lags[[j]] <- var_fixed_vset_all_lags
  }
  
  # print("combinations_of_variables_considered")
  # print(combinations_of_variables_considered)
  
  results_all_models <- flatten(var_all_vset_all_lags)
  
  results_all_models <- discard(results_all_models, 
                                ~ is.null(.x[["cv_test_data"]][[1]]))
  
  if (length(results_all_models) == 0) {
    print("No model passed all tests")
    
    print(paste("Number of models analyzed:", model_number))
    print(paste("Total models dropped after significance restrictions applied:", 
                models_with_eqn_dropping, "out of", model_number))
    print(paste("Total significant models unstable:", 
                models_unstable, "out of", model_number - models_with_eqn_dropping))
    print(paste("Total significant stable models, but with non-white residuals:", 
                models_non_white_fs, "out of", model_number -
                  models_with_eqn_dropping - models_unstable ))
    
    return(list(accu_rankings_models = list(),
                cv_objects = list(),
                combinations_of_variables_considered = combinations_of_variables_considered
    )
    )
  }
  
  column_names <- names(results_all_models[[1]])
  
  # transitory names to allow conversion to tibble (columns must be names)
  names(results_all_models) <- seq_along(results_all_models)
  
  # transpose tibble, ensure result is still a tibble
  results_all_models <- as_tibble(t(as_tibble(results_all_models)))
  
  
  names(results_all_models) <- column_names
  
  if (rgdp_current_form != "yoy") {
    if (rgdp_current_form == "diff_yoy") {
      
      auxiliary_ts <-  rgdp_yoy_ts
      
      results_all_models <- results_all_models %>% 
        rename(cv_test_data_diff_yoy = cv_test_data,
               cv_fcs_diff_yoy = cv_fcs)
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff_yoy, ~ transform_cv(list_series  = ., 
                                                series_name = "cv_test_data",
                                                current_form = rgdp_current_form,
                                                auxiliary_ts = auxiliary_ts,
                                                n_cv = n_cv) ),
          cv_fcs = map(
            cv_fcs_diff_yoy,  ~ transform_cv(list_series  = .,
                                             series_name = "cv_fcs",
                                             current_form = rgdp_current_form,
                                             auxiliary_ts = auxiliary_ts,
                                             n_cv = n_cv) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
    }
    ##### ESTA PARTE HAY QUE CAMBIAR: DIFF
    if (rgdp_current_form == "diff") {
      auxiliary_ts <-  rgdp_level_ts
      
      results_all_models <- results_all_models %>% 
        rename(cv_test_data_diff = cv_test_data,
               cv_fcs_diff = cv_fcs)
      
      results_all_models <- results_all_models %>% 
        mutate(cv_test_data = map(
          cv_test_data_diff, ~ transform_cv(list_series  = ., 
                                            series_name = "cv_test_data",
                                            current_form = rgdp_current_form,
                                            auxiliary_ts = auxiliary_ts,
                                            n_cv = n_cv) ),
          cv_fcs = map(
            cv_fcs_diff,  ~ transform_cv(list_series  = .,
                                         series_name = "cv_fcs",
                                         current_form = rgdp_current_form,
                                         auxiliary_ts = auxiliary_ts,
                                         n_cv = n_cv) ),
          cv_errors = map2(cv_test_data, cv_fcs, ~ map2(.x, .y, ~ .x - .y) )
        )
      
    }
    
  }
  
  if (keep_only_white_noise_fs){
    
    results_all_models <- results_all_models %>% 
      filter(unlist(is_white_noise_fse))
    
  }
  
  print(paste("Number of models analyzed:", model_number))
  print(paste("Total models dropped after significance restrictions applied:", 
              models_with_eqn_dropping, "out of", model_number))
  print(paste("Total significant models unstable:", 
              models_unstable, "out of", model_number - models_with_eqn_dropping))
  print(paste("Total significant stable models, but with non-white residuals:", 
              models_non_white_fs, "out of", model_number -
                models_with_eqn_dropping - models_unstable ))
  print(paste("As a result,  performed CV on", models_with_cv_excercises, "of them"))
  print(paste("CV repetitions:", n_cv))
  print(paste("Total estimations (full sample + cv rounds):", 
              n_cv*models_with_cv_excercises + model_number))
  print(paste("Total times p exceeded max_p_for_e:", binding_max_p))
  
  
  if (nrow(results_all_models) > 0) {
    
    results_all_models <- get_rmses_h_rankings_h(data = results_all_models,
                                                 h_max = h_max)
    
    results_all_models <- results_all_models %>% 
      filter_at( vars(starts_with("rank")), any_vars(. <= max_rank)) %>% 
      mutate(cv_vbl_names = map(cv_vbl_names, 1),
             cv_lag = map(cv_lag, 1))
    
    
    cv_objects <- results_all_models %>% 
      dplyr::select(cv_vbl_names, cv_lag, 
                    cv_errors, cv_test_data, cv_fcs) %>% 
      rename(variables = cv_vbl_names, lags = cv_lag)
    
    
    if (keep_varest) {
      accu_rankings_models <- results_all_models %>% 
        dplyr::select(cv_vbl_names, cv_lag, lag_sel_method, t_treshold,
                      starts_with("rmse"), starts_with("rank"), 
                      overall_cv_white_noise, is_white_noise_fse,
                      full_sample_varest) %>% 
        rename(variables = cv_vbl_names, lags = cv_lag, 
               wn_cv = overall_cv_white_noise, wn_fs = is_white_noise_fse)
    } else {
      accu_rankings_models <- results_all_models %>% 
        dplyr::select(cv_vbl_names, cv_lag, lag_sel_method, t_treshold,
                      starts_with("rmse"), starts_with("rank"), 
                      overall_cv_white_noise, is_white_noise_fse) %>% 
        rename(variables = cv_vbl_names, lags = cv_lag, 
               wn_cv = overall_cv_white_noise, wn_fs = is_white_noise_fse)
    }
    
    accu_rankings_models <- accu_rankings_models %>% 
      mutate(short_name = map2(variables, lags,
                               ~ make_model_name(variables = .x, lags = .y))
      )
    
    accu_rankings_models <- accu_rankings_models %>% 
      dplyr::select(short_name, everything())
  }
  
  accu_rankings_models <- as_tibble(accu_rankings_models)
  accu_rankings_models$model_function <- "VAR"
  
  if (return_cv) {
    return(list(accu_rankings_models = accu_rankings_models,
                cv_objects = cv_objects,
                combinations_of_variables_considered = combinations_of_variables_considered))
  } else {
    return(list(accu_rankings_models = accu_rankings_models,
                combinations_of_variables_considered = combinations_of_variables_considered))
  }
}



specs_to_rmse <- function(var_data, 
                          variables, 
                          lags,
                          h, 
                          n_cv, 
                          training_length, 
                          future_exo_cv,
                          target_transform, 
                          target_level_ts,
                          number_of_qrt_in_year_1 = 4,
                          t_thresholds = 0, 
                          do_tests = TRUE,
                          names_exogenous = c("")) {
  pass_tests <- TRUE
  
  this_freq <- frequency(var_data)
  
  
  # print(paste0("t_thresholds:", t_thresholds))
  # t_length <- length(t_thresholds)
  # t_0 <- t_thresholds == 0
  # print(paste0("t_length: ", t_length))
  # print(paste0("is t0: ",t_0))
  
  if (length(t_thresholds) == 1) {
    if (t_thresholds == 0) {
      is_unrestricted <- TRUE
    } else {
      is_unrestricted <- FALSE
    }
  } else {
    is_unrestricted <- FALSE
  }
  
  # print(paste0("is unrestricted?: ", is_unrestricted))
  
  # do the unrestricted even if it is restricted
  
  # print(var_data)
  # print(variables)
  # print(lags)
  # print(t_thresholds)
  # print(names_exogenous)
  
  fit <- try(fit_VAR_rest(var_data = var_data, variables = variables,
                          p = lags, t_thresh = t_thresholds,
                          names_exogenous = names_exogenous),
             silent = TRUE)
  
  # print(fit)
  
  if (is_unrestricted) {
    thresh_fit_tbl <- tibble(t_threshold = t_thresholds, fit = list(fit))
    # print("initial tibble for unrestricted")
    # print(thresh_fit_tbl)
    
  } else {
    thresh_fit_tbl <- fit
  }

  # print("thresh_fit_tbl")
  # print(thresh_fit_tbl)
  
  nfits <- nrow(thresh_fit_tbl)
  all_fits_list <- list_along(seq(1, nfits))
  
  for (f in seq(1, nfits)) {
    this_row <- thresh_fit_tbl[f, ]
    this_fit <- this_row[["fit"]][[1]]
    # print("this_row")
    # print(this_row)
    # print("this_thresh")
    
    this_thresh <- this_row[["t_threshold"]]
    
    # print(this_thresh)
    # print("this_fit")
    # print(this_fit)
    # print("fit_class")
    fit_class <- class(this_fit)[[1]]
    # print(fit_class)
    
    if (fit_class != "varest") {
      do_tests <- FALSE
      pass_tests <- FALSE
      tested <- FALSE
    }
    
    msg <- "ok"
    
    if (this_thresh > 0 & fit_class != "varest") {
      msg <- "restr_fail"
    }
    
    if (this_thresh == 0 & fit_class != "varest") {
      msg <- "unrestr_fail"
    }
    
    # print(paste0("antes de dotest, es unrestricted?: ", is_unrestricted))
    
    if (do_tests) {
      tested <- TRUE
      is_stable <-  try(all(vars::roots(this_fit) < 1))
      if(class(is_stable) == "try-error") {
        print("problem with var roots. Current variables are")
        print(variables)
        is_stable <- FALSE 
      }
      is_white_noise <-  check_resid_VAR(this_fit)
      pass_tests <- is_stable & is_white_noise
      # print("doing tests")
      # print(pass_tests)
    }
    
    if (tested) {
      if (!is_stable) {
        msg <- "unstable"
      } 
      
      if (is_stable & !is_white_noise) {
        msg <- "not_white_noise"
      }
    }
    
    if (!tested) {
      is_stable <- NA
      is_white_noise <- NA
      pass_tests <- NA
    }
    
    names_rmses <- paste0("rmse_", seq(1, h))
    rmse_yoy_all_h <- rep(NA, h)
    names(rmse_yoy_all_h) <- names_rmses
    
    
    number_of_years <- ceiling(h/this_freq)
    
    names_yearly_rmses <- paste0("rmse_yr_", seq(1, number_of_years))
    rmse_yoy_all_years <- rep(NA, number_of_years)
    names(rmse_yoy_all_years) <- names_yearly_rmses 
    
    
    if (is.na(pass_tests)) {
      do_cv <- fit_class == "varest"
    } else {
      do_cv <- pass_tests
    }
    
    
    # print(paste0("antes de do_cv, es unrestricted?: ", is_unrestricted))
    
    
    if (do_cv) {
      # print("this_fit")
      # print(this_fit)
      cv_obj <- cv_var_from_one_row(fit = this_fit, 
                                    var_data = var_data, 
                                    variables = variables, 
                                    lags = lags, 
                                    h = h, 
                                    n_cv = n_cv, 
                                    training_length = training_length, 
                                    names_exogenous = names_exogenous, 
                                    this_type = "const",
                                    this_thresh = t_thresholds, 
                                    future_exo_cv = future_exo_cv,
                                    number_of_qrt_in_year_1 = number_of_qrt_in_year_1)
      
      full_sample_resmat = cv_obj[["full_sample_resmat"]]
      
      # print("in specs to rmse:")
      # print("cv_obj")
      # print(cv_obj)
      
      if (target_transform != "yoy") {
        
        if (target_transform == "diff_yoy") {
          cv_obj_diff_yoy <-  cv_obj
          
          cv_obj_yoy = transform_all_cv(cv_obj_diff_yoy,
                                        current_form = target_transform,
                                        target_level_ts =  target_level_ts,
                                        n_cv = n_cv)
        }
        
        if (target_transform == "diff") {
          auxiliary_ts <-  target_level_ts
          
          cv_obj_diff <-  cv_obj
          
          cv_obj_yoy <- transform_all_cv(cv_object  = cv_obj_diff,
                                        current_form = target_transformation,
                                        target_level_ts = target_level_ts,
                                        n_cv = n_cv)
        }
        
      }
      
      if (target_transform == "yoy") {
        # print("Already in yoy form, but will compute yearly averages")
        cv_obj_yoy <- transform_all_cv(cv_object  = cv_obj,
                                       current_form = target_transformation,
                                       target_level_ts = target_level_ts,
                                       n_cv = n_cv)
      }
      
      # 
      # print("cv_obj_yoy")
      # print(cv_obj_yoy)
      
      
      rmse_yoy_all_h <-  all_rmse_from_cv_obj(cv_obj_yoy)
      names(rmse_yoy_all_h) <- names_rmses
      
      
      rmse_yoy_all_years <-  all_yearly_rmse_from_cv_obj(cv_obj_yoy)
      names(rmse_yoy_all_years) <- names_yearly_rmses
    }
    
    
    # print(paste0("despues de do_cv, es unrestricted?: ", is_unrestricted))
    # print("voy a poner en ttr estas variables y este threshold")
    # print(variables)
    # print(this_thresh)
    
    tibble_to_return <- tibble(msg = msg, tested = tested, pass_tests = pass_tests,
                               t_threshold = this_thresh, variables = list(variables))

    
    tibble_to_return <- as_tibble(c(tibble_to_return, rmse_yoy_all_h))
    tibble_to_return <- as_tibble(c(tibble_to_return, rmse_yoy_all_years))
    
    # print("tibble_to_return")
    # print(tibble_to_return)
    
    # if(! "variables" %in% names(tibble_to_return)) {
    #   tibble_to_return <- mutate(tibble_to_return,
    #                              variables = list(variables),
    #                              t_threshold = this_thresh)
    # }
    
    # if(! is_unrestricted) {
    #   # print("is restricted")
    #   tibble_to_return <- mutate(tibble_to_return,
    #                              variables = list(variables),
    #                              t_threshold = this_thresh)
    # }
    
    # print("intermediate tibble_to_return")
    # print(tibble_to_return)
    
    
    # print("tibble_to_return despues de in")
    # print(tibble_to_return)
    
    all_fits_list[[f]] <- tibble_to_return
    
    
    # print(paste0("al final de una vuelta de loop, es unrestricted?: ", is_unrestricted))
    
    
  }
  
  
  # print("final tibble_to_return")
  tibble_to_return <- reduce(all_fits_list, rbind)
  
  # print(tibble_to_return)
  
  return(tibble_to_return)
  
}




specs_to_rmse_old <- function(var_data, 
                          variables, 
                          lags,
                          h, 
                          n_cv, 
                          training_length, 
                          future_exo_cv,
                          target_transform, 
                          target_level_ts,
                          t_thresholds = 0, 
                          do_tests = TRUE,
                          names_exogenous = c("")) {
  pass_tests <- TRUE
  this_freq <- frequency(var_data)
  # print(paste0("t_thresholds:", t_thresholds))
  # t_length <- length(t_thresholds)
  # t_0 <- t_thresholds == 0
  # print(paste0("t_length: ", t_length))
  # print(paste0("is t0: ",t_0))
  
  if (length(t_thresholds) == 1) {
    if (t_thresholds == 0) {
      is_unrestricted <- TRUE
    } else {
      is_unrestricted <- FALSE
    }
  } else {
    is_unrestricted <- FALSE
  }
  
  # print(paste0("is unrestricted?: ", is_unrestricted))
  
  # do the unrestricted even if it is restricted
  
  # print(var_data)
  # print(variables)
  # print(lags)
  # print(t_thresholds)
  # print(names_exogenous)
  
  fit <- try(fit_VAR_rest(var_data = var_data, variables = variables,
                          p = lags, t_thresh = t_thresholds,
                          names_exogenous = names_exogenous),
             silent = TRUE)
  
  # print(fit)
  
  if (is_unrestricted) {
    thresh_fit_tbl <- tibble(t_threshold = t_thresholds, fit = list(fit))
    # print("initial tibble for unrestricted")
    # print(thresh_fit_tbl)
    
  } else {
    thresh_fit_tbl <- fit
  }
  
  # print("thresh_fit_tbl")
  # print(thresh_fit_tbl)
  
  nfits <- nrow(thresh_fit_tbl)
  all_fits_list <- list_along(seq(1, nfits))
  
  for (f in seq(1, nfits)) {
    this_row <- thresh_fit_tbl[f, ]
    this_fit <- this_row[["fit"]][[1]]
    # print("this_row")
    # print(this_row)
    # print("this_thresh")
    
    this_thresh <- this_row[["t_threshold"]]
    
    # print(this_thresh)
    # print("this_fit")
    # print(this_fit)
    # print("fit_class")
    fit_class <- class(this_fit)[[1]]
    # print(fit_class)
    
    if (fit_class != "varest") {
      do_tests <- FALSE
      pass_tests <- FALSE
      tested <- FALSE
    }
    
    msg <- "ok"
    
    if (this_thresh > 0 & fit_class != "varest") {
      msg <- "restr_fail"
    }
    
    if (this_thresh == 0 & fit_class != "varest") {
      msg <- "unrestr_fail"
    }
    
    # print(paste0("antes de dotest, es unrestricted?: ", is_unrestricted))
    
    if (do_tests) {
      tested <- TRUE
      is_stable <-  try(all(vars::roots(this_fit) < 1))
      if(class(is_stable) == "try-error") {
        print("problem with var roots. Current variables are")
        print(variables)
        is_stable <- FALSE 
      }
      is_white_noise <-  check_resid_VAR(this_fit)
      pass_tests <- is_stable & is_white_noise
      # print("doing tests")
      # print(pass_tests)
    }
    
    if (tested) {
      if (!is_stable) {
        msg <- "unstable"
      } 
      
      if (is_stable & !is_white_noise) {
        msg <- "not_white_noise"
      }
    }
    
    if (!tested) {
      is_stable <- NA
      is_white_noise <- NA
      pass_tests <- NA
    }
    
    names_rmses <- paste0("rmse_", seq(1, h))
    rmse_yoy_all_h <- rep(NA, h)
    names(rmse_yoy_all_h) <- names_rmses
    
    
    number_of_years <- ceiling(h/this_freq)
    
    names_yearly_rmses <- paste0("rmse_yr_", seq(1, number_of_years))
    rmse_yoy_all_years <- rep(NA, number_of_years)
    names(rmse_yoy_all_years) <- names_yearly_rmses 
    
    if (is.na(pass_tests)) {
      do_cv <- fit_class == "varest"
    } else {
      do_cv <- pass_tests
    }
    
    
    # print(paste0("antes de do_cv, es unrestricted?: ", is_unrestricted))
    
    
    if (do_cv) {
      # print("this_fit")
      # print(this_fit)
      cv_obj <- cv_var_from_one_row(fit = this_fit, 
                                    var_data = var_data, 
                                    variables = variables, 
                                    lags = lags, 
                                    h = h, 
                                    n_cv = n_cv, 
                                    training_length = training_length, 
                                    names_exogenous = names_exogenous, 
                                    this_type = "const",
                                    this_thresh = t_thresholds, 
                                    future_exo_cv = future_exo_cv)
      
      full_sample_resmat = cv_obj[["full_sample_resmat"]]
      
      if (target_transform != "yoy") {
        
        if (target_transform == "diff_yoy") {
          cv_obj_diff_yoy <-  cv_obj
          
          cv_obj_yoy = transform_all_cv(cv_obj_diff_yoy,
                                        current_form = target_transform,
                                        target_level_ts =  target_level_ts,
                                        n_cv = n_cv)
        }
        
        if (target_transform == "diff") {
          auxiliary_ts <-  target_level_ts
          
          cv_obj_diff = cv_obj
          
          cv_obj_yoy = transform_all_cv(cv_object  = cv_obj_diff,
                                        current_form = target_transformation,
                                        target_level_ts = target_level_ts,
                                        n_cv = n_cv)
        }
        
      }
      
      if (target_transform == "yoy") {
        # print("Already in yoy form")
        cv_obj_yoy <- cv_obj
      }
      
      rmse_yoy_all_h <-  all_rmse_from_cv_obj(cv_obj_yoy)
      names(rmse_yoy_all_h) <- names_rmses
      
      rmse_yoy_all_years <-  all_yearly_rmse_from_cv_obj(cv_obj_yoy)
      names(rmse_yoy_all_years) <- names_yearly_rmses
    }
    
    
    # print(paste0("despues de do_cv, es unrestricted?: ", is_unrestricted))
    # print("voy a poner en ttr estas variables y este threshold")
    # print(variables)
    # print(this_thresh)
    
    tibble_to_return <- tibble(msg = msg, tested = tested, pass_tests = pass_tests,
                               t_threshold = this_thresh, variables = list(variables))
    
    
    
    tibble_to_return <- as_tibble(c(tibble_to_return, rmse_yoy_all_h))
    tibble_to_return <- as_tibble(c(tibble_to_return, rmse_yoy_all_years))
    
    # print("tibble_to_return")
    # print(tibble_to_return)
    
    # if(! "variables" %in% names(tibble_to_return)) {
    #   tibble_to_return <- mutate(tibble_to_return,
    #                              variables = list(variables),
    #                              t_threshold = this_thresh)
    # }
    
    # if(! is_unrestricted) {
    #   # print("is restricted")
    #   tibble_to_return <- mutate(tibble_to_return,
    #                              variables = list(variables),
    #                              t_threshold = this_thresh)
    # }
    
    # print("intermediate tibble_to_return")
    # print(tibble_to_return)
    
    
    # print("tibble_to_return despues de in")
    # print(tibble_to_return)
    
    all_fits_list[[f]] <- tibble_to_return
    
    
    # print(paste0("al final de una vuelta de loop, es unrestricted?: ", is_unrestricted))
    
    
  }
  
  
  # print("final tibble_to_return")
  tibble_to_return <- reduce(all_fits_list, rbind)
  
  # print(tibble_to_return)
  
  return(tibble_to_return)
  
}



stack_models <- function(models_list) {
  
  # print("inside stack mdoels")
  # print("length(models_list)")
  # print(length(models_list))
  # 
  # print("first loop")
  # 
  # for (m in seq(1, length(models_list))) {
  #   print(paste0("m = ", m))
  #   this_thing <- as_tibble(models_list[[m]])
  #   print(names(this_thing))
  # }


  
  
  
  all_models <- as_tibble(reduce(models_list, rbind)) %>%
    dplyr::select(vars_select(names(.), -starts_with("wn"))) %>%
    dplyr::select(vars_select(names(.), -starts_with("rank"))) 
  
  
  all_models <- all_models %>%
    mutate(lags = unlist(lags))
  
  
  all_models <- all_models %>% 
    mutate(short_name = map2(variables, lags,
                             ~ make_model_name(variables = .x, lags = .y)),
           short_name = unlist(short_name),
           m_short_name = paste0(short_name, "_", model_function),
           var_size = map_dbl(variables, length)
    )
  
  all_models <- all_models %>% dplyr::distinct(m_short_name, .keep_all = TRUE)
  all_models_ranked <- add_rmse_rankings(all_models)
  
  all_models_ranked <- all_models_ranked %>% 
    dplyr::select(model_function, everything())
  
  return(all_models_ranked)
}


time_fit_cv <- function(var_data,
                        target_level_ts, 
                        reps = 1, 
                        var_sizes = c(3,4,5), 
                        tosample = 100,
                        lags = c(1,3,5),
                        use_info_lags = FALSE,
                        t_thresholds = 0,
                        training_length = "per_cv_maxs",
                        n_cv = 10, 
                        target_transform = "diff_yoy",
                        fc_horizon = 8,
                        names_exogenous = c("")) {
  
  all_reps_timings <- list_along(seq(1, reps))
  all_reps_counts <- list_along(seq(1, reps))
  mean_timing <- 0

  for (r in seq(1, reps)) {
    print(paste0("rep ", r, " of ", reps))
    all_et_fit <- vector(mode = "numeric", length = length(var_sizes)) 
    all_et_cv <- vector(mode = "numeric", length = length(var_sizes)) 
    all_n_fit <- vector(mode = "numeric", length = length(var_sizes)) 
    all_n_cv <- vector(mode = "numeric", length = length(var_sizes)) 
    all_n_lost_to_roots <- vector(mode = "numeric", length = length(var_sizes)) 
    all_n_lost_to_white <- vector(mode = "numeric", length = length(var_sizes)) 
    all_n_lost_to_threshold <- vector(mode = "numeric", length = length(var_sizes)) 
    for (i in seq(length(var_sizes))) {
      print(paste0("Starting var_size = ", var_sizes[i], 
                   " (", i, " of ", length(var_sizes),")"))
      
      specs <- all_specifications(var_size = var_sizes[i], 
                                  all_variables = colnames(var_data),
                                  lag_choices = lags,
                                  use_info_lags = use_info_lags,
                                  var_data = var_data, 
                                  t_thresholds = t_thresholds,
                                  silent = TRUE, 
                                  names_exogenous = names_exogenous)
      
      specs_sample <- sample_n(specs, tosample)
      
      tic()
      fit_list <- fit_tests_models_table(specs_sample, var_data = var_data, 
                                         names_exogenous = names_exogenous)
      fit_toc <- toc(quiet = TRUE)
      et_fit <- (fit_toc$toc - fit_toc$tic) 
      all_et_fit[i] <- et_fit
      
      all_n_fit[i] <- tosample
      all_n_cv[i] <- nrow(fit_list$passing_models)
      all_n_lost_to_roots[i] <- fit_list$n_lost_to_roots
      all_n_lost_to_white[i] <- fit_list$n_lost_to_white
      all_n_lost_to_threshold[i] <- fit_list$n_lost_to_threshold
      
      tic()
      cv_tbl <- cv_var_from_model_tbl(h = fc_horizon,
                                      training_length = training_length, 
                                      n_cv = n_cv,
                                      models_tbl = fit_list$passing_models, 
                                      var_data = var_data, 
                                      fit_column = "fit", 
                                      target_transform = target_transform,
                                      target_level_ts = target_level_ts,
                                      names_exogenous = names_exogenous
      )
      cv_toc <- toc(quiet = TRUE)
      et_cv <- (cv_toc$toc - cv_toc$tic) 
      all_et_cv[i] <- et_cv
    }
    
    all_et_fit_cv <- all_et_cv + all_et_fit 
    
    all_timings <- rbind(all_et_fit, all_et_fit/all_et_fit[1],
                         all_et_cv, all_et_cv/all_et_cv[1],
                         all_et_fit_cv, all_et_fit_cv/all_et_fit_cv[1])
    
    mean_timing <- mean_timing + all_timings
    
    all_counts <- rbind(all_n_fit, all_n_lost_to_threshold, all_n_lost_to_roots,
                        all_n_lost_to_white, all_n_cv)
    
    all_reps_timings[[r]] <- all_timings
    all_reps_counts[[r]] <- all_counts
  }
  
  mean_timing <- mean_timing/reps
  
  return(list(ave_timing = mean_timing,
              timings = all_reps_timings, 
              counts = all_reps_counts))
}

time_size_3 <- function(var_data = var_data,
                        target_level_ts = this_target_ts,
                        reps = 3,
                        t_thresholds = c(1.65, 2),
                        ratios_u_4_5 = c(1.093545, 1.051442),
                        ratios_r1_4_5 = c(1.391993, 1.730808), 
                        ratios_r2_4_5 = c(1.593183, 1.812038), 
                        lags = c(1,3,5),
                        n_specs = NULL, names_exogenous = c("")) {
  
  r1 <- t_thresholds[1]
  r2 <- t_thresholds
  
  t_100_u_s3 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts,
                            reps = reps, var_sizes = 3, lags = lags, names_exogenous = names_exogenous)
  t_100_r1_s3 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts,
                             reps = reps, t_thresholds = r1, var_sizes = 3, lags = lags, names_exogenous = names_exogenous)
  t_100_r2_s3 <- time_fit_cv(var_data = var_data, target_level_ts = this_target_ts,
                             reps = reps, t_thresholds =  r2, var_sizes = 3, lags = lags, names_exogenous = names_exogenous) 
  
  ts3_u <- t_100_u_s3$ave_timing[3,1]
  ts3_r1 <- t_100_r1_s3$ave_timing[3,1]
  ts3_r2 <- t_100_r2_s3$ave_timing[3,1]
  
  ts3_u_unitary <- ts3_u/100
  ts3_r1_unitary <- ts3_r1/100
  ts3_r2_unitary <- ts3_r2/100
  
  ts345_u_unitary  <- c(ts3_u_unitary , ts3_u_unitary * ratios_u_4_5[1], ts3_u_unitary * ratios_u_4_5[2])
  ts345_r1_unitary  <- c(ts3_r1_unitary , ts3_r1_unitary * ratios_r1_4_5[1], ts3_r1_unitary * ratios_r1_4_5[2])
  ts345_r2_unitary  <- c(ts3_r2_unitary , ts3_r2_unitary * ratios_r2_4_5[1], ts3_r2_unitary * ratios_r2_4_5[2])
  
  if (is.null(n_specs)) {
    n_s3 <- nrow(all_specifications(
      var_size = 3, all_variables = colnames(var_data), 
      lag_choices = c(3,4,5), use_info_lags = FALSE, 
      var_data = var_data, t_thresholds = 0, silent = TRUE))
    
    n_s4 <- nrow(all_specifications(
      var_size = 4, all_variables = colnames(var_data), 
      lag_choices = c(3,4,5), use_info_lags = FALSE, 
      var_data = var_data, t_thresholds = 0, silent = TRUE))
    
    n_s5 <- nrow(all_specifications(
      var_size = 5, all_variables = colnames(var_data), 
      lag_choices = c(3,4,5), use_info_lags = FALSE, 
      var_data = var_data, t_thresholds = 0, silent = TRUE))
  }
  
  time_u_all_specs_per_size  <- ts345_u_unitary*c(n_s3, n_s4, n_s5)
  time_r1_all_specs_per_size  <- ts345_r1_unitary*c(n_s3, n_s4, n_s5)
  time_r2_all_specs_per_size  <- ts345_r2_unitary*c(n_s3, n_s4, n_s5)
  
  total_time_u_all_specs <- sum(time_u_all_specs_per_size)
  total_time_r1_all_specs <- sum(time_r1_all_specs_per_size)
  total_time_r2_all_specs <- sum(time_r2_all_specs_per_size)
  
  return(list(total_time_u_all_specs = total_time_u_all_specs,
              total_time_r1_all_specs = total_time_r1_all_specs,
              total_time_r2_all_specs = total_time_r2_all_specs,
              time_per_spec_u_s345 = ts345_u_unitary,
              time_per_spec_r1_s345 = ts345_r1_unitary,
              time_per_spec_r2_s345 = ts345_r2_unitary,
              n_specs_s345 = c(n_s3, n_s4, n_s5)
  )
  )
}

var_cv <- function(var_data,
                   this_p, 
                   this_type = "const", 
                   n_cv = 8,
                   h_max = 8, 
                   train_test_marks = NULL,
                   training_length = "common_max",
                   timetk_idx = TRUE,
                   external_idx = NULL, 
                   test_residuals = TRUE,
                   full_sample_resmat = NULL,
                   names_exogenous = c(""),
                   exo_lag = NULL,
                   future_exo_cv = NULL,
                   this_thresh = 0,
                   number_of_qrt_in_year_1 = 4) {
  
  # print("this is var_cv")
  # print("chosen training length:")
  # print(training_length)
  

  vbls_for_var <- colnames(var_data)
  
  endov <- vbls_for_var[!vbls_for_var %in% names_exogenous] 
  
  if (length(endov) == 1) {
    this_cv <- NA
    print("only one endogenous variable, not a real VAR, returning NA")
    return(this_cv)
  }
  
  endodata <- var_data[ , endov]
  exov <- vbls_for_var[vbls_for_var %in% names_exogenous] 
  exodata <- var_data[ , exov]
  
  # print("in var_cv, exov is")
  # print(exov)
  

  cv_restriction_status <- NULL
  
  if (is.null(exo_lag)) {
    exo_lag <- this_p
  }
  
  
  if (training_length == "common_max") {
    total_obs <- nrow(var_data)
    training_length <- total_obs - h_max - (n_cv - 1)
    print(paste0("common_max = ", training_length))
  }
  
  if (is.null(train_test_marks)) {
    train_test_dates <- make_test_dates_list(ts_data = var_data, 
                                             type = "tscv",
                                             n = n_cv, 
                                             h_max = h_max, 
                                             training_length = training_length, 
                                             timetk_idx = timetk_idx, 
                                             external_idx = external_idx)
    
    train_test_dates <- train_test_dates[["list_of_year_quarter"]]
  }

  exo_and_lags <- make_exomat(exodata = exodata, exov = exov, exo_lag = exo_lag)
  
  if (is.null(dim(endodata))) {
    names(endodata) <- endov
  } else {
    colnames(endodata) <- endov
  }
  
  n <- nrow(var_data)
  
  cv_errors <- list_along(1:n_cv)
  cv_test_data <- list_along(1:n_cv)
  cv_fcs <- list_along(1:n_cv)
  # cv_fc_object <- list_along(1:n_cv)
  cv_vbl_names <- list_along(1:n_cv)
  cv_lag <- list_along(1:n_cv)
  cv_is_white_noise <- vector(mode = "logical", length = n_cv)

  total_obs <- nrow(var_data)

  if (is.numeric(training_length)) {
    cv_obs_used <- n_cv + training_length + h_max - 1
    if (total_obs < cv_obs_used) {
      print(paste("Warning: For selected variables, balanced sample has only", 
                  total_obs, "obs. Fixed-length cv needs", cv_obs_used, " obs."))
      
      print(paste0("Forecast length: ", h_max, ". Training length: ", 
                   training_length, ". CV rounds: ", n_cv))
    }
  }
  
  for (i in seq_along(1:n_cv)) {
    
    # print(paste0("i = ", i))
    
    this_tra_s <- train_test_dates[[i]]$tra_s
    this_tra_e <- train_test_dates[[i]]$tra_e
    
    this_tes_s <- train_test_dates[[i]]$tes_s
    this_tes_e <- train_test_dates[[i]]$tes_e
    
    # print(this_tra_s)
    # print(this_tra_e)
    # print(this_tes_s)
    # print(this_tes_e)
    
    training_y <- window(endodata, 
                         start = this_tra_s,
                         end = this_tra_e)
    
    this_training_y <- training_y
    
    test_y <- window(endodata, 
                     start = this_tes_s,
                     end = this_tes_e)
    
    
    if (is.null(exo_and_lags)) {
      training_exo <- NULL
      training_exo_and_lags <- NULL
      test_exo <- NULL
      test_exo_and_lags <- NULL
    } else {
      training_exo <- window(exodata, 
                             start = this_tra_s,
                             end = this_tra_e)

      training_exo_and_lags <- window(exo_and_lags, 
                                      start = this_tra_s,
                                      end = this_tra_e)

      assign("training_exo_and_lags", training_exo_and_lags, 
             envir = .GlobalEnv)

      if (!is.null(future_exo_cv)) {
        this_future_exo_cv <- future_exo_cv[[i]]
        # print("in varcv and inner loop, this future exo cv is")
        # print(class(this_future_exo_cv))
        # print(this_future_exo_cv)
        test_exo <- this_future_exo_cv[, exov]

        pretest_exodata <- window(exodata, end = this_tra_e)

        if (is.null(dim(exodata))) {
          # print("one exodata series")
          this_exodata <- ts(c(pretest_exodata, test_exo ), frequency =  frequency(pretest_exodata), start = start(pretest_exodata))
        } else {
          # print("multiple exodata series")
          this_exodata <- ts(rbind(pretest_exodata, test_exo ), frequency =  frequency(pretest_exodata), start = start(pretest_exodata))
        }

        this_exo_and_lags <- make_exomat(exodata = this_exodata, exov = exov, 
                                    exo_lag = exo_lag)
        test_exo_and_lags <- window(this_exo_and_lags, 
                                    start = this_tes_s,
                                    end = this_tes_e)
      } else {
        test_exo <- window(exodata, 
                           start = this_tes_s,
                           end = this_tes_e)
        
        test_exo_and_lags <- window(exo_and_lags, 
                                    start = this_tes_s,
                                    end = this_tes_e)
      }
    }

    if (is.null(dim(test_y))) {
      test_rgdp <- test_y
    } else {
      test_rgdp <- test_y[ , "rgdp"]
    }
    
    if (is.null(training_exo_and_lags)) {
      this_var <- vars::VAR(y = training_y, p = this_p, type = this_type) 
      
    } else {
      this_var <- vars::VAR(y = training_y, p = this_p, type = this_type, 
                            exogen = training_exo_and_lags)
    }

    if (!is.null(full_sample_resmat)) {
      # print(paste0("fit restriction inside cv, with t-thresh = "), this_thresh)
      # this_var_r <- try(vars::restrict(this_var, method = "manual", 
      #                            resmat = full_sample_resmat), silent = TRUE)
      this_var_r <- try(vars::restrict(this_var, method = "ser", thresh = this_thresh), silent = TRUE)
      
      # print("this_var_r")
      # print(this_var_r)
      
      if (class(this_var_r) == "try-error") {
        cv_restriction_status <- 0
      } else {
        cv_restriction_status <- 1
        this_var <- this_var_r
      }
    }
    
    this_effective_lag <- max_effective_lag(this_var)

    if (test_residuals) {
      resid_result <- check_resid_VAR(this_var)
      if (is.na(resid_result)) {
        print(paste("Error in resid test. Lag is", this_p, ", variables are", 
                    paste(colnames(var_data), collapse = "_")))
      }
      is_white_noise <- resid_result
    } else {
      is_white_noise <- TRUE
    }
    
    if (is.null(training_exo_and_lags)) {
      this_fc <- forecast(this_var, h = h_max)
    } else {
      
      this_fc <- forecast(this_var, h = h_max, dumvar = test_exo_and_lags,
                          exogen = training_exo_and_lags)
    }
    
    
    
    this_rgdp_fc_mean <- this_fc[["forecast"]][["rgdp"]][["mean"]]
    
    fc_error <- test_rgdp - this_rgdp_fc_mean
    
    vbl_names <- colnames(training_y)
    
    lag <- this_p
    
    cv_vbl_names[[i]] <- vbl_names
    cv_lag[[i]] <- lag
    cv_errors[[i]] <- fc_error
    cv_test_data[[i]] <- test_rgdp
    cv_fcs[[i]] <- this_rgdp_fc_mean
    # cv_fc_object[[i]] <- this_fc
    cv_is_white_noise[[i]] <- is_white_noise
    
  }

  cv_test_data_mat <- reduce(cv_test_data, rbind)

  cv_fcs_mat <- reduce(cv_fcs, rbind)

  # eliminate pesky "out" of it
  dimnames(cv_test_data_mat) <- NULL
  dimnames(cv_fcs_mat) <- NULL

  mean_cv_rmse <- fcs_accu(cv_fcs_mat, cv_test_data_mat)
  
  return(list(cv_errors = cv_errors,
              cv_test_data = cv_test_data,
              cv_fcs = cv_fcs,
              mean_cv_rmse = mean_cv_rmse,
              cv_vbl_names = cv_vbl_names,
              cv_lag = cv_lag,
              cv_is_white_noise = cv_is_white_noise,
              cv_restriction_status = cv_restriction_status,
              full_sample_resmat = full_sample_resmat))
}


variable_freq_by_n <- function(tbl_of_models, 
                               h_max = 8,
                               max_rank = 20, 
                               n_freq = 10, 
                               is_wide = FALSE, 
                               max_small_rank = 3) {

  rmse_names <- paste("rmse", seq(h_max), sep = "_")
  
  if ("full_sample_varest" %in% names(tbl_of_models)) {
    tbl_of_models <-  tbl_of_models %>% 
      dplyr::select(-full_sample_varest)
  }
  
  if (is_wide) {

    tbl_of_models <- tbl_of_models %>%  
      gather(key = "rmse_h", value = "rmse", rmse_names)


    tbl_of_models <- tbl_of_models %>% 
      group_by(rmse_h)

    
    tbl_of_models <- tbl_of_models %>% 
      mutate(rank_h = rank(rmse)) %>% 
      ungroup()

  }
  
  summary_of_tom <- tbl_of_models %>% 
    group_by(rmse_h) %>% 
    summarize(n_models = n(),
              less_than_max_rank = sum(rank_h < max_rank +1)
    )
  
  
  vec_of_rmse_h <- sort(unique(tbl_of_models$rmse_h))
  
  list_best <- map(vec_of_rmse_h, 
                   ~ tbl_of_models %>% 
                     filter(rmse_h == .x, rank_h < max_rank + 1) %>% 
                     dplyr::select("variables") %>% 
                     unlist() %>% 
                     table() %>% 
                     as_tibble() %>% 
                     arrange(desc(n)) %>% 
                     rename(., vbl = .)
  ) 
  

  tbl_best <- reduce(list_best, full_join, by = c("vbl"))
  

  
  names(tbl_best) <- c("vbl", paste("h", seq(h_max), sep = "_"))

  tbl_best <- tbl_best %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE),
           avg = total_n/length(rmse_names)) %>% 
    arrange(desc(total_n))

  list_best_small <- map(vec_of_rmse_h, 
                         ~ tbl_of_models %>% 
                           dplyr::filter(rmse_h == .x) 
  )

  
  small_effective_rank <- map_dbl(list_best_small,
                                  ~ sort(.x[["rank_h"]])[max_small_rank])
  

  new_lbs <- list_along(list_best_small)
  list_tables_best_vbl <- list_along(list_best_small)
  
  for (i in seq(1, length(new_lbs))) {
    this_lbs <- as_tibble(list_best_small[[i]])
    this_rank <- small_effective_rank[[i]]
    this_vbl <- dplyr::filter(this_lbs, rank_h <= this_rank) %>% 
      dplyr::select("variables")
    new_lbs[[i]] <- this_vbl
    this_table <- this_vbl %>%  unlist() %>%  table() %>%  as_tibble() %>%
      arrange(desc(n)) 
    names(this_table) <- c("vbl", "n")
    list_tables_best_vbl[[i]] <- this_table
  }
  
  new_lbs <- new_lbs 

  list_best_small <- list_tables_best_vbl


  tbl_best_small <- reduce(list_best_small, full_join, by = c("vbl"))

  names(tbl_best_small) <- c("vbl", paste("h", seq(h_max), sep = "_"))

  tbl_best_small <- tbl_best_small %>% 
    mutate(total_n = rowSums(.[2:(h_max + 1)], na.rm = TRUE),
           avg = total_n / length(rmse_names)) %>% 
    arrange(desc(total_n))
  
  variables_in_top_small <- unique(unlist(tbl_best_small[, "vbl"]))

  tbl_best_not_in_small <- tbl_best %>% 
    filter(! vbl %in% variables_in_top_small) %>% 
    arrange(desc( total_n ))
  
  variables_not_in_top_small <- unique(unlist(tbl_best_not_in_small[, "vbl"]))

  by_total_not_in_top_small <- unique(unlist(tbl_best_not_in_small[, "vbl"]))

  by_total <- tbl_best %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_h1 <- tbl_best %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_hlast <- tbl_best %>% 
    arrange(desc(h_7)) %>% 
    dplyr::select(vbl) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  vbl_by_h1 <-  by_h1$vbl
  vbl_by_hlast <-  by_hlast$vbl
  vbl_by_total <-  by_total$vbl
  
  vbl_all <- unique(c(vbl_by_h1, vbl_by_total, vbl_by_hlast))
  
  list_best_lags <- map(vec_of_rmse_h, 
                        ~ tbl_of_models %>% 
                          filter(rmse_h == .x, rank_h < max_rank +1 ) %>% 
                          dplyr::select("lags") %>% 
                          unlist() %>% 
                          table() %>% 
                          as_tibble() %>% 
                          arrange(desc(n)) %>% 
                          rename(., max_lag = .)
  ) 
  
  
  tbl_best_lags <- reduce(list_best_lags, full_join, by = c("max_lag"))
  names(tbl_best_lags) <- c("max_lag", paste("h", seq(h_max), sep = "_"))
  
  tbl_best_lags <- tbl_best_lags %>% 
    mutate(total_n = rowSums(.[2:(h_max+1)], na.rm = TRUE),
           avg = total_n/length(rmse_names))
  
  by_total_lags <- tbl_best_lags %>% 
    arrange(desc(total_n)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_h1_lags <- tbl_best_lags %>% 
    arrange(desc(h_1)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  by_hlast_lags <- tbl_best_lags %>% 
    arrange(desc(h_7)) %>% 
    dplyr::select(max_lag) %>% 
    dplyr::filter(row_number() <= n_freq)
  
  lags_by_h1 <-  by_h1_lags$max_lag
  lags_by_hlast <-  by_hlast_lags$max_lag
  lags_by_total <-  by_total_lags$max_lag
  
  lags_all <- unique(c(lags_by_h1, lags_by_total, lags_by_hlast))
  
  return(list(vbl_freqs_by_h = tbl_best, vbl_multi = vbl_all, 
              vbl_by_h1 = vbl_by_h1, vbl_by_total = vbl_by_total, 
              vbl_by_hlast = vbl_by_hlast, 
              lags_freqs_by_h = tbl_best_lags,
              lags_multi = lags_all, 
              lags_by_h1 = lags_by_h1, lags_by_total = lags_by_total, 
              lags_by_hlast = lags_by_hlast,
              list_best = list_best, 
              variables_in_top_small = variables_in_top_small,
              by_total_not_in_top_small = by_total_not_in_top_small))
}


var_search <- function(country, 
                       search_plan,
                       forecast_exercise_year, 
                       forecast_exercise_number,
                       fc_horizon,
                       target_variable = c("rgdp"),
                       default_t_treshold = 1.65,
                       default_lags = c(2, 3, 4, 5),
                       add_aic_bic_hq_fpe_lags =  FALSE,
                       restrict_by_signif = TRUE,
                       number_of_cv = 8,
                       train_span = 25,
                       ret_cv = TRUE,
                       max_rank_some_h =50,
                       max_rank_some_h_for_freq = 50,
                       max_small_rank = 3,
                       results_file_name = NULL,
                       names_exogenous = c(""),
                       exo_lag = NULL,
                       combn_already_tried = NULL
) {
  
  initial_time <- Sys.time()
  tic(msg = "Total time for this country")
  
  
  # file paths
  excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                            "_exercise_", forecast_exercise_number, "/")
  
  output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                        forecast_exercise_year, 
                        "_exercise_", forecast_exercise_number, "/")
  
  country_data_ts <- get_raw_data_ts(country = country, data_path = excel_data_path)
  external_data_ts <- get_raw_external_data_ts(data_path = excel_data_path)

  data_ts <- ts.union(country_data_ts, external_data_ts)
  colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))

  rgdp_level_ts <- data_ts[, "rgdp"]
  rgdp_level_ts <- na.omit(rgdp_level_ts)
  rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)
  
  print(paste0("This country: ", country))
  print(paste0("Number of variables (incl. rgdp): ", ncol(data_ts)))
  print("Names of variables: ")
  print(colnames(data_ts))
  
  tic()
  print("Finding and applying stationary transformations to all variables")
  reco_all_variables <- find_statio_diffs(data_ts, country)
  country_transformed_data <- follow_rec(data_ts, reco_all_variables)
  print("Done.")
  toc()
  
  rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
  print(paste0("Stationary transformation for rgdp: ", rgdp_rec))
  
  VAR_data_for_estimation  <- country_transformed_data
  
  print(paste0("rgdp obs. after transformation: ", 
               length(na.omit(VAR_data_for_estimation[ , "rgdp"]))
  )
  )
  
  print(paste0("rgdp obs. before transformation: ", length(rgdp_level_ts)))
  
  exodata_fullsample <- VAR_data_for_estimation[,names_exogenous]
  
  tic()
  print("extending (after stationary transformation) exogenous variables for forecasts")
  extension_of_exo <- extending_exogenous(exodata = exodata_fullsample, h = 8, 
                                          endo_end = end_target_in_VAR)
  toc()
  
  
  tic()
  print("extending (after stationary transformation) exogenous variables for cv")
  cv_extension_of_exo  <- extending_exogenous_for_cv(
    exodata = exodata_fullsample, h = 8, endo_end = end_target_in_VAR, 
    n_cv = n_cv, same_model_across_cv = FALSE)
  toc()
  
  variable_names <- colnames(VAR_data_for_estimation)
  ncolumns <- ncol(VAR_data_for_estimation)
  
  max_common_train_span_guaranted <- nrow(na.omit(VAR_data_for_estimation)) - fc_horizon - number_of_cv
  print(paste0("Taking all variables together, maximum common training span is ",
               max_common_train_span_guaranted))
  upper_bound_for_train_span <- length(na.omit(VAR_data_for_estimation[ , "rgdp"])) - fc_horizon - number_of_cv
  print(paste0("For variables encompasing rgdp extent, max training span is ",
               upper_bound_for_train_span))
  
  if (train_span == "common_max") {
    print(paste0("Using common_max span for training sets: ", max_common_train_span_guaranted))
    train_span <- max_common_train_span_guaranted
  }
  
  saveRDS(VAR_data_for_estimation, 
          paste0(output_path, "VAR_data_", country, ".rds"))
  
  

  
  n_steps <- length(search_plan)
  
  per_size_results <- list_along(1:n_steps)
  f_vbls_list <- list_along(1:n_steps)
  current_consolidated_models_list <- list_along(1:n_steps)
  cv_objects_list <- list_along(1:n_steps)
  prechosen_variables_at_each_step <- list_along(1:n_steps)
  all_prechosen_variables_at_each_step <- list_along(seq(1, n_steps))
  
  tic(msg = "Finish var search")
  
  for (i in seq(1, n_steps)) {
    
    set_of_prechosen_to_use <- NULL
    
    n_searches_for_this_size <- 0
    this_search_step <- search_plan[[i]]
    this_size <- this_search_step[["size"]]
    this_selection_type <- this_search_step[["vbl_selection_type"]]
    
    print("")
    print("--------------------------------------")
    print("")
    print(paste0("Starting the estimation of VAR with ", this_size," vbls"))
    print(paste0("Variable selection type for this size: ", this_selection_type))
    
    if (is.null(this_search_step$lags)) {
      this_lags <- default_lags
    } else 
    {
      this_lags <- this_search_step[["lags"]]
    }
    
    # print("This lags = ")
    # print(this_lags)
    
    
    if (is.null(this_search_step$t_treshold)) {
      this_t_tresh <- default_t_treshold
    } else {
      this_t_tresh <- this_search_step[["t_treshold"]]
    }
    
    # print("This t tresh = ")
    # print(this_t_tresh)
    
    if (this_selection_type == "none") {
      print("Using all variables without pre-chosen variables")
      this_VAR_data <- VAR_data_for_estimation
      this_prechosen_variables <- NULL
      f_vbls <- NULL
      new_select_vbls <- colnames(VAR_data_for_estimation) 
      vbls_top_small <- NA
      by_total_not_in_tsm <- NA
    }
    
    
    if (i > 1 & is.numeric(this_selection_type)) {
      f_vbls <- variable_freq_by_n(current_consolidated_models, 
                                   h_max = fc_horizon,
                                   max_rank = max_rank_some_h_for_freq,
                                   n_freq = this_selection_type, 
                                   is_wide = TRUE,
                                   mas_small_rank)
      freq_sel_vbls_by_multi <- f_vbls$vbl_multi
      vbls_top_small <- f_vbls$variables_in_top_small
      
      if(length(vbls_top_small) > this_selection_type) {
        print(paste0("Number of best-n-VAR variables (", length(vbls_top_small), 
                     "exceeds next_freq_limit (",  this_selection_type, "). We will preserve 
        the integrity of best VARs and use those",  length(vbls_top_small), " variables in next size." )  )
        
        print(paste0("If you want to decrease the number of variables, reduce the mas_small_rank 
                     parameter to some value lower than ", max_small_rank))
        
        vbls_top_small <- vbls_top_small
      }
      
      by_total_not_in_tsm <- f_vbls$by_total_not_in_top_small
      
      by_total_na <- is.na(by_total_not_in_tsm)
      
      by_total_not_in_tsm <- by_total_not_in_tsm[!by_total_na]
      
      n_gap_vbls <- this_selection_type - length(vbls_top_small)
      
      if (n_gap_vbls > 0) {
        extra_vbls <- by_total_not_in_tsm[1:n_gap_vbls]
      } else {
        extra_vbls <- c()
      }
      
      new_select_vbls <- c(vbls_top_small, extra_vbls)
      
      print("Using this subset of variables: ")
      print(new_select_vbls)
      
      this_VAR_data <- VAR_data_for_estimation[, new_select_vbls]
    }
    
    if (this_selection_type == "manually_prechosen_variables") {
      print("Using automatic incrementally added pre-chosen variables")
      print("This option does not automatically inherits prechosen variables from previous steps")
      
      current_consolidated_models <- current_consolidated_models_list[[i-1]]
      
      
      # print("before addig this step manual variables, we have:")
      # print(all_prechosen_variables_at_each_step)
      
      updated_list_of_prechosen <- add_prechosen_for_this_step(
        search_plan = search_plan, step_index = i, 
        prechosen_so_far = all_prechosen_variables_at_each_step,
        max_rank_some_h_for_freq = max_rank_some_h_for_freq,
        models_table = current_consolidated_models)
      
      # print("And after add_prechosen_for_this_step, the updated version of it is")
      # print(updated_list_of_prechosen)
      all_prechosen_variables_at_each_step <- updated_list_of_prechosen
      
      set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
      
      # print("And in this step we will add the following variables as prechosen, one at the time:")
      # print(set_of_prechosen_to_use)
      
    }
    
    if (this_selection_type == "incremental_auto_prechosen") {
      
      print("Using automatic incrementally added pre-chosen variables")
      
      print("Inherits from previous step, the following prechosen variables:")
      print(all_prechosen_variables_at_each_step[[i - 1]])
      
      current_consolidated_models <- current_consolidated_models_list[[i-1]]
      
      updated_list_of_prechosen <- add_prechosen_for_this_step(
        search_plan = search_plan, step_index = i, 
        prechosen_so_far = all_prechosen_variables_at_each_step,
        max_rank_some_h_for_freq = max_rank_some_h_for_freq,
        models_table = current_consolidated_models)
      
      all_prechosen_variables_at_each_step <- updated_list_of_prechosen
      
      set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
      
      print("And in this step we will add the following variables as prechosen, one at the time:")
      print(set_of_prechosen_to_use)
      
      # print("all_prechosen_variables_at_each_step")
      # print(all_prechosen_variables_at_each_step)
      
      # print("set_of_prechosen_to_use")
      # print(set_of_prechosen_to_use)
    }
    
    add_augmented_models <- this_search_step[["add_augmented_models"]]
    
    if (is.null(add_augmented_models)) {
      add_augmented_models <- FALSE
    }
    
    if (add_augmented_models) {
      
      n_best_per_h <- 2
      rmse_names <- paste("rmse", seq(fc_horizon), sep = "_")
      
      print(paste0(
        "Also including one-extra-variable augmented versions of the best ",
        n_best_per_h, " size-",search_plan[[i-1]]$size, "-VAR of each horizon",
        " (including ties).")
      )
      
      potential_models <- current_consolidated_models_list[[i-1]]
      
      potential_models <- potential_models %>% 
        gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
        dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
        group_by(rmse_h) %>% 
        arrange(rmse_h, rmse) %>% 
        mutate(rank_h = rank(rmse),
               nth_rmse = nth(rmse, n_best_per_h)) %>% 
        ungroup()
      
      print("potential_models")
      print(potential_models)
      
      vec_of_rmse_h <- sort(unique(potential_models$rmse_h))
      
      print("vec_of_rmse_h")
      print(vec_of_rmse_h)
      
      list_best <- map(vec_of_rmse_h, 
                       ~ potential_models %>% 
                         filter(rmse_h == .x, rmse <= nth_rmse)
      ) 
      
      print("list_best")
      print(list_best)
      
      break
      
    }
    
    tic(msg = paste0("Finished VARs with ", this_size, " variables"))
    
    if (!is.null(set_of_prechosen_to_use)) {
      # print("Inside the prechose vbls loop:")
      # print("set_of_prechosen_to_use")
      # print(set_of_prechosen_to_use)
      
      var_res_each_prechosen <- list_along(seq(1, length(set_of_prechosen_to_use)))
      
      for (ptu in seq(1, length(set_of_prechosen_to_use))) {
        print(paste0("new prechosen ", ptu, " of ", length(set_of_prechosen_to_use)))
        
        this_prechosen_variables <- set_of_prechosen_to_use[ptu][[1]]
        
        print("pre-chosen variables to be use in the coming VAR search:")
        print(this_prechosen_variables)
        
        print("is.list(this_prechosen_variables)")
        print(is.list(this_prechosen_variables))
        
        var_res <- search_var_one_size(
          var_size = this_size,
          vec_lags = this_lags,
          var_data = this_VAR_data,
          rgdp_level_ts = rgdp_level_ts,
          rgdp_yoy_ts = rgdp_yoy_ts,
          target_v = target_variable,
          pre_selected_v = this_prechosen_variables,
          is_cv = TRUE,
          training_length = train_span,
          h_max = fc_horizon,
          n_cv = number_of_cv,
          return_cv = ret_cv,
          rgdp_current_form = rgdp_rec,
          max_rank = max_rank_some_h,
          check_residuals_cv = TRUE,
          check_residuals_full_sample = TRUE,
          restrict_by_signif = restrict_by_signif,
          t_tresh = this_t_tresh,
          max_p_for_estimation = 12,
          add_info_based_lags = add_aic_bic_hq_fpe_lags,
          names_exogenous = names_exogenous,
          exo_lag = exo_lag)
        
        # print("names(var_res)")
        # 
        # print(names(var_res))
        
        var_res[["explored_size"]] <- this_size
        var_res[["used_prechosen"]] <- this_prechosen_variables
        
        var_res_each_prechosen[[ptu]] <- var_res
        
        n_searches_for_this_size <- n_searches_for_this_size + 1
        print("N of searches for this size:")
        print(n_searches_for_this_size)
      }
      
      all_models <- map(var_res_each_prechosen, "accu_rankings_models")
      all_models <- reduce(all_models, rbind)
      
      all_cv_obj <- map(var_res_each_prechosen, "cv_objects")
      all_cv_obj <- reduce(all_cv_obj, rbind)
      
      var_res <- list(accu_rankings_models = all_models,
                      cv_objects = all_cv_obj)
      
    }
    
    if (is.null(set_of_prechosen_to_use)) {
      var_res <- search_var_one_size(
        var_size = this_size,
        vec_lags = this_lags,
        var_data = this_VAR_data,
        rgdp_level_ts = rgdp_level_ts,
        rgdp_yoy_ts = rgdp_yoy_ts,
        target_v = target_variable,
        pre_selected_v = this_prechosen_variables,
        is_cv = TRUE,
        training_length = train_span,
        h_max = fc_horizon,
        n_cv = number_of_cv,
        return_cv = ret_cv,
        rgdp_current_form = rgdp_rec,
        max_rank = max_rank_some_h,
        check_residuals_cv = TRUE,
        check_residuals_full_sample = TRUE,
        restrict_by_signif = restrict_by_signif,
        t_tresh = this_t_tresh,
        max_p_for_estimation = 12,
        add_info_based_lags = add_aic_bic_hq_fpe_lags, 
        names_exogenous = names_exogenous, 
        exo_lag = exo_lag)
      
      n_searches_for_this_size <- n_searches_for_this_size + 1
      print("N of searches for this size:")
      print(n_searches_for_this_size)
      
      var_res[["explored_size"]] <- this_size
      var_res[["used_prechosen"]] <- this_prechosen_variables
    }
    
    per_size_results[[i]] <- var_res
    
    if (i == 1) {
      current_consolidated_models <- stack_models(
        list(var_res[["accu_rankings_models"]])
      ) 
    } else {
      current_consolidated_models <- stack_models(map(per_size_results, "accu_rankings_models"))
    }
    
    combn_already_tried <- c(combn_already_tried, 
                             var_res[["combinations_of_variables_considered"]])
    
    file_suffix <- paste0("_size_", this_size,
                          "_t_", this_t_tresh, "mr", max_rank_some_h,
                          "_mrfq", max_rank_some_h_for_freq, ".rds")
    
    filename <- paste0("var_results_", country, file_suffix)
    
    saveRDS(var_res, paste0(output_path, filename))
    
    per_size_results[[i]] <- var_res
    f_vbls_list[[i]] <- f_vbls
    
    prechosen_variables_at_each_step[[i]] <- this_prechosen_variables
    current_consolidated_models_list[[i]] <- current_consolidated_models
    cv_objects_list[[i]] <- var_res[["cv_objects"]]
    
    toc()
  }
  
  toc()
  
  bind_var_res_all_sizes <- reduce(map(per_size_results, "accu_rankings_models"), rbind)
  
  consolidated_var_res <- stack_models(map(per_size_results, "accu_rankings_models"))
  
  final_time <- Sys.time()
  
  elapsed_time <- final_time - initial_time
  
  if (ret_cv) {
    res_and_info <- list(consolidated_var_res = consolidated_var_res,
                         f_vbls_all_sizes = f_vbls_list,
                         var_data = VAR_data_for_estimation,
                         elapsed_time = elapsed_time, 
                         prechosen = all_prechosen_variables_at_each_step,
                         cv_objects = cv_objects_list,
                         target_variable_transform = rgdp_rec,
                         names_exogenous = names_exogenous,
                         fc_horizon = fc_horizon,
                         train_span = train_span,
                         number_of_cv = number_of_cv,
                         max_rank_some_h = max_rank_some_h)
    
  } else {
    res_and_info <- list(consolidated_var_res = consolidated_var_res,
                         f_vbls_all_sizes = f_vbls_list,
                         var_data = VAR_data_for_estimation,
                         prechosen = all_prechosen_variables_at_each_step,
                         elapsed_time = elapsed_time,
                         target_variable_transform = rgdp_rec,
                         names_exogenous,
                         fc_horizon = fc_horizon,
                         train_span = train_span,
                         number_of_cv = number_of_cv,
                         max_rank_some_h = max_rank_some_h)
  }
  
  allsizes <- paste(n_steps, collapse = "")
  allthresh <- "foo"
  # allthresh <- paste(t_tresh, collapse = "")
  allfqlim <- paste(c(9,6,6), collapse = "")
  
  file_suffix_all_sizes <-  paste0("_s", allsizes,
                                   "_t", allthresh, "_mr", max_rank_some_h,
                                   "_mrfq", max_rank_some_h_for_freq,
                                   "_cv",number_of_cv,"_tspan", train_span,
                                   "_h", fc_horizon,".rds")
  
  
  if(is.null(results_file_name)) {
    filename <- paste0("vr_", country, file_suffix_all_sizes)
  } else {
    filename <- results_file_name
  }
  
  print("filename")
  print(filename)
  
  saveRDS(res_and_info, paste0(output_path, filename))
  
  return(res_and_info)
}




var_search_old <- function(country, 
                       search_plan,
                       forecast_exercise_year, 
                       forecast_exercise_number,
                       fc_horizon,
                       target_variable = c("rgdp"),
                       default_t_treshold = 1.65,
                       default_lags = c(2, 3, 4, 5),
                       add_aic_bic_hq_fpe_lags =  FALSE,
                       restrict_by_signif = TRUE,
                       number_of_cv = 8,
                       train_span = 25,
                       ret_cv = TRUE,
                       max_rank_some_h =50,
                       max_rank_some_h_for_freq = 50,
                       max_small_rank = 3,
                       results_file_name = NULL,
                       names_exogenous = c(""),
                       exo_lag = NULL,
                       combn_already_tried = NULL
) {
  
  initial_time <- Sys.time()
  tic(msg = "Total time for this country")
  
  
  # file paths
  excel_data_path <- paste0("./data/edd_exercises/", forecast_exercise_year, 
                            "_exercise_", forecast_exercise_number, "/")
  
  output_path <- paste0("./analysis/VAR_output/edd_exercises/",
                        forecast_exercise_year, 
                        "_exercise_", forecast_exercise_number, "/")
  
  country_data_ts <- get_raw_data_ts(country = country, data_path = excel_data_path)
  external_data_ts <- get_raw_external_data_ts(data_path = excel_data_path)
  
  data_ts <- ts.union(country_data_ts, external_data_ts)
  colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))
  
  rgdp_level_ts <- data_ts[, "rgdp"]
  rgdp_level_ts <- na.omit(rgdp_level_ts)
  rgdp_yoy_ts <- make_yoy_ts(rgdp_level_ts)
  
  print(paste0("This country: ", country))
  print(paste0("Number of variables (incl. rgdp): ", ncol(data_ts)))
  print("Names of variables: ")
  print(colnames(data_ts))
  
  tic()
  print("Finding and applying stationary transformations to all variables")
  reco_all_variables <- find_statio_diffs(data_ts, country)
  country_transformed_data <- follow_rec(data_ts, reco_all_variables)
  print("Done.")
  toc()
  
  rgdp_rec <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["kpss_05_level"]]
  print(paste0("Stationary transformation for rgdp: ", rgdp_rec))
  
  VAR_data_for_estimation  <- country_transformed_data
  
  print(paste0("rgdp obs. after transformation: ", 
               length(na.omit(VAR_data_for_estimation[ , "rgdp"]))
  )
  )
  
  print(paste0("rgdp obs. before transformation: ", length(rgdp_level_ts)))
  
  variable_names <- colnames(VAR_data_for_estimation)
  ncolumns <- ncol(VAR_data_for_estimation)
  
  max_common_train_span_guaranted <- nrow(na.omit(VAR_data_for_estimation)) - fc_horizon - number_of_cv
  print(paste0("Taking all variables together, maximum common training span is ",
               max_common_train_span_guaranted))
  upper_bound_for_train_span <- length(na.omit(VAR_data_for_estimation[ , "rgdp"])) - fc_horizon - number_of_cv
  print(paste0("For variables encompasing rgdp extent, max training span is ",
               upper_bound_for_train_span))
  
  if (train_span == "common_max") {
    print(paste0("Using common_max span for training sets: ", max_common_train_span_guaranted))
    train_span <- max_common_train_span_guaranted
  }
  
  saveRDS(VAR_data_for_estimation, 
          paste0(output_path, "VAR_data_", country, ".rds"))
  
  n_steps <- length(search_plan)
  
  per_size_results <- list_along(1:n_steps)
  f_vbls_list <- list_along(1:n_steps)
  current_consolidated_models_list <- list_along(1:n_steps)
  cv_objects_list <- list_along(1:n_steps)
  prechosen_variables_at_each_step <- list_along(1:n_steps)
  all_prechosen_variables_at_each_step <- list_along(seq(1, n_steps))
  
  tic(msg = "Finish var search")
  
  for (i in seq(1, n_steps)) {
    
    set_of_prechosen_to_use <- NULL
    
    n_searches_for_this_size <- 0
    this_search_step <- search_plan[[i]]
    this_size <- this_search_step[["size"]]
    this_selection_type <- this_search_step[["vbl_selection_type"]]
    
    print("")
    print("--------------------------------------")
    print("")
    print(paste0("Starting the estimation of VAR with ", this_size," vbls"))
    print(paste0("Variable selection type for this size: ", this_selection_type))
    
    if (is.null(this_search_step$lags)) {
      this_lags <- default_lags
    } else 
    {
      this_lags <- this_search_step[["lags"]]
    }
    
    # print("This lags = ")
    # print(this_lags)
    
    
    if (is.null(this_search_step$t_treshold)) {
      this_t_tresh <- default_t_treshold
    } else {
      this_t_tresh <- this_search_step[["t_treshold"]]
    }
    
    # print("This t tresh = ")
    # print(this_t_tresh)
    
    if (this_selection_type == "none") {
      print("Using all variables without pre-chosen variables")
      this_VAR_data <- VAR_data_for_estimation
      this_prechosen_variables <- NULL
      f_vbls <- NULL
      new_select_vbls <- colnames(VAR_data_for_estimation) 
      vbls_top_small <- NA
      by_total_not_in_tsm <- NA
    }
    
    
    if (i > 1 & is.numeric(this_selection_type)) {
      f_vbls <- variable_freq_by_n(current_consolidated_models, 
                                   h_max = fc_horizon,
                                   max_rank = max_rank_some_h_for_freq,
                                   n_freq = this_selection_type, 
                                   is_wide = TRUE,
                                   mas_small_rank)
      freq_sel_vbls_by_multi <- f_vbls$vbl_multi
      vbls_top_small <- f_vbls$variables_in_top_small
      
      if(length(vbls_top_small) > this_selection_type) {
        print(paste0("Number of best-n-VAR variables (", length(vbls_top_small), 
                     "exceeds next_freq_limit (",  this_selection_type, "). We will preserve 
        the integrity of best VARs and use those",  length(vbls_top_small), " variables in next size." )  )
        
        print(paste0("If you want to decrease the number of variables, reduce the mas_small_rank 
                     parameter to some value lower than ", max_small_rank))
        
        vbls_top_small <- vbls_top_small
      }
      
      by_total_not_in_tsm <- f_vbls$by_total_not_in_top_small
      
      by_total_na <- is.na(by_total_not_in_tsm)
      
      by_total_not_in_tsm <- by_total_not_in_tsm[!by_total_na]
      
      n_gap_vbls <- this_selection_type - length(vbls_top_small)
      
      if (n_gap_vbls > 0) {
        extra_vbls <- by_total_not_in_tsm[1:n_gap_vbls]
      } else {
        extra_vbls <- c()
      }
      
      new_select_vbls <- c(vbls_top_small, extra_vbls)
      
      print("Using this subset of variables: ")
      print(new_select_vbls)
      
      this_VAR_data <- VAR_data_for_estimation[, new_select_vbls]
    }
    
    if (this_selection_type == "manually_prechosen_variables") {
      print("Using automatic incrementally added pre-chosen variables")
      print("This option does not automatically inherits prechosen variables from previous steps")
      
      current_consolidated_models <- current_consolidated_models_list[[i-1]]
      
      
      # print("before addig this step manual variables, we have:")
      # print(all_prechosen_variables_at_each_step)
      
      updated_list_of_prechosen <- add_prechosen_for_this_step(
        search_plan = search_plan, step_index = i, 
        prechosen_so_far = all_prechosen_variables_at_each_step,
        max_rank_some_h_for_freq = max_rank_some_h_for_freq,
        models_table = current_consolidated_models)
      
      # print("And after add_prechosen_for_this_step, the updated version of it is")
      # print(updated_list_of_prechosen)
      all_prechosen_variables_at_each_step <- updated_list_of_prechosen
      
      set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
      
      # print("And in this step we will add the following variables as prechosen, one at the time:")
      # print(set_of_prechosen_to_use)
      
    }
    
    if (this_selection_type == "incremental_auto_prechosen") {
      
      print("Using automatic incrementally added pre-chosen variables")
      
      print("Inherits from previous step, the following prechosen variables:")
      print(all_prechosen_variables_at_each_step[[i - 1]])
      
      current_consolidated_models <- current_consolidated_models_list[[i-1]]
      
      updated_list_of_prechosen <- add_prechosen_for_this_step(
        search_plan = search_plan, step_index = i, 
        prechosen_so_far = all_prechosen_variables_at_each_step,
        max_rank_some_h_for_freq = max_rank_some_h_for_freq,
        models_table = current_consolidated_models)
      
      all_prechosen_variables_at_each_step <- updated_list_of_prechosen
      
      set_of_prechosen_to_use <- all_prechosen_variables_at_each_step[[i]]
      
      print("And in this step we will add the following variables as prechosen, one at the time:")
      print(set_of_prechosen_to_use)
      
      # print("all_prechosen_variables_at_each_step")
      # print(all_prechosen_variables_at_each_step)
      
      # print("set_of_prechosen_to_use")
      # print(set_of_prechosen_to_use)
    }
    
    add_augmented_models <- this_search_step[["add_augmented_models"]]
    
    if (is.null(add_augmented_models)) {
      add_augmented_models <- FALSE
    }
    
    if (add_augmented_models) {
      
      n_best_per_h <- 2
      rmse_names <- paste("rmse", seq(fc_horizon), sep = "_")
      
      print(paste0(
        "Also including one-extra-variable augmented versions of the best ",
        n_best_per_h, " size-",search_plan[[i-1]]$size, "-VAR of each horizon",
        " (including ties).")
      )
      
      potential_models <- current_consolidated_models_list[[i-1]]
      
      potential_models <- potential_models %>% 
        gather(key = "rmse_h", value = "rmse", rmse_names) %>% 
        dplyr::select(vars_select(names(.), -starts_with("rank"))) %>% 
        group_by(rmse_h) %>% 
        arrange(rmse_h, rmse) %>% 
        mutate(rank_h = rank(rmse),
               nth_rmse = nth(rmse, n_best_per_h)) %>% 
        ungroup()
      
      print("potential_models")
      print(potential_models)
      
      vec_of_rmse_h <- sort(unique(potential_models$rmse_h))
      
      print("vec_of_rmse_h")
      print(vec_of_rmse_h)
      
      list_best <- map(vec_of_rmse_h, 
                       ~ potential_models %>% 
                         filter(rmse_h == .x, rmse <= nth_rmse)
      ) 
      
      print("list_best")
      print(list_best)
      
      break
      
    }
    
    tic(msg = paste0("Finished VARs with ", this_size, " variables"))
    
    if (!is.null(set_of_prechosen_to_use)) {
      # print("Inside the prechose vbls loop:")
      # print("set_of_prechosen_to_use")
      # print(set_of_prechosen_to_use)
      
      var_res_each_prechosen <- list_along(seq(1, length(set_of_prechosen_to_use)))
      
      for (ptu in seq(1, length(set_of_prechosen_to_use))) {
        print(paste0("new prechosen ", ptu, " of ", length(set_of_prechosen_to_use)))
        
        this_prechosen_variables <- set_of_prechosen_to_use[ptu][[1]]
        
        print("pre-chosen variables to be use in the coming VAR search:")
        print(this_prechosen_variables)
        
        print("is.list(this_prechosen_variables)")
        print(is.list(this_prechosen_variables))
        
        var_res <- search_var_one_size(
          var_size = this_size,
          vec_lags = this_lags,
          var_data = this_VAR_data,
          rgdp_level_ts = rgdp_level_ts,
          rgdp_yoy_ts = rgdp_yoy_ts,
          target_v = target_variable,
          pre_selected_v = this_prechosen_variables,
          is_cv = TRUE,
          training_length = train_span,
          h_max = fc_horizon,
          n_cv = number_of_cv,
          return_cv = ret_cv,
          rgdp_current_form = rgdp_rec,
          max_rank = max_rank_some_h,
          check_residuals_cv = TRUE,
          check_residuals_full_sample = TRUE,
          restrict_by_signif = restrict_by_signif,
          t_tresh = this_t_tresh,
          max_p_for_estimation = 12,
          add_info_based_lags = add_aic_bic_hq_fpe_lags,
          names_exogenous = names_exogenous,
          exo_lag = exo_lag)
        
        # print("names(var_res)")
        # 
        # print(names(var_res))
        
        var_res[["explored_size"]] <- this_size
        var_res[["used_prechosen"]] <- this_prechosen_variables
        
        var_res_each_prechosen[[ptu]] <- var_res
        
        n_searches_for_this_size <- n_searches_for_this_size + 1
        print("N of searches for this size:")
        print(n_searches_for_this_size)
      }
      
      all_models <- map(var_res_each_prechosen, "accu_rankings_models")
      all_models <- reduce(all_models, rbind)
      
      all_cv_obj <- map(var_res_each_prechosen, "cv_objects")
      all_cv_obj <- reduce(all_cv_obj, rbind)
      
      var_res <- list(accu_rankings_models = all_models,
                      cv_objects = all_cv_obj)
      
    }
    
    if (is.null(set_of_prechosen_to_use)) {
      var_res <- search_var_one_size(
        var_size = this_size,
        vec_lags = this_lags,
        var_data = this_VAR_data,
        rgdp_level_ts = rgdp_level_ts,
        rgdp_yoy_ts = rgdp_yoy_ts,
        target_v = target_variable,
        pre_selected_v = this_prechosen_variables,
        is_cv = TRUE,
        training_length = train_span,
        h_max = fc_horizon,
        n_cv = number_of_cv,
        return_cv = ret_cv,
        rgdp_current_form = rgdp_rec,
        max_rank = max_rank_some_h,
        check_residuals_cv = TRUE,
        check_residuals_full_sample = TRUE,
        restrict_by_signif = restrict_by_signif,
        t_tresh = this_t_tresh,
        max_p_for_estimation = 12,
        add_info_based_lags = add_aic_bic_hq_fpe_lags, 
        names_exogenous = names_exogenous, 
        exo_lag = exo_lag)
      
      n_searches_for_this_size <- n_searches_for_this_size + 1
      print("N of searches for this size:")
      print(n_searches_for_this_size)
      
      var_res[["explored_size"]] <- this_size
      var_res[["used_prechosen"]] <- this_prechosen_variables
    }
    
    per_size_results[[i]] <- var_res
    
    if (i == 1) {
      current_consolidated_models <- stack_models(
        list(var_res[["accu_rankings_models"]])
      ) 
    } else {
      current_consolidated_models <- stack_models(map(per_size_results, "accu_rankings_models"))
    }
    
    combn_already_tried <- c(combn_already_tried, 
                             var_res[["combinations_of_variables_considered"]])
    
    file_suffix <- paste0("_size_", this_size,
                          "_t_", this_t_tresh, "mr", max_rank_some_h,
                          "_mrfq", max_rank_some_h_for_freq, ".rds")
    
    filename <- paste0("var_results_", country, file_suffix)
    
    saveRDS(var_res, paste0(output_path, filename))
    
    per_size_results[[i]] <- var_res
    f_vbls_list[[i]] <- f_vbls
    
    prechosen_variables_at_each_step[[i]] <- this_prechosen_variables
    current_consolidated_models_list[[i]] <- current_consolidated_models
    cv_objects_list[[i]] <- var_res[["cv_objects"]]
    
    toc()
  }
  
  toc()
  
  bind_var_res_all_sizes <- reduce(map(per_size_results, "accu_rankings_models"), rbind)
  
  consolidated_var_res <- stack_models(map(per_size_results, "accu_rankings_models"))
  
  final_time <- Sys.time()
  
  elapsed_time <- final_time - initial_time
  
  if (ret_cv) {
    res_and_info <- list(consolidated_var_res = consolidated_var_res,
                         f_vbls_all_sizes = f_vbls_list,
                         var_data = VAR_data_for_estimation,
                         elapsed_time = elapsed_time, 
                         prechosen = all_prechosen_variables_at_each_step,
                         cv_objects = cv_objects_list,
                         target_variable_transform = rgdp_rec,
                         names_exogenous = names_exogenous,
                         fc_horizon = fc_horizon,
                         train_span = train_span,
                         number_of_cv = number_of_cv,
                         max_rank_some_h = max_rank_some_h)
    
  } else {
    res_and_info <- list(consolidated_var_res = consolidated_var_res,
                         f_vbls_all_sizes = f_vbls_list,
                         var_data = VAR_data_for_estimation,
                         prechosen = all_prechosen_variables_at_each_step,
                         elapsed_time = elapsed_time,
                         target_variable_transform = rgdp_rec,
                         names_exogenous,
                         fc_horizon = fc_horizon,
                         train_span = train_span,
                         number_of_cv = number_of_cv,
                         max_rank_some_h = max_rank_some_h)
  }
  
  allsizes <- paste(n_steps, collapse = "")
  allthresh <- "foo"
  # allthresh <- paste(t_tresh, collapse = "")
  allfqlim <- paste(c(9,6,6), collapse = "")
  
  file_suffix_all_sizes <-  paste0("_s", allsizes,
                                   "_t", allthresh, "_mr", max_rank_some_h,
                                   "_mrfq", max_rank_some_h_for_freq,
                                   "_cv",number_of_cv,"_tspan", train_span,
                                   "_h", fc_horizon,".rds")
  
  
  if(is.null(results_file_name)) {
    filename <- paste0("vr_", country, file_suffix_all_sizes)
  } else {
    filename <- results_file_name
  }
  
  print("filename")
  print(filename)
  
  saveRDS(res_and_info, paste0(output_path, filename))
  
  return(res_and_info)
}





transform_all_cv <- function(cv_object, current_form,
                             target_level_ts, n_cv) {
  
  old_test_data_list <- cv_object[["cv_test_data"]]
  old_fcs_list <- cv_object[["cv_fcs"]]
  
  new_test_data_list <- list_along(1:n_cv)
  new_fcs_list <- list_along(1:n_cv)
  new_fcs_errors_list  <- list_along(1:n_cv)
  
  new_test_data_yr_avg_list <- list_along(1:n_cv)
  new_fcs_yr_avg_list <- list_along(1:n_cv)
  new_fcs_errors_yr_avg_list <- list_along(1:n_cv)
  
  if (all(is.na(cv_object))) {
    return(cv_object)
  }

  if (current_form == "yoy") {
    #noting to transform to yoy
    for (td in seq_along(1:n_cv)) {
      new_test_data <- old_test_data_list[[td]]
      new_fcs <- old_fcs_list[[td]]
      
      new_fcs_errors <-  new_test_data - new_fcs
      new_fcs_errors_list[[td]] <- new_fcs_errors

      new_test_data_yr_avg <- yr_averages(new_test_data)
      new_test_data_yr_avg_list[[td]] <- new_test_data_yr_avg
      
      new_fcs_yr_avg <- yr_averages(new_fcs)
      new_fcs_yr_avg_list[[td]] <- new_fcs_yr_avg
      
      new_errors_of_yr_avg <- new_test_data_yr_avg - new_fcs_yr_avg
      new_fcs_errors_yr_avg_list[[td]] <- new_errors_of_yr_avg

    }
    
  }


  
  if (current_form == "diff_yoy") {
    
    len_initial_cond <- 1
    auxiliary_ts <- make_yoy_ts(target_level_ts)
    
    for (td in seq_along(1:n_cv)) {
      
      this_test_data <- old_test_data_list[[td]]
      this_fcs <- old_fcs_list[[td]]
      
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
      new_fcs <- un_diff_ts(initial_cond_ts, this_fcs)
      
      new_fcs_errors <-  new_test_data - new_fcs
      new_fcs_errors_list[[td]] <- new_fcs_errors
      
      new_test_data_list[[td]] <- new_test_data
      new_fcs_list[[td]] <- new_fcs
      
      new_test_data_yr_avg <- yr_averages(new_test_data)
      new_fcs_yr_avg <- yr_averages(new_fcs)
      new_test_data_yr_avg_list[[td]] <- new_test_data_yr_avg
      new_fcs_yr_avg_list[[td]] <- new_fcs_yr_avg
      
      new_errors_of_yr_avg <- new_test_data_yr_avg - new_fcs_yr_avg
      new_fcs_errors_yr_avg_list[[td]] <- new_errors_of_yr_avg
      
    }
    
  }
  
  if (current_form == "diff") {
    len_initial_cond <- 1
    auxiliary_ts <- target_level_ts
    
    for (td in seq_along(1:n_cv)) {
      
      this_test_data <- old_test_data_list[[td]]
      this_fcs <- old_fcs_list[[td]]
      
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
      level_fcs <- un_diff_ts(initial_cond_ts, this_fcs)
      
      pre_test_level_data <- window(auxiliary_ts, end = end_initial_cond_y_q)
      
      data_and_test_level <- ts(c(pre_test_level_data, level_test_data),
                                frequency = 4, start = start(auxiliary_ts))
      data_and_fcs_level <- ts(c(pre_test_level_data, level_fcs),
                               frequency = 4, start = start(auxiliary_ts))
      
      data_and_test_yoy <- make_yoy_ts(data_and_test_level, freq = 4, 
                                       is_log = FALSE)
      data_and_fcs_yoy <- make_yoy_ts(data_and_fcs_level, freq = 4, 
                                      is_log = FALSE)
      
      new_test_data <- window(data_and_test_yoy, start = start(this_test_data),
                              end = end(this_test_data))
      new_fcs <- window(data_and_fcs_yoy, start = start(this_fcs),
                        end = end(this_fcs))
      
      new_fcs_errors <-  new_test_data - new_fcs
      new_fcs_errors_list[[td]] <- new_fcs_errors
      
      new_fcs_list[[td]] <- new_fcs
      new_test_data_list[[td]] <- new_test_data
      
      new_test_data_yr_avg <- yr_averages(new_test_data)
      new_fcs_yr_avg <- yr_averages(new_fcs)
      new_test_data_yr_avg_list[[td]] <- new_test_data_yr_avg
      new_fcs_yr_avg_list[[td]] <- new_fcs_yr_avg
      
      new_errors_of_yr_avg <- new_test_data_yr_avg - new_fcs_yr_avg
      new_fcs_errors_yr_avg_list[[td]] <- new_errors_of_yr_avg
    }
    
  }
  
  new_cv_object = list(cv_test_data = new_test_data_list, 
                       cv_fcs = new_fcs_list,
                       cv_errors = new_fcs_errors_list,
                       cv_fcs_yr_avg = new_fcs_yr_avg_list,
                       cv_test_data_yr_avg = new_test_data_yr_avg_list,
                       cv_errors_from_yr_avg = new_fcs_errors_yr_avg_list)
  
  return(new_cv_object)
}





yr_averages_of_fcs <- function(yoy_fcs_to_average, level_series, yta = "current_and_next") {
  
  level_series <- na.omit(level_series)
  time_first_fc <- time(yoy_fcs_to_average)[1]
  time_last_before_fc <- time_first_fc - 1/frequency(yoy_fcs_to_average)
  level_series_before_fc <- window(level_series, end = time_last_before_fc)
  yoy_obs_before_fc <- make_yoy_ts(level_series_before_fc)
  yoy_obs_and_fc <- ts(data = c(yoy_obs_before_fc, yoy_fcs_to_average),
                       start = start(yoy_obs_before_fc),
                       frequency = frequency(yoy_obs_before_fc))
  
  if (is.numeric(yta)) {
    years_in_avg <- yta
  } else {
    first_year_in_fc <- floor(tsp(yoy_fcs_to_average)[1])
    current_year <- first_year_in_fc
    
    if (yta == "current_and_next") {
      years_in_avg <- c(current_year, current_year+1)
    }
    
    if (yta == "current") {
      years_in_avg <- c(current_year)
    }
  }
  
  number_of_years <- length(years_in_avg)
  yearly_averages <- vector(mode = "numeric", length = number_of_years) 
  
  for (i in seq(1, number_of_years)) {
    this_year <- years_in_avg[i]
    is_from_this_year <- floor(time(yoy_obs_and_fc)) == this_year
    only_from_this_year <- yoy_obs_and_fc[is_from_this_year]
    this_year_average <- mean(only_from_this_year)
    yearly_averages[i] <-  this_year_average
  }
  
  return(yearly_averages)
}





