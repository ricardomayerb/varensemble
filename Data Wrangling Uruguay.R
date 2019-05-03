source('./R/combinations_functions.R')
# To turn rmd into r scropt use knitr::purl(input = "./Rmd/basics_prepare_data_for_VAR.Rmd", "./R/basics_prepare_data_for_VAR.R")
# to keep the text use documentation - 2, 
# knitr::purl(input = "./Rmd/basics_prepare_data_for_VAR.Rmd", "./R/basics_prepare_data_for_VAR2.R", documentation = 2)

country_data_ts <- get_raw_data_ts(id = "Uruguay", 
                                   data_path = "./data/excel/Uruguay.xlsx",
                                   general_variables_to_drop = c("year", "quarter", "hlookup", "rgdp_sa", "trim", 
                                                                 "month", "exist", "cred", "imp_nonpetro") ,
                                   sheet_q_variables =  "quarterly",
                                   sheet_m_variables = "monthly", 
                                   column_number_date_q = 1,
                                   column_number_date_m = 1)

# print(colnames(country_data_ts))

variables_to_drop_external <- c("year", "month", "hlookup")
external_data_ts <- get_raw_external_data_ts(data_path = "./data/external/external.xlsx",
                                             variables_to_drop = variables_to_drop_external,
                                             sheet_m_variables = "monthly",
                                             column_number_date_m = 1)
data_ts <- ts.union(country_data_ts, external_data_ts)
colnames(data_ts) <- c(colnames(country_data_ts), colnames(external_data_ts))
# print(colnames(data_ts))

# Later we need the raw data so we save it here
saveRDS(data_ts, "./data/raw_VAR_data/raw_VAR_data_Uruguay.rds")

# get recomendations with deter = level

reco_all_variables <- find_statio_diffs(data_ts= data_ts, 
                                        id = "Uruguay", 
                                        return_4_seas = FALSE, 
                                        do_other_seas = FALSE, seas_test = "seas",
                                        tests_alpha = c(0.01, 0.05, 0.1),
                                        this_alpha = 0.05,
                                        this_deterministic = "level", this_sta_test = "kpss") 

# get recomentations with deter = trend

reco_all_variables_trend <- find_statio_diffs(data_ts= data_ts, 
                                              id = "Uruguay", 
                                              return_4_seas = FALSE, 
                                              do_other_seas = FALSE, seas_test = "seas",
                                              tests_alpha = c(0.01, 0.05, 0.1),
                                              this_alpha = 0.05,
                                              this_deterministic = "trend", this_sta_test = "kpss") 

# inform which variables get different recomendations   
level_and_trend_rec_diff <- get_variable_diff_stationarity_recom(reco_level = reco_all_variables, 
                                                                 reco_trend = reco_all_variables_trend);level_and_trend_rec_diff

# rgdp recommendation: save it somewhere
rgdp_rec_level <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["this_test_alpha_deter"]]
rgdp_rec_trend <- reco_all_variables_trend[reco_all_variables_trend$variable == "rgdp", ][["this_test_alpha_deter"]]
print(paste0("Stationary transformation level for rgdp: ", rgdp_rec_level))
print(paste0("Stationary transformation trend for rgdp: ", rgdp_rec_trend))

# data_tbl_ts, table_of_recommendations
country_transformed_data_level <- follow_rec(data_tbl_ts = data_ts, table_of_recommendations = reco_all_variables)
country_transformed_data_trend <- follow_rec(data_tbl_ts = data_ts, table_of_recommendations = reco_all_variables_trend)

VAR_data_for_estimation_level  <- country_transformed_data_level
VAR_data_for_estimation_trend  <- country_transformed_data_trend
# VAR_data_for_estimation_level  <- na.omit(country_transformed_data_level)
# VAR_data_for_estimation_trend  <- na.omit(country_transformed_data_trend)

stationarity_plots <- make_stationarity_plots(vector_variables_with_diff_sta_rec = level_and_trend_rec_diff, 
                                              VAR_data_for_estimation_level = VAR_data_for_estimation_level,
                                              VAR_data_for_estimation_trend = VAR_data_for_estimation_trend)

walk(stationarity_plots, print)

# Examine the graphs, and decide which transformation leads to the most stationary series and choose that transformation
trend_recom <- c("serv", "exp", "m1", "tax")

create_recommendation_df <- function(variables, 
                                     recom_level, 
                                     recom_trend, 
                                     vector_trend_recom, 
                                     default_followed_recom){
  
  for (i in 1:length(variables)){
    # print(paste0("i = ", i))
    # print("current_variable:")
    # print(variables[i])
    
    if (variables[i] %in% vector_trend_recom){
      # print("following the trend recomendation for variable:")
      # print(variables[i])
      default_followed_recom[i] <- recom_trend[i]  
    }
    else(default_followed_recom[i] <- recom_level[i])
  }
  df_recom <- data.frame(variables = variables,
                         level_recom = recom_level,
                         trend_recom = recom_trend,
                         followed_recom = default_followed_recom)
  
  return(df_recom)
  
}


recommendation_df <- create_recommendation_df(variables = reco_all_variables$variable,
                                              recom_level = reco_all_variables$this_test_alpha_deter, 
                                              recom_trend = reco_all_variables_trend$this_test_alpha_deter, 
                                              vector_trend_recom = trend_recom,
                                              default_followed_recom = reco_all_variables$this_test_alpha_deter)




# make sure level and trend transformed series have the same length
final_VAR_data_for_estimation <- make_final_VAR_data_for_estimation(VAR_data_level = VAR_data_for_estimation_level, 
                                                                    VAR_data_trend = VAR_data_for_estimation_trend,
                                                                    variables_with_trend_recom = trend_recom)

saveRDS(final_VAR_data_for_estimation, "./data/VAR_data_Uruguay.rds")

# Also save the final transformation used for the target variable. You need this information later. 
target_transformation  <- reco_all_variables[reco_all_variables$variable == "rgdp", ][["this_test_alpha_deter"]]
country <- id

saveRDS(object = list(country_name = country, 
                      target_transformation = target_transformation,
                      all_transformations = recommendation_df),
        file = "./data/target_transformation/target_transformation_Uruguay.rds")
