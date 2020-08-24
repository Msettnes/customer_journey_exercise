library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(xgboost)
library(readr)
library(lubridate)
library(caret)
library(ROCR)

training_data <- read_csv("training_data.csv", 
                            col_types = cols(conversion_time = col_character(), 
                            session_end_time = col_character(), 
                            country = col_character(), 
                            region = col_character(), 
                            sub_region = col_character())) 


data_df = training_data %>%
                dplyr::mutate(country    = ifelse(is.na(country),"otherCountry",country),
                               sub_region = ifelse(is.na(sub_region),"otherSubregion",sub_region),
                               region     = ifelse(is.na(region),"otherRegion",region),
                               browser    = ifelse(is.na(browser),"otherBrowser",browser),
                               os         = ifelse(is.na(os),"otherOS",os),
                               device     = ifelse(is.na(device),"otherDevice",device)) %>%
                dplyr::mutate(session_end_time   = lubridate::ymd_hms(session_end_time),
                              session_start_time = lubridate::ymd_hms(session_start_time),
                              conversion_time    = lubridate::ymd_hms(conversion_time),
                              later_session_start_time = lubridate::ymd_hms(later_session_start_time))

# # filter out sessions after the first sign-up
# # Too many mismatch between session start and conversion time so a large percent of conversions get removed in this transformation
# tmp = data_df %>% dplyr::filter(conversion == T) %>% 
#       dplyr::group_by(user_id) %>% 
#       dplyr::mutate(min_conversion_time = min(conversion_time)) %>%
#       dplyr::select(min_conversion_time,user_id) %>% ungroup() %>% 
#       dplyr::distinct(min_conversion_time,user_id)
# 
# data_df_clean = left_join(data_df, tmp) %>% 
#   dplyr::mutate(after_conversion = session_start_time > min_conversion_time + dseconds(10)) %>%
#   dplyr::mutate(after_conversion = ifelse(is.na(after_conversion),FALSE,after_conversion))
# 
# aa = data_df_clean %>% dplyr::filter(after_conversion == F)  
# 
# sum(aa$conversion)/sum(!aa$conversion)
# 
# sum(data_df$conversion)/sum(!data_df$conversion)

# filter out session times = 0 
# instead of filtering out -> make feature that indicate "wasOpen"



