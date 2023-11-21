# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS") # nolint
ipak(pkg)

##########################################################################################################################################################
# 
# 전체 
data <- read_excel("/Users/yj.noh/Desktop/on_boarding_data.xlsx")

head(data)
str(data)
dim(data)

data <- data  %>% mutate(is_recom = ifelse (is_recom == TRUE, 1, 0))


data[c("birth", "delivery_method", "insurance_type", "is_recom", "gender", "avg_daily_delivery", "active_days", "avg_distance", "avg_fee", "outcome")] %>% 
  tbl_summary(
    by = outcome,
   type = list(
    avg_daily_delivery ~ "continuous2",
    active_days ~ "continuous2",
    avg_distance ~ "continuous2",
    avg_fee ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()

##########################################################################################################################################################

# bike만
bike <- read_excel("/Users/yj.noh/Documents/GitHub/prj_on_boarding/data_filtered_both.xlsx")
str(bike)

bike[c("birth", "delivery_method", "insurance_type", "is_recom", "gender", "avg_daily_delivery", "active_days", "avg_distance", "avg_fee", "avg_distance_1_to_3", "avg_fee_1_to_3", "join_period", "from_join_to_first_able", "from_first_able_to_start", "outcome")] %>% 
  tbl_summary(
    by = outcome,
   type = list(
    avg_daily_delivery ~ "continuous2",
    active_days ~ "continuous2",
    avg_distance ~ "continuous2",
    avg_fee ~ "continuous2",
    avg_distance_1_to_3 ~ "continuous2",
    avg_fee_1_to_3 ~ "continuous2",
    join_period ~ "continuous2",
    from_join_to_first_able ~ "continuous2",
    from_first_able_to_start ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()


##########################################################################################################################################################
# RFM  
data <- read_excel("/Users/yj.noh/Desktop/rfm_data.xls")

head(data)
str(data)
dim(data)

data <- data  %>% filter(!is.na(working_cycle))
data$work_start_week<- factor(data$work_start_week, levels = c('8W1D', '8W2D', '8W3D', '8W4D', '9W1D', '9W2D', '9W3D', '9W4D'
,'10W1D',  '10W2D', '10W3D', '10W4D','10W5D'))

df = subset(data, select = -c(brms_rider_id))
df %>%  
  tbl_summary(
    by = work_start_week,
   type = list(
    all_continuous() ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()
