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
#data <- read_excel("/Users/yj.noh/Desktop/on_boarding_data.xlsx")
data <- read.csv("/Users/yj.noh/Desktop/on_boarding.csv", fileEncoding = "cp949")

data <- data  %>% mutate(is_recom = ifelse (is_recom == TRUE, 1, 0))
data <- data  %>% filter(is_recom == 0)

dim(data) #10215 
table(data$delivery_method)

df <- data  %>% filter(delivery_method == 'WALK')

df[c("birth", "delivery_method", "insurance_type", "is_recom", "gender", "avg_daily_delivery", "active_days", "avg_distance", "avg_fee", "outcome", "avg_distance_1_to_3", "avg_fee_1_to_3", "from_join_to_first_able", "from_first_able_to_start")] %>% 
  tbl_summary(
    by = outcome,
   type = list(
    avg_daily_delivery ~ "continuous2",
    active_days ~ "continuous2",
    avg_distance ~ "continuous2",
    avg_fee ~ "continuous2", 
    avg_distance_1_to_3 ~ "continuous2",
    avg_fee_1_to_3 ~ "continuous2",
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

