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
# data load
data <- read_excel("/Users/yj.noh/Desktop/on_boarding_data.xlsx")
head(data)
str(data)

data[c("birth", "delivery_method", "is_recom", "gender", "day_cnt", "avg_cnt", "outcome")] %>% 
  tbl_summary(
    by = outcome,
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
