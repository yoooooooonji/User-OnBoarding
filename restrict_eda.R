# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS", "MatchIt") # nolint
ipak(pkg)

##########################################################################################################################################################
# 
# 전체 
#data <- read.csv("/Users/yj.noh/Desktop/restrict_data.csv", fileEncoding = "utf-8")
data <- read_excel("/Users/yj.noh/Desktop/restrict_data.xlsx")
head(data)

data <- data  %>% mutate(region = paste(rgn1_nm, "", rgn2_nm))
data <- data  %>% mutate(is_restrict = ifelse (baemin1_single_limited_time >0 ,1, 0))
table(data$is_restrict)

df <- data[c("region","r_value", "is_restrict")]
head(df)

# df <- df  %>% group_by(rgn2_nm) %>% mutate(count = n_distinct(is_restrict))
# test <- df  %>% filter(rgn2_nm == '포천시')

# 수정된 데이터로 요약
df %>%
  tbl_strata(
    strata = region,
    ~ .x %>% 
      tbl_summary(
        by = is_restrict,
        type = r_value ~ "continuous2",
        statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
        missing_text = "(Missing value)", 
        digits = list(all_continuous() ~ c(0, 1), 
                      all_categorical() ~ c(0, 1))
      ) %>%
      add_overall() %>%
      add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
      bold_labels()
  )






test <- data  %>% filter(rgn2_nm == '마포구')
