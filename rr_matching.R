# dataset 
# 0. install packages 
options(scipen=10)

ipak <-function(pkg){
  new.pkg<-pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg,dependencies=TRUE)
  sapply(pkg,require,character.only=TRUE)
}

pkg <- c("readr", "dplyr", "tidytext", "tidyverse", "lubridate", "reshape2", "psych", "gtsummary", "readxl", "MASS", "MatchIt", "viridis") # nolint
ipak(pkg)

##########################################################################################################################################################
# 
# 전체 
data <- read.csv("/Users/yj.noh/Desktop/risk_ratio.csv", fileEncoding = "utf-8")
head(data)
table(data$is_churn)
table(data$cluster)

set.seed(123) 
data$outcome <- sample(c(0, 1), nrow(data), replace = TRUE, prob = c(0.5, 0.5))
table(data$outcome)

# 이탈/잔존 매칭
set.seed(1234)
mod_1st <- matchit(outcome ~ risk_ratio + is_churn + cluster, data = data , ratio = 2 , caliper = .0001)
dta_1st <- match.data(mod_1st)

dim(dta_1st)
table(dta_1st$outcome)

dta_1st[c(2,3,4,5)] %>% 
  tbl_summary(
    by = outcome,
   type = list(
    risk_ratio ~ "continuous2"
   ),  
    statistic = all_continuous() ~ c("{mean} ({sd})", "{min}, {max}"),
    missing_text = "(Missing value)", 
    digits = list(all_continuous() ~ 2, all_categorical() ~ c(0, 1))
  ) %>%
  add_overall() %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3)) %>%
  bold_labels()


 
ggplot(dta_1st, aes(x=risk_ratio,
                 color=factor(outcome),
                 fill=factor(outcome)))+
  geom_histogram(position='identity',
                 binwidth=0.15,
                 alpha=0.6)+
  scale_fill_viridis(discrete = TRUE)+
  scale_color_viridis(discrete = TRUE)
