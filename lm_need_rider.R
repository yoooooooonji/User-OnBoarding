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
data <- read_excel("/Users/yj.noh/Desktop/need_rider.xlsx")

head(data)
str(data)
dim(data)


model <- lm(rider_cnt ~ rgn1_nm + ord_cnt + ord_single_cnt + ord_common_cnt + ord_bmart + ord_bmstore, data = data)
summary(model)
