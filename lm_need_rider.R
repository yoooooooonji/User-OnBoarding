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

data <- data  %>% mutate(bm1 = (ord_single_cnt + ord_common_cnt),
                        bm1_food = (ord_single_cnt + ord_common_cnt + ord_bmart))


train <- data  %>% filter(order_month %in% c(5,6,7,8,9))
test <- data  %>% filter(order_month == 10)


# 전체 라이더 수 예측
model <- lm(rider_cnt ~ rgn1_nm + bm1 + ord_single_cnt  + ord_common_cnt , data = train)
summary(model)

# test 적용
predict1 <- predict(model, newdata = test)

# MAE
mae1 <- mean(abs(predict1 - test$rider_cnt))
print(mae1) #700
 
# 신규 라이더수 예측
model2 <- lm(rider_cnt_new ~ rgn1_nm + ord_cnt + ord_single_cnt + ord_common_cnt, data = train)
summary(model2)

# test 적용
predict2 <- predict(model2, newdata = test)

# MAE
mae2 <- mean(abs(predict2 - test$rider_cnt_new))
print(mae2) # 267


# all data
model_all <- lm(rider_cnt ~ rgn1_nm + bm1 + ord_single_cnt, data = data)
summary(model_all)

# new data
model_new <- lm(rider_cnt_new ~ rgn1_nm + bm1 + ord_single_cnt  , data = data)
summary(model_new)


