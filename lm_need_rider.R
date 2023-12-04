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
# rgn1_nm 
# 전체 
data <- read_excel("/Users/yj.noh/Desktop/need_rider.xlsx")

head(data)
str(data)
dim(data)

train <- data  %>% filter(order_month %in% c(5,6,7,8,9))
test <- data  %>% filter(order_month == 10)

# 전체 라이더 수 예측
model <- lm(rider_cnt ~ rgn1_nm + ord_cnt + ord_single_cnt , data = train)
summary(model)

# test 적용
predict1 <- predict(model, newdata = test)

# MAE
mae1 <- mean(abs(predict1 - test$rider_cnt))
print(mae1) #700
 
# 신규 라이더수 예측
model2 <- lm(rider_cnt_new ~ rgn1_nm + ord_cnt + ord_single_cnt, data = train)
summary(model2)

# test 적용
predict2 <- predict(model2, newdata = test)

# MAE
mae2 <- mean(abs(predict2 - test$rider_cnt_new))
print(mae2) # 165

######################################################################################
# all data
model_all <- lm(rider_cnt ~ rgn1_nm + ord_cnt + ord_single_cnt, data = data)
summary(model_all)

# new data
model_new <- lm(rider_cnt_new ~ rgn1_nm + ord_cnt + ord_single_cnt , data = data)
summary(model_new)


##########################################################################################################################################################
# rgn2_nm
data <- read_excel("/Users/yj.noh/Desktop/rgn2_rider.xlsx")
head(data)
table(data$order_month)

data <- data  %>% filter(rgn1_nm == '서울특별시')

train <- data  %>% filter(order_month %in% c(5,6,7,8,9))
test <- data  %>% filter(order_month == 10)

# 전체 라이더 수 예측
model <- lm(rider_cnt ~ rgn2_nm + ord_cnt + ord_single_cnt + ord_common_cnt +  ord_bmart, data = train)
summary(model) #0.9897

# test 적용
predict1 <- predict(model, newdata = test)

# MAE
mae1 <- mean(abs(predict1 - test$rider_cnt))
print(mae1) #140
 
######################################################################################
# all data
model_all <- lm(rider_cnt ~  rgn2_nm + ord_cnt + ord_single_cnt + ord_common_cnt +  ord_bmart, data = data)
summary(model_all) #0.9896 


##########################################################################################################################################################
# rgn2_day
data <- read_excel("/Users/yj.noh/Desktop/rgn2_rider_day.xlsx")
#data <- read.csv("/Users/yj.noh/Desktop/rgn2_rider_day.csv", fileEncoding = "utf-8")
head(data)

# 요일 변수 넣기 
data$business_day <- as.Date(data$business_day)
data <- data  %>% mutate(month = month(business_day))

table(data$weekday)
table(data$month)

colSums(is.na(data))

# train/test split

train <- data  %>% filter(month %in% c(7,8,9,10))
test <- data  %>% filter(month == 11)

dim(train)
dim(test)

model <- lm(rider_cnt ~ rgn2_nm + ord_single_cnt + ord_common_cnt + ord_bmart + weekday, data = train)
summary(model) #0.9701

# test 적용
predict1 <- predict(model, newdata = test)

# MAE
mae1 <- mean(abs(predict1 - test$rider_cnt))
print(mae1) # 57.98


# model_all
model_all <- lm(rider_cnt ~  rgn2_nm + ord_single_cnt + ord_common_cnt + ord_bmart + weekday, data = data)
summary(model_all) #0.9708

#######################################################################################################################################################
# rgn2 별로 12/1~12/31 만들기

dates <- seq(as.Date("2023-12-01"), as.Date("2023-12-31"), by="day")
unique_rgn2_nm <- unique(data$rgn2_nm)
expanded_data <- expand.grid(rgn2_nm = unique_rgn2_nm, date = dates)

table(expanded_data$rgn2_nm)
table(expanded_data$date)

expanded_data$date <- as.Date(expanded_data$date)
write.csv(expanded_data, "/Users/yj.noh/Desktop/expaneded_data.csv", fileEncoding = "utf-8")
