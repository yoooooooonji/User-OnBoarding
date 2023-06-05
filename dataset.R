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
df <- read_excel("/Users/yj.noh/Desktop/rider_pattern_hour_method.xlsx")

n_distinct(df$rider_user_id) #64954
dim(df) # 66311 

df <- df  %>% group_by(rider_user_id ) %>% mutate(rider_n = n())

n_1 <- df  %>% filter(rider_n==1)
n_2 <- df  %>% filter(rider_n==2)
n_3 <- df  %>% filter(rider_n==3)
n_4 <- df  %>% filter(rider_n==4)

n_distinct(n_1$rider_user_id) #63696
n_distinct(n_2$rider_user_id) # 1161
n_distinct(n_3$rider_user_id) # 95
n_distinct(n_4$rider_user_id) # 2 

# 파생 변수 생성 
hours = c("00", "09", "11", "14", "18", "21")

week_1_dates = c()
for (date in 15:21) {
    for (hour in hours) {
        week_1_dates = c(week_1_dates, paste0("day_05_", sprintf("%02d", date), "_hour_", hour))
    }
}
week_2_dates = c()
for (date in 22:28) {
    for (hour in hours) {
        week_2_dates = c(week_2_dates, paste0("day_05_", sprintf("%02d", date), "_hour_", hour))
    }
}

# 각 주의 일자에 대해 작업이 수행되었는지 확인하고 새로운 변수 생성
df$week_1 <- ifelse(rowSums(df[, week_1_dates, drop = FALSE]) > 0, 1, 0)
df$week_2 <- ifelse(rowSums(df[, week_2_dates, drop = FALSE]) > 0, 1, 0)

# group 분리 
df <- df  %>% 
mutate(group = case_when (
  week_1 == 1 & week_2 == 0 ~ "group_1",
  week_1 == 0 & week_2 == 1 ~ "group_2",
  week_1 == 1 & week_2 == 1 ~ "group_3"))

table(df$group)

# 첫 배달 수행일 비교
df<- df %>% 
  mutate(signup_duration = case_when(
    group %in% c("group_1", "group_3")  ~ as.numeric(as.Date("2023-05-15") - as.Date(first_available_date)), 
    group == "group_2" ~ as.numeric(as.Date("2023-05-22") - as.Date(first_available_date))))
                                                    

###############################################
# rider_n = 1
df_filtered <- df  %>% filter(rider_n == 1)
dim(df_filtered) # 63696

table(df_filtered$group) #8316, 9283, 46097

# 운송수단 라이더 수
table_method <- df_filtered %>%
  group_by(group, delivery_method) %>%
  summarise(count = n_distinct(rider_user_id)) %>%
  mutate(total_in_group = sum(count)) %>%
  mutate(percentage = round(count/total_in_group * 100,2))

# 가입기간
df_filtered <- df_filtered %>% 
  mutate(signup_group = case_when(
    signup_duration <= 7 ~ "1 week",
    signup_duration > 7 &signup_duration <= 30 ~ "1 month",
    signup_duration > 30 & signup_duration <= 60 ~ "2 month",
    signup_duration> 60 & signup_duration <= 90 ~ "3 month",
    signup_duration > 90 & signup_duration<= 180 ~ "6 month",
    TRUE ~ "More than 6 months"))

table_signup <- df_filtered %>% 
  group_by(group, signup_group) %>% 
  summarise(count = n_distinct(rider_user_id)) %>%
  mutate(total_in_group = sum(count)) %>%
  mutate(percentage = round(count/total_in_group * 100,2))

# 운송수단 별 배달건수 
table_dilivery <- df_filtered %>% 
  gather(key = "date_time", value = "count", starts_with("day_")) %>% 
  group_by(group, delivery_method) %>% 
  summarise(total = sum(count, na.rm = TRUE)) %>%
  mutate(total_all_groups = sum(total)) %>% 
  mutate(percentage = round(total/total_all_groups * 100,2))

# 날짜별 합 
df_long <- df_filtered %>% 
  gather(key = "datetime", value = "count", starts_with("day_")) %>% 
  mutate(date = str_sub(datetime, 1, 10)) %>% 
  filter(count>0) %>% 
  group_by(group,delivery_method, date) %>% 
  summarise(total_deliver = sum(count, na.rm = TRUE),
            total_riders = n_distinct(rider_user_id))

# group2, car hour
# 5월 27일, 28일에 해당하는 컬럼 이름들을 추출
selected_dates <- grep("^day_05_(27|28)_", names(df), value = TRUE)

# 필터링하고 조합
car_df <- df_filtered %>%
  filter(group == 'group_2', delivery_method == 'CAR') %>%
  dplyr::select(rider_user_id, all_of(selected_dates)) %>%
  gather(key = 'date_time', value = 'count', -rider_user_id) %>%
  group_by(date_time) %>%
  summarise(total_count = sum(count, na.rm = TRUE),
            unique_rider_count = n_distinct(rider_user_id[count > 0])) 

