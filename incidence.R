library(forecast)
library(tseries)
library(ggfortify)
library(lubridate)
library(tidyverse)


d <- read.csv("input/cancer_incidence_jpn.csv",check.names = F)
pop <- read.csv("cancer/input/population_all_jpn.csv",check.names = F)

# get dataframe long
d_long <- pivot_longer(d,cols=c("all","0_4","5_9","10_14","15_19",         
                                "20_24","25_29","30_34","35_39","40_44",         
                                "45_49","50_54","55_59","60_64","65_69",         
                                "70_74","75_79","80_84","over85"),
                       names_to = "age",
                       values_to = "count") %>% 
  filter(code %in% c(1:67)) %>% mutate(year=floor_date(as.Date(as.character(year),format = "%Y"), unit = "months"))

df_pop <- mutate(pop, year=floor_date(as.Date(as.character(year),format = "%Y"), unit = "months")) %>% 
  pivot_longer(cols=c("all","0_4","5_9","10_14","15_19",         
                      "20_24","25_29","30_34","35_39","40_44",         
                      "45_49","50_54","55_59","60_64","65_69",         
                      "70_74","75_79","80_84","over85"),
               names_to = "age",
               values_to = "population")

# filter over 70 and sum
d_long_old <- filter(d_long, age %in% c("70_74","75_79","80_84","over85")) %>% 
  group_by(code,part,year) %>% summarise(count = sum(count))
df_pop_old <- filter(df_pop, age %in% c("70_74","75_79","80_84","over85")) %>%
  group_by(year) %>% summarise(population=sum(population))

# join and adjust population
df <- left_join(d_long_old,df_pop_old,by="year") %>% 
  mutate(adj_count=count/population*1000)

# separate into five indications
df_liver <- ts(df[df$code==8,"adj_count"],start = 1975, frequency = 1)
df_bil <- ts(df[df$code==9,"adj_count"],start = 1975, frequency = 1)
df_lymph <- ts(df[df$code==25,"adj_count"],start = 1975, frequency = 1)
df_gast <- ts(df[df$code==67,"adj_count"],start = 1975, frequency = 1)

# plot
ggtsdisplay(df_liver)
ggtsdisplay(df_lymph)
ggtsdisplay(df_gast)
ggtsdisplay(df_bil)

ggtsdisplay(diff(df_liver))
ggtsdisplay(diff(df_lymph))
ggtsdisplay(diff(df_gast))
ggtsdisplay(diff(df_bil))

# train
df_train <- filter(df, year<"2011-01-01")
df_train_liver <- ts(df_train[df_train$code==8,"adj_count"],start = 1975, frequency = 1)
df_train_lymph <- ts(df_train[df_train$code==25,"adj_count"],start = 1975, frequency = 1)
df_train_gast <- ts(df_train[df_train$code==67,"adj_count"],start = 1975, frequency = 1)
df_train_bil <- ts(df_train[df_train$code==9,"adj_count"],start = 1975, frequency = 1)

# pred
train.liver <- auto.arima(df_train_liver,ic="aic",stepwise = F,approximation = F)
autoplot(df_liver) + autolayer(train.liver$fitted) + autolayer(forecast(train.liver,h=6),series = "ARIMA",alpha=.5)
train.lymph <- auto.arima(df_train_lymph,ic="aic",stepwise = F,approximation = F)
autoplot(df_lymph) + autolayer(train.lymph$fitted) + autolayer(forecast(train.lymph,h=6),series = "ARIMA",alpha=.5)
train.gast <- auto.arima(df_train_gast,ic="aic",stepwise = F,approximation = F)
autoplot(df_gast) + autolayer(train.gast$fitted) + autolayer(forecast(train.gast,h=6),series = "ARIMA",alpha=.5)
train.bil <- auto.arima(df_train_bil,ic="aic",stepwise = F,approximation = F)
autoplot(df_bil) + autolayer(train.bil$fitted) + autolayer(forecast(train.bil,h=6),series = "ARIMA",alpha=.5)

