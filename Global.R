library(fpp3)
library(shiny)
library(shinythemes)
library(regclass)
library(lubridate)
library(tidyverse)
library(forcats)
data("us_gasoline")



#us_gasoline$Week <- as.Date(us_gasoline$Week, format = "%m/%d/%y")


# us_gasoline_month <- us_gasoline %>%
# group_by(Month = yearmonth(Week)) %>%
# summarise_all(mean)
# 
# us_gasoline_month <- us_gasoline_month %>%
#   mutate(Month = yearmonth(Month)) %>%
#   as_tsibble(key = Barrels, index = Month)



us_gas_mth <- us_gasoline %>%
  
  index_by(Month = yearmonth(Week)) %>%
  
  summarise(
    
    BarrelsSum = sum(Barrels),
    
    BarrelsMean = mean(Barrels),
    
    BarrelsMedian = median(Barrels)
    
  )



interval(us_gasoline)

interval(us_gas_mth)



us_gasoline %>%
  
  autoplot()



us_gas_mth %>%
  
  autoplot(BarrelsSum)



us_gas_mth %>%
  
  autoplot(BarrelsMean)



us_gas_mth %>%
  
  autoplot(BarrelsMedian)



us_gas_mth %>%
  
  select(-BarrelsSum) %>%
  
  pivot_longer(-Month) %>%
  
  autoplot()
#us_gasoline$Year <- format(as.Date(us_gasoline$Week, format="%d/%m/%Y"),"%Y")



lambda <- us_gasoline %>%
  features(Barrels, features = guerrero) %>% 
  pull(lambda_guerrero)