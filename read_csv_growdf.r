rm(list = ls()) 
library(tidyverse)
library(lubridate)
setwd("~/Documents/Rspat")
source("fun_defs.r")

#2018
normcolnams <- function(df) {
       df <- df %>% transmute(value = Value, calc = Calc, unit = Units, location = Location, nwscode = Code, basin = Basin, param, date)
}
{
df1 <- read_csv("2018-08-28_to_2018-09-30.csv") %>% normcolnams()
df2 <- read_csv("3_2018.csv") %>% normcolnams()
df3 <- read_csv("4_2018.csv") %>% normcolnams()
df4 <- read_csv("5_2018.csv") %>% normcolnams()
df5 <- read_csv("6_2018.csv") %>% normcolnams()
df6 <- read_csv("7_2018.csv") %>% normcolnams()
df7 <- read_csv("8_2018.csv") %>% normcolnams()
df8 <- read_csv("3_2018.csv") %>% normcolnams()
df9 <- read_csv("wy2018_allp_10.1.17thru3.01.18.csv") %>% normcolnams()

#2019
df10 <- read_csv("2019-04-07_to_2019-04-11.csv")
df11 <- read_csv("wy2019_allp_thruApr7.csv")


df <- rbind(df1, df2 , df3, df4, df5, df6, df7,df8, df9, df10, df11) 
}
df_1 <- df %>% unique()
df_2 <- df_1 %>% mutate(valnum = as.double(value))
df_3 <- df_2 %>% mutate(calc = as.factor(calc), unit = as.factor(unit), location = as.factor(location),
                        nwscode = as.factor(nwscode), param = as.factor(param), date = ymd(date))
head(df_3)

df_4 <- df_3 %>% filter( nwscode == "RBBC1" | nwscode == "FRAC1", location == "Entire Basin", param == "sb"| param == "sm" | param == "swe")
df_5 <- add_timeclasses(df_4)
ggplot(df_5, aes(dowy, valnum,color = nwscode)) + geom_line() + facet_grid(param~wy, scales = "free")
p1
