rm(list = ls())
library(tidyverse)
library(rvest)
library(lubridate)


# webpage to scrape data from, March27's parameter "swe"      
march27_param_swe <- 
  "https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product=swe&year=2019&month=3&day=27&hour=12"

####### snow water equivalent (swe) [inches] ##########

# scrape
scrapedtext <- read_html(march27_param_swe) %>% html_node(".notes") %>%
  html_text() 



swe <- tibble(txt = read_lines(scrapedtext)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% 
      str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% 
      str_sub(start = 2)
  ) 

swe <- swe %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%  
  mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_) %>%
      as.numeric) %>% filter(!is.na(value)) 

swe <- swe %>% mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
                      elevz = gsub(".*(inches))","",txt))  %>%
  select(code, value, basin_desc, elevz)  %>% mutate(elevz = trimws(elevz))

dim(swe) 
#[1] 643   4

head(swe)  
# # A tibble: 6 x 4
# code    value basin_desc               elevz             
# <chr>   <dbl> <chr>                    <chr>             
# 1 ACSC1   0   San Antonio Ck - Sunol   " Entire Basin"   
# 2 ADLC1   0   Arroyo De La Laguna      " Entire Basin"   
# 3 ADOC1   0   Santa Ana R - Prado Dam  " Entire Basin"   
# 4 AHOC1   0   Arroyo Honda nr San Jose " Entire Basin"   
# 5 AKYC1  41.8 SF American nr Kyburz    " Entire Basin"   
# 6 AKYC1   3.9 SF American nr Kyburz    " Base  to 5000' "

#which is what I'm hoping for, except that I'd like the `value` to be 
#<chr> to be able to accommodate the numbers and "NE" values reported

# # A tibble: 6 x 4
# code  value basin_desc               elevz             
# <chr> <chrl> <chr>                    <chr>          

#######  surface sublimation (sub) ##########

# same locations and day, different parameter, "sb", blowing snow 
# sublimation [inches]

march27_param_temp <- "https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product=sb&year=2019&month=3&day=27&hour=12"

scrapedtext <- read_html(march27_param_temp) %>%
  html_node(".notes") %>% html_text() 

sub <- tibble(txt = read_lines(scrapedtext)) %>%
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% 
      str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>%
      str_sub(start = 2)
  ) 

sub <- sub %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%  
  mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_) %>%
      as.numeric) %>% filter(!is.na(value)) 

sub <- sub %>% mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
                      elevz = gsub(".*(inches))","",txt))  %>%
  select(code, value, basin_desc, elevz) %>% mutate(elevz = trimws(elevz))

dim(sub)
#[1] 263   4    #dim[swe] was 643x4

head(sub)

# A tibble: 6 x 4
#code     value   basin_desc                elevz             
#<chr>    <dbl>   <chr>                     <chr>             
#1 ADOC1     0    Santa Ana R - Prado Dam   " Entire Basin"   
#2 ADOC1     0    Santa Ana R - Prado Dam   " Base  to 5000' "
#3 ARCC1     0    Mad River - Arcata        " Entire Basin"   
#4 ARCC1     0    Mad River - Arcata        " Base  to 5000' "
#5 BCAC1     0    Little Truckee - Boca Dam " Entire Basin"   

#So `sub` should be the same size `data.frame` as swe, and 
#sub$value's are supposed to be (as per the source page above: 
# https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product=sb&year=2019&month=3&day=27&hour=12 ):

#head(desired_sub)
# A tibble: 6 x 4
#code        value  basin_desc                elevz             
#<chr>       <chr>  <chr>                     <chr>             
#1 ADOC1     NE     Santa Ana R - Prado Dam   " Entire Basin"   
#2 ADOC1     NE     Santa Ana R - Prado Dam   " Base  to 5000' "
#3 ARCC1     0.000  Mad River - Arcata        " Entire Basin"   
#4 ARCC1     NE     Mad River - Arcata        " Base  to 5000' "
#5 BCAC1    -0.016  Little Truckee - Boca Dam " Entire Basin"   