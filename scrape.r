rm(list = ls())
library(tidyverse)
library(rvest)
library(lubridate)




today <- Sys.Date()
year <- as.character(year(today))
monthnum <- as.character(month(today))
dayofmon <- as.character(day(today))
nohrscparams <- c("swe", "sca", "se", "sm", "sb", "sd", "su")
nohrscparamvars <- c("nohrsc_swe", "nohrsc_sca", "nohrsc_se", "nohrscsm", "nohrsc_sb", "nohrsc_sd", "nohrsc_su")

nohrscparamnam <- c("Snow, Water Equivalent", "Snow, Areal Extent of Basin Snow Cover", "Snow, Average Snowpack Temperature",
                   "Snow, Melt", "Snow, Blowing Snow Sublimation" , "Snow, Depth", "Snow, Surface Sublimation" )
nohrscparamunit <- c("in", "perc", "F", "in", "in", "in", "in")


url1 <- "https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product="

url3 <- "&year="
url4 <- year
url5 <- "&month="
url6 <- monthnum
url7 <- "&day="
url8 <- dayofmon
url9 <- "&hour=12"

######## swe #################

{
url2 <- nohrscparams[1]

nohrs_dayparam_url <- paste0(url1, url2, url3, url4, url5, url6, url7, url8, url9)
nohrs_dayparam_url

#df <- data.frame(matrix(ncol = 4, nrow = 0))
text <- read_html(nohrs_dayparam_url) %>% 
  
  html_node(".notes") %>% 
  html_text() 

df <- tibble(txt = read_lines(text)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% str_sub(start = 2)
  ) %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%   mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_
  ) %>% as.numeric) %>% filter(!is.na(value)) %>%
  mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
         elevz = gsub(".*(inches))","",txt))  %>%
  select( code, value, basin_desc, elevz) %>% mutate(unit = nohrscparamunit[1],
                                                       param = nohrscparams[1],
                                                     param_n = nohrscparamnam[1])

nohrsc_swe <- df  %>% mutate(elevz = trimws(elevz), zone = ifelse(elevz == "Entire Basin", "total", NA))



}
######## areal % #################
{
url2 <- nohrscparams[2]

nohrs_dayparam_url <- paste0(url1, url2, url3, url4, url5, url6, url7, url8, url9)
nohrs_dayparam_url

#df <- data.frame(matrix(ncol = 4, nrow = 0))
text <- read_html(nohrs_dayparam_url) %>% 
  
  html_node(".notes") %>% 
  html_text() 

df <- tibble(txt = read_lines(text)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% str_sub(start = 2)
  ) %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%   mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_
  ) %>% as.numeric) %>% filter(!is.na(value)) %>%
  mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
         elevz = gsub(".*%","",txt))  %>%
  select( code, value, basin_desc, elevz) %>% mutate(unit = nohrscparamunit[2],
                                                     param = nohrscparams[2],
                                                     param_n = nohrscparamnam[2])

nohrsc_sca <- df 
}
######## avg temp #################
{
url2 <- nohrscparams[3]

nohrs_dayparam_url <- paste0(url1, url2, url3, url4, url5, url6, url7, url8, url9)
nohrs_dayparam_url

#df <- data.frame(matrix(ncol = 4, nrow = 0))
text <- read_html(nohrs_dayparam_url) %>% 
  
  html_node(".notes") %>% 
  html_text() 

df <- tibble(txt = read_lines(text)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% str_sub(start = 2)
  ) %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%   mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_
  ) %>% as.numeric) %>% filter(!is.na(value)) %>%
  mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
         elevz = gsub(".*(Degrees F))","",txt))  %>%
  select( code, value, basin_desc, elevz) %>% mutate(unit = nohrscparamunit[3],
                                                       param = nohrscparams[3],
                                                   param_n = nohrscparamnam[3])

nohrsc_se <- df 
}
######## melt#################
{
url2 <- nohrscparams[4]

nohrs_dayparam_url <- paste0(url1, url2, url3, url4, url5, url6, url7, url8, url9)
nohrs_dayparam_url

#df <- data.frame(matrix(ncol = 4, nrow = 0))
text <- read_html(nohrs_dayparam_url) %>% 
  
  html_node(".notes") %>% 
  html_text() 

df <- tibble(txt = read_lines(text)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% str_sub(start = 2)
  ) %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%   mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_
  ) %>% as.numeric) %>% filter(!is.na(value)) %>%
  mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
         elevz = gsub(".*(inches))","",txt))  %>%
  select( code, value, basin_desc, elevz) %>% mutate(unit = nohrscparamunit[4], 
                                                       param = nohrscparams[4],
                                                   param_n = nohrscparamnam[4])

nohrsc_sm <- df 
}
######## blowing sublimation #################
{
url2 <- nohrscparams[5]

nohrs_dayparam_url <- paste0(url1, url2, url3, url4, url5, url6, url7, url8, url9)
nohrs_dayparam_url

#df <- data.frame(matrix(ncol = 4, nrow = 0))
text <- read_html(nohrs_dayparam_url) %>% 
  
  html_node(".notes") %>% 
  html_text() 

df <- tibble(txt = read_lines(text)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% str_sub(start = 2)
  ) %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%   mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_
  ) %>% as.numeric) %>% filter(!is.na(value)) %>%
  mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
         elevz = gsub(".*(inches))","",txt))  %>%
  select( code, value, basin_desc, elevz) %>% mutate(unit = nohrscparamunit[5], 
                                                       param = nohrscparams[5],
                                                    param_n = nohrscparamnam[5])

nohrsc_sb <- df 
}
######## depth #################
{
url2 <- nohrscparams[6]

nohrs_dayparam_url <- paste0(url1, url2, url3, url4, url5, url6, url7, url8, url9)
nohrs_dayparam_url

#df <- data.frame(matrix(ncol = 4, nrow = 0))
text <- read_html(nohrs_dayparam_url) %>% 
  
  html_node(".notes") %>% 
  html_text() 

df <- tibble(txt = read_lines(text)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d")   %>% str_extract("[:digit:]+\\.?[:digit:]"),
   basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% str_sub(start = 2)
 ) %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%   mutate(value = case_when(
   !is.na(val) ~ val,
   !is.na(wo_code) ~ wo_code,
   TRUE ~ NA_character_
 ) %>% as.numeric) %>% filter(!is.na(value)) %>%
 mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
        elevz = gsub(".*(inches))","",txt))  %>%
 select( code, value, basin_desc, elevz) %>% mutate(unit = nohrscparamunit[6], 
                                                      param = nohrscparams[6],
                                                  param_n = nohrscparamnam[6])

nohrsc_sd <- df 
}
######## surf sub #################
{
url2 <- nohrscparams[7]

nohrs_dayparam_url <- paste0(url1, url2, url3, url4, url5, url6, url7, url8, url9)
nohrs_dayparam_url

#df <- data.frame(matrix(ncol = 4, nrow = 0))
text <- read_html(nohrs_dayparam_url) %>% 
  
  html_node(".notes") %>% 
  html_text() 

df <- tibble(txt = read_lines(text)) %>%
  
  mutate(
    row = row_number(),
    with_code = str_extract(txt, "^[A-z0-9]{5}\\s+\\d+(\\.)?\\d"),
    wo_code = str_extract(txt, "^:?\\s+\\d+(\\.)?\\d") %>% str_extract("[:digit:]+\\.?[:digit:]"),
    basin_desc = if_else(!is.na(with_code), lag(txt, 1), NA_character_) %>% str_sub(start = 2)
  ) %>% separate(with_code, c("code", "val"), sep = "\\s+") %>%   mutate(value = case_when(
    !is.na(val) ~ val,
    !is.na(wo_code) ~ wo_code,
    TRUE ~ NA_character_
  ) %>% as.numeric) %>% filter(!is.na(value)) %>%
  mutate(code = zoo::na.locf(code), basin_desc = zoo::na.locf(basin_desc) ,
         elevz = gsub(".*(inches))","",txt))  %>%
  select( code, value, basin_desc, elevz) %>% mutate(unit = nohrscparamunit[7], 
                                                       param = nohrscparams[7],
                                                   param_n = nohrscparamnam[7])

nohrsc_su <- df 
}

######## dbug surf sub #################

 
