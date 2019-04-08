rm(list = ls()) 
library(tidyverse)
library(rvest)
library(lubridate)
library(gganimate)
library(gifski)
library(transformr)
library(tweenr)
library(sf)
library(rgdal)
library(ggrepel)
#library(cowplot)

setwd("~/Documents/Rspat")
source("fun_defs.r")
source("df_init.r")
params <- c ("swe",
             "sca", 
           "se", 
             "sm", 
             "sb", 
             "sd", 
             "su")
paramnams <- c("Snow Water Equivalent [in]",
                "Areal Extent of Basin Snow Cover [%]",
                "Average Snowpack Temperature [F]",
                "Snow Melt [in]", 
                "Blowing Snow Sublimation [in]" , 
                "Snow Depth [in]", 
                "Snow Surface Sublimation [in]" )


start_date <- ymd("2018-10-01")
end_date <- ymd("2019-04-07")

dates <- seq(start_date, end_date, by = "days") %>% rev()



for (i in 1:length(dates)) { 
  
  year <- as.character(year(dates[i]))
  monthnum <- as.character(month(dates[i]))
  dayofmon <- as.character(day(dates[i]))
  
        for (j in 1:length(params)) {

    url1 <- "https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product="
    url2 <- params[j]
    url3 <- "&year="
    url4 <- year
    url5 <- "&month="
    url6 <- monthnum
    url7 <- "&day="
    url8 <- dayofmon
    url9 <- "&hour=12"
    
    ####### snow water equivalent (swe) [inches] ##########
    
    #swe = "https://www.nohrsc.noaa.gov/shef_archive/index.html?rfc=cnrfc&product=swe&year=2019&month=3&day=27&hour=12"
    urlfin <- paste0(url1, url2, url3, url4, url5, url6, 
                                  url7, url8, url9)
    # scrape
    #if (url8 == 1 | url8 == 5 | url8 == 10 | url8 == 15 | url8 == 20 | url8 == 25 ) { Sys.sleep(0.5) }
    if((url8 %% 2) == 0) {Sys.sleep(1) }
    if((url8 %% 9) == 0) {Sys.sleep(3) }
   # if (url8 == 3) { Sys.sleep(3.5) }

    scrapedtext = html_text(html_node(read_html(urlfin),".notes"))
    scrapedtext = unlist(str_split(scrapedtext,"\n"))
    block_index = findBlocks(scrapedtext)
    dftemp_i = readAllBlocks(block_index, scrapedtext) %>% mutate( param = params[j], date = dates[i])
    

    df <- rbind(df,dftemp_i) %>% unique()

 
        }  
}

write_csv(df, "wy2019_allp_thruApr7.csv")
