# Read an individual block
readBlock = function(text){

  basin = str_replace(string = text[1], pattern = "^:", replacement = "")
  block = text[-1]
  code = str_match(block[1], "[A-Z0-9]{5}")[1]
  block = str_replace(block, "^(:?[^ ]+|:)", "")

  block = str_replace(block, "%", "(perc)")
  block = str_replace(block, "5633' to 5998", "5633' to 5998'")
  block = str_replace(block, "4400' to 5000", "4400' to 5000'") 
  block = str_replace(block, "0784' to 5000", "0784' to 5000'") 


  block = str_replace_all(block, "[;():]", "|")
  block = trimws(block)
  block = str_split(block,"\\|")
  block = as.data.frame(do.call(rbind, block))
  colnames(block) = c("value","calc", "unit", "location")
  block$nwscode = code
  block$basin = basin
  return(block)
}

# Find blocks starting index
findBlocks = function(text){
  index = which(str_detect(text,"^:?[A-Z0-9]{5}"))
  index = index[index > 10]
  index = index - 1
  index = c(index, 1 + which(str_detect(text,"\\.END")))
  return(index)
}

# return a data frame with all blocks
readAllBlocks = function(index, text){
  blocks = lapply(1:(length(index)-1), function(x){
    blockText = text[index[x]:(index[x+1]-2)]
    readBlock(blockText)
  })
  blocks = do.call(rbind, blocks)
  return(blocks)
}



water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}

normcolnams <- function(df) {
  df <- df %>% transmute(value = Value, numval = as.double(value), calc = Calc, unit = as.factor(Units), location = Location, nwscode = as.factor(Code), basin = as.factor(Basin), param = as.factor(param), date = ymd(date))
}


#df <- df %>% mutate(yday = yday(date), year = year(date))

#df_yr <- df %>% group_by(year) %>%  summarize(cumdoy= max(yday, na.rm=T))


add_timeclasses <- function(df) {
  df <- df %>% mutate(year = year(date), yday = yday(date))
  df_yr <- df %>% group_by(year) %>%  summarize(cumdoy= max(yday, na.rm=T))
  df <- merge(df, df_yr, by.x = "year", by.y = "year", all.x=TRUE)
  df <- df %>% mutate(dowy =   ifelse(cumdoy > 365, 
                                      ifelse(yday>274, yday-274, yday+92), 
                                      ifelse(yday>273, yday-273, yday+92)),
                      wy = water_year(date), wm =water_month(date)) %>% select(-cumdoy)
  
}


# define days of awter year for regular and leap years 
#todaydate <- c("2019-10-01")#date(Sys.time()) 
#todaydate <- ymd(todaydate)
#thisyear <- year(todaydate)
#yday <- as.integer(strftime(todaydate, format = "%j"))
#
#if((thisyear %% 4) == 0) {
#  if((thisyear %% 100) == 0) {
#    if((thisyear %% 400) == 0) {
#     diy <- 366 # print(paste(year,"is a leap year"))
#    } else {
#      diy <- 365 #print(paste(year,"is not a leap year"))
#    }
#  } else {
#   diy <- 366 # print(paste(year,"is a leap year"))
#  }
#} else {
#  diy <- 365 #print(paste(year,"is not a leap year"))
#}
#df <- data.frame(todaydate, thisyear, yday, diy)
#df <- df %>% mutate(dowy =   ifelse(diy > 365, 
#                ifelse(yday>274, yday-274, yday+92), 
#                ifelse(yday>273, yday-273, yday+92)))
# 