#install required packages and load libraries 
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes)


# function to calculate average of specific aggregate and return as tibble------------------
calcAverage <- function(df, time,nameofAggregate){
  
  df2 <- df %>% 
    group_by(aggregate) %>%
    filter(period == time) %>% 
    summarise("Average" = mean(percent,na.rm = TRUE))
  
  Average <- df2 %>% filter(aggregate == nameofAggregate) %>% select(Average)
  
  return(Average)
}

#function for importing and setting column names--------------------------------------------
createTibbleGrowth <- function(xlsFile,xlsSheet,numRowsSkip,numRowsSkipHeader){
  
  headers_temp <- read_excel(xlsFile, sheet = xlsSheet, 
                             col_names = FALSE, skip = numRowsSkipHeader, n_max = 1)
  
  headers_temp[1] <- "date"
  
  temp.raw <- read_excel(xlsFile, sheet = xlsSheet, 
                         
                         
                         col_types = c("guess"),
                         skip = numRowsSkip)
  
  temp.raw <- temp.raw %>% mutate_if(is.character, as.numeric)
  
  colnames(temp.raw) = headers_temp
  
  if(xlsFile == "d01hist.xls"){
    names(temp.raw) <- gsub("; Monthly growth", ".m", names(temp.raw))
    names(temp.raw) <- gsub("; 12-month ended growth", ".y", names(temp.raw))
    growth.raw <<- temp.raw
  }
  else if(xlsFile == "d02hist.xls"){
    credit.raw <<- temp.raw
  }
  else if(xlsFile == "d03hist.xls"){
    money.raw <<- temp.raw
  }
  
}

#functions---------------------------------------------------------------------------------- 
rawPlot <- function(df, t, xvar = "date",yvar = "dollars"){
  
  df <- df %>% drop_na()

  df2 <- df %>% filter(aggregate == t)
  

  
  if (str_detect(t,"Seasonally adjusted")){
    head = paste0(str_sub(t,1,-22), " ", "-", " ", "$Billions") 
  }
  else{
    head = paste0(t, " ", "-", " ", "$Billions")
  }
  
  if (str_detect(t,"Other borrowings from private sector by AFIs")){
    head = paste0("Other borrowings from private sector ", "-", " ", "$Billions")
  }
  
  
  df_plot <- 
    ggplot(df2,aes_string(x = xvar, y = yvar)) +
    geom_line() +
    scale_y_continuous(trans = "log10") + 
    stat_smooth(method = 'lm', colour = 'purple', se = F, size = 0.4) +
    theme_bw() +
    labs(y = "$Billions (AUD)", x = "Year", title = head)
  
  return(df_plot)
}

#create function for % change YOY plot------------------------------------------------------ 
pctChangePlotRaw <- function(df, df2, y, t, xvar = "date", yvar = "percent"){
  

  df <- df %>% drop_na()
  
  df <- df %>% filter(period == y & aggregate == t)
  
  df3 <- df2 %>% filter(aggregate == t)
  
  head = paste0(t, " ", "-", " ", "% Change")
  
  if (str_detect(t,"Other borrowings from private sector by AFIs")){
    head = paste0("Other borrowings from private sector ", "-", " ", "% Change")
  }
  
  df_plot <- 
    ggplot(df,aes_string(x = xvar, y = yvar)) +
    geom_line() +
    geom_smooth(size = 0.3, color = 'purple') +
    geom_hline(data = df3, aes(yintercept = Average), linetype = 3, colour = "orange") +
    geom_hline(yintercept = 0, linetype = 1, colour = "grey") +
    theme_bw() +
    labs(y = "% Change yoy", x = "Year", title = head)
  
  return(df_plot)
}

# % change plot with Growth data function for money data------------------------------------
pctChangePlotGrowth <- function(df, df2, df3, y, t, xvar = "date", yvar = "percent"){
  df <- df %>% drop_na
  df2 <- df2 %>% drop_na()
  df3 <- df3 %>% drop_na()
  
  
  df <- df %>% filter(period == y & aggregate == t)
   
  df4 <- df2 %>% filter(aggregate == t)
  
  head = paste0(t, " ", "-", " ", "% Change")
  
  df_plot <- 
    ggplot(df,aes_string(x = xvar, y = yvar)) +
    geom_line(colour = 'darkgrey', linetype = 3, size = 0.3) +
    geom_smooth(size = 0.3, color = 'purple') +
    geom_hline(data = df4, aes(yintercept = Average), linetype = 3, colour = "orange") +
    geom_hline(yintercept = 0, linetype = 1, colour = "grey") +
    theme_bw() +
    labs(y = "% Change yoy", x = "Year", title = head) +
    geom_line(data = df3, colour = 'black', linetype = 1)
    
  return(df_plot)

}


# % change plot with Growth data function for credit data----------------------------------
pctChangePlotGrowthCredit <- function(df, df2, df3, y, t, xvar = "date", yvar = "percent"){
  df <- df %>% drop_na
  df2 <- df2 %>% drop_na()
  df3 <- df3 %>% drop_na()
  
  df <- df %>% filter(period == y & aggregate == t)
  
  if (str_detect(t,"Seasonally adjusted")){
    head = paste0(str_sub(t,1,-22), " ", "-", " ", "% Change")
    t2 <- str_sub(t,1,-22)
  }else{
    head = paste0(t, " ", "-", " ", "% Change")
    t2 <- t
  }
  
  
  df2 <- df2 %>% filter(aggregate == t2)
  
  df3 <- df3 %>% filter(aggregate == t2 & period == y)
  
  
  df_plot <- 
    ggplot(df,aes_string(x = xvar, y = yvar)) +
    geom_line(colour = 'darkgrey', linetype = 3, size = 0.3) +
    geom_smooth(size = 0.3, color = 'purple') +
    geom_hline(data = df2, aes(yintercept = Average), linetype = 3, colour = "orange") +
    geom_hline(yintercept = 0, linetype = 1, colour = "grey") +
    theme_bw() +
    labs(y = "% Change yoy", x = "Year", title = head) +
    geom_line(data = df3, colour = 'black', linetype = 1)

  return(df_plot)
  
}

#api username and key for plotly 
Sys.setenv("plotly_username"="demystifyingmoney")
Sys.setenv("plotly_api_key"="ytrNWCDfNNPMIBDupYss")

#download money aggreagate excel files from RBA website and save them in project directory as .xls files. 
download.file("https://www.rba.gov.au/statistics/tables/xls/d01hist.xls",destfile = "./d01hist.xls", mode = "wb")
download.file("https://www.rba.gov.au/statistics/tables/xls/d02hist.xls",destfile = "./d02hist.xls", mode = "wb")
download.file("https://www.rba.gov.au/statistics/tables/xls/d03hist.xls",destfile = "./d03hist.xls", mode = "wb")

#call function to create tibble from downloaded .xls file
createTibbleGrowth("d01hist.xls","Data",11,1)
createTibbleGrowth("d02hist.xls","Data",10,1)
createTibbleGrowth("d03hist.xls","Data",11,1)

#combine money aggregate tibble and credit aggregate tibble into one giant tibble
rba.raw <- full_join(money.raw,credit.raw,by = "date")

rba.tidy <- rba.raw %>%
  gather(`Currency`,`Transaction Deposits with ADIs`,`M1`,`Certificates of deposit issued by ADIs`,
         `Non-Transaction Deposits with ADIs`,`M3`,`Other borrowings from private sector by AFIs`,
         `Broad money`,`Currency: Seasonally adjusted`,`M1: Seasonally adjusted`,`M3: Seasonally adjusted`,
         `Offshore borrowings by AFIs`,`Broad money: Seasonally adjusted`,`Money base`,
         "Loans and advances; Banks","Loans and advances; NBFIs","Loans and advances; AFIs",
         "Bills on issue","Narrow credit","Narrow credit; Seasonally adjusted","Credit; Total",
         "Credit; Total; Seasonally adjusted","Credit; Owner-occupier housing",
         "Credit; Owner-occupier housing; Seasonally adjusted","Credit; Investor housing",
         "Credit; Investor housing; Seasonally adjusted","Credit; Other personal","Credit; Other personal; Seasonally adjusted",
         "Credit; Business","Credit; Business; Seasonally adjusted","Lending to the government sector by AFIs",
         "Net switching of housing loan purpose", key = 'aggregate', value = 'dollars')

#gather rba.raw tibble into tidy format and add %change columns for each money aggregate. Both MoM and YoY. 
#and then gather Mom and YoY into new period variable (i.e. month,year) and leave only percent
rba.tidy.pct <- rba.tidy %>%
  group_by(aggregate) %>%
  mutate(pct_mom = (dollars/lag(dollars,1) -1) * 100) %>%
  mutate(pct_yoy = (dollars/lag(dollars,12) -1) * 100) %>% 
  group_by(date,aggregate) %>% 
  select(pct_mom,pct_yoy) %>% 
  mutate(month = pct_mom) %>% 
  mutate(year = pct_yoy) %>% 
  select(-c(pct_mom,pct_yoy)) %>%
  gather("month", "year", key = period, value = percent) %>% 
  as_tibble()

#spread mom pct data into wide format
rba.raw.pct.mom <- rba.tidy %>%
  group_by(aggregate) %>%
  mutate(pct_mom = (dollars/lag(dollars,1) -1) * 100) %>%
  mutate(pct_yoy = (dollars/lag(dollars,12) -1) * 100) %>%
  group_by(date,aggregate) %>% 
  select(pct_mom) %>% 
  spread(aggregate, pct_mom) %>% 
  as_tibble()
#spread yoy pct data into wide format
rba.raw.pct.yoy <- rba.tidy %>% 
  group_by(aggregate) %>%
  mutate(pct_mom = (dollars/lag(dollars,1) -1) * 100) %>%
  mutate(pct_yoy = (dollars/lag(dollars,12) -1) * 100) %>%
  group_by(date,aggregate) %>% 
  select(pct_yoy) %>% 
  spread(aggregate, pct_yoy) %>% 
  as_tibble() 
#join mom and yoy pct change data (wide format)
rba.raw.pct <- full_join(rba.raw.pct.mom,rba.raw.pct.yoy,by = "date", suffix = c(".m",".y"))

#create tidy growth tibble
#write.table(names(growth.raw),"tmp.txt",sep="\t",row.names=FALSE)
rba.tidy.growth <- growth.raw %>% 
  gather("Credit; Housing.m","Credit; Housing.y","Credit; Owner-occupier housing.m","Credit; Owner-occupier housing.y","Credit; Investor housing.m",
        "Credit; Investor housing.y","Credit; Other personal.m","Credit; Other personal.y",
        "Credit; Business.m","Credit; Business.y","Credit; Total.m","Credit; Total.y",
        "M3.m","M3.y","Broad money.m","Broad money.y", key = 'aggregate', value = 'percent') %>% 
  mutate(period = case_when(
    endsWith(aggregate, ".m") ~ "month",
    endsWith(aggregate, ".y") ~ "year",
    TRUE                      ~NA_character_
  ))

#reorder column names for the rba growth tables
rba.tidy.growth <- rba.tidy.growth[, c("date","aggregate","period","percent")]

#tidy growth data 
growth.raw.month <- rba.tidy.growth %>%
  filter(period == "month") %>% 
  pivot_wider(names_from = aggregate, values_from = percent)
names(growth.raw.month) <- gsub("[.]m", "", names(growth.raw.month))   
#........
growth.raw.year <- rba.tidy.growth %>%
  filter(period == "year") %>% 
  pivot_wider(names_from = aggregate, values_from = percent)
names(growth.raw.year) <- gsub("[.]y", "", names(growth.raw.year))   
#........
growth.tidy.month <- growth.raw.month %>% 
  gather("Credit; Housing", "Credit; Owner-occupier housing","Credit; Investor housing","Credit; Other personal",
         "Credit; Business","Credit; Total",
         "M3","Broad money", key = 'aggregate', value = 'percent')
#.......
growth.tidy.year <- growth.raw.year %>% 
  gather("Credit; Housing","Credit; Owner-occupier housing","Credit; Investor housing","Credit; Other personal",
         "Credit; Business","Credit; Total",
         "M3","Broad money", key = 'aggregate', value = 'percent')

##gather growth data into tidy tibble and gather percent.x(month) 
##and percent.y(year) into new period variable (i.e. month,year) and leave only percent
growth.tidy.1 <- inner_join(growth.tidy.month,growth.tidy.year, by = c("date","aggregate"))                  
growth.tidy <- growth.tidy.1 %>% 
  select(-c(period.x,period.y)) %>% 
  group_by(date,aggregate) %>% 
  select(percent.x,percent.y) %>% 
  mutate(month = percent.x) %>% 
  mutate(year = percent.y) %>% 
  select(-c(percent.x,percent.y)) %>%
  gather("month", "year", key = period, value = percent) %>% 
  as_tibble()

#remove undwanted tibbles 
rm(growth.tidy.1)
rm(growth.raw.month)
rm(growth.raw.year)
rm(growth.tidy.month)
rm(growth.tidy.year)
rm(rba.raw.pct.yoy)
rm(rba.raw.pct.mom)
rm(rba.raw.pct)
rm(rba.tidy.growth)
rm(credit.raw)
rm(growth.raw)
rm(money.raw)
rm(rba.raw)


#calculate average pct change year on year for all aggregates
avg_pct_yoy <- rba.tidy.pct %>% 
  group_by(aggregate) %>%
  filter(period == 'year') %>% 
  summarise(Average = mean(percent,na.rm = TRUE))


#calculate avgerage pct (growth data) year on year for all aggregates
avg_pct_growth <- growth.tidy %>% 
  group_by(aggregate) %>%
  filter(period == 'year') %>% 
  summarise(Average = mean(percent,na.rm = TRUE))

#create data table of growth data for M3 and Broad Money to be used to overlay on percent you graphs
growth.M3 <- growth.tidy %>% 
  filter(period == 'year', aggregate == 'M3') %>% 
  select(-period)

growth.broadmoney <- growth.tidy %>% 
  filter(period == 'year', aggregate == 'Broad money') %>% 
  select(-period)

#-------------------------------------------------------------------------------------------------------------
#---------------------------------------------MONETARY AGGREGATE PLOTS----------------------------------------
#-------------------------------------------------------------------------------------------------------------

#create individual plots for raw dollars vs. time  calling rawPlot function and piping to plotly and uploading
  rawPlot(rba.tidy, "Currency") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-Currency")

  rawPlot(rba.tidy, "Transaction Deposits with ADIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-plot.dollars.Transaction_Deposits_with_ADIs")

  rawPlot(rba.tidy, "M1") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-M1")

  rawPlot(rba.tidy, "Certificates of deposit issued by ADIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-Certificates_of_deposit_issued_by_ADIs")

  rawPlot(rba.tidy, "Non-Transaction Deposits with ADIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-Non_Transaction_Deposits_with_ADIs")

  rawPlot(rba.tidy, "M3") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-M3")

  rawPlot(rba.tidy, "Other borrowings from private sector by AFIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-Other_borrowings_from_private_sector_by_AFIs")

  rawPlot(rba.tidy, "Broad money") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-dollars-BroadMoney")

#create individual plots for yoy percent change vs. time calling pctChangePlot function
  pctChangePlotRaw(rba.tidy.pct, avg_pct_yoy, 'year', "Currency") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-Currency")

  pctChangePlotRaw(rba.tidy.pct, avg_pct_yoy, 'year',  "Transaction Deposits with ADIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-Transaction_Deposits_with_ADIs")

  pctChangePlotRaw(rba.tidy.pct, avg_pct_yoy, 'year', "M1") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-M1")

  pctChangePlotRaw(rba.tidy.pct, avg_pct_yoy, 'year', "Certificates of deposit issued by ADIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-plot.pct.change.yoy_Certificates_of_deposit_issued_by_ADIs")

  pctChangePlotRaw(rba.tidy.pct, avg_pct_yoy, 'year', "Non-Transaction Deposits with ADIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-Non_Transaction_Deposits_with_ADIs")

  pctChangePlotGrowth(rba.tidy.pct, avg_pct_yoy, growth.M3, 'year', "M3") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-M3")

  pctChangePlotRaw(rba.tidy.pct, avg_pct_yoy, 'year', "Other borrowings from private sector by AFIs") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-Other_borrowings_from_private_sector_by_AFIs")

  pctChangePlotGrowth(rba.tidy.pct, avg_pct_yoy, growth.broadmoney, 'year', "Broad money") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-money-pct-change-BroadMoney")

#----------------------------------------------------------------------------------------------------------------------
#----------------------------------------CREDIT AGGREGATE PLOTS--------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------

#create individual plots for raw dollars vs. time  calling rawPlot function and piping to plotly and uploading
rawPlot(rba.tidy, "Credit; Total; Seasonally adjusted") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-total-dollars") 
  
rawPlot(rba.tidy, "Credit; Owner-occupier housing; Seasonally adjusted") %>%
    ggplotly() %>% 
  api_create(filename = "r-rba-credit-owner-dollars")   
  
rawPlot(rba.tidy, "Credit; Investor housing; Seasonally adjusted") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-investor-dollars")

rawPlot(rba.tidy, "Net switching of housing loan purpose") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-switching-dollars") 

rawPlot(rba.tidy, "Credit; Business; Seasonally adjusted") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-business-dollars")

rawPlot(rba.tidy, "Credit; Other personal; Seasonally adjusted") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-personal-dollars") 

#create individual plots for % change vs. time  calling pctChangePlotGrowthCredit function and piping to plotly and uploading
pctChangePlotGrowthCredit(rba.tidy.pct, avg_pct_growth, growth.tidy, 'year', "Credit; Total; Seasonally adjusted") %>% 
  ggplotly() %>%
  api_create(filename = "r-rba-credit-total-pct-change")

pctChangePlotGrowthCredit(rba.tidy.pct, avg_pct_growth, growth.tidy, 'year', "Credit; Owner-occupier housing; Seasonally adjusted") %>% 
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-owner-pct-change")

pctChangePlotGrowthCredit(rba.tidy.pct, avg_pct_growth, growth.tidy, 'year', "Credit; Investor housing; Seasonally adjusted") %>% 
  ggplotly() %>%
  api_create(filename = "r-rba-credit-investor-pct-change")

pctChangePlotRaw(rba.tidy.pct, avg_pct_yoy, 'year', "Net switching of housing loan purpose") %>% 
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-switching-pct-change")

pctChangePlotGrowthCredit(rba.tidy.pct, avg_pct_growth, growth.tidy, 'year', "Credit; Business; Seasonally adjusted") %>% 
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-business-pct-change")

pctChangePlotGrowthCredit(rba.tidy.pct, avg_pct_growth, growth.tidy, 'year', "Credit; Other personal; Seasonally adjusted") %>% 
  ggplotly() %>% 
  api_create(filename = "r-rba-credit-personal-pct-change")

#combined plot money and credit--------------------------------------------------------------
#============================================================================================

#function for plotting two growth aggregates on the same graph
pctChangePlotGrowthCombined <- function(df, df2, timePeriod, aggregate1, aggregate2, aggregate3, colour1, colour2, colour3, xvar = "date", yvar = "percent"){
  
  if (aggregate3 == F & colour3 == F){
  
  df3 <- df %>% filter(period == timePeriod & aggregate == aggregate1)
  
  df4 <- df %>% filter(period == timePeriod & aggregate == aggregate2)
  
  #calculate average for given aggregate calling calcAverageAsVector function
  aveAggregate1 <- calcAverage(df, timePeriod, aggregate1)
  aveAggregate2 <- calcAverage(df, timePeriod, aggregate2)
  
  head = paste0(aggregate1, " ", "/", " ", aggregate2, " ", "-", " ", "% Change")
  
  df_plot <- 
    ggplot(df3,aes_string(x = xvar, y = yvar)) +
    geom_line(colour = colour1, linetype = 1, ) +
    geom_text(aes(x = as.POSIXct("1980-01-31"), y = 6), label = aggregate1, color = colour1) +
    geom_text(aes(x = as.POSIXct("1980-01-31"), y = 4), label = aggregate2, color = colour2) +
    geom_line(data = df4, colour = colour2, linetype = 1) +
    geom_hline(data = aveAggregate1, aes(yintercept = Average), linetype = 3, colour = colour1,size = 0.3) +
    geom_hline(data = aveAggregate2, aes(yintercept = Average), linetype = 3, colour = colour2,size = 0.3) +
    theme_bw() +
    labs(y = "% Change yoy", x = "Year", title = head)

  return(df_plot)
  
  }else{
    
    df3 <- df %>% filter(period == timePeriod & aggregate == aggregate1)
    
    df4 <- df %>% filter(period == timePeriod & aggregate == aggregate2)
    
    df5 <- df %>% filter(period == timePeriod & aggregate == aggregate3)
    
    #calculate average for given aggregate calling calcAverageAsVector function
    aveAggregate1 <- calcAverage(df, timePeriod, aggregate1)
    aveAggregate2 <- calcAverage(df, timePeriod, aggregate2)
    aveAggregate3 <- calcAverage(df, timePeriod, aggregate3)
    
    head = paste0(aggregate1,"/", aggregate2,"/",aggregate3, " ", "-", " ", "% Change")
    
    df_plot <- 
      ggplot(df3,aes_string(x = xvar, y = yvar)) +
      geom_line(colour = colour1, linetype = 1, ) +
      geom_line(data = df4, colour = colour2, linetype = 1) +
      geom_line(data = df5, colour = colour3, linetype = 1) +
      geom_text(aes(x = as.POSIXct("1980-01-31"), y = 6), label = aggregate1, color = colour1) +
      geom_text(aes(x = as.POSIXct("1980-01-31"), y = 4), label = aggregate2, color = colour2) +
      geom_text(aes(x = as.POSIXct("1980-01-31"), y = 2), label = aggregate3, color = colour3) +
      geom_hline(data = aveAggregate1, aes(yintercept = Average), linetype = 3, colour = colour1,size = 0.3) +
      geom_hline(data = aveAggregate2, aes(yintercept = Average), linetype = 3, colour = colour2,size = 0.3) +
      geom_hline(data = aveAggregate3, aes(yintercept = Average), linetype = 3, colour = colour3,size = 0.3) +
      theme_bw() +
      labs(y = "% Change yoy", x = "Year", title = head)
    
    return(df_plot)
     }
 }

gg <- pctChangePlotGrowthCombined(growth.tidy, avg_pct_growth, "year", "Broad money", "Credit; Total", "M3", 'purple', 'black', "orange") %>% 
  ggplotly() 

gg$x$data[[4]]$hoverinfo <- 'none'
gg$x$data[[5]]$hoverinfo <- 'none'
gg$x$data[[6]]$hoverinfo <- 'none'

gg %>% 
  api_create(filename = "r-rba-credit-broadmoney-m3-pct-change")