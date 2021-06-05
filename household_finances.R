#install required packages and load libraries 
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

if (!require(readxl)) install.packages('readxl')
library(readxl)

if (!require(plotly)) install.packages('plotly')
library(plotly)

if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes)

options(scipen=999)

#function for importing and setting column names--------------------------------------------
createTibbleGrowth <- function(xlsFile,xlsSheet,numRowsSkip,numRowsSkipHeader){
  
  headers_temp <- read_excel(xlsFile, sheet = xlsSheet, 
                             col_names = FALSE, skip = numRowsSkipHeader, n_max = 1)
  headers <<- headers_temp
  
  headers_temp[1] <- "date"
  
  temp.raw <- read_excel(xlsFile, sheet = xlsSheet, 
                         
                         
                         col_types = c("guess"),
                         skip = numRowsSkip)
  
  temp.raw <- temp.raw %>% mutate_if(is.character, as.numeric)
  
  colnames(temp.raw) = headers_temp
  
  household.raw <<- temp.raw
  
}

#download money aggreagate excel files from RBA website and save them in project directory as .xls files. 
download.file("https://www.rba.gov.au/statistics/tables/xls/e02hist.xls",destfile = "./e02hist.xls", mode = "wb")

createTibbleGrowth("e02hist.xls","Data",10,1)

#gather into tibble 
household.tidy <- household.raw %>%
  gather("Household debt to assets","Housing debt to housing assets","Household debt to income",
         "Housing debt to income","Owner-occupier housing debt to income","Household assets to income","Housing assets to income",
         "Household financial assets to income","Household interest payments to income",
         "Housing interest payments to income" , key = 'aggregate', value = 'percent')

household.tidy


household.pct.yoy <- household.tidy %>%
  group_by(aggregate) %>%
  mutate(pct_yoy = (percent/lag(percent,4) -1) * 100) %>% 
  group_by(date,aggregate) %>% 
  select(pct_yoy) %>% 
  mutate(year = pct_yoy) %>% 
  select(-c(pct_yoy)) %>%
  gather("year", key = period, value = percent) %>% 
  as_tibble()


#calculate average percent for all aggregates
household_avg_pct <- household.tidy %>% 
  group_by(aggregate) %>%
  summarise(Average = mean(percent,na.rm = TRUE))

household_avg_pct_yoy <- household.pct.yoy %>% 
  group_by(aggregate) %>%
  summarise(Average = mean(percent,na.rm = TRUE))

household_avg_pct
household_avg_pct_yoy


#functions---------------------------------------------------------------------------------- 
rawPlot <- function(df, t, xvar = "date",yvar = "percent"){
  
  df <- df %>% drop_na()
  
  df2 <- df %>% filter(aggregate == t) 
  
  head = paste0(t, " ", "-", " ", "Ratio")
  
  
  df_plot <- 
    ggplot(df2,aes_string(x = xvar, y = yvar)) +
    geom_line() +
    scale_y_continuous(trans = "log10") + 
    stat_smooth(method = 'lm', colour = 'purple', se = F, size = 0.4) +
    theme_bw() +
    labs(y = "Ratio(%)", x = "Year", title = head)
  
  return(df_plot)
}

pctPlot <- function(df, df_ave, t,  xvar = "date",yvar = "percent"){
  
  df <- df %>% drop_na()
  
  df2 <- df %>% filter(aggregate == t) 
  
  df_ave <- df_ave %>% filter(aggregate == t)
  
  head = paste0(t, " ", "-", " ", "% Change")
  
  df_plot <- 
    ggplot(df2,aes_string(x = xvar, y = yvar)) +
    geom_line(colour = 'black') +
    geom_smooth(size = 0.3, color = 'purple') +
    geom_hline(data = df_ave, aes(yintercept = Average), linetype = 3, colour = "orange") +
    theme_bw() +
    labs(y = "% Change yoy", x = "Year", title = head)
  
  
  return(df_plot)
}

#create raw plots and upload to plotly using for loop
for (x in 2:ncol(headers)){
rawPlot(household.tidy,toString(headers[x])) %>%
  ggplotly() %>% 
  api_create(filename = toString(headers[x]))
}

pctPlot(household.pct.yoy,household_avg_pct_yoy,toString(headers[x])) %>%
  ggplotly()


#create pct_change_yoy plots
for (x in 2:ncol(headers)){
  pctPlot(household.pct.yoy,household_avg_pct_yoy,toString(headers[x])) %>%
    ggplotly() %>% 
    api_create(filename = paste(toString(headers[x])," -%Change"))
}