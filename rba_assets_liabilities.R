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
  
  headers_temp[1] <- "date"
  
  temp.raw <- read_excel(xlsFile, sheet = xlsSheet, 
                         
                         
                         col_types = c("guess"),
                         skip = numRowsSkip)
  
  temp.raw <- temp.raw %>% mutate_if(is.character, as.numeric)
  
  colnames(temp.raw) = headers_temp
  
  asset.raw <<- temp.raw

}

#api username and key for plotly 
#Sys.setenv("plotly_username"="XXX")
#Sys.setenv("plotly_api_key"="XXX")

#download money aggreagate excel files from RBA website and save them in project directory as .xls files. 
download.file("https://rba.gov.au/statistics/tables/xls/a01whist-summary.xls",destfile = "./a01whist-summary.xls", mode = "wb")

createTibbleGrowth("a01whist-summary.xls","Data",10,1)

#gather into tibble 
asset.tidy <- asset.raw %>%
  gather("Capital and Reserve Bank Reserve Fund","Notes on issue","Exchange settlement balances",
         "Deposits (excluding Exchange Settlement balances)","Other liabilities","Total liabilities","Gold and foreign exchange",
         "Australian dollar investments","Other assets (including clearing items)",
         "Total assets" , key = 'aggregate', value = 'dollars')

asset.tidy

asset.tidy <- asset.tidy %>%
  mutate(dollars = (dollars/1000)) %>% 
  as_tibble()

asset.tidy

#gather asset.raw tibble into tidy format and add %change columns for each money aggregate. Both MoM and YoY. 
#and then gather Mom and YoY into new period variable (i.e. month,year) and leave only percent
asset.tidy.pct <- asset.tidy %>%
  group_by(aggregate) %>%
  mutate(pct_change = (dollars/lag(dollars,1) -1) * 100) %>% 
  as_tibble()


#calculate average pct change for all aggregates
asset_avg_pct_change <- asset.tidy.pct %>% 
  group_by(aggregate) %>%
  summarise(Average = mean(pct_change,na.rm = TRUE))

#calculate average dollars for all aggregates
asset_avg_dollars <- asset.tidy.pct %>% 
  group_by(aggregate) %>%
  summarise(Average = mean(dollars,na.rm = TRUE))

asset_avg_pct_change


#functions---------------------------------------------------------------------------------- 
rawPlot <- function(df, t, xvar = "date",yvar = "dollars"){
  
  df <- df %>% drop_na()
  
  df2 <- df %>% filter(aggregate == t) 
  
  head = paste0(t, " ", "-", " ", "$Billions")

  
  df_plot <- 
    ggplot(df2,aes_string(x = xvar, y = yvar)) +
    geom_line() +
    scale_y_continuous(trans = "log10") + 
    stat_smooth(method = 'lm', colour = 'purple', se = F, size = 0.4) +
    theme_bw() +
    labs(y = "$Billion (AUD)", x = "Year", title = head)
  
  return(df_plot)
}
# percent plot
pctPlot <- function(df, df_ave, t,  xvar = "date",yvar = "pct_change"){
  
  df <- df %>% drop_na()
  
  df2 <- df %>% filter(aggregate == t) 
  
  df_ave <- df_ave %>% filter(aggregate == t)
  
  head = paste0(t, " ", "-", " ", "Percent Change")
  
  df_plot <- 
      ggplot(df2,aes_string(x = xvar, y = yvar)) +
      geom_line() +
      geom_hline(data = df_ave, aes(yintercept = Average), linetype = 3, colour = "orange") +
      stat_smooth(method = 'lm', colour = 'purple', se = F, size = 0.4) +
      theme_bw() +
      labs(y = "%Percent", x = "Year", title = head)
  
  
  return(df_plot)
}

#create raw plots
rawPlot(asset.tidy.pct,"Capital and Reserve Bank Reserve Fund") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-capital-dollars") 

rawPlot(asset.tidy.pct,"Notes on issue") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-notes-dollars") 

rawPlot(asset.tidy.pct,"Exchange settlement balances") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-esb-dollars") 

rawPlot(asset.tidy.pct,"Deposits (excluding Exchange Settlement balances)") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-deposits-dollars") 

rawPlot(asset.tidy.pct,"Other liabilities") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-other-liabilities-dollars") 

rawPlot(asset.tidy.pct,"Total liabilities") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-total-liabilities-dollars") 

rawPlot(asset.tidy.pct,"Gold and foreign exchange") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-gold-dollars")

rawPlot(asset.tidy.pct,"Other assets (including clearing items)") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-other-assets-dollars")

rawPlot(asset.tidy.pct,"Australian dollar investments") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-Aussie-investments-dollars")

rawPlot(asset.tidy.pct,"Total assets") %>%
  ggplotly() %>% 
  api_create(filename = "r-rba-asset-total-assets-dollars") 

#create pct_change plots
pctPlot(asset.tidy.pct,asset_avg_pct_change,"Capital and Reserve Bank Reserve Fund") %>%
  ggplotly()

pctPlot(asset.tidy.pct,asset_avg_pct_change,"Notes on issue") %>%
  ggplotly()

pctPlot(asset.tidy.pct,asset_avg_pct_change,"Exchange settlement balances") %>%
  ggplotly() 

pctPlot(asset.tidy.pct,asset_avg_pct_change,"Deposits (excluding Exchange Settlement balances)") %>%
  ggplotly()

pctPlot(asset.tidy.pct,asset_avg_pct_change,"Other liabilities") %>%
  ggplotly() 

pctPlot(asset.tidy.pct,asset_avg_pct_change,"Total liabilities") %>%
  ggplotly() 

pctPlot(asset.tidy.pct,asset_avg_pct_change,"Gold and foreign exchange") %>%
  ggplotly()

pctPlot(asset.tidy.pct,asset_avg_pct_change,"Total assets") %>%
  ggplotly() 