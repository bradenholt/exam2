# clear environment
rm(list=ls(all = TRUE))

#setwd
setwd("C:/Users/Braden/Documents/Data Science/Exam2/exam2")

# load packages
library(tidyverse)
library(rio)
library(stargazer)
library(data.table)
library(WDI)

# import dataset
inequality_data <- import("inequality.xlsx")

# look at data
View(inequality_data)

# is cross sectional
cross.sectional <- inequality_data$year != 2015
print(cross.sectional)

# subset inequality_gini
inequality_gini_denmark <- subset(inequality_data, country == 'Denmark', select = inequality_gini)
inequality_gini_sweden <- subset(inequality_data, country == 'Sweden', select = inequality_gini)
inequality_gini_brazil <- subset(inequality_data, country == 'Brazil', select = inequality_gini)

# data head
head(inequality_data)

# accent remove function
accent.remove <- function(s) {
  old1 <- "ú"
  new1 <- "u"
  s1 <- chartr(old1,new1,s)
}

# apply function
inequality_data$country <- accent.remove(inequality_data$country)

# run head #2
head(inequality_data)

# sort dataframe by gini score
inequality_data <- inequality_data[order (inequality_data$inequality_gini),]

# run head again
head(inequality_data)

# mean inequality gini
mean(na.omit(inequality_data$inequality_gini))

# if else commands
inequality_data$high_inequality = NA
inequality_data$low_inequality = NA

inequality_data$high_inequality <-
  if(inequality_data$inequality_gini > 36.81375){
    inequaity_data$high_inequality = 1
  } else if (inequality_data$inequality_gini < 36.81375) {
    inequality_data$high_inequality = 0
  }

low_inequality$low_inequality <- 
  if(inequality_data$inequality_gini < 36.81375){
    inequaity_data$low_inequality = 1
  } else if (inequality_data$inequality_gini < 36.81375) {
    inequality_data$low_inequality = 0
  }

x <- c("The World Bank", "African Development Bank", "Bill and Melinda Gates Foundation")

# for loop
for(i in x){
  print(x)
}

# WDI
gdp_per_cap <- WDI(country = 'all',
                       indicator = c('NY.GDP.PCAP.CD'),
                       start = 2015,
                       end = 2015, 
                       cache = NULL,
                       extra = FALSE)

# rename variable
setnames(gdp_per_cap, "NY.GDP.PCAP.CD", "GDP Per Capita")

# merge data
merged_df <- left_join(inequality_data, gdp_per_cap, keep = FALSE)

# remove missing data
merged_df<- merged_df %>% 
  dplyr::filter(!(inequality_gini == "NA"))

# keep inequality scores >30
data_greater_30 <- 
  merged_df %>% 
  dplyr::filter(inequality_gini > 30)

# sum of ai
sum(grep("ai", data_greater_30))

# apply
sapply(data_greater_30$inequality_gini, sum)

# label
install.packages("labelled")
library(labelled)

var_label(merged_df) <- list('country' = "Country",
                             'iso2c' = "ISO2C Country Code",
                             'inequality_gini' = "Inequality Score",
                             'year' = "Year",
                             'high_inequality' = "High Inequality",
                             'low_inequality' = "Low Inequality",
                             'GDP Per Capita' = "GDP Per Capita")
                             
                               
library(rio)
export(merged_df, file = "final_data.dta")
