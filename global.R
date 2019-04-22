## load Packages______________________________

library(shiny)
library(dplyr) 
library(stringr)
library(ggplot2)
library(sp)
library(rgeos)
library(leaflet)
library(DBI)
library(RPostgres)
library(shinydashboard)
library(plotly)
library(reshape2)
library(tidyr) 
library(lattice)
library(rjson)
library(forcats)
library(ggrepel)
library(ggpubr)
library(shinyWidgets)
library(DT)
library(ggthemes)
library(devtools)

#=================================  Scripts  =================================

source("functions.R")

#================================= Load_files  =================================

#dataset: https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016
data <- read.csv("Suicide.csv", header = TRUE)
data <- data %>% select(country, year, sex,age, suicides_no, population,suicides.100k.pop, gdp_for_year...., gdp_per_capita....,HDI.for.year)

#dataset: https://www.kaggle.com/akshaychavan/average-temperature-per-country-per-year/data
average_temp <- read.csv("average_temp.csv", header = TRUE)
average_temp<- average_temp %>% select(country,Average)
average_temp.cor <- average_temp
names(average_temp)<- c("X1","Average_temperature")


# dataset (the world bank) https://data.worldbank.org/indicator/SI.POV.GINI?end=2017&start=2017&view=bar&year_low_desc=false
Gini_data <- read.csv("Gini.csv", header = TRUE)

Gini_data.cor <- Gini_data
names(Gini_data.cor) <- c("country","avg.Gini")

#coordinates
json_data <- fromJSON(file="label_points-country.json")

Country_vec<-NULL
Coo1_vec<-NULL
Coo2_vec<-NULL
for (i in 1:length(json_data[["features"]])){
  Country_vec[i]<- json_data[["features"]][[i]][["properties"]][["sr_subunit"]]
  Coo1_vec[i]<- json_data[["features"]][[i]][["geometry"]][["coordinates"]][1]
  Coo2_vec[i]<- json_data[["features"]][[i]][["geometry"]][["coordinates"]][2]
}
X1 <- unique(Country_vec)

geo_data <- data.frame(X1,Coo1_vec,Coo2_vec)
geo_data$X1 <- as.factor(geo_data$X1)


### labels Data 
Countries.information <- data.frame(matrix(ncol=10,nrow = length(unique(data$country))))

Countries.information[,1:2] <-  data %>%
  select(country,year) %>%
  group_by(country) %>%
  summarise(
            year_start = (min(year,na.rm=TRUE)),
            year_end = (max(year,na.rm=TRUE))) %>%
  unite("Years",year_start,year_end,sep = " - ")

Countries.information[,3:7] <-  data %>%
  select(country,year,suicides_no,suicides.100k.pop,HDI.for.year,gdp_per_capita....) %>%
  group_by(country) %>%
  summarise(
    Total_Cases = sum(suicides_no,na.rm = TRUE),
    Total_Cases_per_100k = mean(suicides.100k.pop,na.rm = TRUE),
    Avg_GDP_per_capita = mean(gdp_per_capita....,na.rm=TRUE),
    Avg_HDI = mean(HDI.for.year,na.rm=TRUE))


avg.suicide_rates<- data %>% 
                    select(country,year,suicides.100k.pop) %>% 
                    group_by(country,year) %>% 
                    summarise(yearly.suicide_rate = sum(suicides.100k.pop,na.rm = TRUE)) %>% 
                    group_by(country) %>% 
                    summarise(avg.suicide_rate = mean(yearly.suicide_rate,na.rm = TRUE)) %>% 
                    select(avg.suicide_rate)

Countries.information[,8] <- round(avg.suicide_rates[,1],1)

#=================================  Merge DFs  =================================

Label_Info1 <- merge(Countries.information,geo_data,by="X1")

Final_Label_Info <- merge(Label_Info1,average_temp,by="X1")

Final_Label_Info$rank_rate <- round(rank(desc(Final_Label_Info$X8)),0)
Final_Label_Info$rank_total.cases <- round(rank(desc(Final_Label_Info$X4)),0)

