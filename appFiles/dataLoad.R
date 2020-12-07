library(readr)
library(readxl)
coronavirus = read.csv("data/coronavirus2.csv", stringsAsFactors = F) 
population = read.csv("data/population.csv",stringsAsFactors = F)[,-1]
dataset <- read_excel("data/dataset.xlsx")
covid <- read_csv("data/file3.csv")


coronavirus = coronavirus %>% 
                  dplyr::mutate(country = dplyr::if_else(countryName == "United States", "United States of America", countryName)) %>% 
              select(-countryName)


names(coronavirus) = c("date","countryCode","region","lat","lon",
                       "Confirmed","Recovered","Deaths","countryName")


covid$"SARS-Cov-2 exam result" [covid$"SARS-Cov-2 exam result"  == "TRUE"] <- 1

names(covid)[names(covid) == "Age Group"] <- "age_group"
names(covid)[names(covid) == "SARS-Cov-2 exam result"] <- "covid_positive"
names(covid)[names(covid) == "Patient addmited to semi-intensive unit (1=yes, 0=no)"] <- "semi_icu"
names(covid)[names(covid) == "Patient addmited to regular ward (1=yes, 0=no)"] <- "regular_ward"
names(covid)[names(covid) == "Patient addmited to intensive care unit (1=yes, 0=no)"] <- "icu"
names(covid)[names(covid) == "Patient age quantile"] <- "patient_age_quantile"
covid_ward = covid %>% select(c("age_group","patient_age_quantile","covid_positive","semi_icu","regular_ward","icu"))

covid$target[covid$target == 1] <- "regular_ward"  
covid$target[covid$target == 0] <- "not_admitted" 
covid$target[covid$target == 2] <- "semi_icu" 
covid$target[covid$target == 3] <- "icu" 

drops <- c("covid_positive","semi_icu","regular_ward","icu","X1","Patient ID","binary_target")
covid <- covid[ , !(names(covid) %in% drops)]

choices_for_plot <- colnames(covid)


#### New data
# 
# coronavirus <- read.csv(file = "https://raw.githubusercontent.com/ulklc/covid19-timeseries/master/countryReport/raw/rawReport.csv", stringsAsFactors = F)
# write.csv(df1,"data/coronavirus1.csv")

hideAllBut = function(divList, butNdx) {
  library("shinyjs")
  divList[-butNdx] %>% sapply(function(x) {shinyjs::hide(x)})
  shinyjs::show(divList[butNdx])
}

#### Old data
# 
# df1 <- read.csv(file = "https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv", stringsAsFactors = F)
# #
# write.csv(df1,"data/coronavirus.csv")
