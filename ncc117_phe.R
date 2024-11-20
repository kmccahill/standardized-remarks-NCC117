#load packages
library(ggplot2)
library(neonUtilities)
library(tidyverse)
library(dplyr)
library(lubridate)

#set working directory
setwd('C:/Users/pittssj/OneDrive - National Ecological Observatory Network/Documents/NCC117')

#download data from 2021 - 2023 
PHE <-loadByProduct(
  dpID='DP1.10055.001', 
  site = "all", 
  startdate = "2023-01", 
  enddate = "2024-12", 
  check.size = F, 
  package = "basic",
  include.provisional = TRUE)

phe <- PHE$phe_perindividualperyear
View(phe)

#save locally since takes so long to download. 2023 data is provisional 
write.csv(phe,"C:/Users/pittssj/OneDrive - National Ecological Observatory Network/Documents/NCC117/phedata.csv", row.names = FALSE)

#start here in the future
phe <- read.csv(file = "phedata.csv")

#filter field data to show collected samples and columns of interest
phe.filt <- phe %>% 
    select(domainID, siteID, date,plantStatus, remarks)  
View(phe.filt)

#remove records without remarks
phe.remarks <- subset(phe.filt, !(phe.filt$remarks == ""))
View(phe.remarks) 

write.csv(phe.remarks,"C:/Users/pittssj/OneDrive - National Ecological Observatory Network/Documents/NCC117/pheremarks.csv", row.names = FALSE)

############################

#Create new dataframe of each remark based on domain, year, and number of each remark
#number of each remark per domain:

#separate collect date into year, month, day
phe.remarks2 <- phe.remarks %>%         
  mutate(date = as.Date(date), 
         year = year(date), month = month(date), day = day(date))
View(phe.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- phe.remarks2 %>%
  group_by(domainID, year, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/pittssj/OneDrive - National Ecological Observatory Network/Documents/NCC117/pheremarks.counts.csv", row.names = FALSE)

#total number of remarks per year
n_remarks_per_year <- phe.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)


#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/pittssj/OneDrive - National Ecological Observatory Network/Documents/NCC117/phe.remarks.n.csv", row.names = FALSE)

