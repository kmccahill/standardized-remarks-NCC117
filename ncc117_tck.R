# NCC 117 script to review existing remarks 
# Last protocol revision was 2/1/2022

# Ticks sampled using drag cloths (DP1.10093.001) includes all data from: 
# TCK: Field Sampling application

# tck_fielddata - field data
# tck_taxonomyProcessed - ID to species (individual count and scientific name)

# Looks like tck_fielddata has child record remarks (no parent)

# Load packages
library(neonUtilities)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Set working directory
setwd('~/neon_r_scripts/NCC_117/ticks')

################################################################################
# Download data from the Tick Field Sampling app records:
TCK <-loadByProduct(
  dpID='DP1.10093.001', 
  site = "all", 
  startdate = "2022-01", 
  enddate = "2024-12", 
  check.size = F, 
  package = "basic",
  include.provisional = TRUE)

tck_fld <- TCK$tck_fielddata

# save locally since takes so long to download. 2024 data is provisional 
write.csv(tck_fld,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/ticks/tckdata.csv", row.names = FALSE)

###

#run if previously downloaded
tck_fld <- read.csv(file = "/neon_r_scripts/NCC_117/ticks/tckdata.csv")

# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
tck.remarks.initial <- subset(tck_fld, !(tck_fld$remarks == ""))
# 783 total remarks in 2024


# Select columns of interest, group by domain and collectDate
tck.filt <- tck_fld %>% 
  select(domainID, siteID, collectDate, samplingImpractical, sampleCondition, remarks) %>%  #dataQF column exists
  group_by(domainID, collectDate)

# Show remarks only
tck.remarks <- subset(tck.filt, !(tck.filt$remarks == ""))
# 783 total (don't filter any values out)

#1. Create new dataframe of each remark based on domain, year, and number of each remark
# separate collect date into year, month, day
tck.remarks2 <- tck.remarks %>%         
  mutate(collectDate = as.Date(collectDate), 
         year = year(collectDate), month = month(collectDate), day = day(collectDate))

# Number of each remark
# Group  by domain and year, and sort # of remarks from high to low
n_remarks <- tck.remarks2 %>%
  group_by(domainID, year, samplingImpractical, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of individual remarks per year by domain
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/ticks/tck.remarks.raw.csv", row.names = FALSE)

#2. Total number of remarks per year
n_remarks_per_year <- tck.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)

#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/ticks/tck.remarks.n.csv", row.names = FALSE)

# 3. Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))

## Preliminary - remarks per year (for PPT)
 remarks_year <- tck.remarks2 %>%
   group_by(year) %>%
   summarise(n = n())
 View(remarks_year)

# 3. Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))

################################################################################

# Professional taxonomist ID - scientific name and individual count
  tck_tax <- TCK$tck_taxonomyProcessed

