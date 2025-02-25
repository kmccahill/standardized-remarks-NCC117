# NCC 117 script to review existing remarks 
# Last mosquito protocol revision: 2/8/2024

# Mosquitoes sampled from CO2 traps (DP1.10043.001) includes all data from: 
# MOS: Trap Setting and Collection app suite: trap setting and field collection

# mos_trapping - field data
# mos_sorting - MOS lab sorting
# mos_ExpertIDTaxonomistIDProcessed - ID to species (individual count and scientific name)


# Looks like mos_trapping data set has child record remarks (no parent)

#load packages
library(ggplot2)
library(neonUtilities)
library(tidyverse)
library(dplyr)
library(lubridate)

# Set working directory
setwd('~/neon_r_scripts/NCC_117/mosquitoes')

# Download data from the MOS Trap Setting and Collection app records:
MOS <-loadByProduct(
  dpID='DP1.10043.001', 
  site = "all", 
  startdate = "2024-01", 
  enddate = "2024-12", 
  check.size = F, 
  package = "basic",
  include.provisional = TRUE)

###################################

# Remarks for MOS: Trap Setting and Collection app
mos_trap <- MOS$mos_trapping

#save locally since takes so long to download. 2024 data is provisional 
write.csv(mos_trap,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mosquitoes/mosTrapdata.csv", row.names = FALSE)

###

#run if previously downloaded
mos_trap <- read.csv(file = "mosTrapdata.csv")

# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
mos.remarks.initial <- subset(mos_trap, !(mos_trap$remarks == ""))
# 2673 total remarks in 2024


# Select columns of interest, group by domain and trapType
mos.filt <- mos_trap %>% 
  select(domainID, siteID, collectDate, samplingImpractical, trapType, CO2Status, sampleCondition, remarks) %>%  #dataQF column exists
  group_by(domainID, trapType)

# Show remarks only
mos.remarks <- subset(mos.filt, !(mos.filt$remarks == ""))
# 2673 total (don't filter any values out)

#1. Create new dataframe of each remark based on domain, year, and number of each remark
# separate collect date into year, month, day
mos.remarks2 <- mos.remarks %>%         
  mutate(collectDate = as.Date(collectDate), 
         year = year(collectDate), month = month(collectDate), day = day(collectDate))

# Number of each remark
# Group  by domain and year, and sort # of remarks from high to low
n_remarks <- mos.remarks2 %>%
  group_by(domainID, year, samplingImpractical, trapType, sampleCondition, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of individual remarks per year by domain
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mosquitoes/mos.trap.remarks.raw.csv", row.names = FALSE)

#2. Total number of remarks per year
n_remarks_per_year <- mos.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)

#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mosquitoes/mos.trap.remarks.n.csv", row.names = FALSE)

# 3. Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))

## Preliminary - remarks per year (for PPT)
# remarks_year <- mos.remarks2 %>%
#   group_by(year) %>%
#   summarise(n = n())
# View(remarks_year)

# 3. Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))


###################################

# Remarks for Doc Weissman's Identifications
mos_sort <- MOS$mos_sorting

#save locally since takes so long to download
write.csv(mos_sort,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mosquitoes/mosSortdata.csv", row.names = FALSE)

###

#run if previously downloaded
mos_sort <- read.csv(file = "mosSortdata.csv")

# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
mos.sort.remarks.initial <- subset(mos_sort, !(mos_sort$remarks == ""))
# 4 total - no need for analysis

###################################

# Professional ID of MOS species, contains individual count and scientific name
mos_tax <- MOS$mos_expertTaxonomistIDProcessed

write.csv(mos_tax,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mosquitoes/mosTaxdata.csv", row.names = FALSE)


# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
mos.tax.remarks.initial <- subset(mos_tax, !(mos_tax$remarks == ""))
# None

