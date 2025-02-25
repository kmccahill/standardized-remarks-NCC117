# NCC 117 script to review existing remarks 
# Last beetle protocol revision: 2022 

# Ground beetles sampled from pitfall traps (DP1.10022.001) includes all data from: 
# BET: Field Sampling app suite: trap setting and field collection
# BET: Lab Processing app suite: sorting, pinning, pooling

# bet_fielddata - field data
# bet_sorting - sorting of IB, carabids, herp bycatch
# bet_archivepooling - pooling

# bet_parataxonomistID -  Field ecologist IDs, pulled from pinning app
# bet_expertTaxonomistIDProcessed - Taxonomist IDs

# Looks like all data sets have child record remarks (no parent)

#load packages
library(ggplot2)
library(neonUtilities)
library(tidyverse)
library(dplyr)
library(lubridate)

#set working directory
setwd('~/neon_r_scripts/NCC_117/beetles')

#download data from 2022 - 2024
BET <-loadByProduct(
  dpID='DP1.10022.001', 
  site = "all", 
  startdate = "2022-01", 
  enddate = "2024-12", 
  check.size = F, 
  package = "basic",
  include.provisional = TRUE)

###################################

# Remarks for BET: Field Sampling app
bet_fld <- BET$bet_fielddata
View(bet_fld)

#save locally since takes so long to download. 2023 data is provisional 
write.csv(bet_fld,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/field/bet_fld.csv", row.names = FALSE)

###

#run if previously downloaded
bet_fld <- read.csv(file = "~/neon_r_scripts/NCC_117/beetles/field/bet_fld.csv")

# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
bet.remarks.initial <- subset(bet_fld, !(bet_fld$remarks == ""))
# 3397 total remarks 2022 - 2024

#filter field data to show collected samples and columns of interest
bet.filt <- bet_fld %>% 
  filter(sampleCollected=="Y") %>%
  select(domainID, siteID, plotID, trapID, collectDate, nlcdClass, sampleCondition,
         cupStatus, lidStatus, fluidLevel, remarks)  
View(bet.filt)

#remove records without remarks
bet.remarks <- subset(bet.filt, !(bet.filt$remarks == ""))
View(bet.remarks) 
# 1190 remarks once filtered to sample collected

# Don't end up using this
# write.csv(bet.remarks,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/field/bet_fld.remarks.csv", row.names = FALSE)

#1. Create new dataframe of each remark based on domain, year, and number of each remark
#number of each remark per domain:

#separate collect date into year, month, day
bet.remarks2 <- bet.remarks %>%         
  mutate(collectDate = as.Date(collectDate), 
         year = year(collectDate), month = month(collectDate), day = day(collectDate))
View(bet.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- bet.remarks2 %>%
  group_by(domainID, year, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/field/bet_fld.remarks.raw.csv", row.names = FALSE)

#2. Total number of remarks per year
n_remarks_per_year <- bet.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)


#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/field/bet_fld.remarks.n.csv", row.names = FALSE)

# 3. Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))

################################################################################

# Remarks for BET: Lab Processing app suite (sorting)
# based on sampleType (IB, carabid, herp)
bet_sort <- BET$bet_sorting
View(bet_sort)

#save locally since takes so long to download. 2023 data is provisional 
write.csv(bet_sort,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/sorting/bet_sort.csv", row.names = FALSE)

# Sorting data -> sampleType == "Invert Bycatch" is IB tube vials
# sorting data -> sampleType == "carabid" is carabid tube vials

# Carabids have "Tube Data" remark in sorting, and "Pinning Data" remarks in parataxonomistID data set

###

#run if previously downloaded
bet_sort <- read.csv(file = "~/neon_r_scripts/NCC_117/beetles/sorting/bet_sort.csv")

# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
bet.sort.remarks.initial <- subset(bet_sort, !(bet_sort$remarks == ""))
# 665 total remarks 2022 - 2024

#filter to only columns of interest
bet_sort <- bet_sort %>%
  select(domainID, siteID, processingDate, sampleCondition, sampleType, remarks)
View(bet_sort)

# Sorting -> Invert byatch data remarks
bet_sort_IB <- bet_sort %>% 
  filter(sampleType == "invert bycatch")
View(bet_sort_IB)

bet.remarks.IB <- subset(bet_sort_IB, !(bet_sort_IB$remarks == ""))
View(bet.remarks.IB) #146 remarks

# Sorting -> carabid remarks
bet_sort_car <- bet_sort %>% 
  filter(sampleType == "carabid")
View(bet_sort_car)

bet.remarks.car <- subset(bet_sort_car, !(bet_sort_car$remarks == ""))
View(bet.remarks.car) #507 remarks

# Sorting -> herp bycatch remarks
bet_herp <- bet_sort %>% 
  filter(sampleType == "vert bycatch herp")
View(bet_herp)

bet.remarks.herp <- subset(bet_herp, !(bet_herp$remarks == ""))
View(bet.remarks.herp) #12 remarks

##

# 1. Invert Bycatch

#separate collect date into year, month, day
bet.remarks2 <- bet.remarks.IB %>%         
  mutate(processingDate = as.Date(processingDate), 
         year = year(processingDate), month = month(processingDate), day = day(processingDate))
View(bet.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- bet.remarks2 %>%
  group_by(domainID, year,sampleCondition, sampleType, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/sorting/bet_sort_IB.remarks.raw.csv", row.names = FALSE)

# Total number of remarks per year
n_remarks_per_year <- bet.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)

#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/sorting/bet_sort_IB.remarks.n.csv", row.names = FALSE)

# Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))


# 2. Carabids 

#separate collect date into year, month, day
bet.remarks2 <- bet.remarks.car %>%         
  mutate(processingDate = as.Date(processingDate), 
         year = year(processingDate), month = month(processingDate), day = day(processingDate))
View(bet.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- bet.remarks2 %>%
  group_by(domainID, year,sampleCondition, sampleType, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/sorting/bet_sort_car.remarks.raw.csv", row.names = FALSE)

# Total number of remarks per year
n_remarks_per_year <- bet.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)

#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/sorting/bet_sort_car.remarks.n.csv", row.names = FALSE)

# Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))


# 3. Herps

bet.remarks2 <- bet.remarks.herp %>%         
  mutate(processingDate = as.Date(processingDate), 
         year = year(processingDate), month = month(processingDate), day = day(processingDate))
View(bet.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- bet.remarks2 %>%
  group_by(domainID, year,sampleCondition, sampleType, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/sorting/bet_sort_herp.remarks.raw.csv", row.names = FALSE)

# Total number of remarks per year
n_remarks_per_year <- bet.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)


#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/sorting/bet_sort_herp.remarks.n.csv", row.names = FALSE)


################################################################################

# Remarks for BET: Lab Processing app suite (pooling)
bet_pool <- BET$bet_archivepooling
View(bet_pool)

# No sample type?

#save locally since takes so long to download. 2023 data is provisional 
write.csv(bet_pool,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/pooling/bet_pool.csv", row.names = FALSE)

###

#run if previously downloaded
bet_pool <- read.csv(file = "~/neon_r_scripts/NCC_117/beetles/pooling/bet_pool.csv")

# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
bet.pool.remarks.initial <- subset(bet_pool, !(bet_pool$remarks == ""))
# 87 total remarks 2022 - 2024

#filter to only columns of interest
bet_pool <- bet_pool %>%
  select(domainID, siteID, processingDate, sampleCondition, remarks)
View(bet_pool)

# only remarks
bet.remarks <- subset(bet_pool, !(bet_pool$remarks == ""))
View(bet.remarks) # 87 remarks (no filtering)

#separate collect date into year, month, day
bet.remarks2 <- bet.remarks %>%         
  mutate(processingDate = as.Date(processingDate), 
         year = year(processingDate), month = month(processingDate), day = day(processingDate))
View(bet.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- bet.remarks2 %>%
  group_by(domainID, year,sampleCondition, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/pooling/bet_pool.remarks.raw.csv", row.names = FALSE)

# Total number of remarks per year
n_remarks_per_year <- bet.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)


#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/pooling/bet_pool.remarks.n.csv", row.names = FALSE)

# Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))

################################################################################

# Remarks for BET: Lab Processing app suite (pinning - carabids)
bet_pin <- BET$bet_parataxonomistID
View(bet_pin)

#save locally since takes so long to download. 2023 data is provisional 
write.csv(bet_pin,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/pinning/bet_pin.csv", row.names = FALSE)

###

#run if previously downloaded
bet_pin <- read.csv(file = "~/neon_r_scripts/NCC_117/beetles/pinning/bet_pin.csv")

# Preliminary - First look at number of remarks across entire data frame to compare number to filtered data
bet.pin.remarks.initial <- subset(bet_pin, !(bet_pin$remarks == ""))
# 478 total remarks 2022 - 2024

#filter to only columns of interest
bet_pin <- bet_pin %>%
  select(domainID, siteID, identifiedDate, sampleCondition, taxonID, remarks)
View(bet_pin)

bet.remarks <- subset(bet_pin, !(bet_pin$remarks == ""))
View(bet.remarks) #478 remarks

#separate collect date into year, month, day
bet.remarks2 <- bet.remarks %>%         
  mutate(identifiedDate = as.Date(identifiedDate), 
         year = year(identifiedDate), month = month(identifiedDate), day = day(identifiedDate))
View(bet.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- bet.remarks2 %>%
  group_by(domainID, year, sampleCondition, taxonID, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/pinning/bet_pin.remarks.raw.csv", row.names = FALSE)

# Total number of remarks per year
n_remarks_per_year <- bet.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)

#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/pinning/bet_pin.remarks.n.csv", row.names = FALSE)

# Graphs to visualize total number of remarks for 2 seasons:
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")

# by year
ggplot(n_remarks_per_year, aes(x=domainID, y=n, fill = domainID))+
  geom_col(position = "dodge", stat = "identity")+ facet_wrap(vars(year))




############# **Finished with preparing NEON recorded data** ###################





################################################################################

# Remarks from professional taxonomist data
bet_tax <- BET$bet_expertTaxonomistIDProcessed
View(bet_tax)

#save locally since takes so long to download. 2023 data is provisional 
write.csv(bet_tax,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/taxonomist/bet_tax.csv", row.names = FALSE)

###

#run if previously downloaded
bet_tax <- read.csv(file = "bet_tax.csv")

#filter to only columns of interest
bet_tax <- bet_tax %>%
  select(domainID, 
         siteID, 
         identifiedDate, 
         taxonID, 
         sex, 
         identificationRemarks, 
         sampleCondition, 
         remarks)
View(bet_tax)

write.csv(bet_tax,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/taxonomist/bet_tax.filt.csv", row.names = FALSE)

bet.remarks <- subset(bet_tax, !(bet_tax$remarks == ""))
View(bet.remarks) #265 remarks

#separate collect date into year, month, day
bet.remarks2 <- bet.remarks %>%         
  mutate(identifiedDate = as.Date(identifiedDate), 
         year = year(identifiedDate), month = month(identifiedDate), day = day(identifiedDate))
View(bet.remarks2)

#group  by domain and year, and sort # of remarks from high to low
n_remarks <- bet.remarks2 %>%
  group_by(domainID, year, taxonID, sex, identificationRemarks, sampleCondition, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
#number of each remark per year
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/taxonomist/bet_tax.remarks.raw.csv", row.names = FALSE)

# Total number of remarks per year
n_remarks_per_year <- bet.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)

#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/beetles/taxonomist/bet_tax.remarks.n.csv", row.names = FALSE)


