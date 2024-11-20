# NCC 117 script to review existing remarks 
# Check out mammal data remarks across 2 seasons (2023 - 2024)

# update as of 10/30: 2023 protocol revision added multiple app updates.
# download data from 2023 - 2024 (provisional)

# 1. First, review the Trap Collection app child record remarks in the mam_pertrapnight dataset.
# 2. Then, review the Trap Collection app parent record remarks in the mam_perplotnight dataset.

# Load packages
library(ggplot2)
library(neonUtilities)
library(tidyverse)
library(dplyr)
library(lubridate)

# Set working directory
setwd('~/neon_r_scripts/NCC_117/mammals')

################################################################################
# Download data from the Trap Collection app - parent and child records:
MAM <-loadByProduct(
  dpID='DP1.10072.001', 
  site = "all", 
  startdate = "2023-01", 
  enddate = "2024-12", 
  check.size = F, 
  package = "basic",
  include.provisional = TRUE)

mam.c <- MAM$mam_pertrapnight
View(mam.c)

# Save locally due to lengthy download time
write.csv(mam.c,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mammals/mamdata.csv", row.names = FALSE)
read.csv(file = "mamdata_new.csv")

# Filter data to captures only
mam.caps <- mam.c %>% filter(trapStatus=="5 - capture"|trapStatus=="4 - more than 1 capture in one trap") 
View(mam.caps)

# Select columns that might be helpful when reviewing remarks
mam.filt <- mam.caps %>% 
  select(domainID, siteID, collectDate, taxonID, 
         scientificName, fate, remarks)  
View(mam.filt)

# Group by domain and collect date
mam.grp <- mam.filt %>%
  group_by(domainID, collectDate)
View(mam.grp)

# Show remarks only
mam.remarks <- subset(mam.grp, !(mam.grp$remarks == ""))
View(mam.remarks)

#1. Create new dataframe of each remark based on domain, year, and number of each remark
#separate collect date into year, month, day
mam.remarks2 <- mam.remarks %>%         
  mutate(collectDate = as.Date(collectDate), 
         year = year(collectDate), month = month(collectDate), day = day(collectDate))
View(mam.remarks2)

# Number of each remark
# Group  by domain and year, and sort # of remarks from high to low
n_remarks <- mam.remarks2 %>%
  group_by(domainID, year, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
View(n_remarks)

write.csv(n_remarks, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mammals/mam.remarks.raw.child.csv", row.names = FALSE)

# Number of each remark that was recorded over 1 time (excludes remarks written once):
# n_remarks2 <- n_remarks %>%  filter(n > 1)
# View(n_remarks2)


#2. Total number of remarks per year
n_remarks_per_year <- mam.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year)

# Pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final <- n_remarks_per_year %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final)

write.csv(n_remarks_final, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mammals/mam.remarks.n.child.csv", row.names = FALSE)


# Graph for number of remarks for 3 seasons:
# ggplot(n_remarks_per_year, aes(x=domainID, y=n))+
#     geom_col(position = "dodge", stat = "identity")



################################################################################
# Preliminary analyses:
  # #Domain 01 remarks
  # 
  # mam.remarks.d01 <- mam.remarks %>% filter(domainID=="D01")
  # View(mam.remarks.d01)
  # 
  # #subset function to remove specific words
  # mam.remarks.d01.rm <- subset(mam.remarks.d01, !(remarks %in% c("Fleas", "Mites", "Botfly", "?fleas?", "?mites?")))
  # View(mam.remarks.d01.rm)
  # rm(mam.remarks.d01.rm)
  # 
  # #grepl function to remove remarks that contain certain characters (what to use since people spell things wrong so much)
  # mam.remarks.d01.rm <- filter(mam.remarks.d01, !grepl('Flea|Mites|Botfly|fleas|mites|botflies|botfly|parasite|Bot flies|Botflies|Bot fly|Flees', remarks))
  # View(mamremarkd01)

  ################
  # #Domain 02 remarks
  # mam.remarks.d02 <- mam.remarks %>% filter(domainID=="D02")
  # View(mam.remarks.d02)
  # 
  # mam.remarks.d02.asc<-mam.remarks.d02[order(mam.remarks.d02$collectDate),]
  # View(mam.remarks.d02.asc)
  # 
  # mam.remarks.d02.rm <- filter(mam.remarks.d02.asc, !grepl('Flea|Mites|Botfly|fleas|mites|
  #                                                      botflies|botfly|parasite|Bot flies|
  #                                                      Botflies|bot flies|Bot fly|Flees', remarks)) 
  # View(mam.remarks.d02.rm)

  ################
  # #Domain 03 remarks
  # mam.remarks.d03 <- mam.remarks %>% filter(domainID=="D03")
  # View(mam.remarks.d03)
  # 
  # mam.remarks.d03.asc<-mam.remarks.d03[order(mam.remarks.d03$collectDate),]
  # View(mam.remarks.d03.asc)
  # 
  # mam.remarks.d03.rm <- filter(mam.remarks.d03.asc, !grepl('Flea|Mites|Botfly|fleas|mites|
  #                                                      botflies|botfly|parasite|Bot flies|
  #                                                      Botflies|bot flies|Bot fly|Flees', remarks)) 
  # View(mam.remarks.d03.rm)


  ################
  # #All domains removed parasite remark
  # mam.remarks.asc <- mam.remarks[order(mam.remarks$domainID, mam.remarks$collectDate),]
  # View(mam.remarks.asc)
  # 
  # mam.remarks.rm <- filter(mam.remarks.asc, !grepl('Flea|Mites|Botfly|fleas|mites|
  #                                                      botflies|botfly|parasite|Bot flies|
  #                                                      Botflies|bot flies|Bot fly|Flees', remarks)) 
  # View(mam.remarks.rm)

  # #Getting rid of parasite remarks reduced 5603 remarks to 2489 (cut in half!)
  # write.csv(mam.remarks.rm,"C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mam.remarks.csv", row.names = FALSE)


################################################################################
mam.c <- MAM$mam_pertrapnight

# Now look at parent, plotlevel remarks from mam_perplotnight dataset:
mam.p <- MAM$mam_perplotnight
View(mam.p)

# Select columns that might be helpful when reviewing remarks
mam.p.filt <- mam.p %>% 
  select(domainID, siteID, collectDate, samplingImpractical, remarks)  
View(mam.p.filt)

# Group by domain and collect date
mam.p.grp <- mam.p.filt %>%
  group_by(domainID, collectDate)
View(mam.p.grp)

# Show remarks only
mam.p.remarks <- subset(mam.p.grp, !(mam.p.grp$remarks == ""))
View(mam.p.remarks)

#1. Create new dataframe of each remark based on domain, year, and number of each remark
#separate collect date into year, month, day
mam.p.remarks2 <- mam.p.remarks %>%         
  mutate(collectDate = as.Date(collectDate), 
         year = year(collectDate), month = month(collectDate), day = day(collectDate))
View(mam.p.remarks2)

# Number of each remark
# Group  by domain and year, and sort # of remarks from high to low
n_remarks2 <- mam.p.remarks2 %>%
  group_by(domainID, year, samplingImpractical, remarks) %>%
  summarise(n = n())%>%
  arrange(desc(n))
View(n_remarks2)

write.csv(n_remarks2, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mammals/mam.remarks.raw.parent.csv", row.names = FALSE)

#2. Total number of remarks per year
n_remarks_per_year.2 <- mam.p.remarks2 %>%
  group_by(domainID, year) %>%
  summarise(n = n())
View(n_remarks_per_year.2)

#pivot wider to show year as column, domain as rows, and number of remarks as values
n_remarks_final2 <- n_remarks_per_year.2 %>%
  pivot_wider(names_from = year, values_from = n)
View(n_remarks_final2)

write.csv(n_remarks_final2, "C:/Users/mccahill/Documents/neon_r_scripts/NCC_117/mammals/mam.remarks.n.parent.csv", row.names = FALSE)

