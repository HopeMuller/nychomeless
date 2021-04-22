# load libraries
library(plyr)
library(tidyverse)
library(foreach)
library(lubridate)

# setwd to doe folder
# Kenny: setwd("~/RANYCS/sasdata/development/kmt")

# load in data
raw.nsc <- read_csv('nsc_all.csv')
nsc <- raw.nsc

raw.student <- foreach(year=2013:2019, .combine='rbind.fill') %do% {
  filename <- paste0('student_',year, '.csv')
  this.data <- read_csv(filename)
  this.data
}
doe.full <- raw.student

# clean the data
doe.full <- doe.full %>%

  # rename columns
  dplyr::rename(id = RANYCSID,
                pov = ANYPOV,
                hmls = STHFLG,
                shlt = SHLFLG,
                iep = IEPSPDBIO,
                ell = LEPBIO,
                year = YEAR,
                hlang = HLANG,
                bplace = BPLACE,
                gen = GENCAT,
                eth = ETHCAT,
                dob = DOB,
                grade = DOEGLVOCT,
                sch.fall = DBNOCT,
                status.fall = AGDDCATOCT,
                sch.spr = DBNJUN,
                status.spr = AGDDCATJUN,
                abs = ABSTOT,
                sus = SUSTOT,
                sus.days = SUSTOTDAYS,
                ela = ELASSC,
                mth = MTHSSC) %>%
  
  # change grade to numeric
  mutate(grade = as.numeric(grade),
         ela = as.numeric(ela),
         mth = as.numeric(mth),
         # recode gender male as 0
         gen = ifelse(gen == 2, 0, 1),
         # code percentage days absent per year and percent days suspended per year
         per.abs = abs/180,
         per.sus = sus.days/180) %>%
  
  # filter out above grade 8
  filter(grade >= 8)

# filter out students who were not going to graduate by 2019
doe.full <- doe.full %>%
  filter(!(grade < 12 & year = 2019) |
           (grade < 11 & year == 2018) |
           (grade < 10 & year == 2017))
          
# parse birth year to new column
doe.full <- doe.full %>%
  mutate(birth.yr = year(mdy(dob)))

# create column to show if they moved mid-year
doe.full <- doe.full %>%
  mutate(mvd.mid = case_when(sch.fall != sch.spr ~ 1,
                         sch.fall == sch.spr ~ 0))

# add total school count
doe.full <- doe.full %>%
  group_by(id)%>%
  dplyr::summarise(num.schools = n_distinct(sch.fall))

# status for each year and "any" flags
doe.full <- doe.full %>% 
  group_by(id) %>% 
  mutate(gr9.stat.fall = ifelse(grade == 9, status.fall, NA),
         gr9.stat.spr = ifelse(grade == 9, status.spr, NA),
         gr10.stat.fall = ifelse(grade == 10, status.fall, NA),
         gr10.stat.spr = ifelse(grade == 10, status.spr, NA),
         gr11.stat.fall = ifelse(grade == 11, status.fall, NA),
         gr11.stat.spr = ifelse(grade == 11, status.spr, NA),
         gr12.stat.fall = ifelse(grade == 12, status.fall, NA),
         gr12.stat.spr = ifelse(grade == 12, status.spr, NA),
         any.pov = as.numeric(pov > 0),
         any.hmls = as.numeric(hmls > 0),
         any.shlt = as.numeric(shlt > 0),
         any.iep = as.numeric(iep > 0),
         any.ell = as.numeric(ell > 0),
         any.shlt = ifelse(is.na(any.shlt) == T, 0, any.shlt)
         )

# sum number of days of absences and suspensions, and total suspensions
# calculate mean percentage of days absent and suspended
doe.full <- doe.full %>% 
  group_by(id) %>% 
  dplyr::summarise(tot.abs = sum(abs, na.rm=T),
                   av.abs = mean(per.abs, na.rm=T),
                   tot.sus = sum(sus, na.rm=T),
                   av.sus = mean(per.sus, na.rm=T),
                   tot.sus.days = sum(sus.days, na.rm=T)) %>% 
  ungroup() %>% 
  right_join(doe.full)

# create column for freshman year, this needs editing due to the repeated rows for people's 9th grade year
doe.full <- doe.full %>%
  mutate(frsh = case_when(grade == 9 ~ year - 1))

# create column of age difference between NYC mandated school-age entry and grade 9-age entry
doe.full <- doe.full %>%
  mutate(age.diff = frsh - birth.yr - 14)

# clean up columns
doe.full <- doe.full %>% 
  select(-pov, -hmls, -shlt, -iep, -ell, -status.fall, -status.spr, -abs, -sus, -sus.days)

# indicate flag if student ever repeated a grade
doe.full <- doe.full %>% 
  add_count(id) %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(any.repeats = case_when(n > 1 & (n_distinct(year) != n_distinct(grade)) ~ 1, 
                             n == 1 | (n_distinct(year) == n_distinct(grade)) ~ 0)) %>% 
  select(-n)

# count number of schools, report final school
doe.full <- doe.full %>% 
  group_by(id) %>% 
  mutate(total.sch = n_distinct(c(sch.fall, sch.spr)))
  
# checkpoint, for troubleshooting
backup <- doe.full
doe.full <- backup


#######################################
# THINGS THAT AREN'T WORKING PROPERLY #
#######################################

# math and ela scores then remove 8th grade 
doe.full <- doe.full %>% 
  group_by(id) %>% 
  dplyr::summarise(gr8.ela = tail(ela),
                   gr8.mth = tail(mth)) %>% 
  ungroup() %>% 
  right_join(doe.full)

# remove grade 8 rows
doe.full <- doe.full %>% 
  filter(grade > 8)

