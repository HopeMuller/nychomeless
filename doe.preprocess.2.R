# load libraries
library(plyr)
library(tidyverse)
library(foreach)
library(lubridate)

# setwd to doe folder
# Kenny: setwd("~/RANYCS/sasdata/development/kmt")
# Hope: setwd("/Users/Home/mnt/sasdata/development/kmt")

# load in data
# nsc college attendance data
raw.nsc <- read_csv('nsc_all.csv')
nsc <- raw.nsc

# doe data
raw.student <- foreach(year=2013:2019, .combine='rbind.fill') %do% {
  filename <- paste0('student_',year, '.csv')
  this.data <- read_csv(filename)
  this.data
}

# assign student-level data to new name
doe.full <- raw.student

# read in school-level data
# Hope: sch.doe <- read.csv("/Users/Home/Documents/MessyData/finalproj/DOE_schooldata.csv")

# rename school column for merging
sch.doe <- sch.doe %>%
  rename(final.sch = DBN)

# clean the student-level data
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
                sus.days = SUSTOTDAYS)

# delete ela and math scores, too many are missing
doe.full <- doe.full %>%
  select(-ELASSC, - MTHSSC) %>%
  
  # subset data to grade levels used
  filter(grade == "09" | grade == "10" | grade == "11" | grade == "12") %>%
  
  # change grade and scores to numeric values
  mutate(grade = as.numeric(grade),
         # recode gender male as 0
         gen = ifelse(gen == 2, 0, 1),
         # code percentage days absent per year and percent days suspended per year
         per.abs = abs/182,
         per.sus = sus.days/182, 
         per.miss = (abs + sus.days)/182) %>%
  select(-abs, -sus.days) %>%
  
  # filter out students who were not going to graduate by 2019
  filter(!(grade < 12 & year == 2019) |
           (grade < 11 & year == 2018) |
           (grade < 10 & year == 2017)) %>%
  
  # parse birth year to new column
  mutate(birth.yr = year(mdy(dob))) %>%
  
  # create column to show if they moved mid-year
  mutate(mvd.mid = case_when(sch.fall != sch.spr ~ 1,
                             sch.fall == sch.spr ~ 0)) %>%

  # filter out students listed with birthdays prior to 1994
  filter(birth.yr > 1993) %>%
  
  # filter out suspensions listed with more days that school year
   filter(sus.days < 183)

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
         gr12.stat.spr = ifelse(grade == 12, status.spr, NA)) %>%
  select(-status.fall, -status.spr)

# column for grades begun within NYC DOE, starting with grade 9
doe.full <- doe.full %>%
  group_by(id) %>%
  dplyr::summarise(comp.grades = n_distinct(grade)) %>%
  ungroup() %>% 
  right_join(doe.full)

# create binary graduate column
doe <- doe.full %>%
  group_by(id) %>%
  select(status.fall, status.spr) %>%
  mutate(grad = case_when((status.fall == 2 | status.spr == 2) ~ 1)
                          (status.fall == 3 | status.spr == 3) ~ 0) %>%
  filter(year == 12) %>%
  select(-year) %>%
  right_join(doe.full)
  
# report final school attended
doe.full <- doe.full %>%
  group_by(id) %>%
  select(sch.fall, year) %>%
  dplyr::rename(final.sch = sch.fall) %>%
  filter(year == max(year)) %>%
  select(-year) %>%
  right_join(doe.full)

# create column to specify freshman year
doe.full <- doe.full %>%
  group_by(id) %>%
  mutate(frsh = case_when(grade == 9 ~ year - 1),
         frsh = min(year)) %>%
  ungroup() %>%
  right_join(doe.full)

# create column of age difference between NYC mandated school-age entry and grade 9-age entry
doe.full <- doe.full %>%
  mutate(age.diff = frsh - birth.yr - 14) %>%
  select(-frsh, - birth.yr)

# take out grades 11 and 12 --------------------------------------------------------------
doe.full <- doe.full %>%
  filter(grade < 11)

# add total count of schools each student attended in grade 9 and 10
doe.full <- doe.full %>%
  group_by(id) %>%
  dplyr::summarise(num.schools = n_distinct(interaction(sch.fall, sch.spr))) %>%
  ungroup() %>% 
  right_join(doe.full)

# create if ever summary for shelter, poverty, iep, all
doe.full <- doe.full %>% 
  group_by(id) %>% 
  mutate(any.pov = as.numeric(pov > 0),
          any.shlt = as.numeric(shlt > 0),
          any.iep = as.numeric(iep > 0),
          any.ell = as.numeric(ell > 0),
          any.shlt = ifelse(is.na(any.shlt) == T, 0, any.shlt)) %>%
  select(-pov, -shlt, -iep, -ell)

# calculate mean percentage of days absent and suspended
# average suspensions for each year
doe.full <- doe.full %>% 
  group_by(id) %>% 
  dplyr::summarise(av.abs = mean(per.abs, na.rm=T),
                   tot.sus = mean(sus, na.rm=T),
                   av.sus = mean(per.sus, na.rm=T),
                   av.missed = mean(per.miss, na.rm = T)) %>% 
  ungroup() %>% 
  right_join(doe.full) %>%
  select(-sus)

# indicate flag if student repeated a grade in grades 9 or 10
doe.full <- doe.full %>% 
  add_count(id) %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(any.repeats = case_when(n > 1 & (n_distinct(year) != n_distinct(grade)) ~ 1, 
                                 n == 1 | (n_distinct(year) == n_distinct(grade)) ~ 0)) %>% 
  select(-n)


# report final school attended, prior to grade 11
doe.full <- doe.full %>%
  group_by(id) %>%
  select(sch.fall, year) %>%
  dplyr::rename(soph.sch = sch.fall) %>%
  filter(year == max(year)) %>%
  select(-year) %>%
  right_join(doe.full)

# change all NaN values to NAs, this doesn't work
# doe.full[is.nan(doe.full)] <- NA

# clean up columns
doe.full <- doe.full %>% 
  select(-sch.fall, -sch.spr)

# add school level columns
doe.all <- doe.full %>%
  left_join(soph.sch)

# checkpoint, for troubleshooting
backup <- doe.full
doe.full <- backup


#######################################
# THINGS THAT AREN'T WORKING PROPERLY #
#######################################



