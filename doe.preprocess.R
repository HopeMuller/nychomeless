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

# rename is and year variables
nsc <- nsc %>% 
  dplyr::rename(id = RANYCSID,
                year = YEAR)

# read in school-level data
# Hope: sch.doe <- read.csv("/Users/Home/Documents/MessyData/finalproj/DOE_schooldata.csv")
# Kenny: sch.doe <- read_csv("DOE_schooldata.csv")

# rename school column for merging
sch.doe <- sch.doe %>%
  rename(mod.sch = DBN)

# doe data
raw.student <- foreach(year=2013:2019, .combine='rbind.fill') %do% {
  filename <- paste0('student_',year, '.csv')
  this.data <- read_csv(filename)
  this.data
}

# assign student-level data to new name
doe.full <- raw.student

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
  # filter out suspensions listed with more days that school year
  filter(sus.days < 183) %>%
  # change grades to numeric values
  mutate(grade = as.numeric(grade),
         # recode gender male as 0
         gen = ifelse(gen == 2, 0, 1),
         # combine absent days and suspended days as days missing from school
         missed = abs + sus.days,
         # code percentage days absent per year and percent days suspended per year
         per.missed = round(missed/182,2),
         per.missed = ifelse(per.missed > 1, 1, per.missed),
         # parse birth year to new column
         birth.yr = year(mdy(dob)),
         # create column to show if they moved mid-year
         mvd.mid = case_when(sch.fall != sch.spr ~ 1,
                             sch.fall == sch.spr ~ 0)) %>%

# filter out students who were not going to graduate by 2019
  filter(!(grade < 12 & year == 2019) |
         (grade < 11 & year == 2018) |
         (grade < 10 & year == 2017))

# filter out students who didn't attend 9th grade in a DOE
doe.full <- doe.full %>%
  group_by(id) %>%
  filter(min(grade) == 9)

# column for total grades completed within NYC DOE (grades 9 - 12): visualization variable
doe.full <- doe.full %>%
  group_by(id) %>%
  dplyr::summarise(comp.grades = n_distinct(grade)) %>%
  ungroup() %>% 
  right_join(doe.full)

# report final school attended (grade 9 - 12): visualization variable
doe.full <- doe.full %>%
  group_by(id) %>%
  select(sch.spr, year) %>%
  rename(final.sch = sch.spr) %>%
  filter(year == max(year)) %>%
  select(-year) %>%
  right_join(doe.full)

# report final status as of 2019: outcome variable
doe.full <- doe.full %>% 
  group_by(id) %>% 
  dplyr::summarise(final.status = last(status.spr)) %>% 
  ungroup() %>% 
  right_join(doe.full)

# filter out students whose final status is moving since we don't have the outcome we need
doe.full <- doe.full %>% 
  filter(final.status != 4)

# take out grades 11 and 12 for predictive variables below -----------------------------------
doe.full <- doe.full %>%
  filter(grade < 11)

# add "any" flags
doe.full <- doe.full %>% 
  group_by(id) %>% 
  mutate(any.pov = as.numeric(pov > 0),
         any.shlt = as.numeric(shlt > 0),
         any.iep = as.numeric(iep > 0),
         any.ell = as.numeric(ell > 0),
         any.shlt = ifelse(is.na(any.shlt) == T, 0, any.shlt),
         any.mvd = max(mvd.mid)
  )

# add total count of schools each student attended
doe.full <- doe.full %>%
  group_by(id) %>%
  dplyr::summarise(num.schools = n_distinct(interaction(sch.fall, sch.spr))) %>%
  ungroup() %>% 
  right_join(doe.full)

# calculate mean percentage and total days missed (absent and suspended) 
# calculate mean number of suspensions per year
doe.full <- doe.full %>% 
  group_by(id) %>% 
  dplyr::summarise(mn.days.miss = round(mean(missed, na.rm=T)),
                   av.per.miss = round(mean(per.missed, na.rm=T),2),
                   mn.num.sus = round(mean(sus, na.rm=T))) %>% 
  ungroup() %>% 
  right_join(doe.full)

# create column for freshman year
doe.full <- doe.full %>%
  group_by(id) %>%
  mutate(frsh = case_when(grade == 9 ~ year - 1),
         frsh = min(year)) %>%
  ungroup() %>%
  right_join(doe.full)

# create column of age difference between NYC mandated school-age entry and grade 9-age entry
doe.full <- doe.full %>%
  mutate(age.diff = frsh - birth.yr - 14) %>%

# filter out students listed as starting their freshman year at age 19 or after
  filter(age.diff < 7)

# indicate flag if student repeated a grade, in grade 9 or 10
doe.full <- doe.full %>% 
  add_count(id) %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(any.repeats = case_when(n > 1 & (n_distinct(year) != n_distinct(grade)) ~ 1, 
                                 n == 1 | (n_distinct(year) == n_distinct(grade)) ~ 0)) %>% 
  select(-n)

# report final school attended (grade 9 and 10)
doe.full <- doe.full %>%
  group_by(id) %>%
  select(sch.spr, year) %>%
  rename(mod.sch = sch.spr) %>%
  filter(year == max(year)) %>%
  select(-year) %>%
  right_join(doe.full)

# change all NaN values in av.missed to 0
# doe.full <- doe.full %>% 
#   mutate(av.missed = ifelse(is.na(av.missed) == T, 0, av.missed))

# clean up columns
doe.full <- doe.full %>% 
  select(-pov, -hmls, -shlt, -iep, -ell, -abs, -sus, -sus.days, -missed,
         -per.missed, -dob,-sch.fall, -sch.spr, -status.fall, -status.spr, -frsh, 
         -birth.yr, -mvd.mid)

# collapsing rows, so that there is only one row per student
doe.full <- doe.full %>% 
  group_by(id) %>% 
  filter(year == max(year))

# checkpoint, for troubleshooting
backup <- doe.full
doe.full <- backup

# add school level columns and nsc first year college columns
doe.all <- doe.full %>%
  left_join(sch.doe) %>% 
  left_join(nsc)
