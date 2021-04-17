# load libraries
library(tidyverse)
library(foreach)
library(lubridate)

# setwd to doe folder

# load in all data
doe.full <- foreach(year=2013:2019, .combine='rbind.fill') %do% {
  filename <- paste0('student_',year, '.csv')
  this.data <- read_csv(filename)
  this.data
}

nsc <- read_csv('nsc_all.csv')

# rename columns
doe.full <- doe.full %>%
  dplyr::rename(id = RANYCSID,
                pov = ANYPOV,
                hmls = STHFLG,
                shlt = SHLFLG,
                iep = IEPSPDBIO,
                ell = LEPBIO,
                year = YEAR,
                hlang = HLANG,
                bplace = BPLACE,
                GENCAT = GENCAT,
                eth = ETHCAT,
                dob = DOB,
                DOEGLVOCT = DOEGLVOCT,
                sch.fall = DBNOCT,
                AGDDCATOCT = AGDDCATOCT,
                sch.spr = DBNJUN,
                abs = ABSTOT,
                sus = SUSTOT,
                sus.days = SUSTOTDAYS,
                ela = ELASSC,
                mth = MTHSSC)

# identify students that have no school id listed
doe.st.sc <- doe.full %>%
  mutate(noschool = case_when(is.na(DBNOCT) & is.na(DBNJUN) ~ 1))

# figure out how we want to address number of years posted
# identify dropouts/moved out of city
# eliminate those who we only have their senior year
doe.full %>%
group_by(id) %>%
  tally() %>%
  filter(n()<4)

# parse birth year to new column
doe.full <- doe.full %>%
  mutate(birth.yr = year(mdy(dob)))

# create moved mid-year column
doe.full <- doe.full %>%
  mutate(mvd = case_when(sch.fall != sch.spr ~ 1,
                         sch.fall == sch.spr ~ 0))

# summarize student level data
doe.st <- doe.full %>%
  group_by(id) %>%
  summarise(ell, ell, abs, sus.days)
