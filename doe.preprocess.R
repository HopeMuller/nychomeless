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
                mth = MTHSSC)

# filter out grades below grade 9
doe.full <- doe.full %>%
  filter(grade == "08" | grade == "09" | grade == "10" | grade == "11" | grade == "12")

# change grade to numeric
doe.full <- doe.full %>%
  mutate(grade = as.numeric(grade)) 

# add grade 8 scores to rows for all high school years
# subset grade 8 rows with math or ela scores
# doe.88 <- doe.full %>%
#   group_by(id) %>%
#   mutate(gr8.ela = case_when(grade == 8 ~ ela)) %>%
#   mutate(grade8.ela = case_when(grade != 8 ~ sum(gr8.ela)))
# 
# doe.8 <- doe.full %>%
#   filter(grade == 8) %>%
#   filter(!is.na(ela | mth)) %>%
#   select(id, ela, mth)
# 
# # create a row for grade 8 math and ela scores
# doe.full$gr8.ela <- NULL
# doe.full$gr8.mth <- NULL
# 
# # fill main data frame with grade 8 math and ela scores for occurences where they exist
# ids <- unique(doe.8$id)
# for (i in seq_along(ids)){
#   i = "i"
#   ela <- doe.8$ela[doe.8$id == i]
#   mth <- doe.8$mth[doe.8$id == i]
#   doe.full$gr8.ela[doe.full$id == i] <- ela
#   doe.full$gr8.mth[doe.full$id == i] <- mth
# }

# filter out grade 8 rows
doe.full <- doe.full %>%
  filter(grade > 8)

# add a count column for how many rows each student has
doe.full <- doe.full %>%
  add_count(id)

# create column to flag students who repeated grades or may be entered multiple times
doe.full <- doe.full %>%
  group_by(id) %>%
  arrange(id) %>%
  mutate(repeats = case_when(n > 1 & (n_distinct(year) != n_distinct(grade)) ~ 1, 
                             n == 1 | (n_distinct(year) == n_distinct(grade)) ~ 0))

# recode gender male as 0
doe.full$gen[doe.full$gen == 2] <- 0

# identify students that have no school ids listed
doe.full <- doe.full %>%
  mutate(noschool = case_when(is.na(sch.fall) & is.na(sch.spr) ~ 1))

# parse birth year to new column
doe.full <- doe.full %>%
  mutate(birth.yr = year(mdy(dob)))

# create column for freshman year, this needs editing due to the repeated rows for people's 9th grade year
doe.full <- doe.full %>%
  mutate(frsh = case_when(grade == 9 ~ year - 1))

# create column of age difference between NYC mandated school-age entry and grade 9-age entry
doe.full <- doe.full %>%
  mutate(age.diff = frsh - birth.yr - 14)

# create column to show if they moved mid-year
doe.full <- doe.full %>%
  mutate(mvd = case_when(sch.fall != sch.spr ~ 1,
                         sch.fall == sch.spr ~ 0))
