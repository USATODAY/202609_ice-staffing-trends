
## analysis on ICE staffing changes

## data from the Office of Personnel Management: 
# https://data.opm.gov/explore-data/data/data-downloads

# data dictionary: https://www.opm.gov/data/datasets/

library(tidyverse)
library(janitor)
library(data.table)

rm(list = ls())

## pt. 1: new hires
# we will use the 'Accessions' data to examine hires

# list all the monthly files we downloaded since 2005
# the latest version for each (v3 when available, otherwise v2 or v1)
# note that the snapshots are from the last day of the month
files_acc <- tibble(file = list.files("inputs/accessions")) %>%
  mutate(file = file.path("inputs/accessions", file))

# now make a function to pull raw files accounting for column changes over time
data_reader_fc <- function(file, var, keep_cols) {
  
  # read while normalizing col type
  df <- tryCatch({
    
    read_delim(file, delim = "|", col_types = cols(.default = col_character())) %>%
      as_tibble()
    
  }, error = function(e) {
    message("Error reading: ", file)
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)
  
  # add missing cols
  add_missing_cols <- function(df, cols) {
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0) {
      df[missing] <- NA
    }
    df
  }
  
  df <- add_missing_cols(df, keep_cols)
  
  # keep only desired columns
  df <- df %>% select(all_of(keep_cols))
  
  # extract date from file name
  var_regex <- paste0(var, "_(\\d{4})(\\d{2})")
  
  # put cleaner file together
  df <- df %>%
    mutate(file = basename(file)) %>%
    extract(file, into = c("year", "month"),
            regex = var_regex,
            remove = FALSE) %>%
    mutate(year  = as.integer(year),
           month = as.integer(month),
           date = ymd(paste(year, month, "01"))) %>%
    relocate(year, month, date, .before = everything())
  
  return(df)
  
}

# iterate targeting the following columns
cols_acc <- c("accession_category", "age_bracket", "agency", "agency_subelement",
              "appointment_not_to_exceed_date", "appointment_type", "duty_station_state",
              "duty_station_county", "count", "education_level", "education_level_bracket", 
              "education_level_code", "occupational_group_code", "occupational_group",
              "occupational_series_code", "occupational_series", 
              "veteran_indicator", "work_schedule")
  
acc_df <- map_dfr(.x = files_acc$file, .f = data_reader_fc,
                  var = "accessions", keep_cols = cols_acc) %>%
  mutate(count = as.numeric(count))

# now get a subset for ICE
# this data starts mid 2011
acc_ice <- acc_df %>%
  filter(agency_subelement == "IMMIGRATION AND CUSTOMS ENFORCEMENT")

# filter further to keep new hires only (so exclude transfers)
# and remove anything that is not full-time (seasonal, part-time, intermittent)
acc_ice_ft_hires <- acc_ice %>%
  filter(accession_category %in% c("NEW HIRE - COMPETITIVE SERVICE APPOINTMENT",
                                   "NEW HIRE - EXCEPTED SERVICE APPOINTMENT",
                                   "NEW HIRE - SENIOR EXECUTIVE SERVICE (SES) APPT"),
         work_schedule == "FULL-TIME")

## now let's start exploring this data

# monthly tally
acc_ice_ft_hires_monthly <- acc_ice_ft_hires %>%
  group_by(date) %>%
  summarise(count = n())

# sept 2025 is by far the month with the most hires: 2,498
# before trump’s second term, the highest was september 2019 with 278
acc_ice_ft_hires_monthly %>%
  ggplot(aes(x = date, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "ICE new full-time hires by month") +
  theme_linedraw()

# group by year too
# note: since 2011 starts in August we will remove it and start at 2012
# 2026 runs through June
acc_ice_ft_hires_yearly <- acc_ice_ft_hires %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  filter(year != 2011)

acc_ice_ft_hires_yearly %>%
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "ICE new full-time hires by year*", 
       subtitle = "*2026 numbers through June") +
  theme_linedraw()

# now let's compare some characteristics from hires since jan 2025
# versus those in the decade before (2015-2024)
acc_ice_ft_hires_2015_2024 <- acc_ice_ft_hires %>%
  filter(year %in% c(2015:2024))

acc_ice_ft_hires_2025_present <- acc_ice_ft_hires %>%
  filter(year >= 2025)

# the hires from sept. 2025 alone are higher than any full year before
acc_ice_ft_hires_monthly %>% filter(date == "2025-09-01")
acc_ice_ft_hires_yearly %>% arrange(desc(count)) %>% head(5)

# 11,840 people were hired from January 2025 through june 2026,
# more than the previous decade combined (2015:2024)
sum(acc_ice_ft_hires_2025_present$count)
sum(acc_ice_ft_hires_2015_2024$count)

# duty station state: mostly redacted
acc_ice_ft_hires_2015_2024 %>%
  group_by(duty_station_state) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/nrow(acc_ice_ft_hires_2015_2024), 2)) %>%
  adorn_totals()

acc_ice_ft_hires_2025_present %>%
  group_by(duty_station_state) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/nrow(acc_ice_ft_hires_2025_present), 2)) %>%
  adorn_totals()

# occupational series
acc_ice_ft_hires_2015_2024 %>%
  rename(code = occupational_series_code) %>%
  group_by(occupational_series, code) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/nrow(acc_ice_ft_hires_2015_2024), 2)) %>%
  arrange(desc(count)) %>%
  adorn_totals()

acc_ice_ft_hires_2025_present %>%
  rename(code = occupational_series_code) %>%
  group_by(occupational_series, code) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/nrow(acc_ice_ft_hires_2025_present), 2)) %>%
  arrange(desc(count)) %>%
  adorn_totals()

# education level
acc_ice_ft_hires_2015_2024 %>%
  group_by(education_level_bracket) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/nrow(acc_ice_ft_hires_2015_2024), 2)) %>%
  arrange(desc(pct)) %>%
  adorn_totals()

acc_ice_ft_hires_2025_present %>%
  group_by(education_level_bracket) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/nrow(acc_ice_ft_hires_2025_present), 2)) %>%
  arrange(desc(pct)) %>%
  adorn_totals()

# do a streamlined version combining masters and phd into graduate degrees
ice_ed_level_2015_2024 <- acc_ice_ft_hires_2015_2024 %>%
  mutate(education_level_bracket_simplified = case_when(
    education_level_bracket == "MASTERS OR PROFESSIONAL DEGREE" ~ "GRADUATE DEGREE",
    education_level_bracket == "DOCTORATE DEGREE" ~ "GRADUATE DEGREE",
    T ~ education_level_bracket)) %>%
  group_by(education_level_bracket_simplified) %>%
  summarise(count_2015_2024 = n()) %>%
  mutate(pct_2015_2024 = round(count_2015_2024/nrow(acc_ice_ft_hires_2015_2024), 2)) %>%
  arrange(desc(pct_2015_2024))

ice_ed_level_2025_present <- acc_ice_ft_hires_2025_present %>%
  mutate(education_level_bracket_simplified = case_when(
    education_level_bracket == "MASTERS OR PROFESSIONAL DEGREE" ~ "GRADUATE DEGREE",
    education_level_bracket == "DOCTORATE DEGREE" ~ "GRADUATE DEGREE",
    T ~ education_level_bracket)) %>%
  group_by(education_level_bracket_simplified) %>%
  summarise(count_2025_present = n()) %>%
  mutate(pct_2025_present = round(count_2025_present/nrow(acc_ice_ft_hires_2025_present), 2)) %>%
  arrange(desc(pct_2025_present))

# clean up format to export
ice_education_level <- left_join(ice_ed_level_2015_2024, ice_ed_level_2025_present, 
                                 by = "education_level_bracket_simplified") %>%
  mutate(education_level_bracket_simplified = str_to_sentence(education_level_bracket_simplified),
         pct_2015_2024 = pct_2015_2024 * 100,
         pct_2025_present = pct_2025_present * 100)

rm(ice_ed_level_2015_2024, ice_ed_level_2025_present)

ice_education_level %>%
  select(-count_2015_2024, -count_2025_present) %>%
  write_csv("outputs/ice_education_level.csv")

## now do this for separations data

files_sep <- tibble(file = list.files("inputs/separations")) %>%
  mutate(file = file.path("inputs/separations", file))

cols_sep <- c("separation_category", "age_bracket", "agency", "agency_subelement", 
              "appointment_not_to_exceed_date", "appointment_type", "duty_station_state", 
              "duty_station_county", "count", "education_level", "education_level_bracket",
              "education_level_code", "occupational_group_code", "occupational_group",
              "occupational_series_code", "occupational_series", "veteran_indicator",
              "work_schedule", "length_of_service_years")

sep_df <- map_dfr(.x = files_sep$file, .f = data_reader_fc,
                  var = "separations", keep_cols = cols_sep) %>%
  mutate(length_of_service_years = as.numeric(length_of_service_years),
         count = as.numeric(count))

# get a subset for ICE
sep_ice <- sep_df %>%
  filter(agency_subelement == "IMMIGRATION AND CUSTOMS ENFORCEMENT")

# for ICE, create a new column grouping some separation categories
sep_ice <- sep_ice %>%
  mutate(separation = case_when(
    separation_category %in% c("QUIT") ~ "quit",
    separation_category %in% c("TERMINATION (EXPIRED APPT/OTHER)") ~ "termination",
    separation_category %in% c("REDUCTION IN FORCE (RIF)") ~ "reduction_in_force",
    separation_category %in% c("RETIREMENT - VOLUNTARY", 
                               "RETIREMENT - OTHER", 
                               "RETIREMENT - EARLY OUT") ~ "retirement",
    separation_category %in% c("TRANSFER OUT - INDIVIDUAL TRANSFER", 
                               "TRANSFER OUT - MASS TRANSFER",
                               "OTHER SEPARATION") ~ "other",
    T ~ "other"))

# get a monthly aggregation by sep type
sep_ice_monthly <- sep_ice %>%
  group_by(date, separation) %>%
  summarise(count = n())

sep_ice_monthly %>%
  ggplot(aes(x = date, y = count, 
             group = separation, fill = separation)) +
  geom_bar(stat = "identity") +
  labs(title = "ICE separations by month") +
  theme_linedraw()

# pivot it wider for easier comparison
# march 2026 was the month with the highest number of ICE staffers that quit. 
# march 2026 is also the highest for terminations, 
# for terminations, the 6 months in 2026 are the highest 6 months
sep_ice_monthly_wide <- sep_ice_monthly %>%
  pivot_wider(names_from = "separation", values_from = "count", values_fill = 0)

# group by year too
# note that 2011 starts in Aug so we will remove that year again
sep_ice_yearly <- sep_ice %>%
  group_by(year, separation) %>%
  summarise(count = n()) %>%
  filter(year != 2011)

sep_ice_yearly %>%
  ggplot(aes(x = year, y = count,
             group = separation, fill = separation)) +
  geom_bar(stat = "identity") +
  labs(title = "ICE separations by year*", 
       subtitle = "*2026 data through June") +
  theme_linedraw()

# also pivot wide for easier comparison
sep_ice_yearly_wide <- sep_ice_yearly %>%
  pivot_wider(names_from = "separation", values_from = "count", values_fill = 0) %>%
  mutate(total_sep = other + quit + retirement + termination)

# export
sep_ice_yearly_wide %>%
  select(year, Other = other, Retired = retirement, Quit = quit, Terminated = termination) %>%
  write_csv("outputs/sep_ice_yearly_wide.csv")

## now get a few more separation stats 2026 vs 2015-2024
# (rather than 2025-present because of the hiring and separation lag)

# terminations
sep_ice_terminations_2015_2024 <- sep_ice %>%
  filter(year %in% c(2015:2024), separation == "termination")

sep_ice_terminations_2026 <- sep_ice %>%
  filter(year == 2026, separation == "termination")

# so far in 2026, more people were terminated than the whole decade before
# the number of people that quit is also already the highest of any year
sum(sep_ice_terminations_2015_2024$count)
sum(sep_ice_terminations_2026$count)

# median tenure from staffers that were terminated in 2026 is much shorter
summary(sep_ice_terminations_2015_2024$length_of_service_years)
summary(sep_ice_terminations_2026$length_of_service_years)

# quit
sep_ice_quit_2015_2024 <- sep_ice %>%
  filter(year %in% c(2015:2024), separation == "quit")

sep_ice_quit_2026 <- sep_ice %>%
  filter(year == 2026, separation == "quit")

# median tenure from staffers that quit in 2026 is much shorter
summary(sep_ice_quit_2015_2024$length_of_service_years)
summary(sep_ice_quit_2026$length_of_service_years)


