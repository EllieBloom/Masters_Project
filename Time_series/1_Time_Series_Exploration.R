
# Exploring time series ---------------------------------------------------

# Date started: 30th May 2022

# Setup

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fpp3)
library(slider) #for the moving averages

# Loading Google data

google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")

# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)


# Adding bank holidays as a field - ref: https://www.gov.uk/bank-holidays

england_sub_region_1 <- c("Bath and North East Somerset","Bedford", "Blackburn with Darwen",
                          "Blackpool", "Borough of Halton","Bracknell Forest","Brighton and Hove", 
                          "Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire",
                          "Cheshire East","Cheshire West and Chester","Cornwall","County Durham",
                          "Cumbria", "Darlington","Derby","Derbyshire","Devon", "Dorset",
                          "East Riding of Yorkshire", "East Sussex", "Essex","Gloucestershire",
                          "Greater London","Greater Manchester","Hampshire" , "Hartlepool",
                          "Herefordshire", "Isle of Wight", "Kent", "Kingston upon Hull",
                          "Lancashire", "Leicester","Leicestershire", "Lincolnshire","Luton",
                          "Medway", "Merseyside", "Middlesbrough","Milton Keynes", "Norfolk",
                          "North East Lincolnshire", "North Lincolnshire", "North Somerset", 
                          "North Yorkshire", "Northamptonshire", "Northumberland", "Nottingham",
                          "Nottinghamshire", "Oxfordshire", "Peterborough", "Plymouth","Portsmouth",
                          "Reading", "Redcar and Cleveland", "Rutland", "Shropshire", "Slough",
                          "Somerset", "South Gloucestershire", "South Yorkshire", "Southampton",
                          "Southend-on-Sea", "Staffordshire", "Stockton-on-Tees", "Stoke-on-Trent",
                          "Suffolk", "Surrey", "Swindon", "Thurrock", "Torbay","Tyne and Wear",
                          "Warrington", "Warwickshire", "West Berkshire", "West Midlands",
                          "West Sussex", "West Yorkshire", "Wiltshire", "Windsor and Maidenhead",
                          "Wokingham", "Worcestershire","York")


google_england <- google_overall %>% filter(sub_region_1 %in% england_sub_region_1) %>% filter(sub_region_2 == "") # Blank sub_region_2 means at the overall level only



# Converting to tstibble (time series tibble) -----------------------------

# England regional tstibble
england_tibble <- google_england %>%
  select(-c("sub_region_2","metro_area","country_region","census_fips_code","place_id","X","iso_3166_2_code","country_region_code")) %>%
  as_tsibble(key='sub_region_1',index=date)

# Just London tstibble
london_tibble <- england_tibble %>% filter(sub_region_1=="Greater London")
london_tibble


# Exploring London data ---------------------------------------------------

# Autoplot recognises the tibble
autoplot(london_tibble, workplaces_percent_change_from_baseline) +
  labs(title = "Workplace mobility",
       subtitle = "London",
       y = "Change in mobility from baseline (%)",
       x= "Date")+
  theme_light()

# Seasonal plot for parks
london_tibble %>%
  gg_season(parks_percent_change_from_baseline, labels = "both") +
  labs(y = "Change in mobility from baseline (%)",
       title = "Seasonal plot: Park mobility")


# Checking if any seasonality in workplace mobility
london_tibble %>%
  gg_season(workplaces_percent_change_from_baseline, labels = "both") +
  labs(y = "Change in mobility from baseline (%)",
       title = "Seasonal plot: Workplace mobility")
# Christmas/bank holidays have a seasonal effect

library(httr)
library(jsonlite)


# Downloading covid cases for England through API -------------------------
# https://coronavirus.data.gov.uk/details/download


res = GET("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=newCasesBySpecimenDate&format=json")
rawToChar(res$content)
data = fromJSON(rawToChar(res$content))
london_cases <-data$body

london_cases$date<-as.Date(london_cases$date)

london_cases_tibble <- london_cases %>%
  select(-c("areaType","areaCode")) %>%
  as_tsibble(key=areaName,index=date)


# Quick plot
autoplot(london_cases_tibble, newCasesBySpecimenDate) +
  labs(title = "Official cases",
       subtitle = "London",
       y = "Daily cases",
       x= "Date")+
  theme_light()


# Trying to merge cases and mobility time series

london_combined <- merge(x= london_tibble, y= london_cases_tibble, by= 'date', all.x= F)

colnames(london_combined)

london_combined_tibble <- london_combined %>%
  select(-c(sub_region_1)) %>%
  as_tsibble(key=areaName, index=date)



# Correlation between cases and workplace mobility ------------------------

# Over all years
london_combined_tibble %>%
  ggplot(aes(x = workplaces_percent_change_from_baseline, y = newCasesBySpecimenDate)) +
  geom_point() +
  ggtitle("London")+
  labs(x = "Workplace mobility",
       y = "Cases")+
  theme_light()


london_combined_tibble %>%
  filter(year(date) == 2020) %>%
  ggplot(aes(x = workplaces_percent_change_from_baseline, y = newCasesBySpecimenDate)) +
  geom_point() +
  ggtitle("London 2020") +
  labs(x = "Workplace mobility",
       y = "Cases")+
  theme_light()

london_combined_tibble %>%
  filter(year(date) == 2021) %>%
  ggplot(aes(x = workplaces_percent_change_from_baseline, y = newCasesBySpecimenDate)) +
  geom_point() +
  ggtitle("London 2020") +
  labs(x = "Workplace mobility",
       y = "Cases")+
  theme_light()

london_combined_tibble %>%
  filter(year(date) == 2022) %>%
  ggplot(aes(x = workplaces_percent_change_from_baseline, y = newCasesBySpecimenDate)) +
  geom_point() +
  ggtitle("London 2020") +
  labs(x = "Workplace mobility",
       y = "Cases")+
  theme_light()


# Doesn't look very correlated at all


# Autocorrelation ---------------------------------------------------------

# Note this is daily, so shows that there are clear weekly correlations - greatest correlations for multiples of 7
london_combined_tibble %>%
  ACF(workplaces_percent_change_from_baseline) %>%
  autoplot() + labs(title="London workplace mobility ACF")


# How can I change this to show monthly or quarterly lags i.e. seasonal trends? (2.8 in book)



# Looking at components - workplace ----------------------------------------------------

dcmp <- london_combined_tibble %>%
  model(stl = STL(workplaces_percent_change_from_baseline))

components(dcmp)

#Plot
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(workplaces_percent_change_from_baseline, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Change in mobility comapred to baseline (%)",
    title = "Workplace mobility in London - decomposition"
  )

# Need to be careful as there were two lockdown in Jan -> may take this for being seasonal...

components(dcmp) %>% autoplot()
# Looks very much like the remainder is due to lockdowns - test this by looking at parks (would expect to see less of a remainder)


# Looking at components - parks ----------------------------------------------------

dcmp_parks <- london_combined_tibble %>%
  model(stl = STL(parks_percent_change_from_baseline))

components(dcmp)

#Plot
components(dcmp_parks) %>%
  as_tsibble() %>%
  autoplot(parks_percent_change_from_baseline, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Change in mobility comapred to baseline (%)",
    title = "Parks mobility in London - decomposition"
  )

# Need to be careful as there were two lockdown in Jan -> may take this for being seasonal...

components(dcmp_parks) %>% autoplot()
 
# Parks seasonally adjusted

components(dcmp_parks) %>%
  as_tsibble() %>%
  autoplot(parks_percent_change_from_baseline  , colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Change in mobility comapred to baseline (%)",
       title = "Parks mobility in London - seasonality removed")



#  Continue with 3.3 moving averages --------------------------------------

london_ma <- london_combined_tibble %>%
  mutate(
    `7-MA` = slide_dbl(workplaces_percent_change_from_baseline, mean, .before = 3, .after = 3, .complete = TRUE),)

london_ma %>%
  autoplot(workplaces_percent_change_from_baseline, colour = "gray") +
  geom_line(aes(y = `7-MA`), colour = "#D55E00") +
  labs(y = "Moving average change from baseline movement (percentage)",
       title = "London workplace movement - 7-MA ")+
  theme_light()



# STL decomposition -------------------------------------------------------

london_combined_tibble %>%
  model(
    STL(workplaces_percent_change_from_baseline ~ trend(window = 7) +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()



# ACF features ----------------------------------------------------------


london_combined_tibble %>% features(workplaces_percent_change_from_baseline, feat_acf)


# STF features ------------------------------------------------------------

london_combined_tibble %>% features(workplaces_percent_change_from_baseline, feat_stl)

