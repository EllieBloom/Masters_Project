# Basic time series model


# Date started: 31st May 2022


# Setup

# Setup -------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(fpp3)
library(slider) #for the moving averages
library(reshape) # for melt
library(gtsummary) # for tbl_regression

# Useful dates ------------------------------------------------------------

bank_holidays <-as.Date(c("2020-01-01","2020-04-10","2020-04-13","2020-05-08",
                          "2020-05-25","2020-08-31","2020-12-25","2020-12-28",
                          "2021-01-01","2021-04-02","2021-04-05","2021-05-03",
                          "2021-05-31","2021-08-30","2021-12-27","2021-12-28",
                          "2022-01-03","2022-04-15","2022-04-18","2022-05-02"),
                        "%Y-%m-%d")

lockdown_1_start <- as.Date("2020-03-26","%Y-%m-%d")
lockdown_1_end <- as.Date("2020-06-15","%Y-%m-%d")

lockdown_2_start <- as.Date("2020-11-05","%Y-%m-%d")
lockdown_2_end <- as.Date("2020-12-02","%Y-%m-%d")

lockdown_3_start <- as.Date("2021-01-06","%Y-%m-%d")
lockdown_3_end <- as.Date("2021-04-21","%Y-%m-%d")


# Data setup ---------------------------------------------------------------

google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Analysis/Mapping/Outputs/google_regional_long.csv")

str(google_overall)

# Converting dates from string format to date

str(google_overall$date) # Dates are currently stored as characters
google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
str(google_overall$date)

# ref: https://www.gov.uk/bank-holidays
google_overall$bank_holiday <- ifelse(google_overall$date %in% bank_holidays,1,0)
google_overall$bank_holiday <- as.factor(google_overall$bank_holiday)

google_overall <- google_overall[,-1]

# Changing the order

google_overall <- google_overall[
  order(google_overall$region, google_overall$type_mobility, google_overall$date),
]

google_overall


# Creating tsibble

england_tibble <- google_overall %>%
  as_tsibble(key=c(region,type_mobility),index=date, regular=TRUE, validate=FALSE) # Shouldn't have to do validate = false
 
 
# 
# 
# # Old setup:
# # Loading and prepping data -----------------------------------------------
# 
# 
# google_overall <- read.csv("/Users/elliebloom/Desktop/Masters/Project/Data/Google/google_overall.csv")
# 
# # Converting dates from string format to date
# 
# str(google_overall$date) # Dates are currently stored as characters
# google_overall$date <- as.Date(google_overall$date,format="%Y-%m-%d")
# str(google_overall$date)
# 
# google_overall$bank_holiday <- ifelse(google_overall$date %in% bank_holidays,1,0)
# google_overall$bank_holiday <- as.factor(google_overall$bank_holiday)
# 
# 
# # Adding bank holidays as a field - ref: https://www.gov.uk/bank-holidays
# 
# england_sub_region_1 <- c("Bath and North East Somerset","Bedford", "Blackburn with Darwen",
#                           "Blackpool", "Borough of Halton","Bracknell Forest","Brighton and Hove", 
#                           "Bristol City","Buckinghamshire","Cambridgeshire","Central Bedfordshire",
#                           "Cheshire East","Cheshire West and Chester","Cornwall","County Durham",
#                           "Cumbria", "Darlington","Derby","Derbyshire","Devon", "Dorset",
#                           "East Riding of Yorkshire", "East Sussex", "Essex","Gloucestershire",
#                           "Greater London","Greater Manchester","Hampshire" , "Hartlepool",
#                           "Herefordshire", "Isle of Wight", "Kent", "Kingston upon Hull",
#                           "Lancashire", "Leicester","Leicestershire", "Lincolnshire","Luton",
#                           "Medway", "Merseyside", "Middlesbrough","Milton Keynes", "Norfolk",
#                           "North East Lincolnshire", "North Lincolnshire", "North Somerset", 
#                           "North Yorkshire", "Northamptonshire", "Northumberland", "Nottingham",
#                           "Nottinghamshire", "Oxfordshire", "Peterborough", "Plymouth","Portsmouth",
#                           "Reading", "Redcar and Cleveland", "Rutland", "Shropshire", "Slough",
#                           "Somerset", "South Gloucestershire", "South Yorkshire", "Southampton",
#                           "Southend-on-Sea", "Staffordshire", "Stockton-on-Tees", "Stoke-on-Trent",
#                           "Suffolk", "Surrey", "Swindon", "Thurrock", "Torbay","Tyne and Wear",
#                           "Warrington", "Warwickshire", "West Berkshire", "West Midlands",
#                           "West Sussex", "West Yorkshire", "Wiltshire", "Windsor and Maidenhead",
#                           "Wokingham", "Worcestershire","York")
# 
# 
# google_england <- google_overall %>% filter(sub_region_1 %in% england_sub_region_1) %>% filter(sub_region_2 == "") # Blank sub_region_2 means at the overall level only
# 
# 
# 
# 
# # Converting to tstibble (time series tibble) -----------------------------
# 
# # England regional tstibble
# england_tibble <- google_england %>%
#   select(-c("sub_region_2","metro_area","country_region","census_fips_code","place_id","X","iso_3166_2_code","country_region_code")) %>%
#   as_tsibble(key='sub_region_1',index=date)
# 
# 
# google_england_long <- melt(google_england[,c("retail_and_recreation_percent_change_from_baseline", 
#                                           "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", 
#                                           "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", 
#                                           "residential_percent_change_from_baseline","date","sub_region_1","bank_holiday")], id.vars=c("date","sub_region_1","bank_holiday"))
# 
# 
# # Renaming columns
# colnames(google_england_long) <- c("date","region","bank_holiday","type_mobility","mobility")
# 
# # Changing the mobility type fileds to be shorter
# 
# google_england_long <- google_england_long %>% mutate(type_mobility=recode(type_mobility,
#                                               "retail_and_recreation_percent_change_from_baseline"="workplace", 
#                                               "grocery_and_pharmacy_percent_change_from_baseline"="grocery_pharmacy",
#                                               "parks_percent_change_from_baseline"="parks", 
#                                               "transit_stations_percent_change_from_baseline"="transit_stations",
#                                               "workplaces_percent_change_from_baseline"="workplaces", 
#                                               "residential_percent_change_from_baseline"="residential"))
# # Data is now tidier and easier to filter
# 
# england_tibble <- google_england_long %>%
#   as_tsibble(key=c(region,type_mobility),index=date)



# Investigative plot ------------------------------------------------------

# workplace and London

england_tibble %>%
  filter(region=="LONDON",type_mobility=="workplaces") %>%
  autoplot(mobility) +
    labs(title = "Workplace mobility",
         subtitle = "London",
         y = "Change in mobility from baseline (%)",
         x= "Date")+
    theme_light()



# London workplace decomposition ----------------------------------------------------

dcmp_london <- england_tibble %>%
  filter(region=="LONDON",type_mobility=="workplaces") %>%
  model(stl = STL(mobility))

components(dcmp_london)

components(dcmp_london) %>%
  as_tsibble() %>%
  autoplot(mobility, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Mobility change from baseline (%)",
    title = "London workplace mobility decomposition") +
  theme_light()

components(dcmp_london) %>% autoplot()

# Seasonally adjusted data

components(dcmp_london) %>%
  as_tsibble() %>%
  autoplot(mobility, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Mobility change from baseline (%)",
       title = "London workplace mobility - seasonal adjustment")


# Just London data --------------------------------------------------------
# # https://coronavirus.data.gov.uk/details/download

london_tibble <- england_tibble %>%
  filter(region=="LONDON")

#Adding in London covid cases

res = GET("https://api.coronavirus.data.gov.uk/v2/data?areaType=region&areaCode=E12000007&metric=newCasesBySpecimenDate&format=json")
rawToChar(res$content)
data = fromJSON(rawToChar(res$content))
london_cases <-data$body

london_cases$date<-as.Date(london_cases$date)

london_cases_tibble <- london_cases %>%
  select(-c("areaType","areaCode")) %>%
  as_tsibble(key=areaName,index=date)


london_tibble <- merge(x= london_tibble, y= london_cases_tibble, by= 'date', all.x= F)
london_tibble <- london_tibble %>% select(-c(areaName)) %>% as_tsibble(key=c(type_mobility), index=date)


colnames(london_tibble)[6]<-"cases"


# Now have a combined London tibble

# Also a tibble for London and lockdown 1:

london_tibble_lockdown1<-london_tibble %>% filter(date>lockdown_1_start,date<lockdown_2_start)


# Workplace, London, lockdown 1 -------------------------------------------

# workplace and London

london_tibble_lockdown1 %>%
  filter(type_mobility=="workplaces") %>%
  autoplot(mobility) +
  labs(title = "Workplace mobility lockdown 1",
       subtitle = "London",
       y = "Change in mobility from baseline (%)",
       x= "Date")+
  theme_light()

# Decomposition

dcmp_london_lockdown1 <- london_tibble_lockdown1 %>%
  filter(type_mobility=="workplaces") %>%
  model(stl = STL(mobility))

components(dcmp_london_lockdown1)

components(dcmp_london_lockdown1) %>%
  as_tsibble() %>%
  autoplot(mobility, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Mobility change from baseline (%)",
    title = "London lockdown 1workplace mobility decomposition") +
  theme_light()

components(dcmp_london) %>% autoplot()

# Seasonally adjusted data

components(dcmp_london_lockdown1) %>%
  as_tsibble() %>%
  autoplot(mobility, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Mobility change from baseline (%)",
       title = "London lockdown 1 workplace mobility - seasonal adjustment")



# TSLM model --------------------------------------------------------------

fit_london_lockdown1_tslm <- london_tibble_lockdown1 %>%
  filter(type_mobility=="workplaces") %>%
  model(TSLM(mobility ~ bank_holiday + trend() + season()))

report(fit_london_lockdown1_tslm)
# 0.178 increase per day in London
# weeks mean each day of the week - it is seasonal over each week
# Not sure which day is the reference, maybe it's alphabetical?


# Plotting this fit for lockdown 1
augment(fit_london_lockdown1_tslm) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = mobility, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Mobility change from baseline (%)",
       title = "London mobility lockdown 1 - Time Series Linear Model (TSLM)") +
  guides(colour = guide_legend(title = "Series")) +
  theme_light()

# Actual vs fitted by days of the week
augment(fit_london_lockdown1_tslm) %>%
  ggplot(aes(x = mobility, y = .fitted,
             colour = factor(weekdays(date)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "London mobility lockdown 1 - actual mobility vs fitted mobility") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Weekday"))+
  theme_light()

# Note the 'outliers' here are bank holidays, which are accounted for in the model - trying to add this as the shape
# Could also do this by having an 'intervention' variable to signify when there are and aren't lockdown




# ARIMA models ------------------------------------------------------------


# Looking at stationarity


london_tibble_lockdown1 %>% ACF(mobility) %>% 
  autoplot() + labs(subtitle = "London mobility lockdown 1")
# None look very stationary


london_tibble_lockdown1 %>% ACF(difference(mobility)) %>% 
  autoplot() + labs(subtitle = "Changes in London mobility lockdown 1")
# A lot more stationary however there is still weekly seasonality - is that ok?

# Ljung-Box test for stationarity
# H0: data are independently distributed
# H1: data are not independtly distributed; they exhibit serial correlation

london_tibble_lockdown1 %>%
  mutate(diff_mobility = difference(mobility)) %>%
  features(diff_mobility, ljung_box, lag = 14) # probably makes sense to include a lag of a multiple of 7 

# All p values are <0.05 -> reject null -> not stationary (exhibit serial correlation)

# Try the second difference
london_tibble_lockdown1 %>%
  mutate(diff2_mobility = difference(difference(mobility))) %>%
  features(diff2_mobility, ljung_box, lag = 14) # probably makes sense to include a lag of a multiple of 7 
# Still all non stationary -> moving average could introduce stationarity?



## Seasonal ARIMA model (9.9) --------------------------------------------

london_tibble_lockdown1 %>%
  filter(type_mobility=="workplaces") %>%
  gg_tsdisplay(difference(mobility, 7),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")
# Still non-stationary - try double differencing

london_tibble_lockdown1 %>%
  filter(type_mobility=="workplaces") %>%
  gg_tsdisplay(difference(mobility, 7) %>% difference(), # note this is seasonal differencing -> 7 term
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

# Significant ACF spikes at 1 -> non-seasonal MA(1) component?

# Significant PACF spike at 7 -> try a seasonal MA(1) component

# Therefore arrive at:
# ARIMA (p,d,q) (P,D,Q)m
# m= seasonal period, lower case- non-seasonal part of the model, upper case - seasonal part of the model
# p - order of autoregressive part
# d - degree of first differencing involved
# q - order of the moving average part

# In this case:
# m =7 -> weekly seasonality
# p = 0 (currently not looking at autoregressive part)
# d = 2 (2xdifferencing)
# q = 1 (MA-1 for non-seasonal)
# P = 0 (currently not looking at autoregressive part)
# D = 2
# Q = 1 (1xm)

# Not sure how to interpret this...

fit <- london_tibble_lockdown1 %>%
  filter(type_mobility=="workplaces") %>%
  model(
    arima012011 = ARIMA(mobility ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(mobility ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(mobility, stepwise = FALSE, approx = FALSE)
  )
  
glance(fit) %>% arrange(AICc) %>% select(.model:BIC)
fit # Auto is the best on AIC, and is ARIMA(3,0,2)(0,1,1)[7]

# Looking at residuals from the auto model

fit %>% select(auto) %>% gg_tsresiduals(lag=7)
# Looks pretty good, do the Ljung Box test to check

augment(fit) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=24, dof=4)

# p>0.05 -> residuals are similar to white noise

# Look at the forecast to see what it looks like...

# Could do this to see if we can predict what coming out of lockdown looks like from an earlier date using test/train or cross-validation

forecast(fit, h=7) %>%
  filter(.model=='auto') %>%
  autoplot(london_tibble_lockdown1) +
  labs(title = "Forecast beyond lockdown 1",
       y="Change in workplace mobilty from baseline (%)")


# Exploring granger causality test ----------------------------------------

library(lmtest)
# https://www.r-bloggers.com/2021/11/granger-causality-test-in-r-with-example/
#grangertest(x, y, order = 1, na.action = na.omit, ...)

london_tibble_lockdown1_worplaces <- london_tibble_lockdown1 %>% filter(type_mobility=="workplaces")

grangertest(london_tibble_lockdown1_worplaces$cases, 
            london_tibble_lockdown1_worplaces$mobility, order = 4*7)

# p>0.05 -> mobility not useful in forecasting cases

# Presumably need further ahead data for cases


# Try without the lockdown 1 restriction

london_tibble_workplaces <- london_tibble %>% filter(type_mobility=="workplaces")

grangertest(london_tibble_workplaces$cases, 
            london_tibble_workplaces$mobility, order = 4*7)

# Over all of the data with a 4 week lag very significant!!!! So maybe it predicts the up, but not the decline

# Try with e.g. 2 week lag
grangertest(london_tibble_workplaces$cases, 
            london_tibble_workplaces$mobility, order = 2*7)

# Presumably need to do dynamic time warping to find out what the lag is


ggplot(as.data.frame(london_tibble_workplaces) ) +
  geom_line(aes(x=date,y=mobility)) +
  labs(title = "Workplace mobility",
       subtitle = "London",
       y = "Change in mobility from baseline (%)",
       x= "Date")+
  theme_light()

# date -14 to see if I can see a trend
ggplot(as.data.frame(london_tibble_workplaces) ) +
  geom_line(aes(x=date-14,y=cases)) +
  labs(title = "Workplace mobility",
       subtitle = "London",
       y = "Change in mobility from baseline (%)",
       x= "Date")+
  theme_light()

# Figure out how to plot these on the same axis with a secondary axis...



# Look at dynamic time warping --------------------------------------------
# https://cran.r-project.org/web/packages/dtw/vignettes/dtw.pdf

# Example code:
# library("dtw")
# data("aami3a")
# ref <- window(aami3a,start=0,end=2)
# test <- window(aami3a,start=2.7,end=5)
# alignment <- dtw(test,ref)
# alignment$distance

library(dtw)
alignment <- dtw(london_tibble_workplaces$mobility, london_tibble_workplaces$cases)
alignment$distance      
alignment$normalizedDistance

# Normalisation prior to dtw (and checks)
scaled_mobility <- scale(london_tibble_workplaces$mobility)
apply(scaled_mobility, 2, sd)
apply(scaled_mobility, 2, mean)
scaled_cases <- scale(london_tibble_workplaces$cases)
apply(scaled_cases, 2, sd)
apply(scaled_cases, 2, mean)

alignment <- dtw(scaled_mobility, scaled_cases)
alignment$distance      
alignment$normalizedDistance
# This is not distance in the sense I want - I want lag

# Plotting the normalised cases vs mobility
ggplot(data=london_tibble_workplaces, aes(x=date)) +
  geom_line(aes(y=scale(mobility)), color="blue")+
  geom_line(aes(y=scale(cases)), color="red")


# Maybe can use correlelogram to investigate best lag ---------------------
# Maybe this shouldn't be done on the raw data? https://stats.stackexchange.com/questions/108876/two-or-more-time-series-what-is-the-best-way-to-test-whether-one-of-them-is-lea
# Do on the data with seasonality removed?? Should be 'pre-whitened'

ccf(london_tibble_workplaces$mobility, london_tibble_workplaces$cases, type="correlation", lag.max=100)
# Negative means mobility precedes cases
# https://online.stat.psu.edu/stat510/lesson/8/8.2#:~:text=In%20R%2C%20the%20sample%20CCF,the%20y%2Dvariable%20at%20time%20.

# Maybe try with rolling averages to smooth weekly effects
ccf_rolling_mean <-ccf(rollmean(london_tibble_workplaces$mobility, k=7, fill=NA, align="center"),
                   rollmean(london_tibble_workplaces$cases, k=7, fill=NA, align="center"), 
                   lag.max=200, na.action=na.pass)

# Smooths out - lag appears to be at ~-60 -> seems like a long time?
# Similar approach used here to network paper 2 -> could try to reproduce their figure 2
max(ccf_rolling_mean$acf)
# Max ACF is 0.318
which.max(ccf_rolling_mean$acf)

ccf_rolling_mean$lag[which.max(ccf_rolling_mean$acf)] # Max ACF at -61


# Try and produce a plot that looks a bit different -> first run without the plot:
# https://stackoverflow.com/questions/13881054/altering-the-acf-plot-produced-by-r


ccf_results <- as.data.frame(cbind(ccf_rolling_mean$acf,ccf_rolling_mean$lag))
colnames(ccf_results) <- c("acf","lag")

ggplot(data=ccf_results, aes(x=lag))+
  geom_line(aes(y=acf), color="royal blue") +
  geom_hline(yintercept=0, color="dark grey") +
  geom_hline(yintercept = qnorm(0.975)/sqrt(nrow(london_tibble_workplaces)), linetype="dashed", color="dark grey")+ # upper CI bound (just uses quantiles)
  geom_hline(yintercept = -qnorm(0.975)/sqrt(nrow(london_tibble_workplaces)), linetype="dashed", color="dark grey")+ # lower CI bound (just uses quantiles)
  annotate("pointrange", x=ccf_rolling_mean$lag[which.max(ccf_rolling_mean$acf)],
           y=max(ccf_rolling_mean$acf),ymin=0, ymax=max(ccf_rolling_mean$acf), col="red")+
  annotate("text", label="Max CCF", x=ccf_rolling_mean$lag[which.max(ccf_rolling_mean$acf)],
           y=max(ccf_rolling_mean$acf)+0.01, col="Red") +
  ylim(-0.2,0.4) +
  labs(x="Lag (days)", y="CCF")+
  ggtitle("Cross correlation function (CCF) for workplace mobility and official cases in London")+
  theme_light()

# Confidence intervals using normal quantiles are:
c(-qnorm(0.975)/sqrt(nrow(london_tibble_workplaces)),qnorm(0.975)/sqrt(nrow(london_tibble_workplaces)))
            
                     