library(tidyverse)
library(zoo) # moving averages

population <- read_csv( 'population.csv' )

today <- lubridate::today( tzone="UTC" )
yesterday <- today - lubridate::days(1)
twoWeeksAgo <- yesterday - lubridate::days(14)
threeWeeksAgo <- yesterday - lubridate::days(21)

dir.create(file.path('downloads') )
dir.create(file.path('data') )


#for downloading
filenames <- format( seq(threeWeeksAgo, yesterday, "days"), format="%m-%d-%Y.csv", tz="UTC")

url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'

for (filename in filenames) {
  if(!file.exists(  paste(getwd(), "/downloads/", filename, sep = "") ) ) {
    download.file(paste(url, filename, sep = ""), paste(getwd(), "/downloads/", filename, sep = ""))
  }
}

loadCsv <- function(d) {
  read_csv( paste(getwd(), "/downloads/", d, '.csv', sep = "") ) %>%
    filter( Country_Region == 'India') %>%
    #mutate( date = format(d, format="%Y-%m-%d") ) %>%
    mutate( date = as.Date( as.character(d), "%m-%d-%Y") ) %>%
    merge( population, by=c('Province_State'), all.x=T)
}

readFilenames <- format( seq( threeWeeksAgo, yesterday, "days"), format="%m-%d-%Y", tz="UTC")

merged <- readFilenames %>%
  lapply( loadCsv ) %>%
  bind_rows

withDelta <- merged %>%
  filter( Province_State != 'Unknown') %>%
  arrange( date ) %>%
  group_by( Province_State ) %>%
  mutate( 
    new_cases = Confirmed - lag(Confirmed),
    avg7_new_cases = rollmean(new_cases, k = 7, fill = NA, align="right"),
    avg7_new_cases_100k = (avg7_new_cases / Population) * 1e5,
    avg7_new_cases_100k_round = round(avg7_new_cases_100k),
    new_deaths = Deaths - lag(Deaths),
    avg7_new_deaths = rollmean(new_deaths, k = 7, fill = NA, align="right"),
    avg7_new_deaths_100k = (avg7_new_deaths / Population) * 1e5,
    avg7_new_deaths_100k_round = round(avg7_new_deaths_100k)
  ) %>%
  #mutate( avg7_new_cases = rollmean(new_cases, k = 7, fill = NA, align="right")) %>%
  #mutate( avg7_new_cases_100k = (avg7_new_cases / Population) * 1e5 ) %>%
  #mutate( avg7_new_cases_100k_round = round(avg7_new_cases_100k) ) %>%
  rename( 
    province = Province_State, 
    country = Country_Region, 
    cases = Confirmed,
    deaths = Deaths,
    population = Population
  ) %>%
  filter( date >= twoWeeksAgo )

latest <- withDelta %>%
  filter( date == yesterday )

#dir.create(file.path('data', today))

#write_csv( vals %>% ungroup() %>% select('avg7_new_cases_100k_round'), paste('data/', today, '/values.csv', sep ="") )

keys <- c('date', 'country', 'province', 'population', 'cases', 'new_cases', 'avg7_new_cases', 'avg7_new_cases_100k', 'deaths', 'new_deaths', 'avg7_new_deaths', 'avg7_new_deaths_100k')

write_csv( latest %>% select( keys ), file.path('data/india-provinces.csv') )

write_csv( withDelta %>% select( keys ), file.path('data/india-provinces-two-weeks.csv') )
