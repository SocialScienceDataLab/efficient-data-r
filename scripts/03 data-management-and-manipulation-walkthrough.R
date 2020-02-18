# Efficient Data Management in R

### Data Management and Data Manipulation

##### Data wrangling for complex data structures: A walkthrough

###### Micro-level survey data from the European Social Survey

## ----data-ess----
ess <- read.dta13("raw-data/202090127_ESS9e01_1.dta",
                  fromEncoding = "UTF-8") %>%
  select(                 ### select relevant variables
    cntry,                ### ISO2C country codes
    inwyys,               ### start of interview, year
    inwmms,               ### start of interview, month
    inwdds,               ### start of interview, day
    dweight,              ### design weights
    vote,                 ### voted in last general election?
    starts_with("prtv"),  ### country-specific vote recall
    clsprty,              ### close to any party?
    starts_with("prtcl"), ### country-specific party ID
    uemp3m,               ### ever unemployed for > 3 months
    uemp5yr,              ### unemployed >3 months in past 5 years
    gndr,                 ### gender
    agea,                 ### age
    eisced,               ### education (ISCED)
    blgetmg               ### ethnic minority
  ) %>%
  mutate_if(is.factor, as.character) %>%  ### factor -> character
  mutate(uemp5yr = ifelse(uemp3m == "No", ### recode uemp5yr
                          "No", uemp5yr)) %>%
  mutate_if(is.character,                 ### recode responses to missing values
            function (x)
              ifelse(x %in% c("Refusal", "Don't know",
                              "No answer", "Not applicable"),
                     NA,
                     x))


###### Election Dates

## ----elec-dates1----
ess %<>%
  mutate(inwdate = sprintf('%04d%02d%02d', inwyys, inwmms, inwdds)) %>%
  mutate(inwdate = ymd(inwdate)) %>%
  select(-inwyys, -inwmms, -inwdds)


## ----elec-dates2----
ess_dates <- ess %>%
  group_by(cntry) %>%
  summarize(field_start = min(inwdate),
            field_end = max(inwdate)) %>%
  print()


## ----data-parlgov1----
pgov <- read.csv("raw-data/20200127_parlgov.csv") %>%
  mutate(cntry = countrycode(country_name, "country.name", "iso2c")) %>%
  filter(election_type == "parliament") %>%
  select(cntry, election_date, election_id) %>%
  group_by(election_id) %>%
  summarize_all(unique) %>%
  ungroup() %>%
  right_join(ess_dates, by = "cntry")


## ----data-parlgov2----
pgov %<>%
  mutate(election_date = ymd(election_date)) %>%
  filter(election_date <= field_start) %>%
  group_by(cntry) %>%
  arrange(election_date) %>%
  mutate(most_recent = election_date[which.min(field_start - election_date)],
         prev_election_date = lag(election_date)) %>%
  ungroup() %>%
  filter(election_date == most_recent)


## ----data-parlgov3----
pgov %<>%
  mutate(
    election_year = year(election_date),
    prev_election_year = year(prev_election_date)
  ) %>%
  select(cntry,
         election_date,
         prev_election_date,
         election_year,
         prev_election_year)


###### Party Positions

## ----data-cmp1----
cmp <- read.dta13("raw-data/20200127_cmp2019b.dta") %>%
  mutate(cntry = countrycode(countryname, "country.name", "iso2c")) %>%
  filter(parfam == "soc social democratic") %>%
  mutate(election_date = as.Date(edate))

## ----data-cmp2----
cmp %<>%
  right_join(pgov, by = c("cntry", "election_date")) %>%
  select(
    cntry,
    election_date,
    election_year,
    prev_election_date,
    prev_election_year,
    party,
    partyname,
    partyabbrev,
    pervote,
    rile,
    welfare
  )


## ----data-cmp3----
cmp %<>%
  group_by(cntry) %>%
  filter(pervote == max(pervote, na.rm = TRUE)) %>%
  ungroup()

## View data
cmp %>%
  select(cntry, election_year, partyname, partyabbrev, welfare)

## Add to ESS
ess %<>% left_join(cmp, by = "cntry")


###### Unemployment Rates

## ----data-unemp1----
unemp <- read.csv("raw-data/20200114_worldbank_unemprate.csv",
                  skip = 4L) %>%
  mutate(cntry = countrycode(Country.Name, "country.name", "iso2c")) %>%
  filter(cntry %in% cmp$cntry)

## ----data-unemp2----
unemp %<>%
  select(cntry, starts_with("X"),-X) %>%
  melt(id.vars = "cntry") %>%
  rename(year = variable,
         unemprate = value) %>%
  mutate(year = as.numeric(substr(year, 2L, 5L)))


## ----data-unemp3----
unemp %<>%
  left_join(pgov %>% select(cntry, election_year, prev_election_year),
            by = c("cntry")) %>%
  group_by(cntry) %>%
  filter(year >= prev_election_year &
           year <= election_year) %>%
  summarize(unemprate = mean(unemprate))

ess %<>% left_join(unemp, by = "cntry")
