# Efficient Data Management in R

### Data Management and Data Manipulation

##### Getting to know the tidyverse

## ------------------------------------------------------------------------
# We use a code that is adjusted and based on examples provided here:
# https://declaredesign.org/r/fabricatr/articles/building_importing.html

# We set a seed to make our dataset reproducible.
set.seed(68159)

data <- fabricate(
  countries = add_level(
    N = 10,
    month = recycle(month.abb),
    gdppercapita = runif(N, min = 10000, max = 50000),
    unemployment_rate = 5 + runif(N, 30, 50) + ((gdppercapita > 50) * 10),
    life_expectancy = 50 + runif(N, 10, 20) + ((gdppercapita > 30000) * 10)
  )
)

# Drop countries
data <- data %>% select(-countries)

# We add artificial NAs
data <- data %>%
  mutate(
    gdppercapita = ifelse(gdppercapita > 40000 &
                            gdppercapita < 44000, NA, gdppercapita),
    unemployment_rate = ifelse(
      unemployment_rate > 50 &
        unemployment_rate < 54,
      NA,
      unemployment_rate
    )
  )

# Have a look at the data
data

# If your dataset becomes larger, the command "head(data)" is a useful alternative
# to get a quick glance at the data. It shows by default the first 6 rows.


###### Filtering the data

## ------------------------------------------------------------------------
data %>%
  filter(month == "Jan")


## -----------------------------------------------------------
## data[data$month == "Jan", ]


###### Selecting specific variables

## ------------------------------------------------------------------------
data %>%
  select(month, unemployment_rate) %>%
  head()


## -----------------------------------------------------------
## head(data[, c("month", "unemployment_rate")])


###### Arranging variables

## ------------------------------------------------------------------------
# Arrange in ascending order
data %>%
  select(month, unemployment_rate) %>%
  arrange(unemployment_rate)

# Arrange in descending order
data %>%
  select(month, unemployment_rate) %>%
  arrange(desc(unemployment_rate))


## ------------------------------------------------------------------------
## # Arrange in ascending order
## data[order(data$unemployment_rate), c("month","unemployment_rate")]
##
## # Arrange in descending order
## data[order(data$unemployment_rate, decreasing = TRUE),
##      c("month","unemployment_rate")]


###### Group-wise operations

## ------------------------------------------------------------------------
data %>%
  group_by(month) %>%
  summarise(max_unemployment = max(unemployment_rate))


## ------------------------------------------------------------------------
## aggregate(data$unemployment_rate, by = list(data$month), max)


###### Extracting unique observations

## ------------------------------------------------------------------------
# Get the distinct months in our dataset
data %>%
  distinct(month)

# Sort data alphabetically
data %>%
  distinct(month) %>%
  arrange(month)


## ------------------------------------------------------------------------
# Get the distinct months in our dataset
## unique(data$month)

# Sort data alphabetically
## sort(unique(data$month))


###### Renaming variables

## ------------------------------------------------------------------------
# Rename the variable "gdppercapita" to "gdp_per_capita"
data %<>%
  rename(gdp_per_capita = gdppercapita)


## ------------------------------------------------------------------------
## names(data)[names(data) == "gdppercapita"] <- "gdp_per_capita"


###### Creating new variables

## ------------------------------------------------------------------------
# Create a new variable called "summer"
data %>%
  mutate(summer = ifelse(month %in% c("Jun", "Jul", "Aug"), 1, 0))


## ------------------------------------------------------------------------
## data$summer <- ifelse(data$month %in% c("Jun", "Jul", "Aug"), 1, 0)


###### Additional features

## ------------------------------------------------------------------------
data %<>% mutate_if(is.numeric, replace_na, 0)


## ------------------------------------------------------------------------
## data <- lapply(data, function(x) replace(x, is.na(x), 0))