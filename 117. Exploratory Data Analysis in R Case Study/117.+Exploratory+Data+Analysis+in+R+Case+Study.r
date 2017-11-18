
votes = readRDS("votes.rds")

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(broom)

# Load the dplyr package
library(dplyr)

# Print the votes dataset
votes

# Filter for votes that are "yes", "abstain", or "no"
votes %>% filter(vote<=3)

Adding a year column
100xp
The next step of data cleaning is manipulating your variables (columns) to make them more informative.

In this case, you have a session column that is hard to interpret intuitively. But since the UN started voting in 1946, and holds one session per year, you can get the year of a UN resolution by adding 1945 to the session number.

Instructions
Use mutate() to add a year column by adding 1945 to the session column.

# Add another %>% step to add a year column
votes %>%
  filter(vote <= 3) %>%
  mutate(year=session+1945)

#install.packages('countrycode', repos='http://cran.us.r-project.org')
library(countrycode)

# Load the countrycode package
library(countrycode)

# Convert country code 100
countrycode(100, "cown", "country.name")

# Add a country column within the mutate: votes_processed
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945, country=countrycode(ccode, "cown", "country.name"))

# Print votes_processed
votes_processed

# Find total and fraction of "yes" votes
votes_processed %>% summarize(total=n(), percent_yes=mean(vote==1))

# Change this code to summarize by year
votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Summarize by country: by_country
by_country = votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# You have the votes summarized by country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Print the by_country dataset
by_country

# Sort in ascending order of percent_yes
by_country %>% arrange(percent_yes)

# Now sort in descending order
by_country %>% arrange(desc(percent_yes))

# Filter out countries with fewer than 100 votes
by_country %>%
  arrange(percent_yes) %>% 
  filter(total > 100)

# Define by_year
by_year <- votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Load the ggplot2 package
library(ggplot2)

# Create line plot
ggplot(by_year, aes(x=year, y=percent_yes)) +
  geom_line()

dim(votes_processed)

# Change to scatter plot and add smoothing curve
ggplot(by_year, aes(year, percent_yes)) +
  geom_point() + geom_smooth()


# Group by year and country: by_year_country
by_year_country = votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

#Below steps are added because the data is not matching with the Datacamp data for "votes_processed"

dim(by_year_country)

old = as.character(unique(votes_processed$country))

class(new)

#From Datacamp Console
new = c("United States", "Canada", "Cuba", "Haiti", "Dominican Republic", 
"Mexico", "Guatemala", "Honduras", "El Salvador", "Nicaragua", 
"Costa Rica", "Panama", "Colombia", "Venezuela, Bolivarian Republic of", 
"Ecuador", "Peru", "Brazil", "Paraguay", "Chile", "Argentina", 
"Uruguay", "United Kingdom", "Netherlands", "Belgium", "Luxembourg", 
"France", "Poland", "Czechoslovakia", "Yugoslavia", "Greece", 
"Russian Federation", "Ukraine", "Belarus", "Sweden", "Norway", 
"Denmark", "Iceland", "Liberia", "Ethiopia", "South Africa", 
"Iran, Islamic Republic of", "Turkey", "Iraq", "Egypt", "Syrian Arab Republic", 
"Lebanon", "Saudi Arabia", "Afghanistan", "Taiwan, Province of China", 
"India", "Thailand", "Philippines", "Australia", "New Zealand", 
"Bolivia, Plurinational State of", "Yemen Arab Republic", "Pakistan", 
"Israel", "Myanmar", "Indonesia", "Hungary", "Jordan", "Sri Lanka", 
"Spain", "Romania", "Ireland", "Portugal", "Austria", "Italy", 
"Albania", "Bulgaria", "Finland", "Ghana", "Morocco", "Tunisia", 
"Libya", "Sudan", "Japan", "Nepal", "Cambodia", "Lao People's Democratic Republic", 
"Malaysia", "Guinea", "Cyprus", "Mali", "Senegal", "Niger", "Cote d'Ivoire", 
"Burkina Faso", "Togo", "Cameroon", "Nigeria", "Gabon", "Central African Republic", 
"Chad", "Congo", "Congo, the Democratic Republic of the", "Somalia", 
"Madagascar", "Benin", "Sierra Leone", "Mongolia", "Mauritania", 
"Tanzania, United Republic of", "Jamaica", "Trinidad and Tobago", 
"Uganda", "Burundi", "Rwanda", "Algeria", "Kuwait", "Kenya", 
"Zanzibar", "Zambia", "Malawi", "Maldives", "Singapore", "Malta", 
"Gambia", "Lesotho", "Guyana", "Botswana", "Barbados", "Yemen People's Republic", 
"Mauritius", "Equatorial Guinea", "Swaziland", "Bhutan", "Fiji", 
"Bahrain", "Qatar", "Oman", "China", "United Arab Emirates", 
"Federal Republic of Germany", "German Democratic Republic", 
"Bahamas", "Grenada", "Cabo Verde", "Sao Tome and Principe", 
"Guinea-Bissau", "Mozambique", "Bangladesh", "Comoros", "Papua New Guinea", 
"Suriname", "Djibouti", "Angola", "Viet Nam", "Samoa", "Seychelles", 
"Solomon Islands", "Saint Lucia", "Saint Vincent and the Grenadines", 
"Zimbabwe", "Vanuatu", "Belize", "Antigua and Barbuda", "Dominica", 
"Saint Kitts and Nevis", "Brunei Darussalam", "Liechtenstein", 
"Germany", "Estonia", "Latvia", "Lithuania", "Namibia", "Yemen", 
"Korea, Democratic People's Republic of", "Korea, Republic of", 
"Micronesia, Federated States of", "Marshall Islands", "San Marino", 
"Bosnia and Herzegovina", "Armenia", "Azerbaijan", "Monaco", 
"Andorra", "Czech Republic", "Slovakia", "Macedonia, the former Yugoslav Republic of", 
"Croatia", "Slovenia", "Moldova, Republic of", "Kyrgyzstan", 
"Kazakhstan", "Eritrea", "Turkmenistan", "Georgia", "Tajikistan", 
"Uzbekistan", "Palau", "Tonga", "Tuvalu", "Nauru", "Kiribati", 
"Switzerland", "Timor-Leste", "Montenegro", "South Sudan")

countries = data_frame(old=old, new=as.character(new))

str(countries)

countries$difference = countries$old==countries$new

table(countries$difference)

filter(countries, difference==FALSE)

vp = left_join(votes_processed, countries, by =c("country"="old"))
#vp = merge(votes+)

vp = rename(vp, old=country, country=new)
votes_processed = vp
head(vp)

head(filter(votes_processed, country=="United Kingdom"))

# Start with by_year_country dataset
by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Print by_year_country
by_year_country

# Create a filtered version: UK_by_year
#UK_by_year = by_year_country %>% filter(country=="United Kingdom of Great Britain and Northern Ireland")

UK_by_year = by_year_country %>% filter(country=="United Kingdom")

# Line plot of percent_yes over time for UK only
ggplot(UK_by_year, aes(year, percent_yes)) + 
   geom_line()

# Vector of four countries to examine
countries <- c("United States", "United Kingdom",
               "France", "India")

# Filter by_year_country: filtered_4_countries
filtered_4_countries = by_year_country %>% filter(country %in% countries)

# Line plot of % yes in four countries
ggplot(filtered_4_countries, aes(year, percent_yes, color=country)) +
  geom_line()

# Vector of six countries to examine
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India")

# Filtered by_year_country: filtered_6_countries
filtered_6_countries = by_year_country %>% filter(country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries,aes(year, percent_yes)) +
  geom_line() +
  facet_wrap(~country)

# Vector of six countries to examine
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India")

# Filtered by_year_country: filtered_6_countries
filtered_6_countries <- by_year_country %>%
  filter(country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_6_countries, aes(year, percent_yes)) +
  geom_line() +
  facet_wrap(~ country,scales = "free_y")

# Add three more countries to this list
countries <- c("United States", "United Kingdom",
               "France", "Japan", "Brazil", "India", "United Arab Emirates", "United Arab Emirates","United Arab Emirates")

# Filtered by_year_country: filtered_countries
filtered_countries <- by_year_country %>%
  filter(country %in% countries)

# Line plot of % yes over time faceted by country
ggplot(filtered_countries, aes(year, percent_yes)) +
  geom_line() +
  facet_wrap(~ country, scales = "free_y")

# Percentage of yes votes from the US by year: US_by_year
US_by_year <- by_year_country %>%
  filter(country == "United States")

# Print the US_by_year data
US_by_year

# Perform a linear regression of percent_yes by year: US_fit
US_fit = lm(percent_yes ~ year, data =US_by_year)

# Perform summary() on the US_fit object
summary(US_fit)

> summary(US_fit)

Call:
lm(formula = percent_yes ~ year, data = US_by_year)

Residuals:
      Min        1Q    Median        3Q       Max 
-0.222491 -0.080635 -0.008661  0.081948  0.194307 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) 12.6641455  1.8379743   6.890 8.48e-08 ***
year        -0.0062393  0.0009282  -6.722 1.37e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 0.1062 on 32 degrees of freedom
Multiple R-squared:  0.5854,	Adjusted R-squared:  0.5724 
F-statistic: 45.18 on 1 and 32 DF,  p-value: 1.367e-07
> 

# Load the broom package
library(broom)

# Call the tidy() function on the US_fit object
tidy(US_fit)

# Linear regression of percent_yes by year for US
US_by_year <- by_year_country %>%
  filter(country == "United States")
US_fit <- lm(percent_yes ~ year, US_by_year)

# Fit model for the United Kingdom
UK_by_year <- by_year_country %>%
  filter(country == "United Kingdom")
UK_fit <- lm(percent_yes ~ year, UK_by_year)

# Create US_tidied and UK_tidied
US_tidied = tidy(US_fit)
UK_tidied = tidy(UK_fit)

# Combine the two tidied models
bind_rows(US_tidied, UK_tidied)

print(dim(by_year_country))
print(names(by_year_country))
print(glimpse(by_year_country))

# Load the tidyr package
library(tidyr)

# Nest all columns besides country
by_year_country %>% nest(-country)


library(purrr)

# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

# Print the nested data for Brazil
nested$data[[which(nested$country=="Brazil")]]

# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

# Unnest the data column to return it to its original form
nested <- by_year_country %>%
  nest(-country) %>%
  unnest(data)

# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

# Unnest the data column to return it to its original form
by_year_country %>%
  nest(-country) %>%
  unnest(data)

names(by_year_country)

head(by_year_country)

by_year_country %>%
  nest(-country)

# Load tidyr and purrr
library(tidyr)
library(purrr)


# Perform a linear regression on each item in the data column
by_year_country %>%
  nest(-country) %>% 
  mutate(model=map(data, ~ lm(percent_yes~year, .)))


