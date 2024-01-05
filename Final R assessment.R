# Required Packages -------------------------------------------------------

#install required packages
install.packages('tidyverse')
library(tidyverse)
library(scales)
library(rgl)
install.packages("rgl")
library(MASS)
library(viridis)

# Data Pre-Processing ----------------------------------------------------------

#to switch off scientific notation (default is zero)
options(scipen=999)

#read csv file
data <- read.csv("/Users/kartikgupta/Downloads/owid-covid-data-latest.csv" , header = TRUE)

#Create Data set with required variables
covid_data <- data.frame(
  continent = data$continent,
  location = data$location,
  date = data$date,
  total_cases = data$total_cases,
  new_cases = data$new_cases,
  new_cases_smoothed = data$new_cases_smoothed,
  total_deaths = data$total_deaths,
  new_deaths = data$new_deaths,
  new_deaths_smoothed = data$new_deaths_smoothed,
  total_tests = data$total_tests,
  new_tests = data$new_tests,
  new_tests_smoothed = data$new_tests_smoothed,
  population = data$population,
  population_density = data$population_density,
  total_vaccinations = data$total_vaccinations,
  people_vaccinated = data$people_vaccinated,
  people_fully_vaccinated = data$people_fully_vaccinated,
  human_development_index = data$human_development_index,
  life_expectancy = data$life_expectancy,
  reproduction_rate = data$reproduction_rate,
  stringency_index = data$stringency_index,
  positive_rate = data$positive_rate
)
 

# Data Cleaning  ----------------------------------------------------------

#create new dataset by filtering main dataset using location and date
covid<-filter(covid_data, 
              (location=="India" | 
                 location=="United States" | 
                 location=="South Korea" |
                 location=="United Kingdom") & 
                date<"2023-01-01"
) 

#creating new variable by date format
covid <- covid %>% 
  mutate(date = as.POSIXct(covid$date, format = '%Y-%m-%d'))

#creation new variables year, month, weekday, New cases per population and New Deaths per population 
covid <- covid %>% 
  mutate(
    new_cases_per_population=(new_cases_smoothed/population)*1000000,
    new_deaths_per_population=(new_deaths_smoothed/population)*1000000,
    month=format(date, "%m"),
    year=format(date, "%Y"),
    day=format(date, "%d"),
    weekday=weekdays(covid$date)
  )

#to remove rows with NA values
covid_na <- na.exclude(covid)


# Descriptives ------------------------------------------------------------

#summarise variables
summary(covid)


# Data Visualisation ------------------------------------------------------

# Bar Graph - Total number of Cases among various Countires

library(ggplot2)

ggplot(filter(covid, (date == "2022-12-31")), aes(x = location, y = total_cases, fill = location)) +
  geom_bar(stat = "identity" , width = 0.5) +
  labs(title = "Overview of COVID-19 Pandemic", subtitle = expression(underline("Total number of cases among various Countries")) , 
       x = "Country",
       y = "Total Cases") +
  geom_label(aes(label = total_cases),
             color = "white", label.size = 1,
             position = position_stack(vjust = 1),
             show.legend = FALSE) +
  scale_fill_viridis_d(aesthetics ="fill", option = "H")  + theme_bw() + 
  theme(plot.title = element_text(color = "black", size = 25, hjust=0.5,face = "bold.italic"),
        plot.subtitle = element_text(color = "black", size = 20, hjust=0.5),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ))

# Bar Graph - Total number of Fatalities among various Countires

ggplot(filter(covid, (date == "2022-12-31")), aes(x = location, y = total_deaths, fill = location)) +
  geom_bar(stat = "identity" , width = 0.5) +
  labs(title = "Overview of COVID-19 Pandemic", subtitle = expression(underline("Total number of Fatalities among various Countries")) , 
       x = "Country",
       y = "Total Deaths") +
  geom_label(aes(label = total_deaths),
             color = "white", label.size = 1,
             position = position_stack(vjust = 1),
             show.legend = FALSE) +
  scale_fill_viridis_d(aesthetics ="fill", option = "H")  + theme_bw() + 
  theme(plot.title = element_text(color = "black", size = 25, hjust=0.5,face = "bold.italic"),
        plot.subtitle = element_text(color = "black", size = 20, hjust=0.5),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ))




# Area - Line 

ggplot( filter(covid, !is.na(new_cases_smoothed)) )+ 
  geom_bar(aes(x=date, y=new_cases_smoothed), stat="identity", colour="#00bbf9", size=1.2)+
  facet_grid(. ~ location) +
  labs(title= "Comparing New Cases \n", x="Date",y="New Cases")+
  theme_linedraw()+ 
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 12),
        axis.title.x.bottom = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 12, face = "bold"),
        axis.text = element_text(color = "black", size = 10, face = "bold"))

ggplot( filter(covid, !is.na(stringency_index)) ) + 
  geom_line(aes(x=date, y=stringency_index), stat="identity", colour="#d62828", size=1.2) +
  facet_grid(. ~ location) +
  labs(title= "Comparing Stringency Index \n", x="Date", y="Stringency Index \n") +
  theme_linedraw() + 
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 12),
        axis.title.x.bottom = element_text(color = "black", size = 12, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 12, face = "bold"),
        axis.text = element_text(color = "black", size = 10, face = "bold"))

#Scatter Plot

# Filter data for the two countries you want to compare
df_compare <- covid %>% 
  filter(location %in% c("India", "United States"))

# Create ggplot object
ggplot(df_compare, aes(x = year, y = positive_rate, color = location, size= new_deaths)) +
  geom_point(alpha=0.4) +
  labs(title = "Positivity Rate Comparison with Respect to New Deaths",
       x = "Year",
       y = "Positivity Rate",
       color = "Country") +
  facet_grid(.~location) +
  theme_bw()+
  scale_fill_viridis_d(aesthetics = "color", option = "H")  +
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "black", size = 14),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"), 
        axis.text = element_text(color = "black", size = 9, face = "bold"), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) , 
        legend.text = element_text(colour = "black" , size = 14 , face = "italic" ), 
        legend.key.height = unit(2 , "lines"))

ggplot(df_compare, aes(x = year, y = positive_rate, color = location, size= new_cases)) +
  geom_point(alpha=0.4) +
  labs(title = "Positivity Rate Comparison with Respect to New Cases",
       x = "Year",
       y = "Positivity Rate",
       color = "Country") +
  facet_grid(.~location) +
  theme_bw()+
  scale_fill_viridis_d(aesthetics = "color", option = "H")  +
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "black", size = 14),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"), 
        axis.text = element_text(color = "black", size = 9, face = "bold"), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) , 
        legend.text = element_text(colour = "black" , size = 14 , face = "italic" ), 
        legend.key.height = unit(2 , "lines"))







# Regression Model --------------------------------------------------------

#Creating new dataset for regression | taking only required variables
covid_rgl <- data.frame(
  continent = data$continent,
  location = data$location,
  date = data$date,
  total_cases = data$total_cases,
  new_cases = data$new_cases,
  new_deaths = data$new_deaths,
  total_deaths = data$total_deaths,
  reproduction_rate = data$reproduction_rate,
  stringency_index = data$stringency_index
)

#filtering the required country
covid_rgl <- filter(covid_rgl, 
                    location=="India" & 
                      date<"2023-01-01")

#exclude NA values
covid_na <- na.exclude(covid_rgl)

#check the tables
nrow(covid_na)

#splitting the data
covid_train <- covid_na[1:699,]
covid_test <- covid_na[700:998,]

#Exploring the dataset
covid_train[1:10,]
summary(covid_train)

#plot variable scatter plot
ggplot(
  data=covid_train,
  aes(x=new_cases, y=new_deaths)
) + geom_point()

#######correlation matrix

covid_corr <- data.frame(
  total_cases = covid_na$total_cases,
  new_cases = covid_na$new_cases,
  total_deaths = covid_na$total_deaths,
  new_deaths = covid_na$new_deaths,
  reproduction_rate = covid_na$reproduction_rate,
  stringency_index = covid_na$stringency_index
)

# calculate the correlation matrix using the cor() function
corr_matrix <- cor(covid_corr)

# melt the correlation matrix into a data frame
corr_data <- as.data.frame(as.table(corr_matrix))
names(corr_data) <- c("variable1", "variable2", "correlation")

# plot the correlation matrix using ggplot2
ggplot(corr_data, aes(variable1, variable2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "#5E4FA2", high = "#F9A602", name = "Correlation") +
  labs(title = "\nCorrelation Matrix\n", x = "", y = "" , fill = " Correlation\n") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label=round(correlation,2)), color="white", size=4) +
  scale_fill_viridis_c(option = "H")  +
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        strip.text.x = element_text(color = "White", size = 14),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"), 
        axis.text = element_text(color = "black", size = 9, face = "bold"), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) , 
        legend.text = element_text(colour = "black" , size = 14 , face = "italic" ), 
        legend.key.height = unit(6 , "lines"))



#check correlation between variables
cor.test(
  covid_train$new_cases,
  covid_train$new_deaths
)

#Fitting the first linear regression
mod_cases <- lm(
  formula=new_deaths~new_cases,  # predicting new_deaths using new_cases
  data=covid_train
)

#checking summary and dataset
summary(mod_cases)
coef(mod_cases)

#Regression Model starts:

coefs_cases <- coef(mod_cases)

#plot with fitline
ggplot(
  data=covid_train,
  aes(x=new_cases, y=new_deaths)
) +
  geom_point() +
  geom_abline(mapping=aes(
    slope=coefs_cases["new_cases"],
    intercept=coefs_cases["(Intercept)"]
  ), color='red')

2# Residuals
covid_resid <- covid_train  # make a copy of the dataset, to leave the original untouched

covid_resid$predicted <- predict(mod_cases)    # if data are not specified, uses the data the model was fit to
covid_resid$residuals <- residuals(mod_cases)

# show the data with predicted and residual values
covid_resid[1:10,]

#plotting with distance from prediction line
ggplot(
  data=covid_resid,
  aes(x=new_cases,y=new_deaths)
) +
  geom_point(size=3) +  # make the actual values show up more clearly
  geom_point(size=2, aes(y=predicted), shape=1) +  # show the predicted values
  geom_segment(aes(xend=new_cases, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_cases["new_cases"],
    intercept=coefs_cases["(Intercept)"]
  ), color='gray')

plot(
  mod_cases,
  which=1
)

# Making predictions with the linear regression model

predict(
  mod_cases,
  newdata=covid_test
)

predict(
  mod_cases,
  newdata=covid_test,
  interval='confidence'
)

covid_cases_test <- covid_test  # make a copy of the test data to leave the original unaltered
covid_cases_test$predicted <- predict(mod_cases, newdata=covid_cases_test)
covid_cases_test$residuals <- covid_cases_test$predicted - covid_cases_test$new_deaths
covid_cases_test

ggplot(
  data=covid_cases_test,
  aes(x=new_cases,y=new_deaths)
) +
  geom_point(size=3) +  # make the actual values show up more clearly
  geom_point(size=2, aes(y=predicted), shape=1) +  # show the predicted values
  geom_segment(aes(xend=new_cases, yend=predicted), alpha=0.9, color='red') +
  geom_abline(mapping=aes(
    slope=coefs_cases["new_cases"],
    intercept=coefs_cases["(Intercept)"]
  ), color='gray')


sse_cases <- sum(covid_cases_test$residuals**2)
sse_cases


# Time Series Analysis ---------------------------------------------------

#TIME SERIES ANALYSIS For Total Cases

x <- covid_rgl$total_cases

# library required for decimal_date() function
library(lubridate)
library(forecast)

# creating time series object
# from date 30 January, 2020
mts <- ts(x, start = decimal_date(ymd("2020-01-30")),
          frequency = 365.25 / 1)

# plotting the graph
plot(mts, xlab ="Daily Data",
     ylab ="Total cases",
     main ="COVID-19 Pandemic",
     col.main ="darkgreen")

# forecasting model using arima model

fit <- auto.arima(mts)

# Next 180 forecasted values
forecast(fit, 180)

# plotting the graph with next
# Half-yearly forecasted values
plot(forecast(fit, 180), xlab ="Daily Data",
     ylab ="Total Deaths",
     main ="COVID-19 Pandemic", col.main ="darkgreen")


#TIME SERIES ANALYSIS For New Cases

y <- covid_rgl$new_cases

# library required for decimal_date() function
library(lubridate)
library(forecast)

# creating time series object
# from date 30 January, 2020
mts1 <- ts(y, start = decimal_date(ymd("2020-01-30")),
          frequency = 365.25 / 1)

# plotting the graph
plot(mts1, xlab ="Daily Data",
     ylab ="New cases",
     main ="COVID-19 Pandemic",
     col.main ="darkgreen")

# forecasting model using arima model

fit <- auto.arima(mts1)

# Next 180 forecasted values
forecast(fit, 180)

# plotting the graph with next
# Half-Yearly forecasted values
plot(forecast(fit, 180), xlab ="Yearly Data",
     ylab ="New Cases",
     main ="COVID-19 Pandemic", col.main ="darkgreen")

z <- covid_rgl$total_deaths

# library required for decimal_date() function
library(lubridate)
library(forecast)

# creating time series object
# from date 30 January, 2020
mts1 <- ts(z, start = decimal_date(ymd("2020-01-30")),
           frequency = 365.25 / 1)

# plotting the graph
plot(mts1, xlab ="Daily Data",
     ylab ="Total Deaths",
     main ="COVID-19 Pandemic",
     col.main ="darkgreen")

# forecasting model using arima model

fit <- auto.arima(mts1)

# Next 180 forecasted values
forecast(fit, 180)

# plotting the graph with next
# Half-Yearly forecasted values
plot(forecast(fit, 180), xlab ="Yearly Data",
     ylab ="Total Deaths",
     main ="COVID-19 Pandemic", col.main ="darkgreen")


# Extras ------------------------------------------------------------------


ggplot(filter(covid, location=="South Korea" | location=="United Kingdom" | location=="India"), 
       aes(x = date, y = new_cases_smoothed, color = location)) +
  geom_line() +
  theme_classic()

ggplot(filter(covid, location=="South Korea" | location=="United Kingdom" | location=="India"), 
       aes(x = date, y = new_deaths_smoothed, color = location)) +
  geom_line() +
  theme_classic()


ggplot(filter(covid, (location=="United States" | location=="India") & !is.na(new_deaths_smoothed)), 
       aes(x = date, y = new_deaths_smoothed, color = location , size = reproduction_rate)) +
  geom_line() + 
  labs(colour = "Country \n", title = "Comparing New Deaths of Virus for different Countries\n" , 
       x = "Years" , y = "New Deaths Smoothed\n") +
  theme_linedraw() + facet_wrap(~location)


ggplot(filter(covid, (location=="United States" | location=="India" ) & !is.na(positive_rate)) ,
       aes(x = date, y = positive_rate, color = location  )) +
  geom_point() + 
  labs(colour = "Country \n", title = "Comparing New Deaths with Positivity Rate of Virus for different Countries\n" , 
       x = "Years" , y = "New Deaths Smoothed\n") + 
  scale_color_brewer(palette = "Dark2") +
  theme_minimal() 

ggplot(filter(covid, !is.na(positive_rate)) ,
       aes(x = date, y = positive_rate, color = location , size = reproduction_rate)) +
  geom_point() + 
  labs(colour = "Country \n", title = "Comparing New Deaths with Positivity Rate of Virus for different Countries\n" , 
       x = "Years" , y = "New Deaths Smoothed\n") +
  theme_gray() 


#bar graph :-
ggplot(filter(covid, location=="United States" | location=="United Kingdom" | location=="India"), 
       aes(fill=location, y=new_cases_smoothed, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ continent) +
  theme_classic()

ggplot(filter(covid, location=="United States" | location=="United Kingdom" | location=="India" | location=="South Korea"), 
       aes(fill=location, y=total_deaths, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ continent) +
  theme_classic()

ggplot(filter(covid, location=="United States" | location=="United Kingdom" | location=="India" | location=="South Korea"), 
       aes(fill=location, y=new_deaths, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ continent) +
  theme_classic()

ggplot(filter(covid, location=="United States" | location=="United Kingdom" | location=="India" | location=="South Korea"), 
       aes(fill=location, y= covid$stringency_index , x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ continent) +
  theme_classic()

ggplot(filter(covid, location=="United States" | location=="United Kingdom" | location=="India" | location=="South Korea"), 
       aes(fill=location, y= covid$reproduction_rate , x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ continent) +
  theme_classic() 



#Scatterplot

ggplot(covid, 
       aes(total_cases, total_deaths, color = location)) +
  geom_point(size = 0.5) +
  theme_classic()




#are+line

ggplot(filter(covid, location=="United States" | location=="India" | location=="United Kingdom" | location =="South Korea"))  + 
  geom_bar(aes(x=date, y=new_cases), stat="identity", colour="#006000")+
  geom_line(aes(x=date, y=stringency_index), stat="identity", color="red", size=1) + facet_wrap( ~ location) +
  theme_classic()

ggplot(filter(covid, location=="United States" | location=="India" | location=="United Kingdom" | location =="South Korea"))  + 
  geom_bar(aes(x=date, y=new_deaths_per_population), stat="identity", colour="#006000")+
  geom_line(aes(x=date, y=reproduction_rate), stat="identity", color="red", size=1) + facet_wrap(. ~ location) +
  theme_linedraw() + scale_y_continuous(trans = 'sqrt') +
  labs(fill = "Country \n", title =expression(underline("Fatalities across various Countries with Virus Reproduction Rate")) ,
       subtitle = "Comparison of New Deaths Per Popultation with R Rate by the end of 2022" , y = "New Deaths Per Population" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "black" , size = 20 , face = "bold" , hjust = 0.5 ),
        plot.subtitle = element_text(colour = "black" , size = 16.5 , face = "bold" , hjust = 0.5 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ) ,
        strip.text.x = element_text(colour = "white" , size = 14 , face = "italic" )
  )


#Pie charts

ggplot(covid, aes(fill=location, y=new_cases, x="")) + 
  geom_bar(position="dodge", stat="identity") +
  facet_grid(. ~ year) +
  coord_polar("y", start=0) + 
  theme(legend.position="none") +
  geom_text(aes(y = new_cases, label = new_cases), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

ggplot(covid, aes(fill=location, y=new_cases, x="")) + 
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  theme(legend.position="none") 

#Bubble Graph
library(hrbrthemes) 
library(viridis) 
ggplot (filter(covid))+
  geom_point(aes(x = stringency_index, y = reproduction_rate, colour = location , size = new_cases),alpha = 0.4 ) + 
  scale_size(range = c(1, 20), breaks = c(200000,400000,600000,800000,1000000)) + facet_wrap(.~location) + theme_linedraw()
#+
# theme(legend.position="right") + ylab("Mortality Rate") + xlab("Year") +
#labs(fill=" Percentage of \nNeonatal Mortality Rate", title= "Overview of Neonatal Mortality From 1997 to 2020\n", colour = "Country" , size = "Number of \nNeonatal Deaths")+ scale_fill_viridis_d(aesthetics ="colour", option = "H") +
#theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"), strip.text.x = element_text(color = "White", size = 14),
#     axis.title.x.bottom = element_text (color = "black", size = 14, face = "bold", hjust = 0.5),
#    axis.title.y.left = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),axis.text = element_text(color = "black", size = 12, face = "bold"), legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) , legend.text = element_text(colour = "black" , size = 14 , face = "italic" ))

ggplot (filter(covid))+
  geom_point(aes(x = new_deaths, y = new_cases , colour = location ),alpha = 0.5 ) + 
  scale_size(range = c(1, 20), breaks = c(200000,400000,600000,800000,1000000)) + facet_wrap(.~location) + theme_linedraw()

ggplot (filter(covid))+
  geom_point(aes(x = stringency_index, y = reproduction_rate, colour = location ),alpha = 0.4 ) + 
  scale_size(range = c(1, 20), breaks = c(200000,400000,600000,800000,1000000)) + facet_wrap(.~location) + theme_linedraw()

#Plot New cases for the dataset

#Pie Chart - 
ggplot(filter(covid, date=="2022-12-31"), 
       aes(x = "", y = total_cases, fill = location)) +
  geom_col(color = "black") +
  geom_label(aes(label = total_cases),
             color = "white", label.size = 1,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE)+
  theme_void() +
  labs(fill = "Country \n", title =expression(underline("Overall Covid Cases across various Countries")) ,
       subtitle = "Total Cases by The End of 2022") +
  coord_polar(theta = "y") +
  theme(plot.title = element_text(colour = "black" , size = 20 , face = "bold" , hjust = 0.5 ),
        plot.subtitle = element_text(colour = "black" , size = 16.5 , face = "bold" , hjust = 0.5 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ) ,
  )

#India vs United Kingdom vs South Korea
#Line Plot - 

ggplot(filter(covid, !is.na(new_cases_smoothed) ), 
       aes(x = date, y = new_cases_smoothed, fill = location)) +
  geom_area() + facet_wrap(~ location ) +
  theme_minimal() +
  labs(fill = "Country \n", title =expression(underline("Rise of Cases across various Countries")) ,
       subtitle = "Trends of New Cases From 2020 till 2022" , y = "New Cases" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "black" , size = 20 , face = "bold" , hjust = 0.5 ),
        plot.subtitle = element_text(colour = "black" , size = 16.5 , face = "bold" , hjust = 0.5 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ) ,
        strip.text.x = element_text(colour = "black" , size = 14 , face = "italic" )
  )

ggplot(filter(covid, !is.na(total_deaths) ), 
       aes(x = date, y = total_deaths, colour = location )) +
  geom_point() + 
  theme_minimal() +
  labs(fill = "Country \n", title =expression(underline("Fatalities across various Countries")) ,
       subtitle = "Comparison of Total deaths by the end of 2022" , y = "Total Deaths" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "black" , size = 20 , face = "bold" , hjust = 0.5 ),
        plot.subtitle = element_text(colour = "black" , size = 16.5 , face = "bold" , hjust = 0.5 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ) ,
        strip.text.x = element_text(colour = "black" , size = 14 , face = "italic" )
  )

#Area - Line

ggplot(filter(covid, location=="United States" | location=="India" | location=="United Kingdom" | location =="South Korea"))  + 
  geom_bar(aes(x=date, y=new_deaths_per_population), stat="identity", colour="#006000")+
  geom_line(aes(x=date, y=reproduction_rate), stat="identity", color="red", size=1) + facet_wrap(. ~ location) +
  theme_linedraw() + scale_y_continuous(trans = 'sqrt') +
  labs(fill = "Country \n", title =expression(underline("Fatalities across various Countries with Virus Reproduction Rate")) ,
       subtitle = "Comparison of New Deaths Per Popultation with R Rate by the end of 2022" , y = "New Deaths Per Population" , x = "Date" , colour = "Country") +
  theme(plot.title = element_text(colour = "black" , size = 20 , face = "bold" , hjust = 0.5 ),
        plot.subtitle = element_text(colour = "black" , size = 16.5 , face = "bold" , hjust = 0.5 ), 
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ) ,
        strip.text.x = element_text(colour = "white" , size = 14 , face = "italic" )
  )

#Scatterplot

ggplot(filter(covid, (location=="United States" | location=="India" ) ) ,
       aes(x = date, y = positive_rate, color = location  )) +
  geom_point(alpha= 0.5 , size = 4) + 
  labs(colour = "Country \n", title = "Comparing Positivity Rate of Virus\n" , 
       x = "Date" , y = "Positivity Rate\n") + 
  scale_color_brewer(palette = "Dark2") +
  theme_linedraw() + 
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ))



####

ggplot(filter(covid, (location=="United States" | location=="India" ) ), aes(x= date, group= positive_rate, fill=cut)) +
  geom_(adjust=1.5, position="fill") +
  theme_ipsum()




ggplot(filter(covid, (location=="United States" | location=="India" ) ) ,
       aes(x = date, y = positive_rate, color = location  )) +
  geom_point(alpha= 0.5 , size = 4) + 
  labs(colour = "Country \n", title = "Comparing Positivity Rate of Virus\n" , 
       x = "Date" , y = "Positivity Rate\n") + 
  scale_color_brewer(palette = "Dark2") +
  theme_linedraw() + 
  theme(plot.title = element_text(color = "black", size = 16, hjust=0.5, face = "bold"),
        axis.title.x.bottom = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y.left = element_text(color = "black", size = 14, face = "bold"),
        axis.text = element_text(color = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black" , size = 16 , face = "bold" ) ,
        legend.text =  element_text(colour = "black" , size = 14 , face = "italic" ))