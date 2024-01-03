# Setup ####
load_from_web <- TRUE # Set to TRUE if web-based datasets should be refreshed, or FALSE to read from existing files in working directory instead

# Load packages
library(readxl)
library(tidyverse)
library(rvest)
library(leaps)
library(caret)
library(forecast)
library(gridExtra)
library(TSA)
library(tseries)


library(lmtest)
library(fable)
library(tsibble)
library(feasts)

fuel_url_page <- "https://www.energy.gov.au/publications/australian-petroleum-statistics-2023"
fuel_url_candidates <- read_html(fuel_url_page) %>%
  html_elements("a") %>%    # find all links
  html_attr("href") %>%   # get the url
  str_extract("/sites/default/files/Australian%20Petroleum%20Statistics%20-%20Data%20Extract%20(.*)\\.xlsx")    # identify excel files
fuel_url_months <- as.vector(outer(month.name, seq(2023,2030), paste, sep="%20"))
fuel_url <- fuel_url_candidates[
  str_extract(fuel_url_candidates,paste(fuel_url_months,collapse = "|")) %>%
    match(fuel_url_months) %>%
    which.max
] %>%
  url_absolute(fuel_url_page)

routes_int_url_page <- "https://www.bitre.gov.au/publications/ongoing/international_airline_activity-time_series"
routes_int_url <- read_html(routes_int_url_page) %>%
  html_elements("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_extract("/sites/default/files/documents/International_airline_activity_CityPairs_2009toCurrent_....\\.xlsx") %>%
  .[!is.na(.)] %>%
  url_absolute(routes_int_url_page)

routes_dom_url <- "https://data.gov.au/data/dataset/c5029f2a-39b3-4aef-8ae1-73e7962f6170/resource/677d307f-6a1f-4de4-9b85-5e1aa7074423/download/dom_citypairs_web.csv"    # to-do: switch to primary source: https://www.bitre.gov.au/publications/ongoing/domestic_airline_activity-time_series

airports_url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"

# Load datasets
url_list <- list(
  fuel_url = fuel_url,
  routes_dom_url = routes_dom_url,
  routes_int_url = routes_int_url,
  airports_url = airports_url
)

if (load_from_web) {
  
  # load files in url_list
  for(i in seq_along(url_list)){
    download.file(url_list[[i]], gsub("%20", " ", basename(url_list[[i]])), quiet = TRUE, mode="wb")
  }
  
  # load definitions for airports dataset
  read_html("https://openflights.org/data.html") %>%
    html_elements("table") %>%
    {.[1]} %>%
    html_table() %>%
    as.data.frame() %>%
    rename(
      term = X1,
      definition = X2
    ) %>%
    write_csv("airports_definitions.csv")
}

fuel <- read_excel(gsub("%20", " ", basename(url_list$fuel_url)), "Sales of products")
routes <- read_csv("dom_citypairs_web.csv")
international <- read_excel(gsub("%20", " ", basename(url_list$routes_int_url)), "Data", col_types = c("date", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
airports <- read_csv("airports.dat", col_names = FALSE)
colnames(airports) <- read_csv("airports_definitions.csv")$term

# align airport name format across datasets to facilitate joins
airports <- airports %>% mutate(Name = str_remove_all(Name, " Airport| Airfield| International"), .keep = "unused")

routes <- routes %>%
  mutate(City1 = str_to_title(City1),
         City2 = str_to_title(City2),
         .keep="unused")

# Drop useless variables
fuel <- fuel %>%
  select(Month,
         `Aviation turbine fuel Domestic (ML)`,
         `Aviation turbine fuel International (ML)`)

routes <- routes %>%
  select(-Month)

airports <- airports %>%
  select(Name,
         Latitude,
         Longitude)

# Create consistent time variables
fuel <- fuel %>%
  mutate(month = as_date(Month), .keep = "unused")

routes <- routes %>%
  mutate(month = ym(paste0(Year, Month_num)), .keep = "unused")

# Rename columns
fuel <- fuel %>%
  rename(domestic = `Aviation turbine fuel Domestic (ML)`,
         international = `Aviation turbine fuel International (ML)`)

# check for unmatched airports in route dataset
relevant_airports <- rbind(
  select(routes,City1) %>% setNames("Name"),
  select(routes,City2) %>% setNames("Name")
) %>%
  unique

relevant_airports %>%
  anti_join(airports)

# manually adjust key for unmatched cases
airports <- airports %>%
  mutate(Name =
           case_match(
             Name,
             "Dubbo City Regional" ~ "Dubbo",
             "Kalgoorlie Boulder" ~ "Kalgoorlie",
             "Sydney Kingsford Smith" ~ "Sydney",
             "Ballina Byron Gateway" ~ "Ballina",
             "Ayers Rock Connellan" ~ "Ayers Rock",
             "Proserpine Whitsunday Coast" ~ "Proserpine",
             "Wynyard" ~ "Burnie",
             "Wagga Wagga City" ~ "Wagga Wagga",
             Name ~ Name
           )
  )

# filter airports for which route data is available
airports <- airports %>%
  inner_join(relevant_airports)

# Data exploration ####

comparison_domestic <- routes %>%
  group_by(month) %>%
  select(-Passenger_Load_Factor,-`Distance_GC_(km)`) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  inner_join(select(fuel,month,fuel = domestic), join_by(month)) %>%
  group_by(month) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(sector = "domestic")

comparison_international <- international %>%
  group_by(Month) %>%
  select(month = Month,passengers = TotalPax) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  inner_join(select(fuel,month,fuel = international), join_by(month)) %>%
  group_by(month) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(sector = "international")

comparison <- comparison_domestic %>%
  select(month, passengers = Passenger_Trips, fuel, sector) %>%
  rbind(comparison_international) %>%
  mutate(fuel_per_passenger = fuel/passengers) %>%
  filter(year(month)<2020)

grid.arrange(
  ggplot(comparison %>% filter(sector == "domestic")) +
    geom_line(aes(x=month, y = fuel), colour = "black"),
  ggplot(comparison %>% filter(sector == "domestic")) +
    geom_line(aes(x=month, y = passengers), colour = "red"),
  nrow=2)

ggplot(comparison) +
  geom_line(aes(x=month, y = fuel, colour = sector))

ggplot(comparison) +
  geom_line(aes(x=month, y = fuel_per_passenger, colour = sector))

# Estimate regression model based on route dataset
regdata <- comparison_domestic %>%
  filter(year(month) >= 2018) # exclude data before mandatory reporting standards introduced

max_variables <- 5 # model search becomes computationally intensive above 7 variables (for example, if interaction variables are introduced)

regmodels <- regsubsets(fuel ~ Passenger_Trips + Aircraft_Trips + RPKs + ASKs + Seats,
                        data = regdata,
                        nvmax = max_variables,
                        # intercept = FALSE
)

res.sum <- summary(regmodels)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}

# Compute cross-validation error
model.ids <- 1:max_variables
cv.errors <-  map(model.ids, get_model_formula, regmodels, "fuel") %>%
  map(get_cv_error, 
      data = regdata
  ) %>%
  unlist()

coef(regmodels, which.min(cv.errors))

# no predict method is available for regsubsets, so I create a function to do it
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object[["call"]][[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  # Matrix
  pred_mat <- mat[, xvars] %*% coefi
  # Vector
  pred <- as.numeric(pred_mat)
  names(pred) <- rownames(mat)
  pred
}

fuel_predicted <- predict.regsubsets(regmodels, newdata = comparison_domestic, id = which.min(cv.errors))

comparison_domestic_prediction <- cbind(comparison_domestic, fuel_predicted)

ggplot(comparison_domestic_prediction) +
  geom_line(aes(x=month, y = fuel, colour = "fuel"), colour = "black") +
  geom_line(aes(x=month, y = fuel_predicted, colour = "fuel_predicted"), colour = "red")
# The regression model supports the hypothesis that there is an accounting issue in the data.
# We proceed on the assumption that - prior to 2018 - the traffic data is a better indicator of fuel usage than the petroleum statistics dataset.
# The regression model is used to estimate fuel usage prior to 2018.
fuel <- comparison_domestic_prediction %>%
  filter(month<'2018-01-01') %>%
  select(fuel_predicted) %>%
  setNames('fuel_estimate') %>%
  rbind(
    fuel %>%
      filter(month>='2018-01-01') %>%
      select(domestic) %>%
      setNames('fuel_estimate')
  ) %>%
  cbind(fuel,.)

# Create time-series model of total emissions ####
# convert fuel volume to co2 emmissions (kilotonne) equivalent
fuel$emissions_estimate <- fuel$fuel_estimate * (3.15 + 0.54)/0.8

# Convert to TS object
emissionsts <- fuel$emissions_estimate %>%
  ts(end = c(year(max(fuel$month)), month(max(fuel$month))), frequency=12)

# plot time series object
plot(emissionsts,type='o', ylab = "Carbon dioxide emissions (kt)", main="Monthly carbon emissions \nfrom Australian domestic air travel")

# compare against travel volume
combined <- routes %>%
  group_by(month) %>%
  select(month,Passenger_Trips,Aircraft_Trips) %>%
  summarise(Passenger_Trips = sum(Passenger_Trips), Aircraft_Trips = sum(Aircraft_Trips)) %>%
  arrange(month) %>%
  inner_join(
    fuel, join_by(month)
  )

# truncate time series post-pandemic
emissionsts_prepandemic <- ts(emissionsts[time(emissionsts) < 2020], end = c(2019, 12), frequency=12)

# summary statistics
summary(emissionsts_prepandemic)

# plot histogram
hist(emissionsts_prepandemic,main="Histogram of monthly carbon emissions")

# Examine normality
qqnorm(emissionsts_prepandemic, ylab="Carbon dioxide emissions (kt)", xlab="Normal Scores",main="Normal Q-Q plot of emissions")
qqline(emissionsts_prepandemic)
shapiro.test(emissionsts_prepandemic)
# p-value of 0.1877 means there is not enough evidence to reject the null hypothesis of normality at 5% significance.
# however, transformation could still be beneficial to improve normality for the modelling process.

# Plot correlation with first lag
plot(y=emissionsts_prepandemic,x=zlag(emissionsts_prepandemic),ylab='Emissions', xlab='Previous month emissions', main = "Scatter plot of emissions in consecutive months.")

# correlation with first lag indicates that time-series analysis is appropriate

# Plot ACF and PACF
par(mfrow=c(1,2))
acf(emissionsts_prepandemic, lag.max = 48, main ="ACF plot of \nemissions series")
pacf(emissionsts_prepandemic, lag.max = 48, main ="PACF plot of \nemissions series")
# ACF and indicates seasonality on annual frequency. Need seasonal differencing before exploring ACF/PACF further
par(mfrow=c(1,1))

# Transformation and differencing ----
BC = BoxCox.ar(emissionsts_prepandemic, lambda = seq(2, 5, 0.01))
BC$ci
lambda = BC$lambda[which(max(BC$loglike) == BC$loglike)]
emissionstsBC = (emissionsts_prepandemic^lambda-1)/lambda

# Plot transformed series
par(mfrow=c(1,1))
plot(emissionstsBC,type='l',ylab='BC transformed $', main ="Time series plot of BC transformation of emissions")
points(y=emissionstsBC,x=time(emissionstsBC), pch=as.vector(season(emissionstsBC)))

# Examine normality
qqnorm(emissionstsBC, ylab="$", xlab="Normal Scores",main="Normal Q-Q plot of emissions")
qqline(emissionstsBC)
shapiro.test(emissionstsBC)

# Plot ACF and PACF
par(mfrow=c(1,2))
acf(emissionstsBC, lag.max = 48, main ="ACF plot of \ntransformed emissions series")
pacf(emissionstsBC, lag.max = 48, main ="PACF plot of \ntransformed emissions series")
par(mfrow=c(1,1))

# Test for stationarity
adf.test(emissionstsBC,k=12)

# Test for stationarity in first-differenced series
plot(diff(emissionstsBC,differences = 12), main ="Plot of first-differenced series")
adf.test(diff(emissionstsBC,differences = 12),k=12)

# Model specification ----

# Create dataframe for model orders
orders <- data.frame(p=integer(),
                     d=integer(),
                     q=integer(),
                     P=integer(),
                     D=integer(),
                     Q=integer(),
                     s=integer()
)

# Residuals method for finding seasonal orders
par(mfcol=c(3,1))

m1.emissionstsBC = arima(emissionstsBC,order=c(0,0,0),seasonal=list(order=c(0,1,0), period=12))
res.m1 = residuals(m1.emissionstsBC);  
plot(res.m1,xlab='Time',ylab='Residuals',main="Time series plot of the residuals \nfor SARIMA(0,0,0)x(0,1,0)_12")
acf(res.m1, lag.max = 72, main = "ACF of residuals")
pacf(res.m1, lag.max = 72, main = "PACF of residuals")

m2.emissionstsBC = arima(emissionstsBC,order=c(0,0,0),seasonal=list(order=c(0,1,1), period=12))
res.m2 = residuals(m2.emissionstsBC);  
plot(res.m2,xlab='Time',ylab='Residuals',main="Time series plot of the residuals \nfor SARIMA(0,0,0)x(0,1,1)_12")
acf(res.m2, lag.max = 72, main = "ACF of residuals")
pacf(res.m2, lag.max = 72, main = "PACF of residuals")
orders[nrow(orders)+1, ] <- c(0,0,0,0,1,1,12)

m3.emissionstsBC = arima(emissionstsBC,order=c(0,1,0),seasonal=list(order=c(0,1,1), period=12))
res.m3 = residuals(m3.emissionstsBC);  
plot(res.m3,xlab='Time',ylab='Residuals',main="Time series plot of the residuals \nfor SARIMA(0,1,0)x(0,1,1)_12")
acf(res.m3, lag.max = 72, main = "ACF of residuals")
pacf(res.m3, lag.max = 72, main = "PACF of residuals")
orders[nrow(orders)+1, ] <- c(0,1,0,0,1,1,12)

m4.emissionstsBC = arima(emissionstsBC,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12))
res.m4 = residuals(m4.emissionstsBC);  
plot(res.m4,xlab='Time',ylab='Residuals',main="Time series plot of the residuals \nfor SARIMA(0,1,1)x(0,1,1)_12")
acf(res.m4, lag.max = 72, main = "ACF of residuals")
pacf(res.m4, lag.max = 72, main = "PACF of residuals")
orders[nrow(orders)+1, ] <- c(0,1,1,0,1,1,12)

par(mfrow=c(1,1))

# ARMA subsets
bic.a = armasubsets(y=res.m4,nar=7,nma=7,y.name='p',ar.method='ols')
plot(bic.a)
# no evidence of additional models to consider.

# Parameter estimation ----
# Estimate each model and place them in a list
models <- list()
for (i in 1:nrow(orders)) {
  for (j in c('ML','CSS','CSS-ML')) {
    
    modelname <- paste0("m_",orders$p[i],orders$d[i],orders$q[i],"_",orders$P[i],orders$D[i],orders$Q[i],"_",gsub("-", "", j))
    
    tryCatch({
      models <- append(models,list( 
        arima(emissionstsBC,order=c(orders$p[i],orders$d[i],orders$q[i]), seasonal=list(order=c(orders$P[i],orders$D[i],orders$Q[i]), period=orders$s[i]), method = j)
      ))
      names(models)[length(models)] <- modelname
    }, error=function(e){})
    
    tryCatch({
      assign(modelname,arima(emissionstsBC,order=c(orders$p[i],orders$d[i],orders$q[i]), seasonal=list(order=c(orders$P[i],orders$D[i],orders$Q[i]), period=orders$s[i]), method = j))
    }, error=function(e){})
    
  }
}

# Names of models successfully estimated via maximum likelihood
ls()[grep("\\_ML$", ls())]

# Create sort.score function
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

# Sorting models by AIC and BIC
sc.AIC <- AIC(m_000_011_ML,m_010_011_ML,m_011_011_ML)
sc.BIC <- AIC(m_000_011_ML,m_010_011_ML,m_011_011_ML, k = log(length(emissionstsBC)))
sort.score(sc.AIC, score = "aic")
sort.score(sc.BIC, score = "aic")

# Shortlist of models identified through AIC and BIC ranking
shortlist <- list(m_000_011_ML,m_010_011_ML,m_011_011_ML)
names(shortlist) <- c('m_000_011_ML','m_010_011_ML','m_011_011_ML')

# Examine shortlist model coefficients
lapply(shortlist,coeftest)

# Create residual.analysis function
residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH", "garch", "fGARCH")[1]){
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "garch"){
    res.model = model$residuals[start:model$n.used]  
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  # LBQPlot(res.model, lag.max = 20, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}


# Model diagnostics ----
residual.analysis(m_011_011_ML)

# Forecasting ----
m_011_011_ML_A = Arima(emissionstsBC,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12), method = "ML")
par(mfrow=c(1,1))
steps_fuel <- 38
emissionstsBC_forecast = forecast::forecast(m_011_011_ML_A, h = steps_fuel)

plot(emissionstsBC_forecast, main = "Forecasts based on SARIMA(0,1,1)x(0,1,1)12")

emissionsts_forecast <- cbind(
  forecast.lower95 = emissionstsBC_forecast$lower[,2],
  forecast.lower80 = emissionstsBC_forecast$lower[,1],
  forecast.mean = emissionstsBC_forecast$mean,
  forecast.upper80 = emissionstsBC_forecast$upper[,1],
  forecast.upper95 = emissionstsBC_forecast$upper[,2]) %>%
  {(. * lambda + 1)^(1/lambda)} %>%
  {rbind(setNames(matrix(NA, nrow = length(emissionsts)-steps_fuel, ncol = ncol(.)), colnames(.)),.)} %>%
  cbind(data.frame(year = as.integer(time(emissionsts)), month = cycle(emissionsts), actual = emissionsts),.) %>%
  mutate(across(starts_with("forecast"),~.-actual,.names = "{sub('forecast', 'diff', col)}"))

ggplot(emissionsts_forecast) +
  geom_ribbon(data = emissionsts_forecast, aes(as.Date(paste0(as.character(year*100+month), '01'), format='%Y%m%d'),ymin = actual, ymax = forecast.lower95), fill = "dark green", alpha = 0.1) +
  geom_ribbon(data = emissionsts_forecast, aes(as.Date(paste0(as.character(year*100+month), '01'), format='%Y%m%d'),ymin = actual, ymax = forecast.lower80), fill = "dark green", alpha = 0.1) +
  geom_ribbon(data = emissionsts_forecast, aes(as.Date(paste0(as.character(year*100+month), '01'), format='%Y%m%d'),ymin = actual, ymax = forecast.mean), fill = "dark green", alpha = 0.1) +
  geom_ribbon(data = emissionsts_forecast, aes(as.Date(paste0(as.character(year*100+month), '01'), format='%Y%m%d'),ymin = actual, ymax = forecast.upper80), fill = "dark green", alpha = 0.1) +
  geom_ribbon(data = emissionsts_forecast, aes(as.Date(paste0(as.character(year*100+month), '01'), format='%Y%m%d'),ymin = actual, ymax = forecast.upper95), fill = "dark green", alpha = 0.1) +
  geom_line(data = emissionsts_forecast, aes(as.Date(paste0(as.character(year*100+month), '01'), format='%Y%m%d'), forecast.mean), colour = "dark green") +
  geom_line(data = emissionsts_forecast, aes(as.Date(paste0(as.character(year*100+month), '01'), format='%Y%m%d'), actual), colour = "black")

emissionsts_forecast %>%
  select(starts_with("diff")) %>%
  colSums(na.rm = TRUE)

# time-series model estimation for routes ####

params <- list( # obtained from https://www.myclimate.org/fileadmin/user_upload/myclimate_-_home/01_Information/01_About_myclimate/09_Calculation_principles/Documents/myclimate-flight-calculator-documentation_EN.pdf
  short = list(
    S = 153.51,
    PLF = 0.82,
    DC = 95,
    CF = 1-0.93,
    CW_e = 0.96,
    CW_b = 1.26,
    CW_f = 2.40,
    CW = (12*1.26 + 162*0.96)/(12+162), # proportions taken from https://www.qantas.com/au/en/qantas-experience/onboard/seat-maps/boeing-737-800.html
    EF = 3.15,
    P = 0.54,
    M = 1,
    AF =0.00038 ,
    A = 11.68,
    a = 0,
    b = 2.714,
    c = 1166.52
  ),
  long = list(
    S = 280.21,
    PLF = 0.082,
    DC = 95,
    CF = 1-0.74,
    CW_e = 0.80,
    CW_b = 1.54,
    CW_f = 2.40,
    CW = (12*1.26 + 162*0.96)/(12+162), # proportions taken from https://www.qantas.com/au/en/qantas-experience/onboard/seat-maps/boeing-737-800.html
    EF = 3.15,
    P = 0.54,
    M = 1,
    AF =0.00038 ,
    A = 11.68,
    a = 0.0001,
    b = 7.104,
    c = 5044.93
  )
)

routes <- routes %>%
  mutate(
    emissions = ( # based on https://www.myclimate.org/fileadmin/user_upload/myclimate_-_home/01_Information/01_About_myclimate/09_Calculation_principles/Documents/myclimate-flight-calculator-documentation_EN.pdf
      if_else(`Distance_GC_(km)`<1500,
              (
                (params$short$a * `Distance_GC_(km)`^2 + params$short$b * `Distance_GC_(km)` + params$short$c) *
                  (1-params$short$CF) *
                  params$short$CW *
                  (params$short$EF * params$short$M + params$short$P) +
                  params$short$AF *
                  `Distance_GC_(km)` +
                  params$short$A
              ) * Aircraft_Trips,
              if_else(`Distance_GC_(km)`>=1500 & `Distance_GC_(km)`<=2500,
                      (1-(`Distance_GC_(km)`-1500)/(2500-1500)) *
                        (
                          (params$short$a * `Distance_GC_(km)`^2 + params$short$b * `Distance_GC_(km)` + params$short$c) *
                            (1-params$short$CF) *
                            params$short$CW *
                            (params$short$EF * params$short$M + params$short$P) +
                            params$short$AF *
                            `Distance_GC_(km)` +
                            params$short$A
                        ) * Aircraft_Trips
                      +
                        ((`Distance_GC_(km)`-1500)/(2500-1500)) *
                        (
                          (params$long$a * `Distance_GC_(km)`^2 + params$long$b * `Distance_GC_(km)` + params$long$c) *
                            (1-params$long$CF) *
                            params$long$CW *
                            (params$long$EF * params$long$M + params$long$P) +
                            params$long$AF *
                            `Distance_GC_(km)` +
                            params$long$A
                        ) * Aircraft_Trips,
                      if_else(`Distance_GC_(km)`>2500,
                              (
                                (params$long$a * `Distance_GC_(km)`^2 + params$long$b * `Distance_GC_(km)` + params$long$c) *
                                  (1-params$long$CF) *
                                  params$long$CW *
                                  (params$long$EF * params$long$M + params$long$P) +
                                  params$long$AF *
                                  `Distance_GC_(km)` +
                                  params$long$A
                              ) * Aircraft_Trips, NA
                      )
              )
      )
    )
  )

steps_routes <- length(seq(from=dmy(01012020), to=max(routes$month), by='month'))

# examine data completeness
test <- routes %>%
  filter(month>='2018-01-01') %>%
  complete(City1,City2,month) %>%
  select(City1,City2,month,emissions) %>%
  pivot_wider(names_from = month, values_from = emissions) %>%
  arrange(desc(!!sym(as.character(max(routes$month)))))

routes_forecast_models <- routes %>%
  filter(month>='2018-01-01') %>%
  filter(month<='2019-12-01') %>%
  complete(City1,City2,month) %>%
  group_by(City1,City2) %>%
  filter(all(emissions>0)) %>%
  ungroup() %>%
  filter(month<'2020-01-01') %>%
  mutate(month = yearmonth(month)) %>%
  as_tsibble(key = c("City1", "City2"), index = month) %>%
  model(emissions = ARIMA(emissions)) %>%
  reconcile(emissions = min_trace(emissions)) %>% 
  forecast(h = steps_routes)

emissions_forecast <- routes_forecast_models %>%
  as_tibble() %>% 
  pivot_wider(id_cols = c("City1", "City2", "month"), names_from = ".model", values_from = ".mean")

# compare
emissionsts_forecast %>%
  mutate(month = yearmonth(ym(paste0(year, month))), .keep = "unused") %>%
  left_join(routes %>%
              group_by(month) %>%
              summarise(emissions = sum(emissions)),
            join_by(month)) %>%
  filter(date(month)>='2019-07-01') %>%
  select(month,actual,emissions) %>%
  mutate(fuel_estimate = actual,
         route_calculation = emissions / 1000000,
         .keep = "unused")

# Tidy data for export to Power BI ####

fuel_forecast <- emissionsts_forecast
route_forecast <- emissions_forecast

rm(list = ls()[!ls() %in% c("fuel", "routes", "airports", "fuel_forecast", "route_forecast")])

