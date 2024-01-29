```{r setup, include=FALSE}
library(tidyverse)
library(tidymodels)
library(readr)
library(knitr)
library(zoo)
library(dplyr)
library(lubridate)
library(fpp2)
library(tsibble)
library(seasonal)
library(seasonalview)
library(xts)
library(janitor)
library(forecast)
library(itsmr)
tidymodels_prefer()
set.seed(1738)
```

```{r 2, include=FALSE}
income <- read_csv('174incomeprime.csv',show_col_types = FALSE) ##,header = FALSE)
```

```{r 3, include=FALSE}


#Make the rows in the time values
transpose_df <- function(df) { 
  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.)
  return(t_df)
}

# using the function
income <- transpose_df(income)

#Make row of value names the column header
income <- janitor::row_to_names(income, row_number = 1)

#Not needed
income <- select(income, -Quarter)

#standardize names into underscore_names
income <- income %>% 
  clean_names()

head(income)

```

```{r 4, include=FALSE}

government <- select(income,government)

#values were considered characters, and it somehow changed numerical values in my timeseries
government$government = as.numeric(government$government)

tsgov <- ts(government$government, start= 1947, end = 2000, frequency = 4)

autoplot(tsgov)

#testing diffferencing, and boxcox transformations
diffgov <- diff(tsgov)

autoplot(diffgov)

acf(diffgov)

boxm <- BoxCox(tsgov, lambda = 0)

autoplot(boxm)

autoplot(diff(boxm))

arima(tsgov)

acf(boxm)

acf(diff(boxm))

auto.arima(tsgov)

```

```{r 5, include=FALSE}

#Testing if additive decomposition can help
autoplot(decompose(tsgov))


decomp <- decompose(tsgov)

decomp_model <- tsgov - decomp$seasonal
autoplot(decomp_model)

acf(decomp_model)

decomp2 <- diff(decomp_model, lag = 1, differences = 2)

autoplot(decomp2)
```

```{r 6, include=FALSE}
#titled 'optimal' because after playing around with lags and differences, this performs best
optimal <- diff(tsgov, lag = 2, differences = 2)

#auto was what auto.arima returned on the nonstationary unadjusted tsgov
auto <- diff(tsgov, lag = 1, differences = 2)

autoplot(optimal)

acf(optimal)



Box.test(optimal, type = 'Ljung-Box')


#'smoptimal' is small optimal: reducing the boundaries of my time series to try to achieve stationarity. This did help, but I realized I needed to curtain the dataset then do modeling, not the other way around.
smoptimal <- as.ts(head(as.zoo(optimal), -40))

autoplot(optimal)
autoplot(smoptimal)

acf(smoptimal)

Box.test(smoptimal,type = 'Ljung-Box')

auto.arima(smoptimal)

```

```{r 7, include=FALSE}

#trying my 'optimal' model if I did boxcox transformation first
boptimal <- diff(boxm, lag = 2, differences = 2)

autoplot(boptimal)

acf(boptimal)

Box.test(boptimal, type = 'Ljung-Box')


#testing if a square root transformation is effective
squirt <- diff(sqrt(tsgov), lag = 1, differences = 2)

autoplot(squirt)

acf(squirt)

Box.test(squirt, type = 'Ljung-Box')
```

```{r 8, include=FALSE}
#cut it

autoplot(tsgov)


##separate data to deal with varying variation
early <- ts(tsgov[0:113],start= 1947, end = 1975, frequency = 4)

late <- ts(tsgov[112:208],start= 1975, end = 1999, frequency = 4)

autoplot(early)
autoplot(late)


auto.arima(early)

auto.arima(late)


#tried the model that worked best on the original data
test <- diff(early, lag = 2, differences = 2)

autoplot(test)

acf(test)

Box.test(test, type = 'Ljung-Box')

autoplot(early)

#box transform of early data
ebox <- BoxCox(early, lambda = 0)

autoplot(ebox)

eboxtest <- diff(ebox, lag = 2, differences = 3)

autoplot(eboxtest)

acf(eboxtest)

Box.test(eboxtest, type = 'Ljung-Box')

#Box.test for late gave 1 as the ideal lambda, which means no transformations, so I did not try it in models.

autoplot(diff(late))

#this model, ltest, permformed very well in Box-Ljung test
ltest <- diff(late, lag = 2, differences = 3)

#l2, less differenced, has the second best p-val at .02, and a lower AIC
l2 <- diff(late,lag = 2, differences = 2)

autoplot(ltest)

acf(ltest)


Box.test(ltest, type = 'Ljung-Box')


auto.arima(ltest)


Box.test(l2, type = 'Ljung-Box')


auto.arima(l2)

```
