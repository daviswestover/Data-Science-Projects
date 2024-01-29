                                           Executive Summary

  Within the battle for a $15 dollar minimum wage, Many people argue that the market correctly dictates a fair wage. As inflation increases,
do wages increase steadily with them? Or are there massive increases, a trend of spikes every so often?

  In doing my analysis, I found the trend was not consistent at all. In particular, the rate at which wages increased at times and variance 
lessened after 1975, leading to modeling being more effective by using pre and post 1975 data. Even then, major events/spikes in the data 
biased modeling and made fitting more difficult. The final data forecasted was taken from 1975-2000, and the most effective model offered a 
rather linear line.

                                            Introduction

  Wages keep most Americans alive, and with concerns of the cost of living constantly rising, it's pertinent to keep track of wages and their 
growth. The government may see wages not keeping up with inflation and be galvanized to do something to make sure middle class workers are sunk 
by rising inflation. Companies might look at the standard wages in their field to figure out what raises to give whom, to match growth in their 
industry.

  According to the paper "A time series analysis of real wages, consumption and asset returns" by Thomas F. Cooley and Masao Ogaki, the two 
common ways to attempt to model real wages are the optimal labor contract model and a neoclassical model. Optimal labor contract models balance 
the three types of agency costs (contracting, monitoring, and misbehavior) against one another to minimize the total cost. The neoclassical model 
treats jobs as goods of a free market, determined by supply and demand like everything else. I'm not sure how I would or could factor this into 
my project, but It was good to learn.



                                  Data Analysis and Experimental Design

  This data was collected by the Bureau of Economic Analysis. As explained by them, Governments of all levels and Americans everywhere use their 
numbers; Their work underpins decisions about interest rates and trade policy, taxes and spending, hiring and investing, and more. I frequently 
doubt Capitalism's ability to fairly compensate the actual workers of a system, and thought this was a great opportunity to look into the growth 
of pay in the United States to see how true that is. While I did not have time on this project, a further investigation could compare this growth 
to inflation and projected cost of living to better answer my questions.

  The dataset I downloaded contains the average wages for different market sectors including private industries, manufacturing, and government among 
other things. I looked further into government wages, figuring that would be the easiest place for the government itself to adjust wages. The data is 
collected quarterly, and the BEA is committed to public data released to everyone simultaneously. The BEA gets most of their data from the federal 
government. Agencies like the Labor Department's Bureau of Labor Statistics and the Treasury Department naturally collect data through things like 
corporate tax returns and regulating public utilities that can be used very effectively by the BEA.

  The data ranges from 1947-2000, with four data points per year. That means the original model had about 212 points. The reduced data set contained 
97 points, just under the ideal 100 points to have enough data to comfortably model. The Velocity is not high by time series means. Quarterly reports 
means a forecast does not need to be getting consistently updated week by week. The Variety of this data has already been categorized, as the BEA 
releases their own reports and analysis. The data set I collected was tabled to list values with their corresponding year and quarter. The Veracity of 
this data is top notch, as it comes mostly from government filings, and the Bureau is well respected and trusted by people working with economic policy.
Lastly, this data is clearly valuable to my mission, as their isn't a better way to analyze wages than comprehensive data on US wages.

  No data was missing from the report. I removed some titles from the original table, and pivoted the table so that each row represented a different 
time value. Achieving stationarity was the most difficult part of this process. The original data is clearly not stationary (constant mean and variance 
over time).

```{r 10, echo = FALSE}
autoplot(tsgov, main = 'Government Wages')
```

  With progressively increasing time series, the first transformation to do is difference the data, to focus on the change between points instead of the 
total value.

```{r 11, echo = FALSE}
autoplot(diffgov, main = 'Differenced Wages')
```

  This gets us closer, but there is still an upwards trend, and there seems to be more variation as time progresses. BoxCox methodology tests a log 
transformation and reciprocal transformation to achieve normality in data. The log transform fit best, and makes the line much more linear. Differencing 
this transformation give the most stationary graph yet, but the variation dramatically decreases as time progresses. This is seen in the ACF, which compares 
correlation of each of the data points. The early lags have a much higher impact on the data compared later on. As I continued looking into models, both the 
regular and boxcox transformed version of my data were considered.

```{r 12, echo = FALSE}
autoplot(boxm, main = 'BoxCox Log Transformed Wages')
autoplot(diff(boxm), main = 'Differenced BoxCox Wages')
acf(diff(boxm), main = 'ACF of Diff BoxCox')
```

  auto.arima() provides a reccomendation of the 'best' ARIMA model for a given time series. It consideres the three components of an ARIMA model, p (number of 
autoregressive terms), q (differences), and d, lagged errors. The function uses AIC, a method of measuring quality of fit through log likelihood and penalizing 
large numbers of parameters, to try p and q values after picking a d value from (0,1,2).
```{r 40, echo = FALSE }
autoplot(auto, main = 'auto.arima model')
```

  That gave me the graph above as an ARIMA(0,2,1) model. This time, variation increases as time progresses. By experimenting with similar models, I found my 
best so far by differencing twice and taking 2 lags. The acf mostly falls into the confidence region for stationarity, but it is clear the lags are decreasing 
over time. Ljung-Box tests, which examine the autocorrelations of the residuals to test for a lack of fit, showed I was still far from stationarity, and the 
variation issue needed to be addressed.

```{r 13, echo = FALSE}
autoplot(optimal,main = 'Twice Differenced, Twice lagged')
acf(optimal,main = 'ACF')
#Box.test(optimal, type = 'Ljung-Box')
#Box.test(auto, type = 'Ljung-Box')
```

  Upon research, I learned policies and changes in the economy led to wage increases stagnating a bit and varying much less after 1975. This supports the idea of
wages not increasing sufficiently. To address this issue, I separated the data into to parts, pre and post 1975. After differencing the data, it appeared the 
variance problem was resolved, but transformations still would need to be made to achieve stationarity.

```{r 14, echo = FALSE}
autoplot(diff(early),main = '1947-1975 Wages Differenced')
autoplot(diff(late),main = '1975-2000 Wages Differenced')
```

  I used BoxCox as well as the differencing models I had been trying earlier, and found the best performing one to be 3 Differences and 2 lags on the later data 
collected (1975-200). The acf sees more lags exiting the confidence interval compared to the optimal model on my whole dataset, but unlike that model, this one 
does not see a progressive decrease in lags. Finally, a model passes the Box-Ljung test, with a p-value of .36. Standard procedure is for the p-value to exceed 
.05 to be statistically significant. Still, when looking at this data, there is a significant change in the 90s that strongly affects the rest of the model. I 
chose not to further shrink down my dataset to avoid it, but this again shows the lack of consistency in wages needed to properly forecast it.

```{r 15, echo = FALSE}
autoplot(ltest, main = 'Thrice Differenced, Twice lagged')
acf(ltest,main = 'ACF')
#Box.test(ltest, type = 'Ljung-Box')
```

   To make sure I didn't lose information by differencing too many times, I ran a unit test to confirm the roots of the model are in the unit circle and the model
is therefore invertible, which it is.

```{r 16, echo = FALSE}
autoplot(Arima(late,c(0,3,2)), main = "ARIMA(0,3,2) roots")
```  
   
   
   
   
                      Time Series Modeling and Forecasting
  The two models I selected to test are 'ltest' and 'l2', which correspond to (model 1) ARIMA(0,3,2) and (model 2) ARIMA(0,2,2). Model 1, for example, looks like 
  Y(t) = -Y(t-1) - 2Y(t-1) + Y(t-3) +1.6821e - .6821e, where e is the error term and (t-1) is subscript. Model 2 is a bit simpler seemingly without sacrificing fit, 
as it has a slightly lower AIC (375 compared to 378). That being said, ARIMA models assume stationarity. Model 1 is stationary, while Model 2 did not pass the Box-Ljung 
test. Overall, lack of stationarity greatly reduced the options for model fitting. Using a non-stationary time series can lead to asymptotic normality and generally 
unreliable results. ARIMA models must also use univariate data, which is upheld here as well. Looking at the residuals of both models, both pass stationarity, a good sign 
for being able to forecast. Model 2 has slightly less variance, but not by much.

```{r 17, echo = FALSE}
model1 <- Arima(late,c(0,3,2))
model2 <- Arima(late,c(0,2,2))
res1 <- residuals(model1)
res2 <- residuals(model2)
ggtsdisplay(res1, main = 'Model 1 residuals')
ggtsdisplay(res2, main = 'Model 2 residuals')
Ljung1 <- Box.test(res1, type = 'Ljung-Box')
Ljung2 <- Box.test(res2, type = 'Ljung-Box')
print(paste("The Ljung-Box p-values for model 1 is", Ljung1$p.value))
print(paste("The Ljung-Box p-values for model 2 is", Ljung2$p.value))
```
  
  This prepares us to forecast. Ultimately, I am going with model 1. The AICs are very similar, both residuals pass stationarity, but only one is stationary itself. It makes much
more sense to forecast using model 1. The prediction looks like this:

```{r 18, echo = FALSE}
autoplot(forecast::forecast(model1, h=50))
```
  As the model increases, variation of the confidence intervals for predictions also increases.
  
                                Discussion and Conclusions
  Overall, the process of achieving a forecast answered the questions I had more than forecasting did itself. The models showed that wages do consistently increase, but less so
in the modern era (post 1975) compared to before. There are also significant points in the data that heavily influence model prediction. These major events were not consistent, 
seasonal increases, but seemingly random events of spiking, difficult for a model to interpret. After only considering data past 1975, Differencing 3 times, along with 2 lags of 
the error variables managed to create a stationary model to be used in forecasting. This forecast could be similarly made by trying to draw straight along the already existing line, 
but that seems to imply the model as decently successful. The confidence intervals also give an idea of the standard error of these future predictions, and couldn't indicate how extreme 
the most extreme change in wages would likely be in a given amount of time.
  
  
  
  
  
  
  
  
  
  
  
  
  
  
                                Bibliography and Appendix
Reference list

apps.bea.gov. (2023). BEA Interactive Data Application. [online] Available at: https://apps.bea.gov/iTable/?reqid=19&step=3&isuri=1&1921=survey&1903=58#eyJhcHBpZCI6MTksInN0ZXBzIjpbMSwyLDMsM10sImRhdGEiOltbIk5JUEFfVGFibGVfTGlzdCIsIjU4Il0sWyJDYXRlZ29yaWVzIiwiU3VydmV5Il0sWyJGaXJzdF9ZZWFyIiwiMTk0NyJdLFsiTGFzdF9ZZWFyIiwiMjAyMCJdLFsiU2NhbGUiLCItOSJdLFsiU2VyaWVzIiwiUSJdXX0= [Accessed 14 Mar. 2023].

Cooley, T. and Ogaki, M. (1996). A Time Series Analysis of Real Wages, Consumption, and Asset Returns. Journal of Applied Economics, Vol. 11(No. 2).

Harvey, C. (n.d.). Optimal contract. [online] TheFreeDictionary.com. Available at: https://financial-dictionary.thefreedictionary.com/Optimal+contract [Accessed 14 Mar. 2023].

NIST/SEMATECH (2012). 6.4.4.8.1. Box-Ljung Test. [online] www.itl.nist.gov. Available at: https://www.itl.nist.gov/div898/handbook/pmc/section4/pmc4481.htm.

Tsay, R.S. (2013). An introduction to analysis of financial data with R. Hoboken, N.J.: Wiley.

Vercherand, J. (2014). Labour. Springer. 
