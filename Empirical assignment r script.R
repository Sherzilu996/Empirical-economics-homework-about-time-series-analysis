library(AER)
library(readxl)
library(xts)
library(zoo)
library(dynlm)

# Now, we want to reformat our data so that the "Date" column
# is loaded in as a recognized date. To understand the format string
# format = "%Y Q%q" load the raw xls file and look at how dates are stored
# in that file.
totalpay<- read_excel(path = "UK_totalpay.xls")

totalpay$Date<- as.yearqtr(totalpay$Title)
# We're going to convert our data into an xts formatted object, so we can use
# time series tools easily:

totalpay_xts<-xts(totalpay$totalpay,totalpay$Date)
#UnempRate_UKnational.xls
UK_UnemploymentRate <- read_excel(path = "UnempRate_UKnational.xls")    
# Load the Excel spreadsheet of Unemployment Rate in UK
#UK_UnemploymentRate$Title
UK_UnemploymentRate$Date <- as.yearqtr(UK_UnemploymentRate$Title, format = "%Y Q%q")
# Format the "Date" column as year-quarter

UK_UnemploymentRate_xts <- xts(UK_UnemploymentRate$Unemploymentrate, UK_UnemploymentRate$Date) # Convert this into an xts object
UK_UnemploymentRate_1997_2020 <- UK_UnemploymentRate_xts["1997::2020"]  

# Let's plot some of this data to get a sense of the patterns that exist:
plot(as.zoo(totalpay_xts),col = "blue",
     lwd = 2,
     ylab = "U.K. seasonally ManufacturingTotalPay",
     
     xlab = "Date",
     main = "U.K. seasonally ManufacturingTotalPay")

plot(as.zoo(log(totalpay_xts)),col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Log U.K. seasonally ManufacturingTotalPay")

### Section 1.3: Lags and Differences 
# # Lags are very important for understanding time series.
# Uncomment the line below to explore the details of the lag() operator
# we use for time series data:
# ?lag

# To take the lag of a variable, we use the lag() operator, where the second 
# argument is the order of the lag:
#GDP_xts_10yrsample <- xts(UK_gdp$GDPpc, UK_gdp$Date)["1990::2000"]
totalpay_20yrsample<-xts(totalpay$totalpay,totalpay$Date)["2000::2020"]
totalpay_xts_20yrsample_lag2<-lag(totalpay_xts,2)
#GDP_xts_10yrsample_lag2 <- lag(GDP_xts_10yrsample, 2)

par(mar=c(4.5, 4.5, 8, 2), xpd=TRUE)
plot(as.zoo(totalpay_20yrsample), col = "steelblue",
     lwd = 2,
     ylab = "Quarterly totalpay pc.",
     xlab = "Date",
     mgp = c(2.5, 1, 0),
     mar= c(3,3,3,3), 
     width=5,
     height=5,
     main = "U.K. Quarterly totalpay/Head")
lines(as.zoo(totalpay_xts_20yrsample_lag2),
      type = "l", lwd =2,
      col = "orange")
legend("top",
       c("totalpay", "totalpay, Lag 2"), 
       col = c("steelblue", "orange"), 
       lty = c(1, 1),
       inset=c(0,-0.4),
       cex = 0.8
)

# In addition to lags, we are also interested in differences:
totalpay_diff_xts <- diff(totalpay_20yrsample)
# We can also take lags of differences:
totalpay_diff_lag1_xts <- lag(totalpay_diff_xts,2)

plot(as.zoo(totalpay_diff_xts), col = "steelblue",
     lwd = 1,
     ylab = "Quarterly totalpay .",
     xlab = "Date",
     main = "U.K. Quarterly totalpay/Head")
lines(as.zoo(totalpay_diff_lag1_xts),
      type = "l", lwd =1,
      col = "orange")
legend("top",
       c("totalpay First Difference", "totalpay First Difference, Lag 2"), 
       col = c("steelblue", "orange"), 
       lty = c(1, 1),
       inset=c(0,-0.4),
       cex = 0.8
)

# Now that we know how these operators work, let's generate a 
# dataframe that includes all these useful variables:
totalpay_quantities <- data.frame("Level" = totalpay_xts, "Log" = log(totalpay_xts), "AnnualizedGrowthRate" = 400 * log(totalpay_xts / lag(totalpay_xts)), "Lag1AnnualGrowthRate" = lag(400 * (log(totalpay_xts / lag(totalpay_xts)))))

# To make life easy for ourselves, let's write a function to create these
# useful values:
quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
  )
}

#quants(GDP_xts["1960-01::2013-01"])

### Section 2.1: Autocovariance & Autocorrelation:
# let's take subset of the data to work with and generate the lags of 
totalpay_2000_2020 <- totalpay_xts["2000::2020"]
totalpay_2000_2020_quants = quants(totalpay_2000_2020)
totalpayGrowth_xts <- xts(400 * log(totalpay_xts/lag(totalpay_xts)))
totalpayGR_2000_2020<- totalpayGrowth_xts["2000::2020"]

# We drop the periods where we cannot observe GDP growth lags.
# take a look at the dataframe with our computed growth rates and lags,
# Where do we have missing values?
print(totalpay_2000_2020_quants)
write.csv(totalpay_2000_2020_quants,file = "totalpay_2000_2020_quants")
# We can't compute growth rate in the first period (1990 Q1), because we need 
# to observe the prior period to compute the growth rate.
# So, if we can't observe growth for 1990 Q1, this means we can't compute the lag
# for 1990 Q2, so the data we use for our autocorrelation once we drop these
# missing values begins in 1990 Q3.
totalpay_quants_OmitNA = na.omit(totalpay_2000_2020_quants)
# Now we compute the sample covariance and variance:
autocov_1 = cov(totalpay_quants_OmitNA["AnnualGrowthRate"], totalpay_quants_OmitNA["X1stLagAnnualGrowthRate"])
var_yt = var(totalpay_quants_OmitNA["AnnualGrowthRate"])
# With these two values, use Equation 1 from the exercise handout to compute the 
# the sample estimate of first-order autocorrelation:
autocor_yt_lag1 = autocov_1 / var_yt
print(autocor_yt_lag1)

# Now, use the 'acf' function to compute the first four autocorrelations in one step:
acf(totalpay_xts["2000::2020"], lag.max = 4, plot = F)



### Section 3: Autoregressions

### Section 3.1: Data Setup
# Let's set up data and lags manually so we can run an autoregression.
# length of data set

N <-length(totalpayGR_2000_2020)
# So to manually create lags, we drop the first observation of our Y_t,
# which has the effect of ''shifting'' all observations by one. 
# To ensure we keep the same number of observations, we drop the final
# observation of the time series to get our series of lags

## IF THIS SEEMS CONFUSING, DON'T WORRY! In practice you will always use 
# the lag() function to compute lags, but I thought it would be good to
# demonstrate how we can do this without any special functions.

# Recall: using the [-N] index removes the Nth item from the array
totalpayGR_level <- as.numeric(totalpayGR_2000_2020[-1])
# Now drop the last value to get a series of lags
totalpayGR_lags <- as.numeric(totalpayGR_2000_2020[-N])
# estimate the linear model
armod <- lm(totalpayGR_level ~ totalpayGR_lags)
coeftest(armod)

# We can use the lag() operator to do this in one step:
# This is the same as lm(Y_t ~ Y_(t-1))
totalpay_AR1_lm <- lm(totalpayGR_2000_2020 ~ lag(totalpayGR_2000_2020,1))
coeftest(totalpay_AR1_lm)

summary(totalpay_AR1_lm)$r.squared
#use the BIC to selct the appaopriate lag length
### Section 2: Selecting Lag Length
# First, try computing this using the t-test on the last lag: 
# To make things simpler, you can use the notation 1:6 as the argument
# of lag() to include all lags from 1:6 
totalpayGR_AR4<- lm(totalpay_2000_2020 ~ lag(totalpay_2000_2020,1:4)) # This is an AR6 model, which includes the first six lags, by using lag(GDPGR_1985_2015,1:6)
coeftest(totalpayGR_AR4)     # Check the coefficients -- are the lags significant?

totalpayGR_AR3 <- lm(totalpay_2000_2020 ~ lag(totalpay_2000_2020,1:3))
coeftest(totalpayGR_AR3)

totalpayGR_AR2 <- lm(totalpay_2000_2020 ~ lag(totalpay_2000_2020,1:2))
coeftest(totalpayGR_AR2)     # Check the coefficients -- are the lags significant? YES! This model has all lags significant
summary(totalpayGR_AR2)$r.squared
# Next: compute BIC and AIC for AR models using a convenient function.
IC <- function(model) {
  ssr <- sum(model$residuals^2) # This retrieves the sum of squared residuals
  t <- length(model$residuals) # This retrieves the LENGTH of the residuals - this is the same as the number of observations
  p <- length(model$coef) - 1 # Here we take the length of the list of coefficients and subtract 1 to get p - since this list includes the intercept.
  return(
    round(c("p" = p, # 'p' here is just the length of the vector of coefficients
            # Insert code here to compute the Bayes Information Criterion
            # this is the formula from the Homework Handout where '*' means 
            # multiplication, and '/' means division, and being careful to use brackets.
            # again, recall that log() is to take a log.
            "BIC" = log(ssr/t) + ((p + 1) * log(t)/t), 
            # Now, do the same as above for the Akaike Information Criterion:
            # Remember to add a comma at the end of the line!
            "AIC" = log(ssr/t) + ((p + 1) * 2/t),
            "R2" = summary(model)$r.squared), 
          4) # This last line just says to round the output to 4 decimal points.
  )
}




# loop ICs over models of different orders -- this is exactly the same as the code above but more convenient.
for (p in 1:4) { ## This first line means we will repeat the code in the brackets { } once for every value of 'p' from 1 to 6.
  print(IC(lm(totalpay_2000_2020 ~ lag(totalpay_2000_2020,1:p))))   # This takes the exact code from above, but replaces the final lag as the 'p' variable we loop over
}
#test for a unit root process
# Stationary Time Series:
library(urca)           #进行单位根检验
library(tseries)         #arma模型
library(fUnitRoots)     #进行单位根检验
library(FinTS)         #调用其中的自回归检验函数
library(fGarch)        #GARCH模型
library(nlme)          #调用其中的gls函数


adf.test(totalpay_quants_OmitNA$AnnualGrowthRate)

totalpayGrowth_xts_OmitNA<-na.omit(totalpayGrowth_xts)
adf.test(totalpayGrowth_xts_OmitNA)
#y_stationary_xts<-(totalpay_2000_2020$)
Stationary_DickeyFuller_lm <- lm(diff(totalpayGrowth_xts_OmitNA) ~ lag(totalpayGrowth_xts_OmitNA,1))
DF_stationary_teststat_manual = coeftest(Stationary_DickeyFuller_lm)[2,3]

unit_root_test_Stationary = ur.df(totalpayGrowth_xts_OmitNA, lags=0, type='drift')
DF_stationary_teststat_automatic = unit_root_test_Stationary@teststat[1]
DF_stationary_CriticalValues = unit_root_test_Stationary@cval[1,]
summary(unit_root_test_Stationary)
# Now, let's look at what we find:
print(c("Manual Dickey Fuller Test Statistic for Stationary Series:", round(DF_stationary_teststat_manual,2)))
print(c("Automatic Dickey Fuller Test Statistic for Stationary Series:", round(DF_stationary_teststat_automatic,2)))
# Now, let's look at the Critical Values from the ur.df() function. 
# As we showed above, the t-statistics are not normal, so we need to use special
# Critical Values to test:
print("Dickey-Fuller Intercept-Only Critical Values:")
print(DF_stationary_CriticalValues)
# Can we reject the null hypothesis? What can you conclude from thi
### Section 2.2: QLR Test
D <- 1*(time(totalpay_xts) > time(totalpay_xts)[30]) 
print(D)

# Now, we add this indicator function D to our regression model.
#   Recall the function for the Chow Test: we want to include D as a regressor
#   by itself, as well as a term for D * Y_(t-1).
Chow_test_model = lm(totalpay_xts ~ lag(totalpay_xts,1) + D + (D*lag(totalpay_xts,1)))
coeftest(Chow_test_model)
#   As before, we are using an F-test and using homoskedasticity only errors.
linearHypothesis(Chow_test_model, c("D=0", "lag(totalpay_xts, 1):D=0"), test="F",singular.ok = TRUE,white.adjust = FALSE)

#tau_zero =round(0.15*num_periods,digits=1)
#tau_one =round((1-0.15)*num_periods,digits=1)
#num_tests <- tau_one - tau_zero + 1
tau <- seq(1, 83)

chow_test_statistics <- array(c(0), dim = 83)
for (i in 1:83) {
  D <- 1*(time(totalpay_xts) > time(totalpay_xts)[tau[i]])  
  Chow_test_model = lm(totalpay_xts ~ lag(totalpay_xts,i) + D + (D*lag(totalpay_xts,i)))
  coeftest(Chow_test_model)
  chow_test = linearHypothesis(Chow_test_model, c("D=0", "lag(totalpay_xts, 1):D=0"), test="F", white.adjust = FALSE,singular.ok = TRUE)
  chow_test_statistics[i] = chow_test$F[2]
}
data.frame("Level" = tau, "F-stat" = chow_test_statistics)

QLR_test_stat = max(chow_test_statistics)
tau_hat <- tau[which.max(chow_test_statistics)]
print(QLR_test_stat)
print(tau_hat)
### Section 2.3: Problems with Breaks

cat("Indicator Variable model, Mean beta_0_hat : ", mean(beta_0_hat), "\n",
    "Indicator Variable model, Mean beta_1_hat : ", mean(beta_1_hat), "\n",
    "Indicator Variable model, Mean delta_0_hat : ", mean(delta0_hat), "\n",
    "Indicator Variable model, Mean delta_1_hat : ", mean(delta1_hat), "\n",
    "Naive Model Without Indicator Variable, Mean beta_0_hat : ", mean(beta_0_hat_naive), "\n", 
    "Naive Model Without Indicator Variable, Mean beta_1_hat : ", mean(beta_1_hat_naive), "\n")





# Using the lag notation above, compute the 2nd and 3rd order AR models:
# This is the same as lm(Y_t ~ Y_(t-1) + Y_(t-2)) which is equivalent
# to estimating Y_t = \beta_0 + \beta_1 Y_(t-1) + \beta_2 Y_(t-2)
GDPGR_AR2_lm <- lm(GDPGR_1990_2005 ~ lag(GDPGR_1990_2005,1) + lag(GDPGR_1990_2005,2))
coeftest(GDPGR_AR2_lm)
summary(GDPGR_AR2_lm)$r.squared

GDPGR_AR3_lm <- lm(GDPGR_1990_2005 ~ lag(GDPGR_1990_2005,1) + lag(GDPGR_1990_2005,2)  + lag(GDPGR_1990_2005,3))
coeftest(GDPGR_AR3_lm)
summary(GDPGR_AR3_lm)$r.squared

# Now, we can compute this with a single step without even having to write out
# the individual lags using the ar.ols() module:
arpackage_model <- ar.ols(GDPGR_1990_2005, 
                          demean = FALSE,
                          aic = FALSE,
                          order.max = 3,
                          intercept = TRUE)
print(arpackage_model)
arpackage_model_coef = as.numeric(arpackage_model$ar[1])
arpackage_model_coef_se = as.numeric(arpackage_model$asy.se.coef$ar[2])
arpackage_model_coef_tstat = arpackage_model_coef / arpackage_model_coef_se
print(arpackage_model_coef_tstat)
# This package is a bit of a "black-box" because it automatically adjusts for 
# some more advanced setting we haven't used set, so we will continue manually
# setting up our AR regressions. You might find this more convenient to use in
# the future, however.

### Section 4: Forecasting & Forecast Error
# the terms in the square brackets in GDPGrowth_xts["2005"][4] are used to 
# select a particular observation - here we are choosing Q4 of 2005.
# Recall that with AR1 model the forecast of Y_t is:
# Y_t = \beta_0_AR1 + \beta_1_AR1 * Y_(t-1)
# Here, to forecast we want to predict the first observation outside of our data
# used to estimate the model, which ends in 2005, Q4. So here, T+1 = 2006 Q1,
# so we use the observed Y_T from 2005 Q4 and the estimated coefficients to 
# forecast:
beta_0_AR1 <- coef(GDPGR_AR1_lm)[1]
beta_1_AR1 <- coef(GDPGR_AR1_lm)[2]
forecasted_value_AR1 = beta_0_AR1 + (beta_1_AR1 %*% GDPGrowth_xts["2005"][4])
# Now, let's see how accurate this is:
forecast_error_AR1 = forecasted_value_AR1 - GDPGrowth_xts["2006"][1]
cat("Forecasted Value AR(1):", forecasted_value_AR1, "\n")
cat("Actual Value AR(1):", GDPGrowth_xts["2006"][1], "\n")
cat("Forecast Error AR(1):", forecast_error_AR1, "\n")

# With AR2, the forecast of Y_t is:
# Y_t = \beta_0_AR2 + \beta_1_AR2 * Y_(t-1)  + \beta_2_AR2 * Y_(t-2)
beta_0_AR2 <- coef(GDPGR_AR2_lm)[1]
beta_1_AR2 <- coef(GDPGR_AR2_lm)[2]
beta_2_AR2 <- coef(GDPGR_AR2_lm)[3]
forecasted_value_GDPGR_AR2_lm = beta_0_AR2 + (beta_1_AR2 %*% GDPGrowth_xts["2005"][4])  + (beta_2_AR2 %*% GDPGrowth_xts["2005"][3])
forecast_error_GDPGR_AR2_lm = forecasted_value_GDPGR_AR2_lm - GDPGrowth_xts["2006"][1]
cat("Forecasted Value AR(2):", forecasted_value_GDPGR_AR2_lm, "\n")
cat("Actual Value AR(2):", GDPGrowth_xts["2006"][1], "\n")
cat("Forecast Error AR(2):", forecast_error_GDPGR_AR2_lm, "\n")

# With AR3, the forecast of Y_t is:
# Y_t = \beta_0_AR3 + \beta_1_AR3 * Y_(t-1)  + \beta_2_AR3 * Y_(t-2) + \beta_3_AR3 * Y_(t-3)
beta_0_AR3 <- coef(GDPGR_AR3_lm)[1]
beta_1_AR3 <- coef(GDPGR_AR3_lm)[2]
beta_2_AR3 <- coef(GDPGR_AR3_lm)[3]
beta_3_AR3 <- coef(GDPGR_AR3_lm)[4]
forecasted_value_GDPGR_AR3_lm = beta_0_AR3 + (beta_1_AR3 %*% GDPGrowth_xts["2005"][4])  + (beta_2_AR3 %*% GDPGrowth_xts["2005"][3])  + (beta_3_AR3 %*% GDPGrowth_xts["2005"][2])
forecast_error_GDPGR_AR3_lm = forecasted_value_GDPGR_AR3_lm - GDPGrowth_xts["2006"][1]
cat("Forecasted Value AR(3):", forecasted_value_GDPGR_AR3_lm, "\n")
cat("Actual Value AR(3):", GDPGrowth_xts["2006"][1], "\n")
cat("Forecast Error AR(3):", forecast_error_GDPGR_AR3_lm, "\n")



GDPGrowth_xts <- xts(400 * log(GDP_xts/lag(GDP_xts)))
GDPGR_1990_2005 <- GDPGrowth_xts["1990::2005"]






# Now, add the unemployment rate:
#UnempRate_UKnational.xls
UK_UnemploymentRate <- read_excel(path = "UnempRate_UKnational.xls")    
# Load the Excel spreadsheet of Unemployment Rate in UK
UK_UnemploymentRate$Date <- as.yearqtr(UK_UnemploymentRate$Title, format = "%Y Q%q")
UK_UnemploymentRate_xts <- xts(UK_UnemploymentRate$Unemploymentrate, UK_UnemploymentRate$Date) # Convert this into an xts object
UK_UnemploymentRate_2000_2020 <- UK_UnemploymentRate_xts["2000::2020"]  
#estimate ADL(1,1)model


# Now modify this to estimate the ADL(p,p) model:
totalpayGR_ADL11_lm <- lm(totalpayGR_2000_2020 ~ lag(totalpayGR_2000_2020 ,1) + lag(UK_UnemploymentRate_2000_2020,1) )
coeftest(totalpayGR_ADL11_lm)
summary(totalpayGR_ADL11_lm)$r.squared
for (p in 1:4) {
  print(IC(lm(totalpay_2000_2020 ~ lag(totalpay_2000_2020,1:p) + lag(UK_UnemploymentRate_2000_2020,1:p)))) # Now, we can do the same thing but we add the 1:p lag for unemployment rate as well
}


# use a Granger causality test 
SSR_unrestricted <- sum(totalpayGR_ADL22_lm$residuals^2) # Save out the SSR of this unrestricted model for use in the F-statistic
q = c(2) # Here we are looking at a test with TWO restrictions, so q=2
df <- totalpayGR_ADL22_lm$df.residual # Save out the degrees of freedom (df = n - q - 1) for use in the F-stat computation.

# Manual F-Test for unemployment:
# First, we need to estimate the restricted model, without the two terms for unemployment.
totalpayGR_ADL22_restrict_unemployment <- lm(totalpay_2000_2020 ~ lag(totalpay_2000_2020,1) + lag(totalpay_2000_2020,2))    # Take the exact line of code from above and remove the two terms for the Unemployment Rate.
SSR_restricted <- sum(totalpayGR_ADL22_restrict_unemployment$residuals^2) # Save out the SSR of this restricted model for use in the F-statistic

# Here we just write out the formula we use for the F-statistc based on the SSR.
Fstatistic = ((SSR_restricted - SSR_unrestricted) / 2)/(SSR_unrestricted / df) 
cat("Manually Computed F-statistic", Fstatistic, "\n")

# Now, use an F-Test to check whether the two lags of Unemployment are jointly significant:
# Recall: Our F-Test checks the joint significance of the tow Unemployment Rate terms,
#     which means that we are testing the hypothesis that the coefficients for BOTH
#     these terms are zero. So, our two constraints representing this null hypothesis are:
#       lag(UK_UnemploymentRate_1985_2015, 1)=0
#       lag(UK_UnemploymentRate_1985_2015, 2)=0
# So we put these two constraints together in a LIST or VECTOR
# Recall that this is what the c() or COMBINE function does: we want to test BOTH these constraints so
# we COMBINE them into one joint constraint:  c("lag(UK_UnemploymentRate_1985_2015, 1)=0", "lag(UK_UnemploymentRate_1985_2015, 2)=0")
# So the linearHypothesis() function works like this:
# linearHypothesis(MODEL WE ARE TESTING, LIST OF CONSTRAINTS, test="F", white.adjust = FALSE)
# where test="F" so we are using an F-test
# and white.adjust = FALSE so we just use regular errors
linearHypothesis(totalpayGR_ADL22_lm, c("lag(UK_UnemploymentRate_2000_2020, 1)=0", "lag(UK_UnemploymentRate_2000_2020, 2)=0"), test ="F", white.adjust=FALSE)

#cofficient estimate from ADL(1,1) to ADL(p,p)
View(cbind(lag(totalpayGR_2000_2020 ,1),lag(totalpayGR_2000_2020 ,2)))
totalpayGR_ADL22_lm <- lm(totalpayGR_2000_2020 ~ lag(totalpayGR_2000_2020 ,1)+lag(totalpayGR_2000_2020 ,2) + lag(UK_UnemploymentRate_2000_2020,1)+ lag(UK_UnemploymentRate_2000_2020,2))
totalpayGR_ADL33_lm <- lm(totalpayGR_2000_2020 ~ lag(totalpayGR_2000_2020 ,1)+lag(totalpayGR_2000_2020 ,2) +lag(totalpayGR_2000_2020 ,3)+ lag(UK_UnemploymentRate_2000_2020,1)+ lag(UK_UnemploymentRate_2000_2020,2)+ lag(UK_UnemploymentRate_2000_2020,3))
totalpayGR_ADL44_lm <- lm(totalpayGR_2000_2020 ~ lag(totalpayGR_2000_2020 ,1)+lag(totalpayGR_2000_2020 ,2) +lag(totalpayGR_2000_2020 ,3) +lag(totalpayGR_2000_2020 ,4)+lag(UK_UnemploymentRate_2000_2020,1)+ lag(UK_UnemploymentRate_2000_2020,2)+ lag(UK_UnemploymentRate_2000_2020,3)+lag(UK_UnemploymentRate_2000_2020,4))
coeftest(totalpayGR_ADL11_lm)
coeftest(totalpayGR_ADL22_lm)
coeftest(totalpayGR_ADL33_lm)
coeftest(totalpayGR_ADL44_lm)

#check out of sample Forecast performance
# We record the length of this list of dates and store this as P, the number
# of observations we hold out as our pseudo-out-of sample observations.
end_dates <- seq(2000.00, 2020.50, 0.25)   
P=length(end_dates)
print(P)
### Section 1.3: Set up empty arrays where we store our outcomes:

# initialize vector where we will store forecasts, errors, the true outcomes and
# the standard errors of our regressions.
# As usual, these are arrays of zeros of length P - one for each out of sample observation
forecasts <- array(c(0),dim = c(P))
true_outcomes <- array(c(0),dim = c(P))
PsOOSF_errors <- array(c(0),dim = c(P))
SER <- array(c(0),dim = c(P))
for (i in 1:P-1) {
  EndDate_YearQuarter = as.yearqtr(end_dates[83])  # First, we convert our data back into the Year-Quarter format we are used to.
  # Now, run our regression on the limited sample.
  # First, we limit our sample to observations whose index (i.e the DATE of that observation) comes before the designated EndDate_YearQuarter
  totalpayGrowth_estimating_sample <- totalpayGrowth_xts[index(totalpayGrowth_xts) <  EndDate_YearQuarter] # Now we use this to limit our estimating sample
  ##### COMPLETE THIS MODEL below BY ADDING THE MISSING TERM from and AR2!
  AR2_lm <- lm(totalpayGrowth_estimating_sample ~ lag(totalpayGrowth_estimating_sample,1) + lag(totalpayGrowth_estimating_sample,2)) # Run an AR2 on the restricted estimating sample
  SER[i] <- summary(AR2_lm)$sigma
  # Save Coefficients from the AR2 above:
  beta_0_hat = AR2_lm$coefficients[1] # The first coefficient is the intercept, etc.
  beta_1_hat = AR2_lm$coefficients[2]
  beta_2_hat = AR2_lm$coefficients[3]
  c(beta_0_hat,beta_1_hat,beta_2_hat)
  true_outcome <- totalpayGrowth_xts[EndDate_YearQuarter]    # Find the true value of the outcome:
  # compute forecast
  
  pseudo_forecast <- (beta_0_hat + (beta_1_hat %*% totalpayGrowth_xts[EndDate_YearQuarter - 0.25]) 
                      + (beta_2_hat %*% totalpayGrowth_xts[EndDate_YearQuarter - 0.50]))
  
  pseudo_error <- true_outcome  - pseudo_forecast    # Compute the error:
  true_outcomes[i] <- true_outcome    # Store the true outcome, the forecast and the error in the arrays we initialized earlier
  forecasts[i] <- pseudo_forecast
  PsOOSF_errors[i] <- pseudo_error
}
###  Section 1.5: Analysis of PsOOOF Errors
SER_within_sample <- SER[1] # Here we take the first value from our array of SER's. This is the original SER of our first within-sample regression.
Estimated_RMSFE_OutOfSample <- sd(PsOOSF_errors) # Here we estimate the RMSFE by using the standard error of our pseudo out of sample forecast errors

cat("Within-Sample Errors: ", SER_within_sample, "\n")
cat("Estimated RMSFE: ", Estimated_RMSFE_OutOfSample, "\n")

#### Use the mean(), sd() and sqrt() functions to fill in the formula for the t-statistic for our t-test:
 t_stat_PsOOSF_errors = mean(PsOOSF_errors)*sqrt(P)/sd(PsOOSF_errors)
  print(t_stat_PsOOSF_errors)

t.test(PsOOSF_errors)


#Dynamic Causal Effects
#
### SECTION 3 -- Dynamic Causal Effects w. Strict Exogeneity
library(urca)
library(orcutt)

# Now we are going to look at the two ways of estimating a model where we can
# assume strict exogeneity.
# Our starting point for this is a DL2 model, with X_t and X_(t-1) as regressors

### ============================================================================
# This is a set-up section: as usual, it is good to try and understand, but your
# priority should be the analysis section:
N <- 500
y_AutoCorrelatedErrors_DL2 <- array(c(0),dim = c(N))
d <- as.Date(1:c(N-1))

# As before, we simulate X from a normal distribution, 
# and u with an arima distribution with phi1 = 0.5
X <- runif(N, 1, 10)
u <- arima.sim(n = N , model = list(ar = 0.5))

# Our dynamic multipliers here are 0.1 and 0.25.
beta1 <- 0.1
beta2 <- 0.25

y_AutoCorrelatedErrors_DL2[1] = (beta1 * X[1]) + u[1]
for (t in 2:N) {
  y_AutoCorrelatedErrors_DL2[t] = (beta1 * X[t]) + (beta2 * X[t-1]) + u[t]
}
# We skip the first period (since we don't have the lagged values)
# and set up xts objects with our simulated time series and regressor:
y_AutoCorrelatedErrors_DL2_xts <- xts(y_AutoCorrelatedErrors_DL2[-1], d)
X_xts <- xts(X[-1],d)
# Section 2.1: Set-Up 
# As before, I am clearly separating out the set-up section 
# where I simulate time series, etc. This is good to understand, but less 
# important than the analysis section that comes below.

# First, we will simulate a time series with serially correlated errors:
set.seed(250) ## set seed to replicate randomization

N <- 100
y_AutoCorrelatedErrors <- array(c(0),dim = c(N)) # Set up a vector where we store our simulated Y
# Here we use the arima package to simulate our errors, since this is not as simple as drawing from a random normal.
# Here the  model = list(ar = 0.5) sets up the parameters for the error correlation we want to simulate
# Here, the key part is "ar = 0.5" which means we want autocorrelated errors where the 
#   autocorrelation coefficient is 0.5. This is similar to setting phi1 = 0.5 in
#   the model we use when we set up our GLS quasi-differenced variables.
u <- arima.sim(n = N , model = list(ar = 0.5)) 
d <- as.Date(1:N)

# The beta1 for this time series (the coefficient on X_t) is 0.5
# In order to usethe simple formula we derived in lecture we will just be using
# a Distributed Lag model with the contemporaneous value of X_t:
# Y_t = \beta_0 + \beta_1 X_t + u_t
beta1 <- 0.5

# Here we simulate our X's: they are drawn randomly from a uniform distribution
# from 1 to 10.
X <- runif(N, 1, 10)

# Now we simulate our time series: at each time period, we set Y_t = beta1 X_t + u_t
# where u_t comes from the vector of autocorrelated errors we simulated above.
for (t in 1:N) {
  y_AutoCorrelatedErrors[t] = (beta1 * X[t]) + u[t]
}

# Now we convert this into a time series using the vector d of simulated dates
y_AutoCorrelatedErrors_xts <- xts(y_AutoCorrelatedErrors, d)
# We also set up a time series of X_t:
X_xts <- xts(X,d)

#### ===========================================================================

# Section 2.2: Computing HAC Errors Manually
# First, we estimate our distributed lag model as is:
DL1_lm <- lm(y_AutoCorrelatedErrors_xts ~ X_xts)

# Now we need to turn our residuals into a time series so we can measure the 
# autocorrelation:
OLS_u_hat <- DL1_lm$residuals
# We will also compute the v term we used in lecture slides:
v <- (X - mean(X)) * OLS_u_hat

# Let's look at these residuals - do they look autocorrelated?
plot(as.zoo(OLS_u_hat), main ="DL1 Model Residuals", ylab="u_t", xlab="t")
# Now let's directly plot the autocorrelations - each item on the X-axis
# records the lag length and the Y-axis tells us the autocorrelation at that lag:
acf(OLS_u_hat, main="Residual Autocorrelations")

X_bar = mean(X)
# This isn't exactly the formula for var(beta1) we use in lecture slides:
# it's a heteroskedasticity-robust variance estimator:
var_beta_hat <- 1/N * (1/(N-2) * sum((X - mean(X))^2 * OLS_u_hat^2) ) /
  (1/N * sum((X - mean(X))^2))^2

# We also compute the autocorrelations of v_t to use in computing f_T_hat:
rho_list <- acf(v, plot=FALSE)$acf[-1]

# rule of thumb truncation parameter
m <- floor(0.75 * N^(1/3))

# This sum is a bit confusing, so let's break it down.
# First, we focus on the sum() term -- the first part of 1 + (2* sum())
# should be clearly identifiable in the function
# We use the sum() function, which just takes the sum of a list of outcomes.
# We use sapply() to set up our usual sum over values of a function as in lecture notes.
# The first term of sapply() is the limits on which we want to apply the function,
#   so here we use sapply(1:(m-1), ) to set up a sum over j=1 to j=m-1
#   next we set up the "inside" of this sum, the function of j we want to compute at each value of j
#   that's why the second term of sapply() is function(j) ((m-j)/m)*rho_j_list[j]
#   We tell R that this is a function over j, and we want to compute ((m-j)/m)*rho_list[j]
f_hat_T = 1 + (2 * sum(sapply(1:(m-1), function(j) ((m-j)/m)*rho_list[j])))

# unadjusted standard error. If var_beta_hat is the variance of beta_1, what is the 
#     standard error of beta_1? Recall the sqrt() function takes a square root.
unadjusted_StdErr <- sqrt(var_beta_hat)
cat("Unadjusted Standard Error: ", unadjusted_StdErr, "\n")

# compute Newey-West HAC estimate of the standard error
# Now we have the unadjusted standard error, and we have estimated f_hat_T, 
#     how do we get our adjusted NeweyWest standard error?
NeweyWest_manual_StdErr <- sqrt(var_beta_hat * f_hat_T)
cat("Manual Newey-West Standard Error: ", NeweyWest_manual_StdErr, "\n")

# Using the automatic NeweyWest() function:
NW_VCOV <- NeweyWest(lm(y_AutoCorrelatedErrors_xts ~ X_xts),
                     lag = m - 1, prewhite = F,
                     adjust = T)
# compute standard error
NeweyWest_auto_StdErr <- sqrt(diag(NW_VCOV))[2]
cat("Automatic Newey-West Standard Error: ", NeweyWest_auto_StdErr, "\n")

DL1_lm <- lm(y_AutoCorrelatedErrors_xts ~ X_xts)
coeftest(DL1_lm, vcov=NW_VCOV)


### SECTION 3 -- Dynamic Causal Effects w. Strict Exogeneity

# Now we are going to look at the two ways of estimating a model where we can
# assume strict exogeneity.
# Our starting point for this is a DL2 model, with X_t and X_(t-1) as regressors



### ============================================================================

### Section 3.2: Analysis of Model Residuals
# Now, on to the analysis section.
# First, let's look at our residuals from a simple estimation of this DL2 model
# has serially correlated errors:
DL2_lm <- lm(totalpay_xts ~ lag(UK_UnemploymentRate_2000_2020,0)+lag(UK_UnemploymentRate_2000_2020,1)+ lag(UK_UnemploymentRate_2000_2020,2))
OLS_u_hat <- DL2_lm$residuals
acf(OLS_u_hat)

### Section 3.3: ADL without Autocorrelated Errors
# Now, we estimate the ADL(2,1) representation of the distributed lag model
adl21_lm <- lm(totalpay_xts ~ lag(totalpay_xts) + UK_UnemploymentRate_2000_2020 + lag(UK_UnemploymentRate_2000_2020, 1:2))
 #plot the sample autocorrelations of residuals from this transformed model
#   do they appear to be autocorrelated?
acf(adl21_lm$residuals, main = "ADL21: Autocorrelations")

# We now recover the estimated coefficients we need to compute the first two dynamic multipliers
delta_0_hat <- adl21_lm$coefficients[3]
delta_1_hat <- adl21_lm$coefficients[4]
phi_1_hat <- adl21_lm$coefficients[2]

# Using the formula on slide 15 of Lecture 12 or in the Homework Instructions,
#   compute the first two dynamic multipliers
c("hat_beta_1" = as.numeric(delta_0_hat))
c("hat_beta_2" = delta_1_hat + (delta_1_hat %*% phi_1_hat))

### infeasible GLS:
### We know phi_1 = 0.5, so let's proceed with the infeasible GLS where we use this
# true phi, even though we don't know this in practice:

i_Y_tilde_xts <- totalpay_xts - (0.5* lag(totalpay_xts))
i_X_tilde_xts <- UK_UnemploymentRate_2000_2020 - (0.5* lag(UK_UnemploymentRate_2000_2020))
i_Xlag1_tilde_xts <- lag(UK_UnemploymentRate_2000_2020) - (0.5* lag(UK_UnemploymentRate_2000_2020,2))

# Using these quasi-differenced variables, estimate the GLS DL2 model:
iGLS_DL2_lm <- lm(i_Y_tilde_xts ~ i_X_tilde_xts + i_Xlag1_tilde_xts)
coeftest(iGLS_DL2_lm)

### feasible GLS:
# For the feasible GLS, estimate phi_1.
# First, we estimate the normal DL2 model and un the regression on residuals
# to estimate phi_1
DL2_lm <- lm(totalpay_xts ~ UK_UnemploymentRate_2000_2020 + lag(UK_UnemploymentRate_2000_2020,1))
# Store residuals:
DL2_residuals <- DL2_lm$residuals
# Regress residuals on lag of residuals:
residuals_lm <- lm(DL2_residuals ~ lag(DL2_residuals))
# Store the coefficient on the lag of residuals -- this is our estimated phi_1
coeftest(residuals_lm)
phi1_hat <- residuals_lm$coefficients[2]
cat("Phi_1_hat from manual Cochrane-Orcutt: ", phi1_hat, "\n")

# Now, we generate our quasi-diff variables using the estimated phi1:
Y_tilde_xts <- totalpay_xts - (phi1_hat* lag(totalpay_xts))
X_tilde_xts <- UK_UnemploymentRate_2000_2020 - (phi1_hat* lag(UK_UnemploymentRate_2000_2020))
Xlag1_tilde_xts <- lag(UK_UnemploymentRate_2000_2020) - (phi1_hat* lag(UK_UnemploymentRate_2000_2020,2))

# Using these quasi-differenced variables, we run the GLS model:
fGLS_DL_lm <- lm(Y_tilde_xts ~ X_tilde_xts + Xlag1_tilde_xts)
# we can recover the estimates from this, which are our dynamic multipliers:
coeftest(fGLS_DL_lm)
acf(fGLS_DL_lm$residuals, main = "GLS Residuals: Autocorrelations")

# We can compare this to the results from the iterated cochrane-orcutt method:
totalpay1<-data.frame(totalpay_2000_2020)
UK1<-data.frame(UK_UnemploymentRate_2000_2020)
y_cochrane = c(totalpay1[-1])
x_cochrane = c(UK1[-1])
xl1_cochrane = c(UK1[-83])
iterated_cochrane_orcutt_model <- cochrane.orcutt(lm(y_cochrane ~ x_cochrane + xl1_cochrane))
summary(iterated_cochrane_orcutt_model)
cat("Phi_1_hat from Iterated Cochrane-Orcutt: ", iterated_cochrane_orcutt_model$rh
    
    

#cointegration
library(lmtest)
library(tseries)
sr.reg=lm(totalpay_2000_2020~UK_UnemploymentRate_2000_2020)
error=residuals(sr.reg)
adf.test(error)
summary(sr.reg)
dwtest(sr.reg)
#Volatility Clustering Analysis
library(fGarch) 
fit=arima(totalpayGrowth_xts,order=c(1,1,1))	
for(i in 1:6){
  print(Box.test(fit$residual,type="Ljung-Box",lag=i))
}	
RE=fit$residuals	
plot(RE)	#绘残差图#
for(i in 1:6)print(ArchTest(RE,lag=i))	#检验是否存在异方差#
dwtest(RE~1)	#检验是否存在短期自相关#
P=NULL		#定义空变量用于存储p值#
Q=NULL		#定义空变量用于存储q值#
AIC=NULL	#定义空变量用于存储aic值#
RE=na.omit(RE)
fit1=garch(RE,order=c(1,1))
for(p in 0:2){
  for(q in 1:2){
    fit=garch(RE,order=c(p,q));
    aic=AIC(fit);
    P=c(P,p);
    Q=c(Q,q);
    AIC=c(AIC,aic);
  }
}
for(p in 1:2){
  fit=garch(RE,order=c(p,0));
  aic=AIC(fit);
  P=c(P,p);
  Q=c(Q,0);
  AIC=c(AIC,aic);	
}
L=cbind(P,Q,AIC)
ORDER=order(AIC)
k=ORDER[1]
L[k,]		#显示aic最小的行#
L	#显示全部p,q,aic#
p=L[k,1]		#将最小aic的p值提取#
q=L[k,2]		#将最小aic的q值提取#
print(p,q)
print(q)
fit.garch=garch(RE,order=c(p,q))		#建立aic最小模型#
fit.pred<-predict(fit.garch)	#对拟合期进行预测#
plot(fit.pred)		#绘制残差拟合期预测图#
plot(RE)		#绘制原始序列残差图#
lines(fit.pred[,1],col=2)		#绘制残差预测值各点的置信区间上限#
lines(fit.pred[,2],col=2) 		#绘制残差预测值各点的置信区间下限#
abline(h=1.96*sd(RE),col=4)	#绘制RE序列均值95%置信区间上限#
abline(h=-1.96*sd(RE),col=4)	#绘制RE序列均值95%置信区间下限#

