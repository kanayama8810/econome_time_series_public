library(tidyverse)
library(zoo)


# load data
dat <- readxl::read_xlsx("Keisemi_ch3/data_VAR3.xlsx")
colnames(dat) <- c("YQ", "RSR", "cpi", "gapboj")

# transform into quarterly data
dat <- mutate(dat, 
              YQ = zoo::as.yearqtr(YQ))



#### AR model: GDP gap ####
ts_gdpgap <- ts(data = dat$gapboj, start = c(1972, 3), frequency = 4)  # 1-dim. quarterly data
ts_gdpgap <- ts_gdpgap[is.na(ts_gdpgap) == FALSE]


## AR(1) without intercept
res_ar1 <- arima(ts_gdpgap, c(1, 0, 0), include.mean = FALSE)
res_ar1

## estimated parameters
coef_ar1 <- res_ar1$coef  # coefficients
sigma_ar1 <- res_ar1$sigma2  # variance of innovation


#### recursive methods ####
num_iter <- 1000  # num. iteration
num_obs <- length(ts_gdpgap)

coefs_gdpgap <- numeric(num_iter)

# create a group of residuals
vEps <- res_ar1$residuals
vEps_group <- vEps - mean(vEps)



for (n in 1:num_iter) {
  # sampling
  vEps_hat <- sample(vEps_group, size = num_obs, replace = TRUE)
  
  # construct pseudo data
  ts_gdpgap_pseudo <- numeric(num_obs)
  ts_gdpgap_pseudo[1] <- mean(ts_gdpgap)  # fix init
  
  for (i in 2:num_obs) {
    ts_gdpgap_pseudo[i] <- coef_ar1 * ts_gdpgap_pseudo[i-1] + vEps_hat[i]
  }
  
  # estimate AR(1) with pseudo data
  res_ar1_pseudo <- arima(ts_gdpgap_pseudo, c(1, 0, 0), include.mean = FALSE)
  coefs_gdpgap[n] <- coef(res_ar1_pseudo)
}

quantile(coefs_gdpgap, probs = 0.975)
quantile(coefs_gdpgap, probs = 0.025)












