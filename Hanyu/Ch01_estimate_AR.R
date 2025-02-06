# preparation --------------------------------------------------------
library(tidyverse)
library(zoo)

dat <- readxl::read_excel("Keisemi_ch1/data_VAR1.xlsx", col_types = c("text", "guess", "guess"))
colnames(dat) <- c("year", "gapcao", "gapboj")

glimpse(dat)


# autoregression ----------------------------------------------------------
TL <- 20

## AR(1)
res_ar1 <- lm(gapcao ~ lag(gapcao), data = dat)
sum_ar1 <- summary(res_ar1)
coef(res_ar1)
sigma_ar1 <- sum(sum_ar1$residuals^2) / length(sum_ar1$residuals)  # variance

# inpulse response
ir_ar1 <- numeric(TL)
ir_ar1[1] <- coef(res_ar1)[2]
coef(res_ar2)
for (i in 1:(TL-1)) {
  ir_ar1[i+1] <- coef(res_ar1)[2]*ir_ar1[i]
}

plot(1:TL, ir_ar1, type="l")


## AR(2)
res_ar2 <- lm(gapcao ~ lag(gapcao) + lag(gapcao, 2), data = dat)
sum_ar2 <- summary(res_ar2)
sigma_ar2 <- sum(sum_ar2$residuals^2) / length(sum_ar2$residuals)  # variance

# inpulse response
ir_ar2 <- numeric(TL)
ir_ar2[1] <- coef(res_ar2)[2]
ir_ar2[2] <- coef(res_ar2)[2]*ir_ar2[1] + coef(res_ar2)[3]

for (i in 1:(TL-2)) {
  ir_ar2[i+2] <- coef(res_ar2)[2:3]%*%ir_ar2[(i+1):i]
}

plot(1:TL, ir_ar2, type="l")

irf()
## AR(3)
res_ar3 <- lm(gapcao ~ lag(gapcao) + lag(gapcao, 2) + lag(gapcao, 3), data = dat)
sum_ar3 <- summary(res_ar3)
sigma_ar3 <- sum(sum_ar3$residuals^2) / length(sum_ar3$residuals)  # variance

# inpulse response
ir_ar3 <- numeric(TL)
ir_ar3[1] <- coef(res_ar3)[2]
ir_ar3[2] <- coef(res_ar3)[2]*ir_ar3[1] + coef(res_ar3)[3]
ir_ar3[3] <- coef(res_ar3)[2]*ir_ar3[2] + coef(res_ar3)[3]*ir_ar3[1] + coef(res_ar3)[4]

for (i in 1:(TL-3)) {
  ir_ar3[i+3] <- coef(res_ar3)[2:4]%*%ir_ar3[(i+2):i]
}

plot(1:TL, ir_ar3, type="l", xlabel = "time")

# AIC, BIC
## AR(p)
AIC <- numeric(10)
BIC <- numeric(10)

for(p in 1:10){
  dat_lags <- select(dat, gapcao)
  for (i in 1:p) {
    dat_lags <- cbind(dat_lags, lag(dat$gapcao, i))
  }
  colnames(dat_lags) <- paste0("gapcao", 0:p)
  res_ar <- lm(gapcao0 ~ ., data = dat_lags)
  sum_ar <- summary(res_ar)
  TT <- length(sum_ar$residuals)
  sigma_ar <- sum(sum_ar$residuals^2) / TT  # variance
  
  AIC[p] <- log(sigma_ar) + (p+1)*2/TT
  BIC[p] <- log(sigma_ar) + (p+1)*log(TT)/TT
}
print(AIC)
print(BIC)

which.min(AIC)  # 3
which.min(BIC)  # 1

