library(dplyr)
library(readr)

# load data
dat_raw <- readxl::read_xlsx("Keisemi_ch3/data_VAR3.xlsx")
colnames(dat_raw) <- c("YQ", "RSR", "cpi", "gapboj")

# transform into quarterly data
dat <- mutate(dat_raw, 
              YQ = zoo::as.yearqtr(YQ))

write_csv(dat, "cleaned/data_VAR3.csv")
