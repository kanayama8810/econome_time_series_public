# load data
rawdat <- readxl::read_xlsx("rawdata/GDPgap_naikakufu.xlsx", skip = 6, col_names = FALSE)[, c(1, 2, 3)]
colnames(rawdat) <- c("year", "Quarter", "GDPGap")

# formatting
dat <- rawdat |> 
  mutate(Quarter = str_replace_all(Quarter, c('Ⅰ' = "1", 'Ⅱ' = "2", 'Ⅲ' = "3", 'Ⅳ' = "4"))) |> 
  mutate(year = if_else(is.na(year), lag(year), year)) |> 
  mutate(year = if_else(is.na(year), lag(year, 2), year)) |> 
  mutate(YQ = as.yearqtr(paste0(year, "_", Quarter), format="%Y_%q"))  # (year, quarter) convert into YQ

# plot (GDP gap)
dat |> 
  ggplot(aes(x=YQ)) + 
  geom_line(aes(y=GDPGap))

# output dataset
write_csv(dat, "cleaned/GDPgap_cao_cleaned.xlsx")
