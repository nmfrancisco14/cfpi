
# setup -------------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(daclakeapi)
library(lubridate)
library(scales)
library(glue)
library(zoo)


# cpfi data ---------------------------------------------------------------



cpfi_data <-  read.xlsx("www/fpidata.xlsx",
                        detectDates = T,
                        sheet="values")

glimpse(cpfi_data)

#farmgate prices
fgate_data <-
get_api_data("farmgate_prices") |>
  mutate(date = mdy(data.monthyr),
         fgate = as.numeric(data.farmgate)) |>
  select(date, fgate) |>
  arrange(date)


#for retail prices
ret_data <-
  get_api_data("retail_prices") |>
  mutate(date = mdy(data.monthyr),
         retail = as.numeric(data.rmr)) |>
  select(date, retail) |>
  arrange(date)


#for oil/fuel

fuel <-
read.xlsx("www/fuelprices.xlsx",
          sheet=3,
          detectDates = T) |>
  mutate(mmS = month(Start_Week),
         mmE = month(End_Week),
         yyS = year(Start_Week),
         yyE = year(End_Week),
         date =ymd(paste(yyS,mmS,"1",sep="-"))) |>
  arrange(desc(date)) |>
  group_by(date) |>
  summarise(across(Min_Price_Range:Mean_Price,
                   \(x) (mean(x,na.rm=T)))
  ) |>
  ungroup() |>
  arrange(desc(date))


# for UREA (world fert price)
urea <-
read.xlsx("www/fertilizerWorld.xlsx",
          sheet=2,
          startRow = 7
          ) |>
  mutate(date = ym(str_replace(X1,"M","-"))) |>
  arrange(desc(date))

# rainfall chirps

rain <-
  read_csv("www/rainfall_chirps.csv") |>
    mutate(date = ym(date))


imports <-
get_api_data("imports") |>
  mutate(date = mdy(monthyr),
         imports = as.numeric(imports_nrp)) |>
  select(date,imports)
#combined


viet_global <-
get_api_data("vfa_fob_prices") |>
  filter(data.type == "Vietnam 25%") |>
  mutate(date = mdy(data.monthyr),
         fob = as.numeric(data.value)) |>
  select(date,fob)

urea |> names()
str(cpfi_combined)
cfpi_combined <-
cpfi_data |>
  select(date=Date,cpi,cash_cost:cost_per_kg_real,area_harv:monthly_ave_margin) |>
  left_join(fgate_data) |>
  left_join(ret_data) |>
  left_join(fuel |> select(date,fuel =Mean_Price)) |>
  left_join(urea |> select(date, urea = UREA_EE_BULK)) |>
  left_join(rain |>  select(date, rain=rainfall_mean)) |>
  left_join(imports) |>
  left_join(viet_global) |>
  mutate(fgate_real = (as.numeric(fgate)/cpi)*100,
         retail_real = (as.numeric(retail)/cpi)*100,
         fuel_real = (fuel/cpi)*100,
         urea_real = (urea/cpi)*100)



cfpi_combined |>
  names()


saveRDS(cfpi_combined,"cfpidata.rds")

saveRDS(cfpi_combined_compute,"cfpi_computed.rds")
library(dplyr)
library(zoo)
library(lubridate)

# --- Assume your data frame is named `df` ---
# Columns: date, fgate_real, retail_real, net_returns, total_cost,
#          area_harv, damage_area, area_sem, fuel_real, urea_real,
#          rain, imports, fob

cfpi_combined_compute <- cfpi_combined %>%
  arrange(date) %>%
  mutate(month = month(date)) %>%

  # 1. Farmgate Price Deviation
  group_by(month) %>%
  mutate(FP_hist = mean(fgate_real, na.rm = TRUE),
         FarmDev = (fgate_real - FP_hist) / FP_hist) %>%

  # 2. Farmer Profit Deviation
  mutate(Profit = fgate_real - cost_per_kg_real, # optional; or use net_returns
         Profit_hist = mean(Profit, na.rm = TRUE),
         ProfitDev = (Profit - Profit_hist) / Profit_hist) %>%

  # 3. Retail Price Deviation
  mutate(RP_hist = mean(retail_real, na.rm = TRUE),
         RetDev = (retail_real - RP_hist) / RP_hist) %>%

  # 4. Retail–Farmgate Price Gap Deviation
  mutate(Margin = retail_real - fgate_real,
         Margin_hist = mean(Margin, na.rm = TRUE),
         RetGap = (Margin - Margin_hist) / Margin_hist) %>%

  # 5. Area Planted / Expected Area Harvested
  mutate(Area = (area_harv - damage_area) / area_sem) %>%

  # 6. Fertilizer Cost Deviation
  mutate(FC_hist = mean(urea_real, na.rm = TRUE),
         FertDev = (urea_real - FC_hist) / FC_hist) %>%

  # 7. Fuel Price Deviation (vs 3-month moving average)
  mutate(FuelP_MA3 = rollmean(fuel_real, k = 3, fill = NA, align = "right"),
         FuelDev = (fuel_real - FuelP_MA3) / FuelP_MA3) %>%

  # 8. Monthly Rainfall Deviation
  mutate(Rain_hist = mean(rain, na.rm = TRUE),
         RainDev = (rain - Rain_hist) / Rain_hist) %>%

  # 9. Import Arrival Deviation (3-month moving average)
  mutate(ImportQ_MA3 = rollmean(imports, k = 3, fill = NA, align = "right"),
         ImportDev = (imports - ImportQ_MA3) / ImportQ_MA3) %>%

  # 10. Global Rice Price Deviation (3-month moving average)
  mutate(FOB_MA3 = rollmean(fob, k = 3, fill = NA, align = "right"),
         GlobalPriceDev = (fob - FOB_MA3) / FOB_MA3) %>%

  ungroup()

  select(date, FarmDev, ProfitDev, RetDev, RetGap, Area,
         FertDev, FuelDev, RainDev, ImportDev, GlobalPriceDev)

