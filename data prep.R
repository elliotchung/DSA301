library(openxlsx)
library(tidyverse)
filepath <- "Stress test 2025_tariff impact_15apr.xlsx"

convert_year_and_value <- function(df, value_col, is_excel_date = FALSE) {
  df <- df %>%
    mutate(
      Year = if (is_excel_date) as.Date(as.numeric(Year), origin = "1899-12-30")
      else as.numeric(Year),
      !!value_col := as.numeric(.data[[value_col]])
    )
  return(df)
}

EBITDA <- openxlsx::read.xlsx(filepath, sheet=11, cols = c(1:5)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "EBITDA")
INTEXP <- openxlsx::read.xlsx(filepath, sheet=12, cols = c(1:5)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "INTEXP")
REV <- openxlsx::read.xlsx(filepath, sheet=13, cols = c(1:7)) |> slice(-1) |> pivot_longer(cols = `45382`:`45657`, names_to = "Year", values_to = "REV")
COGS <- openxlsx::read.xlsx(filepath, sheet=14, cols = c(1, 2, 3, 8, 9)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "COGS")
SG.A <- openxlsx::read.xlsx(filepath, sheet=15, cols = c(1, 2, 3, 7, 8)) |> slice(-1)  |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "SG.A")
R.D <- openxlsx::read.xlsx(filepath, sheet=16, cols = c(1, 2, 3, 7, 8)) |> slice(-1)  |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "R.D")
OOPEX <- openxlsx::read.xlsx(filepath, sheet=17, cols = c(1:5)) |> slice(-1)  |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "OOPEX")
TAXEXP <- openxlsx::read.xlsx(filepath, sheet=20, cols = c(1:7)) |> slice(-1)  |> pivot_longer(cols = `2023.(Income.before.taxes)`:`2024.(Income.after.taxes)`, names_to = "Type", values_to = "TAXEXP") |> mutate(Year = substr(Type, 1, 4)) |> mutate(Income.type = str_extract(Type, "(?<=\\().+?(?=\\))")) |> select(-Type) |> pivot_wider(names_from = Income.type, values_from = c(TAXEXP))
MCAPEX <- openxlsx::read.xlsx(filepath, sheet=21, cols = c(1:8)) |> slice(-1)  |> pivot_longer(cols = `2020`:`2024`, names_to = "Year", values_to = "MCAPEX")
TODEBT <- openxlsx::read.xlsx(filepath, sheet=22, cols = c(1:5)) |> slice(-1)  |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "TODEBT")
CWP <- openxlsx::read.xlsx(filepath, sheet=23, cols = c(1:7)) |> slice(-1) |> pivot_longer(cols = `2023.(Liabilities)`:`2024.(Assets)`, names_to = "Type", values_to = "CWP") |> mutate(Year = substr(Type, 1, 4)) |> mutate(CWP.type = str_extract(Type, "(?<=\\().+?(?=\\))")) |> select(-Type) |> pivot_wider(names_from = CWP.type, values_from = c(CWP))
CAlessINV <- openxlsx::read.xlsx(filepath, sheet=24, cols = c(1:5)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "CAlessINV")
TA <- openxlsx::read.xlsx(filepath, sheet=25, cols = c(1:5)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "TA")
CASH.EQV <- openxlsx::read.xlsx(filepath, sheet=26, cols = c(1:5)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "CASH.EQV")
CASH <- openxlsx::read.xlsx(filepath, sheet=27, cols = c(1:5)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "CASH")
CASH.STINVEST <- openxlsx::read.xlsx(filepath, sheet=28, cols = c(1:5)) |> slice(-1) |> pivot_longer(cols = `2023`:`2024`, names_to = "Year", values_to = "CASH.STINVEST")

EBITDA <- convert_year_and_value(EBITDA, "EBITDA") |> filter(Year == 2024)

INTEXP <- convert_year_and_value(INTEXP, "INTEXP") |> filter(!is.na(INTEXP)) |> group_by(Ticker) |> slice_max(Year, n = 1, with_ties = FALSE) |> ungroup()
INTEXP$Year <- 2024

REV$Year <- as.Date(as.numeric(REV$Year), origin = "1899-12-30")
REV <-  REV |> filter(!is.na(REV)) |> filter(Year == max(Year))
REV$REV <- as.numeric(REV$REV)
REV$Year <- 2024

COGS <- convert_year_and_value(COGS, "COGS") |> filter(Year == 2024)
SG.A <- convert_year_and_value(SG.A, "SG.A")|> filter(Year == 2024)
R.D <- convert_year_and_value(R.D, "R.D") |> filter(Year == 2024)
OOPEX <- convert_year_and_value(OOPEX, "OOPEX") |> filter(Year == 2024)

TAXEXP$Year <- as.numeric(TAXEXP$Year)
TAXEXP$Income.before.taxes <- as.numeric(TAXEXP$Income.before.taxes)
TAXEXP$Income.after.taxes <- as.numeric(TAXEXP$Income.after.taxes)

MCAPEX <- convert_year_and_value(MCAPEX, "MCAPEX")

TODEBT <- convert_year_and_value(TODEBT, "TODEBT") |> filter(Year == 2024)

CWP$Year <- as.numeric(CWP$Year)
CWP$Liabilities <- as.numeric(CWP$Liabilities)
CWP$Assets <- as.numeric(CWP$Assets)

CAlessINV <- convert_year_and_value(CAlessINV, "CAlessINV") |> filter(Year == 2024)
TA <- convert_year_and_value(TA, "TA") |> filter(Year == 2024)
CASH.EQV <- convert_year_and_value(CASH.EQV, "CASH.EQV") |> filter(Year == 2024)
CASH <- convert_year_and_value(CASH, "CASH") |> filter(Year == 2024)
CASH.STINVEST <- convert_year_and_value(CASH.STINVEST, "CASH.STINVEST") |> filter(Year == 2024)

# Put all your data frames in a list
dfs <- list(EBITDA, INTEXP, REV, COGS, SG.A, R.D, OOPEX, TAXEXP, MCAPEX, TODEBT,
            CWP, CAlessINV, TA, CASH.EQV, CASH, CASH.STINVEST)

# Merge them all using full_join on the 4 common keys
merged_df <- reduce(dfs, full_join, by = c("Ticker", "Company.Name", "Sector", "Year"))
