library(purrr)
calculate_revenue <- function(Revenue_raw, Growth) {
  if (is.na(Revenue_raw) || is.na(Growth)) {
    return(NA)
  }
  return(Revenue_raw * (1 + Growth))
}

calculate_cogs <- function(COGS, Growth, FC) {
  if (is.na(COGS) || is.na(Growth) || is.na(FC)) {
    return(NA)
  }
  return(COGS * FC + (1 - FC) * (1 + Growth) * COGS)
}

calculate_sga <- function(SG.A, Growth, cost_assumption) {
  if (is.na(SG.A) || is.na(Growth) || is.na(cost_assumption)) {
    return(NA)
  }
  return(SG.A * (1 + Growth * cost_assumption))
}

calculate_rnd <- function(rnd, Growth, cost_assumption) {
  if (is.na(rnd) || is.na(Growth) || is.na(cost_assumption)) {
    return(NA)
  }
  return(rnd * (1 + Growth * cost_assumption))
}

calculate_oopex <- function(OOPEX, Growth, cost_assumption) {
  if (is.na(OOPEX) || is.na(Growth) || is.na(cost_assumption)) {
    return(NA)
  }
  return(OOPEX * cost_assumption + (1 - cost_assumption) * (1 + Growth) * OOPEX)
}

calculate_ebitda <- function(REV, COGS, SG.A, R.D, OOPEX, EBITDA.adj) {
  if (is.na(REV)) return(NA)
  
  # Replace NA with 0
  if (is.na(COGS)) COGS <- 0
  if (is.na(SG.A)) SG.A <- 0
  if (is.na(R.D)) R.D <- 0
  if (is.na(OOPEX)) OOPEX <- 0
  if (is.na(EBITDA.adj)) EBITDA.adj <- 0
  
  return(REV - COGS - SG.A - R.D - OOPEX + EBITDA.adj)
}

calculate_ebitda_adjustment <- function(EBITDA, REV, COGS, SG.A, R.D, OOPEX) {
  if (is.na(EBITDA)) return(NA)
  
  if (is.na(REV)) Revenue <- 0
  if (is.na(COGS)) COGS <- 0
  if (is.na(SG.A)) SG.A <- 0
  if (is.na(R.D)) R.D <- 0
  if (is.na(OOPEX)) OOPEX <- 0
  return(EBITDA - REV + COGS + SG.A + R.D + OOPEX)
}

calculate_icr <- function(EBITDA.FC, INTEXP) {
  if (is.na(EBITDA.FC) || is.na(INTEXP)) {
    return(NA)
  }
  return(EBITDA.FC / INTEXP)
}



# Single-argument row-wise functions
raw_data$Rev.FC <- map_dbl(raw_data$REV, ~ calculate_revenue(.x, 0.1))
raw_data$COGS.FC <- map_dbl(raw_data$COGS, ~ calculate_cogs(.x, 0.1, 0.2))
raw_data$SGA.FC <- map_dbl(raw_data$SG.A, ~ calculate_sga(.x, 0.1, 0.33))
raw_data$RD.FC <- map_dbl(raw_data$R.D, ~ calculate_rnd(.x, 0.1, 0.33))
raw_data$OOPEX.FC <- map_dbl(raw_data$OOPEX, ~ calculate_oopex(.x, 0.1, 0.33))

# Multi-argument row-wise functions
raw_data$EBITDA.adj <- pmap_dbl(raw_data[c("EBITDA", "REV", "COGS", "SG.A", "R.D", "OOPEX")], 
                                calculate_ebitda_adjustment)

raw_data$EBITDA.FC <- pmap_dbl(raw_data[c("REV", "COGS", "SG.A", "R.D", "OOPEX", "EBITDA.adj")], 
                               calculate_ebitda)

raw_data$ICR.FC <- pmap_dbl(raw_data[c("EBITDA.FC", "INTEXP")], 
                            calculate_icr)


glimpse(raw_data)