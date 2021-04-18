# This file includes functions to support the analysis that is contained in
# zoom_sensitivity_analysis.R

ForecastDecay <- function(actual, forecast, years.after, total.years) {
  # For all other variables return decay function
  return(actual + ((actual - forecast) *
                   (years.after ^ 2 - 2 * years.after * total.years)) / total.years ^ 2)
}

PrepareAllFinancialData <- function(financials, forecast.data, forecast.assumptions) {
  # Loop through all forecast.data fields and update accordingly
  for (i in 1:nrow(forecast.data)) {
    # Update sales growth forecast in unique way
    if (forecast.data$variable[i] == 'sales.growth') {
      # Get start and end decay values
      start <- forecast.assumptions$forecast.value[forecast.assumptions$variable == 'sales.growth.2022']
      end <- forecast.assumptions$forecast.value[forecast.assumptions$variable == 'sales.growth']
      
      forecast.data$forecast.jan.2022[i] <- start
      forecast.data$forecast.jan.2023[i] <- ForecastDecay(start, end, 1, 10)
      forecast.data$forecast.jan.2024[i] <- ForecastDecay(start, end, 2, 10)
      forecast.data$forecast.jan.2025[i] <- ForecastDecay(start, end, 3, 10)
      forecast.data$forecast.jan.2026[i] <- ForecastDecay(start, end, 4, 10)
      forecast.data$forecast.jan.2027[i] <- ForecastDecay(start, end, 5, 10)
      forecast.data$forecast.jan.2028[i] <- ForecastDecay(start, end, 6, 10)
      forecast.data$forecast.jan.2029[i] <- ForecastDecay(start, end, 7, 10)
      forecast.data$forecast.jan.2030[i] <- ForecastDecay(start, end, 8, 10)
      forecast.data$forecast.jan.2031[i] <- ForecastDecay(start, end, 9, 10)
      forecast.data$forecast.jan.2032[i] <- end
      forecast.data$forecast.jan.2033[i] <- end
    } # Otherwise use consistent forecast method
    else {
      # Get variable value for this iteration
      var.i <- forecast.data$variable[i]
      
      # Get start and end decay values
      start <- forecast.data$actuals.jan.2021[i]
      end <- forecast.assumptions$forecast.value[forecast.assumptions$variable == var.i]
      
      forecast.data$forecast.jan.2022[i] <- ForecastDecay(start, end, 1, 11)
      forecast.data$forecast.jan.2023[i] <- ForecastDecay(start, end, 2, 11)
      forecast.data$forecast.jan.2024[i] <- ForecastDecay(start, end, 3, 11)
      forecast.data$forecast.jan.2025[i] <- ForecastDecay(start, end, 4, 11)
      forecast.data$forecast.jan.2026[i] <- ForecastDecay(start, end, 5, 11)
      forecast.data$forecast.jan.2027[i] <- ForecastDecay(start, end, 6, 11)
      forecast.data$forecast.jan.2028[i] <- ForecastDecay(start, end, 7, 11)
      forecast.data$forecast.jan.2029[i] <- ForecastDecay(start, end, 8, 11)
      forecast.data$forecast.jan.2030[i] <- ForecastDecay(start, end, 9, 11)
      forecast.data$forecast.jan.2031[i] <- ForecastDecay(start, end, 10, 11)
      forecast.data$forecast.jan.2032[i] <- end
      forecast.data$forecast.jan.2033[i] <- end
    }
  }
  
  # Transpose forecast table to match financial structure
  forecast.t <- as_tibble(t(forecast.data %>% select(-c(variable, readable.name))),
                          rownames = "period")
  colnames(forecast.t) <- c("period", forecast.data$variable)
  
  # Merge forecast assumptions with financial data
  financials.all <- left_join(financials, forecast.t, by = "period")
  
  # Return combined dataset
  return(financials.all)
}

ForecastFinancialStatementValues <- function(financials.all) {
  ############# Forecast financial statement values based on forecast assumptions
  # Update sales growth which is based on lag
  for (i in 1:nrow(financials.all)) {
    # Skip update if data already exists
    if (!is.na(financials.all$sales.net[i])) {
      next
    }
    
    # Update sales iteratively
    financials.all$sales.net[i] <- financials.all$sales.net[i-1] * (1 + financials.all$sales.growth[i])
  }
  
  # Update non-lag variables
  financials.all <- financials.all %>% 
    mutate(cost.of.goods.sold = 
             ifelse(years.from.today > 0,
                    -sales.net * cogs.over.sales,
                    cost.of.goods.sold)) %>%  
    mutate(gross.profit = sales.net + cost.of.goods.sold) %>%
    mutate(r.and.d.expense = 
             ifelse(years.from.today > 0,
                    -sales.net * r.and.d.over.sales,
                    r.and.d.expense)) %>%
    mutate(s.g.and.a.expense = 
             ifelse(years.from.today > 0,
                    -sales.net * sg.and.a.over.sales,
                    s.g.and.a.expense)) %>%
    mutate(ebitda = gross.profit + r.and.d.expense + s.g.and.a.expense) %>%
    mutate(other.income = 
             ifelse(years.from.today > 0,
                    sales.net * other.income.over.sales,
                    other.income)) %>%
    mutate(ext.items = 
             ifelse(years.from.today > 0,
                    sales.net * ext.items.over.sales,
                    ext.items)) %>%
    # Continue with balance sheet asset items
    mutate(cash.and.short.term.investments = 
             ifelse(years.from.today > 0,
                    sales.net * end.op.cash.over.sales,
                    cash.and.short.term.investments)) %>% 
    mutate(accounts.receivable.net = 
             ifelse(years.from.today > 0,
                    sales.net * end.receivables.over.sales,
                    accounts.receivable.net)) %>% 
    mutate(inventories = 
             ifelse(years.from.today > 0,
                    sales.net * end.inventories.over.sales,
                    inventories)) %>% 
    mutate(other.current.assets = 
             ifelse(years.from.today > 0,
                    sales.net * end.current.assets.over.sales,
                    other.current.assets)) %>%
    mutate(total.current.assets = cash.and.short.term.investments + accounts.receivable.net +
             inventories + other.current.assets) %>% 
    mutate(net.ppe = 
             ifelse(years.from.today > 0,
                    sales.net * end.ppe.over.sales,
                    net.ppe)) %>% 
    mutate(long.term.investments = 
             ifelse(years.from.today > 0,
                    sales.net * end.investments.over.sales,
                    long.term.investments)) %>% 
    mutate(intangibles = 
             ifelse(years.from.today > 0,
                    sales.net * end.intangibles.over.sales,
                    intangibles)) %>% 
    mutate(other.non.current.assets = 
             ifelse(years.from.today > 0,
                    sales.net * end.other.assets.over.sales,
                    other.non.current.assets)) %>%
    mutate(total.assets = total.current.assets + net.ppe + long.term.investments +
             intangibles + other.non.current.assets) %>%
    # Continue with balance sheet liabilities and equity items
    mutate(current.debt = 
             ifelse(years.from.today > 0,
                    total.assets * current.debt.over.total.assets,
                    current.debt)) %>% 
    mutate(accounts.payable = 
             ifelse(years.from.today > 0,
                    sales.net * end.accounts.payable.over.sales,
                    accounts.payable)) %>% 
    mutate(income.tax.payable = 
             ifelse(years.from.today > 0,
                    sales.net * end.tax.payable.over.sales,
                    income.tax.payable)) %>% 
    mutate(other.current.liabilities = 
             ifelse(years.from.today > 0,
                    sales.net * end.other.current.liabs.over.sales,
                    other.current.liabilities)) %>%
    mutate(total.current.liabilities = current.debt + accounts.payable +
             income.tax.payable + other.current.liabilities) %>% 
    mutate(non.current.debt = 
             ifelse(years.from.today > 0,
                    total.assets * long.term.debt.over.total.assets,
                    non.current.debt)) %>% 
    mutate(other.non.current.liabilities = 
             ifelse(years.from.today > 0,
                    sales.net * other.liabilities.over.sales,
                    other.non.current.liabilities)) %>% 
    mutate(deferred.tax.liabilities = 
             ifelse(years.from.today > 0,
                    sales.net * deferred.taxes.over.sales,
                    deferred.tax.liabilities)) %>% 
    mutate(total.liabilities = total.current.liabilities + non.current.debt +
             other.non.current.liabilities + deferred.tax.liabilities) %>% 
    mutate(minority.interest = 
             ifelse(years.from.today > 0,
                    total.assets * minority.interest.over.total.assets,
                    minority.interest)) %>% 
    mutate(preferred.stock = 
             ifelse(years.from.today > 0,
                    total.assets * preferred.stock.over.total.assets,
                    preferred.stock))
  
  # Update variables that require averages of lagged variables
  financials.all <- financials.all %>%  
    mutate(ave.net.ppe = rollapply(net.ppe, 2, mean, align='right', fill=NA)) %>%
    mutate(ave.intangibles = rollapply(intangibles, 2, mean, align='right', fill=NA)) %>%
    mutate(depreciation.and.ammortization = 
             ifelse(years.from.today > 0,
                    -(ave.net.ppe + ave.intangibles) * dep.amort.over.ppe,
                    depreciation.and.ammortization)) %>%
    mutate(ebit = ebitda + depreciation.and.ammortization) %>%
    mutate(ave.current.debt = rollapply(current.debt, 2, mean, align='right', fill=NA)) %>%
    mutate(ave.non.current.debt = rollapply(non.current.debt, 2, mean, align='right', fill=NA)) %>%
    mutate(net.interest.expense = 
             ifelse(years.from.today > 0,
                    -(ave.current.debt + ave.non.current.debt) * net.interest.over.net.debt,
                    net.interest.expense)) %>%
    mutate(ebt = ebit + net.interest.expense + other.income) %>%
    mutate(income.taxes = 
             ifelse(years.from.today > 0,
                    -ebt * effective.tax.rate,
                    income.taxes)) %>%
    mutate(other.adjustments = 
             ifelse(years.from.today > 0,
                    sales.net * other.adjustments.over.sales,
                    other.adjustments)) %>%
    mutate(net.income.before.ext.items = ebt + income.taxes + other.adjustments) %>%
    mutate(minority.interest.in.earnings = 
             ifelse(years.from.today > 0,
                    -(ebt + income.taxes) * minority.interest.over.after.tax.income,
                    minority.interest.in.earnings)) %>%
    mutate(ave.preferred.stock = rollapply(preferred.stock, 2, mean, align='right', fill=NA)) %>%
    mutate(dividents.on.preferred.stock = 
             ifelse(years.from.today > 0,
                    -ave.preferred.stock * preferred.dividends.over.stock,
                    dividents.on.preferred.stock)) %>%
    mutate(net.income = net.income.before.ext.items + ext.items +
             minority.interest.in.earnings + dividents.on.preferred.stock) %>%
    mutate(total.liabilities.and.equity = total.assets) %>%
    mutate(total.common.equity = total.liabilities.and.equity - total.liabilities - minority.interest - preferred.stock)
  
  # Forecast retained earnings based on lagged data
  for (i in 1:nrow(financials.all)) {
    # Skip update if data already exists
    if (!is.na(financials.all$retained.earnings[i])) {
      next
    }
    
    # Update retained earnings iteratively
    financials.all$retained.earnings[i] <-
      financials.all$retained.earnings[i-1] + financials.all$net.income[i] * 
      (1 - financials.all$dividend.payout.ratio[i])
  }
  
  # Forecast final balance sheet item based on retained earnings numbers
  financials.all <- financials.all %>% 
    mutate(paid.in.common.capital = total.common.equity - retained.earnings)
  
  # Forecast retained earnings based on forecast assumptions
  financials.all <- financials.all %>% 
    mutate(beg.retained.earnings = ifelse(years.from.today > 0,
                                          lag(retained.earnings, default = retained.earnings[1]),
                                          beg.retained.earnings)) %>%
    mutate(plus.net.income = net.income) %>%
    mutate(minus.dividends.common.stock = -net.income * dividend.payout.ratio) %>%
    mutate(clean.surplus.plug.ignore = ifelse(years.from.today > 0, 0, clean.surplus.plug.ignore)) %>%
    mutate(end.retained.earnings = beg.retained.earnings + plus.net.income +
             minus.dividends.common.stock + clean.surplus.plug.ignore)
  
  # Return fully forecasted data
  return(financials.all)
}

CalculateValuePerShare <- function(financials.all, terminal.growth.rate, 
                                   cost.of.capital, shares.outstanding) {
  # Calculate cash flows based on forecasted financial statements
  financials.all <- financials.all %>% 
    mutate(dividends.on.common.stock = -minus.dividends.common.stock) %>%
    mutate(change.in.common.stock = paid.in.common.capital - 
             lag(paid.in.common.capital, 
                 default = paid.in.common.capital[1])) %>%
    mutate(net.payout = dividends.on.common.stock - change.in.common.stock) %>%
    mutate(present.value.net.payout = net.payout / 
             (1 + cost.of.capital) ^ years.from.today)
  
  # Sum present values for future rows
  present.value.forecasted <- financials.all %>%
    filter(years.from.today > 0 & !is.terminal) %>%
    summarise(sum(present.value.net.payout)) %>%
    as.numeric()
  
  terminal.value <- financials.all$net.payout[financials.all$is.terminal] / 
    (cost.of.capital - terminal.growth.rate)
  
  present.value.terminal.value <-
    terminal.value / (1 + cost.of.capital) ^
    (financials.all$years.from.today[financials.all$is.terminal] - 1)
  
  present.value = present.value.forecasted + present.value.terminal.value
  
  # Adjust the present value based on how long since the fiscal year end
  days.since.fiscal.end <-
    as.numeric(date.of.valuation - most.recent.fiscal.year.end)
  
  present.value.adjusted <-
    present.value * (1 + (cost.of.capital / 2)) *
    (1 + cost.of.capital * ((days.since.fiscal.end + 1) / 360))
  
  # Divide by number of shares for final valuation
  value.per.share <- present.value.adjusted / shares.outstanding
  
  # Return value per share, as the key variable for the sensitivity analysis
  return(value.per.share)
}

# This function does the end to end value calculation
CalculateCompanyValue <- function(financials, forecast.data, forecast.assumptions,
                                  cost.of.capital, shares.outstanding) {
  # Prepare data in single dataset
  financials.prep <- PrepareAllFinancialData(financials, forecast.data, forecast.assumptions)
  
  # Use data to forecast financial statement values
  financials.forecast <- ForecastFinancialStatementValues(financials.prep)
  
  # Use forecast to calculate value of the company and return that value
  terminal.growth.rate <-
    forecast.assumptions$forecast.value[forecast.assumptions$readable.name == "Sales Growth"]
  value.per.share <- CalculateValuePerShare(financials.forecast, terminal.growth.rate, 
                                            cost.of.capital, shares.outstanding)
  return(value.per.share)
}