stocks_closing_prices <- read.csv ("index_rebalance_dataset/stocks_closing_prices.csv")

stocks_info <- read.csv("index_rebalance_dataset/stocks_info.csv")

index_closing_prices <- read.csv("index_rebalance_dataset/index_closing_prices.csv")

portfolio <- read.csv("index_rebalance_dataset/portfolio.csv")

cash_in_hand = 832704

portfolio_value = 0
for (stock_id in portfolio$Symbol) {
  portfolio_value = portfolio_value + portfolio[portfolio$Symbol == stock_id,]$Quantity *
                  stocks_closing_prices[which(stocks_closing_prices$Symbol == stock_id),]$Day_0
}

if (!require(lpSolve)) {
  install.packages("lpSolve")
}
library (lpSolve)

Symbols = portfolio$Symbol

# # solve for Day 1
# f.obj <- stocks_closing_prices$Day_1
# f.con <- matrix(rep(1,length(Symbols)), nrow = 1)
# f.dir <- c("<=")
# f.rhs <- c(75e5)
# 
# day1_dec <- lp ("max", f.obj, f.con, f.dir, f.rhs)

# calculate the index constituent weights
weights = (stocks_closing_prices$Day_0 * stocks_info$Free.Float.Equity.Shares) / 
            sum (stocks_closing_prices$Day_0 * stocks_info$Free.Float.Equity.Shares)

day1_quantity = rep(0,51)
day2_quantity = rep(0,51)

for (i in 1:51) {
  day1_quantity [i] = floor(weights[i] * 75e5 / stocks_closing_prices$Day_1[i])
  day2_quantity [i] = floor(weights[i] * 1e7 / stocks_closing_prices$Day_2[i])
}

day1_quantity [which.min(stocks_closing_prices$Day_1)] = day1_quantity [which.min(stocks_closing_prices$Day_1)] +
                  floor ((75e5 - sum (day1_quantity * stocks_closing_prices$Day_1)) / min(stocks_closing_prices$Day_1))


day2_quantity [which.min(stocks_closing_prices$Day_2)] = day2_quantity [which.min(stocks_closing_prices$Day_2)] +
  floor ((1e7 - sum (day2_quantity * stocks_closing_prices$Day_2)) / min(stocks_closing_prices$Day_2))

results = data.frame(cbind(as.character(portfolio$Symbol), day1_quantity, day2_quantity))
colnames(results) = c("Symbol","Quantity_Day_1","Quantity_Day_2")

write.csv(results, file = "index_constituents.csv", row.names = FALSE, quote = FALSE, col.names = TRUE)
