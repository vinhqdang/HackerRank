if (!require(Metrics)) {
  install.packages("Metrics")
}

if (!require(forecast)) {
  install.packages("forecast")
}

library(forecast)
library(Metrics)

# read data
full_data = read.csv("stocks_closing_prices.csv")

for (stock_id in 1:395) {
  df = full_data [full_data$Stock.ID == stock_id,]
  
  # divide training and testing
  train = df[1:1225,]
  test = df[1226:1230,]
  
  fit1 <- forecast::ets(train$Closing.Price)
  fit2 <- forecast::auto.arima(train$Closing.Price)
  
  pred1 <- forecast::forecast(fit1,5)
  pred2 <- forecast::forecast(fit2,5)
  
  rmse1 <- Metrics::rmse(actual = test$Closing.Price, predicted = pred1$mean)
  rmse2 <- Metrics::rmse(actual = test$Closing.Price, predicted = pred2$mean)
  
  if (rmse1 < rmse2) {
    fit <- forecast::ets(df$Closing.Price)
    pred <- forecast::forecast(fit,3)
    for (day in 1:3) {
      full_data <- rbind (full_data, c(1230+day,1+day,stock_id,pred$mean[day]))
    }
  }
  else {
    fit <- forecast::auto.arima(df$Closing.Price)
    pred <- forecast::forecast(fit,3)
    for (day in 1:3) {
      full_data <- rbind (full_data, c(1230+day,1+day,stock_id,pred$mean[day]))
    }
  }
}

# create a new vector
# contains the difference between price of day 1230 and predicted price at day 1233
diff_absolute <- c()
diff_relative <- c()

for (stock_id in 1:395) {
  diff_price <- full_data[full_data$Stock.ID == stock_id & full_data$Day.Sequence == 1233,]$Closing.Price -
              full_data[full_data$Stock.ID == stock_id & full_data$Day.Sequence == 1230,]$Closing.Price
  diff_absolute <- c(diff_absolute, diff_price)
  diff_relative <- c(diff_relative, 
                     diff_price/full_data[full_data$Stock.ID == stock_id & full_data$Day.Sequence == 1230,]$Closing.Price)
}

# utility function
# to find the index of nth largest value
which_nth_highest <- function(x, n)
{
  for(i in seq_len(n - 1L)) x[x == max(x)] <- -Inf
  which(x == max(x))
}

# a new vector
# contains the quantity bought for each stocks
quantity_bough = rep (0,395)
cash_in_hand = 1e7
max_invest = 1e6

cur_order = 1
while (cash_in_hand > 0) {
  # print (paste("Round: ", count))
  # print (paste("Money left: ", cash_in_hand))
  
  # look for the index of nth largest price increase
  cur_index = which_nth_highest(diff_relative, cur_order)
  
  cur_invest = min (max_invest, cash_in_hand)
  
  last_price = full_data[full_data$Stock.ID == cur_index & full_data$Day.Sequence == 1230,]$Closing.Price
  
  cur_quantity = floor(cur_invest/last_price)
  
  quantity_bough [cur_index] = cur_quantity
  
  money_spent = cur_quantity * last_price
  
  # print (paste("Money spend this turn: ", money_spent))
  
  cash_in_hand = cash_in_hand - money_spent
  
  if (cash_in_hand < min (full_data[full_data$Day.Sequence == 1230,]$Closing.Price)) {
    break
  }
  
  cur_order = cur_order + 1
  
  count = count + 1
}

# writing the results
stock_ids <- 1:395

PriceList <- function(SeqDay) {
  res <- c()
  for (stock in stock_ids) {
    res <- c(res, full_data[full_data$Day.Sequence==SeqDay & full_data$Stock.ID==stock,]$Closing.Price)
  }
  res
}

Closing_Price_1231 = PriceList (1231)
Closing_Price_1232 = PriceList (1232)
Closing_Price_1233 = PriceList (1233)

results = data.frame(cbind(stock_ids, Closing_Price_1231, Closing_Price_1232, Closing_Price_1233, quantity_bough))

colnames(results) <- c("Stock_ID","Closing_Price_1231","Closing_Price_1232","Closing_Price_1233","Quantity")

write.csv(results, col.names = TRUE, row.names = FALSE,sep = ",", file = "predicted_prices.csv", quote = FALSE)
