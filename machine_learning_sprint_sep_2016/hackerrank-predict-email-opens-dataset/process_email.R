process_email_dnn = function(output_file = "prediction.csv")
{
  train=read.csv("training_dataset.csv")
  test = read.csv("test_dataset.csv")
  
  train$click_time = NULL
  train$clicked = NULL
  train$open_time = NULL
  train$unsubscribe_time = NULL
  train$unsubscribed = NULL
  
  if (!require(h2o)) {
    install.packages("h2o")
  }
  
  library (h2o)
  
  h2o.init()
  
  dnn1 = h2o.deeplearning(x=c(1:33,35:49),y=34,nfolds = 5,training_frame = as.h2o(train))
  
  predict1 = h2o.predict(dnn1, as.h2o(test))
  
  output = as.vector (as.numeric(predict1$predict))
  
  write.csv (file = output_file, output, sep = "\n")
  
  h2o.shutdown(prompt = FALSE)
}

process_email_rf = function(output_file = "prediction.csv")
{
  train=read.csv("training_dataset.csv")
  test = read.csv("test_dataset.csv")
  
  train$click_time = NULL
  train$clicked = NULL
  train$open_time = NULL
  train$unsubscribe_time = NULL
  train$unsubscribed = NULL
  
  if (!require(h2o)) {
    install.packages("h2o")
  }
  
  library (h2o)
  
  h2o.init()
  
  rf1 = h2o.randomForest(x=c(1:33,35:49),y=34,nfolds = 5,training_frame = as.h2o(train))
  
  predict1 = h2o.predict(rf1, as.h2o(test))
  
  output = as.vector (as.numeric(predict1$predict))
  
  write.csv (file = output_file, output, sep = "\n")
  
  h2o.shutdown(prompt = FALSE)
}

process_email_glm = function(output_file = "prediction.csv")
{
  train=read.csv("training_dataset.csv")
  test = read.csv("test_dataset.csv")
  
  train$click_time = NULL
  train$clicked = NULL
  train$open_time = NULL
  train$unsubscribe_time = NULL
  train$unsubscribed = NULL
  
  if (!require (caret)) {
    install.packages ("caret")
  }
  
  library (caret)
  
  ctrl <- trainControl(method = "repeatedcv", number = 5, savePredictions = TRUE)
  
  mod_fit <- train(opened ~ .,  data=train, method="glm", family="binomial",
                   trControl = ctrl, tuneLength = 5)
  
  predict1 =predict(mod_fit, test)
  
  output = as.vector (as.numeric(predict1))
  
  write.csv (file = output_file, output, sep = "\n")
}