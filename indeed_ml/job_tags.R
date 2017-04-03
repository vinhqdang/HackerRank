tags = read.table("tags.txt", header = TRUE)

df <- read.table("tfidf_500.txt")

train <- df[1:4375,]
test <- df[4376:7296,]

library (h2o)
h2o.init()

# h_train <- as.h2o (train)
h_test <- as.h2o (test)

final_predicts <- 1:nrow(test)

# prediction for 12 tags
for (i in 1:12) {
  print (paste("Column ",i))
  new_df <- cbind (train, tags[[i]])
  
  h_train <- as.h2o (new_df)
  
  lr <- h2o.glm(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                family = "binomial")
  
  pre_lr <- h2o.predict(lr, newdata = h_test)
  
  gbm1 <- h2o.gbm(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                  ntrees = 500)
  
  pre_gbm1 <- h2o.predict(gbm1, newdata = h_test)
  
  rf1 <- h2o.randomForest(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                          ntrees = 500)
  
  pre_rf1 <- h2o.predict(rf1, newdata = h_test)
  
  d1 <- h2o.deeplearning(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                         hidden = c(512))
  
  pre_d1 <- h2o.predict(d1, newdata = h_test)
  
  d2 <- h2o.deeplearning(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                         hidden = c(128,128))
  
  pre_d2 <- h2o.predict(d2, newdata = h_test)
  
  all_pre = cbind (as.vector(pre_lr$predict),
                   as.vector(pre_gbm1$predict),
                   as.vector(pre_rf1$predict),
                   as.vector(pre_d1$predict),
                   as.vector(pre_d2$predict))
  
  all_pre <- data.frame(all_pre)
  
  preds <- c()
  
  for (j in 1:nrow(all_pre)) {
    a <- all_pre[j,]
    res = (sum(a=="True") >= 2)
    preds <- c(preds, res)
  }
  
  final_predicts <- cbind (final_predicts, preds)
}

write.table(x=final_predicts, file = "final_predicts.txt", row.names = FALSE, col.names = FALSE)