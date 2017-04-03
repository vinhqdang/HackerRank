tags = read.table("tags.txt", header = TRUE)

df <- read.table("tfidf_500.txt")

train <- df[1:4375,]
test <- df[4376:7296,]

library (h2o)
h2o.init(nthreads=-1)

# h_train <- as.h2o (train)
h_test <- as.h2o (test)

final_predicts <- 1:nrow(test)

# prediction for 12 tags
for (i in 1:12) {
  print (paste("Column ",i))
  new_df <- cbind (train, tags[[i]])
  
  h_train <- as.h2o (new_df)
  
  x <- 1:(ncol(new_df)-1)
  y <- ncol(new_df)
  
  nfolds <- 5
  
  # GBM Hyperparamters
  # learn_rate_opt <- c(0.01, 0.03)
  # max_depth_opt <- c(3, 4, 5, 6, 9)
  # sample_rate_opt <- c(0.7, 0.8, 0.9, 1.0)
  # col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  # hyper_params <- list(learn_rate = learn_rate_opt,
  #                      max_depth = max_depth_opt,
  #                      sample_rate = sample_rate_opt,
  #                      col_sample_rate = col_sample_rate_opt)
  # 
  # # search_criteria <- list(strategy = "RandomDiscrete",
  # #                         max_models = 3,
  # #                         seed = 1)
  # 
  # search_criteria <- list(strategy = "RandomDiscrete",
  #                         stopping_metric = "AUC",
  #                         stopping_tolerance = 0.001,
  #                         stopping_rounds = 10)
  # 
  # gbm_grid <- h2o.grid(algorithm = "gbm",
  #                      grid_id = "gbm_grid_binomial",
  #                      x = x,
  #                      y = y,
  #                      training_frame = h_train,
  #                      ntrees = 100,
  #                      seed = 1,
  #                      nfolds = nfolds,
  #                      fold_assignment = "Modulo",
  #                      hyper_params = hyper_params,
  #                      search_criteria = search_criteria)
  # 
  # # random forest model
  # my_rf <- h2o.randomForest(x = x,
  #                           y = y,
  #                           training_frame = h_train,
  #                           ntrees = 500,
  #                           nfolds = nfolds,
  #                           fold_assignment = "Modulo",
  #                           seed = 1)
  # 
  # # deep learning model
  # hyper_params_dl <- list(
  #   hidden=list(c(64,64,64),c(128,128),c(512)),
  #   input_dropout_ratio=c(0,0.05),
  #   rate=c(0.01,0.02),
  #   rate_annealing=c(1e-8,1e-7,1e-6)
  # )
  # 
  # dl_grid <- h2o.grid(
  #   algorithm="deeplearning",
  #   grid_id="dl_grid",
  #   training_frame=h_train,
  #   nfolds=nfolds,
  #   x=x,
  #   y=y,
  #   epochs=20,
  #   stopping_metric="AUC",
  #   stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
  #   stopping_rounds=2,
  #   score_validation_samples=10000, ## downsample validation set for faster scoring
  #   score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  #   adaptive_rate=F,                ## manually tuned learning rate
  #   momentum_start=0.5,             ## manually tuned momentum
  #   momentum_stable=0.9,
  #   momentum_ramp=1e7,
  #   l1=1e-5,
  #   l2=1e-5,
  #   activation=c("Rectifier"),
  #   max_w2=10,                      ## can help improve stability for Rectifier
  #   hyper_params=hyper_params
  # )
  # 
  # ensemble <- h2o.stackedEnsemble(x = x,
  #                                 y = y,
  #                                 training_frame = h_train,
  #                                 model_id = "ensemble_gbm_grid_binomial",
  #                                 base_models = list(gbm_grid@model_ids,my_rf@model_id, dl_grid$model_ids))
  # 
  # perf <- h2o.performance(ensemble, newdata = test)
  # 
  # 
  # 
  # pred <- h2o.predict(ensemble, newdata = test)
  # 
  # final_predicts <- cbind (final_predicts, as.vector(pred$predict))
  
  print ("GLM")

  lr <- h2o.glm(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                 family = "binomial")

  pre_lr <- h2o.predict(lr, newdata = h_test)

  print ("GBM")

  gbm1 <- h2o.gbm(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                 ntrees = 500)
  pre_gbm1 <- h2o.predict(gbm1, newdata = h_test)

  gbm2 <- h2o.gbm(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                   ntrees = 200, max_depth = 10, learn_rate = 0.01)

  pre_gbm2 <- h2o.predict(gbm2, newdata = h_test)

  print ("random forest")

  rf1 <- h2o.randomForest(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                           ntrees = 500)

  pre_rf1 <- h2o.predict(rf1, newdata = h_test)

  print ("DL")
  
  d1 <- h2o.deeplearning(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                          hidden = c(512))

  pre_d1 <- h2o.predict(d1, newdata = h_test)

  d2 <- h2o.deeplearning(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                          hidden = c(128,128))

  pre_d2 <- h2o.predict(d2, newdata = h_test)

  d3 <- h2o.deeplearning(x=1:(ncol(new_df)-1),y=ncol(new_df),training_frame =  h_train,
                          hidden = c(64,64,64), activation = "RectifierWithDropout",
                         hidden_dropout_ratios = c(0.5,0.5,0.5))

  pre_d3 <- h2o.predict(d3, newdata = h_test)

  print ("Ensemble")

  all_pre = cbind (as.vector(pre_lr$predict),
                    as.vector(pre_gbm1$predict),
                    as.vector(pre_gbm2$predict),
                    as.vector(pre_rf1$predict),
                    as.vector(pre_d1$predict),
                    as.vector(pre_d2$predict),
                    as.vector(pre_d3$predict))

   all_pre <- data.frame(all_pre)

   preds <- c()

  for (j in 1:nrow(all_pre)) {
     a <- all_pre[j,]
     res = (sum(a=="True") >= 4)
     preds <- c(preds, res)
  }

  final_predicts <- cbind (final_predicts, preds)
}

write.table(x=final_predicts, file = "final_predicts.txt", row.names = FALSE, col.names = FALSE)