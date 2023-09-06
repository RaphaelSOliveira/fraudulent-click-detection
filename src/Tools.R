to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

nb_multiple_runs <- function(train_fraction, n) {
  fraction_correct <- rep(NA,n)
  for (i in 1:n) {
    train_ind <- sample(seq_len(nrow(balanced_train_sample)), size = smp_size)
    train <- balanced_train_sample[train_ind, ]
    test <- balanced_train_sample[-train_ind, ]
    model_rf_mult <- randomForest(is_attributed ~ app + channel + hour + device + os + nip_h_dev + nip_day_h, data=train, ntree = 300, nodesize = 3)
    nb_test_predict <- predict(model_rf_mult, test[,-5])
    fraction_correct[i] <- mean(nb_test_predict == test$is_attributed)
  }
  return(fraction_correct)
}

factColNames <- c("is_attributed",
                  "wday",
                  "hour")

colNames2 <- c("nip_day_h",
               "nip_h_chan",
               "nip_h_osr",
               "nip_h_app",
               "nip_h_dev")