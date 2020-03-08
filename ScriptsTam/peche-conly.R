if (!file.exists("c_lasso_models.Rdata")) {
  source("c_lasso.R")
}
if (!file.exists("c_randomforest_models.Rdata")) {
  source("c_randomforest.R")
}

source("miseenformedonnees.R")
source("format_data.R")

load("c_lasso_models.Rdata")
load("c_randomforest_models.Rdata")

c_method <- "lassomin"

ret <- format_data(binary=TRUE, month=TRUE, rect=TRUE)
data <- ret$data
xnames <- ret$xnames
ynames <- ret$ynames
nobs = dim(data)[1]
strsumx = paste(xnames, collapse= "+")

#Build train and test sets

set.seed(55)

ret <- create_train_test(data, 0.7)
train <- ret$train
test <- ret$test
train_ind <- ret$train_ind
n_train = dim(train)[1]
n_test = dim(test)[1]

baseline1_train <- colMeans(train)
baseline1_test <- colMeans(train)

rates_train <- data.frame(matrix(ncol = 4, nrow = 0))
rates_test <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(rates_train) = c("tn", "fn", "fp", "tp")
colnames(rates_test) = c("tn", "fn", "fp", "tp")

layers_train <- list()
layers_test <- list()
dec_thr <- 0.5
for (yname in ynames) {
  if (c_method == 'lassomin') {
    if (yname %in% names(c_lasso_fit)) {
      zero_pred_train <- predict(c_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                 type="response", s=c_lasso_lambdamin[[yname]])
      layers_train[[yname]] <- ifelse(zero_pred_train > dec_thr, 1, 0)
      zero_pred_test <- predict(c_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                type="response", s=c_lasso_lambdamin[[yname]])
      layers_test[[yname]] <- ifelse(zero_pred_test > dec_thr, 1, 0)
    } else {
      layers_train[[yname]] <- data.matrix(rep(0, n_train))
      layers_test[[yname]] <- data.matrix(rep(0, n_test))
    }
  } else if (c_method == 'lasso1se') {
    if (yname %in% names(c_lasso_fit)) {
      zero_pred_train <- predict(c_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                 type="response", s=c_lasso_lambda1se[[yname]])
      layers_train[[yname]] <- ifelse(zero_pred_train > dec_thr, 1, 0)
      zero_pred_test <- predict(c_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                type="response", s=c_lasso_lambda1se[[yname]])
      layers_test[[yname]] <- ifelse(zero_pred_test > dec_thr, 1, 0)
    } else {
      layers_train[[yname]] <- data.matrix(rep(0, n_train))
      layers_test[[yname]] <- data.matrix(rep(0, n_test))
    }
  } else if (c_method == 'randomforest') {
    if (yname %in% names(c_randomforest_fit)) {
      zero_pred_train <- predict(c_randomforest_fit[[yname]], newdata=train[c(xnames)], type="prob")[,2]
      layers_train[[yname]] <- ifelse(zero_pred_train > dec_thr, 1, 0)
      zero_pred_test <- predict(c_randomforest_fit[[yname]], newdata=test[c(xnames)], type="prob")[,2]
      layers_test[[yname]] <- ifelse(zero_pred_test > dec_thr, 1, 0)
    } else {
      layers_train[[yname]] <- data.matrix(rep(0, n_train))
      layers_test[[yname]] <- data.matrix(rep(0, n_test))
    }
  } else {
    stop("Don't know this classification method")
  }
  if (length(as.vector(table(train[[yname]], layers_train[[yname]]))) == 1) {
    if (sum(layers_train[[yname]])==0) {
      rates_train[yname,] = c(as.vector(table(train[[yname]], layers_train[[yname]])),0,0,0)
    } else if (sum(layers_train[[yname]])==length(layers_train[[yname]])) {
      rates_train[yname,] = c(0,0,0,as.vector(table(train[[yname]], layers_train[[yname]])))
    }
  } else if (length(as.vector(table(train[[yname]], layers_train[[yname]]))) == 2) {
    if (sum(layers_train[[yname]])==0) {
      rates_train[yname,] = c(as.vector(table(train[[yname]], layers_train[[yname]])),0,0)
    } else if (sum(layers_train[[yname]])==length(layers_train[[yname]])) {
      rates_train[yname,] = c(0,0,as.vector(table(train[[yname]], layers_train[[yname]])))
    }
  } else {
    rates_train[yname,] = as.vector(table(train[[yname]], layers_train[[yname]]))
  }
  if (length(as.vector(table(test[[yname]], layers_test[[yname]]))) == 1) {
    if (sum(layers_test[[yname]])==0) {
      rates_test[yname,] = c(as.vector(table(test[[yname]], layers_test[[yname]])),0,0,0)
    } else if (sum(layers_test[[yname]])==length(layers_test[[yname]])) {
      rates_test[yname,] = c(0,0,0,as.vector(table(test[[yname]], layers_test[[yname]])))
    }
  } else if (length(as.vector(table(test[[yname]], layers_test[[yname]]))) == 2) {
    if (sum(layers_test[[yname]])==0) {
      rates_test[yname,] = c(as.vector(table(test[[yname]], layers_test[[yname]])),0,0)
    } else if (sum(layers_test[[yname]])==length(layers_test[[yname]])) {
      rates_test[yname,] = c(0,0,as.vector(table(test[[yname]], layers_test[[yname]])))
    }
  } else {
    rates_test[yname,] = as.vector(table(test[[yname]], layers_test[[yname]]))
  }
}


sum(layers_train[['Y3']])
length(layers_train[['Y3']])

table(train[['Y3']], layers_train[['Y3']])
rates_train
rates_test

length(layers_train[[yname]])
total_train <- colSums(rates_train)
total_test <- colSums(rates_test)
total_train['tn'] <- total_train['tn']+30*253
total_test['tn'] <- total_test['tn']+30*109
total_train['tp']/(total_train['tp']+total_train['fn'])
total_test['tp']/(total_test['tp']+total_test['fn'])
total_train
total_test
total_train['tn']/(total_train['tn']+total_train['fp'])
total_test['tn']/(total_test['tn']+total_test['fp'])
total_test/total_train
