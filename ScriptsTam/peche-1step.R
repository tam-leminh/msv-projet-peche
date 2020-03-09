if (!file.exists("o_lasso_models.Rdata")) {
  source("o_lasso.R")
}
if (!file.exists("o_relax_models.Rdata")) {
  source("o_relax.R")
}
if (!file.exists("o_randomforest_models.Rdata")) {
  source("o_randomforest.R")
}

source("miseenformedonnees.R")
source("format_data.R")

load("o_lasso_models.Rdata")
load("o_relax_models.Rdata")
load("o_randomforest_models.Rdata")

o_method <- "randomforest"

ret <- format_data(log=FALSE, month=TRUE, rect=TRUE)
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

dec_loop <- function(yname, o_method) {
  predicted_discards_train <- data.frame(matrix(ncol = 0, nrow = n_train))
  predicted_discards_test <- data.frame(matrix(ncol = 0, nrow = n_test))
  if (o_method == 'lassomin') {
    if (yname %in% names(o_lasso_fit)) {
      predicted_discards_train[yname] <- predict(o_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=o_lasso_lambdamin[[yname]])
      predicted_discards_test[yname] <- predict(o_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=o_lasso_lambdamin[[yname]])
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (o_method == 'lasso1se') {
    if (yname %in% names(o_lasso_fit)) {
      predicted_discards_train[yname] <- predict(o_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=o_lasso_lambda1se[[yname]])
      predicted_discards_test[yname] <- predict(o_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=o_lasso_lambda1se[[yname]])
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (o_method == 'relaxmin') {
    if (yname %in% names(o_relax_fit)) {
      predicted_discards_train[yname] <- predict(o_relax_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=o_relax_lambdamin[[yname]], gamma=o_relax_gammamin[[yname]])
      predicted_discards_test[yname] <- predict(o_relax_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=o_relax_lambdamin[[yname]], gamma=o_relax_gammamin[[yname]])
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (o_method == 'relax1se') {
    if (yname %in% names(o_relax_fit)) {
      predicted_discards_train[yname] <- predict(o_relax_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=o_relax_lambda1se[[yname]], gamma=o_relax_gamma1se[[yname]])
      predicted_discards_test[yname] <- predict(o_relax_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=o_relax_lambda1se[[yname]], gamma=o_relax_gamma1se[[yname]])
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (o_method == 'randomforest') {
    if (yname %in% names(o_randomforest_fit)) {
      predicted_discards_train[yname] <- predict(o_randomforest_fit[[yname]], newdata=train[c(xnames)], type="response")
      predicted_discards_test[yname] <- predict(o_randomforest_fit[[yname]], newdata=test[c(xnames)], type="response")
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else {
    stop("Don't know this regression method")
  }
  
  scores <- list()
  #scores[["lm_train"]] <- mean(abs(predicted_discards_train[[yname]] - train[[yname]]))
  #scores[["b0_train"]] <- mean(abs(train[[yname]]))
  #scores[["b1_train"]] <- mean(abs(baseline1_train[[yname]] - train[[yname]]))
  #scores[["lm_test"]] <- mean(abs(predicted_discards_test[[yname]] - test[[yname]]))
  #scores[["b0_test"]] <- mean(abs(test[[yname]]))
  #scores[["b1_test"]] <- mean(abs(baseline1_test[[yname]] - test[[yname]]))
  scores[["lm_train"]] <- mean(abs(log10(predicted_discards_train[[yname]]+1) - log10(train[[yname]]+1)))
  scores[["b0_train"]] <- mean(abs(log10(train[[yname]]+1)))
  scores[["b1_train"]] <- mean(abs(log10(baseline1_train[[yname]]+1) - log10(train[[yname]]+1)))
  scores[["lm_test"]] <- mean(abs(log10(predicted_discards_test[[yname]]+1) - log10(test[[yname]]+1)))
  scores[["b0_test"]] <- mean(abs(log10(test[[yname]]+1)))
  scores[["b1_test"]] <- mean(abs(log10(baseline1_test[[yname]]+1) - log10(test[[yname]]+1)))
  return(scores)
}

scores <- data.frame(yname=character(), lm_train=numeric(), b0_train=numeric(), b1_train=numeric(), 
                     lm_test=numeric(), b0_test=numeric(), b1_test=numeric(), stringsAsFactors=FALSE)
k <- 0
for (yname in ynames) {
  k = k+1
  scores[k,] <- c(yname, as.list(dec_loop(yname, o_method)))
}

colMeans(scores[,c('lm_train', 'b0_train', 'b1_train', 'lm_test', 'b0_test', 'b1_test')])
