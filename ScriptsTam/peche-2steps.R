if (!file.exists("c_lasso_models.Rdata")) {
  source("c_lasso.R")
}
if (!file.exists("c_randomforest_models.Rdata")) {
  source("c_randomforest.R")
}
if (!file.exists("r_lasso_models.Rdata")) {
  source("r_lasso.R")
}
if (!file.exists("r_relax_models.Rdata")) {
  source("r_relax.R")
}
if (!file.exists("r_randomforest_models.Rdata")) {
  source("r_randomforest.R")
}

source("miseenformedonnees.R")
source("format_data.R")

load("c_lasso_models.Rdata")
load("c_randomforest_models.Rdata")
load("r_lasso_models.Rdata")
load("r_relax_models.Rdata")
load("r_randomforest_models.Rdata")

c_method <- "lassomin"
r_method <- "lasso1se"

ret <- format_data(log=TRUE, month=TRUE, rect=TRUE)
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

dec_loop <- function(yname, dec_thr, c_method, r_method) {
  layers_train <- list()
  layers_test <- list()
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

  baseline2_train <- list()
  baseline2_test <- list()
  baseline2_train[[yname]] <- ifelse(layers_train[[yname]]==0, 1, mean(train[train[yname] > 1,][[yname]]))
  baseline2_test[[yname]] <- ifelse(layers_test[[yname]]==0, 1, mean(train[train[yname] > 1,][[yname]]))
  
  predicted_discards_train <- data.frame(matrix(ncol = 0, nrow = n_train))
  predicted_discards_test <- data.frame(matrix(ncol = 0, nrow = n_test))
  if (r_method == 'lassomin') {
    if (yname %in% names(r_lasso_fit)) {
      value_pred_train <- predict(r_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_lasso_lambdamin[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 1, value_pred_train)
      value_pred_test <- predict(r_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                  type="response", s=r_lasso_lambdamin[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 1, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(1, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(1, n_test))
    }
  } else if (r_method == 'lasso1se') {
    if (yname %in% names(r_lasso_fit)) {
      value_pred_train <- predict(r_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_lasso_lambda1se[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 1, value_pred_train)
      value_pred_test <- predict(r_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=r_lasso_lambda1se[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 1, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(1, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(1, n_test))
    }
  } else if (r_method == 'relaxmin') {
    if (yname %in% names(r_relax_fit)) {
      value_pred_train <- predict(r_relax_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_relax_lambdamin[[yname]], gamma=r_relax_gammamin[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 1, value_pred_train)
      value_pred_test <- predict(r_relax_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=r_relax_lambdamin[[yname]], gamma=r_relax_gammamin[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 1, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(1, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(1, n_test))
    }
  } else if (r_method == 'relax1se') {
    if (yname %in% names(r_relax_fit)) {
      value_pred_train <- predict(r_relax_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_relax_lambda1se[[yname]], gamma=r_relax_gamma1se[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 1, value_pred_train)
      value_pred_test <- predict(r_relax_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=r_relax_lambda1se[[yname]], gamma=r_relax_gamma1se[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 1, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(1, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(1, n_test))
    }
  } else if (r_method == 'randomforest') {
    if (yname %in% names(r_randomforest_fit)) {
      value_pred_train <- predict(r_randomforest_fit[[yname]], newdata=train[c(xnames)], type="response")
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 1, value_pred_train)
      value_pred_test <- predict(r_randomforest_fit[[yname]], newdata=test[c(xnames)], type="response")
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 1, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(1, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(1, n_test))
    }
  } else {
    stop("Don't know this regression method")
  }
  
  scores <- list()
  scores[["mod_train"]] <- mean(abs(predicted_discards_train[[yname]] - train[[yname]])^2)
  scores[["b1_train"]] <- mean(abs(baseline1_train[[yname]] - train[[yname]])^2)
  scores[["b2_train"]] <- mean(abs(baseline2_train[[yname]] - train[[yname]])^2)
  scores[["mod_test"]] <- mean(abs(predicted_discards_test[[yname]] - test[[yname]])^2)
  scores[["b1_test"]] <- mean(abs(baseline1_test[[yname]] - test[[yname]])^2)
  scores[["b2_test"]] <- mean(abs(baseline2_test[[yname]] - test[[yname]])^2)
  return(scores)
}

scores <- data.frame(yname=character(), dec_thr=numeric(), mod_train=numeric(), b1_train=numeric(), 
                     b2_train=numeric(), mod_test=numeric(), b1_test=numeric(), b2_test=numeric(), stringsAsFactors=FALSE)
k <- 0
for (yname in ynames) {
  for (d in seq(0.,1.,0.02)) {
    k = k+1
    scores[k,] <- c(yname, d, as.list(dec_loop(yname, d, c_method, r_method)))
  }
}

#Single decision threshold
single_score <- aggregate(scores[,c(3:8)], list(dec_thr = scores$dec_thr), mean)

#Multiple decision threshold
thresholds <- numeric()
thresholds[ynames] <- as.numeric(setDT(scores)[, .SD[which.min(mod_train)], .SDcols='dec_thr', by = yname][['dec_thr']])

best_scores <- setDT(scores)[, .SD[which.min(mod_train)], 
              .SDcols=c('dec_thr', 'mod_train', 'b1_train', 'b2_train', 'mod_test', 'b1_test', 'b2_test'), by = yname]

colMeans(best_scores[,c('mod_train', 'b1_train', 'b2_train', 'mod_test', 'b1_test', 'b2_test')])

#Plots

plot(single_score$dec_thr, single_score$mod_train, col='red', lty=1, type="o", xlim=c(0.,1.), ylim=c(0.,1), 
     xlab="Decision threshold", ylab="MSE", main=paste0("Error of 2-step model (C=", c_method, ", R=", r_method, ")"))
lines(single_score$dec_thr, single_score$mod_test, col='blue', type="o")
abline(h=mean(best_scores$mod_train), col="red", lty=2)
abline(h=mean(best_scores$mod_test), col="blue", lty=2)
abline(h=mean(best_scores$b1_test), col="brown", lty=1)
abline(h=mean(best_scores$b2_test), col="black", lty=2)
legend(0.7, 1.8, legend=c("mod_train (single)", "mod_test (single)", "mod_train (multiple)", "mod_test (multiple)", "b1_test", "b2_test"), 
       col=c("red", "blue", "red", "blue", "brown", "black"), lty=c(1,1,2,2,1,2), pch=list(1,1,"","","",""))

#hist(best_scores$dec_thr, breaks=50, 
#     xlab="Decision threshold", ylab="Species", main="Histogram of best decision thresholds/species")

