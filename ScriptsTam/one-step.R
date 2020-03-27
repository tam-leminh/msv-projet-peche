#1-step model

#Check all models
if (!file.exists("models/step_o_lasso_models.Rdata")) {
  source("step_o_lasso.R")
}
if (!file.exists("models/step_o_relax_models.Rdata")) {
  source("step_o_relax.R")
}
if (!file.exists("models/step_o_randomforest_models.Rdata")) {
  source("step_o_randomforest.R")
}

source("miseenformedonnees.R")
source("format_data.R")

load("models/step_o_lasso_models.Rdata")
load("models/step_o_relax_models.Rdata")
load("models/step_o_randomforest_models.Rdata")

#Model selection (only one step)
#o_method <- "lassomin"
#o_method <- "lasso1se"
#o_method <- "relaxmin"
#o_method <- "relax1se"
o_method <- "randomforest"

#Data formatting
ret <- format_data(log=TRUE, month=TRUE, rect=TRUE)
data <- ret$data
xnames <- ret$xnames
ynames <- ret$ynames
nobs = dim(data)[1]
strsumx = paste(xnames, collapse= "+")

#Build train and test sets
set.seed(55) #for reproducibility
ret <- create_train_test(data, 0.7)
train <- ret$train
test <- ret$test
train_ind <- ret$train_ind
n_train = dim(train)[1]
n_test = dim(test)[1]

#Compute means for B1
baseline1_train <- colMeans(train)
baseline1_test <- colMeans(train)

#Function for applying model to one species
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
  
  #Compute mean errors (log metric)
  scores <- list()
  scores[["lm_train"]] <- mean(abs(predicted_discards_train[[yname]] - train[[yname]]))
  scores[["b1_train"]] <- mean(abs(baseline1_train[[yname]] - train[[yname]]))
  scores[["lm_test"]] <- mean(abs(predicted_discards_test[[yname]] - test[[yname]]))
  scores[["b1_test"]] <- mean(abs(baseline1_test[[yname]] - test[[yname]]))
  return(scores)
}

scores <- data.frame(yname=character(), lm_train=numeric(), b1_train=numeric(), 
                     lm_test=numeric(), b1_test=numeric(), stringsAsFactors=FALSE)

#Apply model for all species
k <- 0
for (yname in ynames) {
  k = k+1
  scores[k,] <- c(yname, as.list(dec_loop(yname, o_method)))
}

#Print mean error (over all species)
colMeans(scores[,c('lm_train', 'b1_train', 'lm_test', 'b1_test')])
