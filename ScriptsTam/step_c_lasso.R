#Train lasso models for C step
source("miseenformedonnees.R")
source("format_data.R")

ret <- format_data(binary=TRUE, month=TRUE, rect=TRUE)
data <- ret$data
xnames <- ret$xnames
ynames <- ret$ynames
nobs <- dim(data)[1]
strsumx <- paste(xnames, collapse= "+")

set.seed(55)
ret <- create_train_test(data, 0.7)
train <- ret$train
test <- ret$test
train_ind <- ret$train_ind

c_lasso_fit <- list()
c_lasso_lambdamin <- list()
c_lasso_lambda1se <- list()
for (yname in ynames) {
  if (sum(train[[yname]]) > 5) {
    cvfit <- cv.glmnet(data.matrix(data[c(xnames)]), data.matrix(data[[yname]]), 
                                             type.measure = "mse", family="binomial", nfolds = 5)
    c_lasso_fit[[yname]] <- cvfit$glmnet.fit
    c_lasso_lambdamin[[yname]] <- cvfit$lambda.min
    c_lasso_lambda1se[[yname]] <- cvfit$lambda.1se
  }
}
save(c_lasso_fit, c_lasso_lambdamin, c_lasso_lambda1se, file="models/step_c_lasso_models.Rdata")