source("miseenformedonnees.R")
source("format_data.R")

ret <- format_data(log=TRUE, month=TRUE, rect=TRUE)
data <- ret$data
xnames <- ret$xnames
ynames <- ret$ynames
nobs <- dim(data)[1]
strsumx <- paste(xnames, collapse= "+")

#Build train and test sets

set.seed(55)

ret <- create_train_test(data, 0.7)
train <- ret$train
test <- ret$test
train_ind <- ret$train_ind

r_lasso_fit <- list()
r_lasso_lambdamin <- list()
r_lasso_lambda1se <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>1),]
  if (dim(train_nz)[1] > 5) {
    cvfit <- cv.glmnet(data.matrix(train_nz[c(xnames)]), 
                       data.matrix(train_nz[[yname]]), family = "gaussian", nfolds=5)
    r_lasso_fit[[yname]] <- cvfit$glmnet.fit
    r_lasso_lambdamin[[yname]] <- cvfit$lambda.min
    r_lasso_lambda1se[[yname]] <- cvfit$lambda.1se
  }
}
save(r_lasso_fit, r_lasso_lambdamin, r_lasso_lambda1se, file="r_lasso_models.Rdata")
