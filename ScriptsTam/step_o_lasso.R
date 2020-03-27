source("miseenformedonnees.R")
source("format_data.R")

ret <- format_data(log=TRUE, month=TRUE, rect=TRUE)
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

o_lasso_fit <- list()
o_lasso_lambdamin <- list()
o_lasso_lambda1se <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>0),]
  if (dim(train_nz)[1] > 5) {
    cvfit <- cv.glmnet(data.matrix(train[c(xnames)]), 
                       data.matrix(train[[yname]]), family = "gaussian", nfolds=6)
    o_lasso_fit[[yname]] <- cvfit$glmnet.fit
    o_lasso_lambdamin[[yname]] <- cvfit$lambda.min
    o_lasso_lambda1se[[yname]] <- cvfit$lambda.1se
  }
}
save(o_lasso_fit, o_lasso_lambdamin, o_lasso_lambda1se, file="models/step_o_lasso_models.Rdata")