#Train relaxed lasso models for R step
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

r_relax_fit <- list()
r_relax_lambdamin <- list()
r_relax_gammamin <- list()
r_relax_lambda1se <- list()
r_relax_gamma1se <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>0),]
  if (dim(train_nz)[1] > 5) {
    cvfit <- cv.glmnet(data.matrix(train_nz[c(xnames)]), data.matrix(train_nz[[yname]]), 
                       family = "gaussian", nfolds=6, relax=TRUE)
    r_relax_fit[[yname]] <- cvfit$glmnet.fit
    r_relax_lambdamin[[yname]] <- cvfit$relaxed$lambda.min
    r_relax_gammamin[[yname]] <- cvfit$relaxed$gamma.min
    r_relax_lambda1se[[yname]] <- cvfit$relaxed$lambda.1se
    r_relax_gamma1se[[yname]] <- cvfit$relaxed$gamma.1se
  }
}
save(r_relax_fit, r_relax_lambdamin, r_relax_gammamin, 
     r_relax_lambda1se, r_relax_gamma1se, file="models/step_r_relax_models.Rdata")
