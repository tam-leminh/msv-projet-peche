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

o_relax_fit <- list()
o_relax_lambdamin <- list()
o_relax_lambda1se <- list()
o_relax_gammamin <- list()
o_relax_gamma1se <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>0),]
  if (dim(train_nz)[1] > 5) {
    cvfit <- cv.glmnet(data.matrix(train[c(xnames)]), data.matrix(train[[yname]]), 
                       family = "gaussian", nfolds=6, relax=TRUE)
    o_relax_fit[[yname]] <- cvfit$glmnet.fit
    o_relax_lambdamin[[yname]] <- cvfit$relaxed$lambda.min
    o_relax_gammamin[[yname]] <- cvfit$relaxed$gamma.min
    o_relax_lambda1se[[yname]] <- cvfit$relaxed$lambda.1se
    o_relax_gamma1se[[yname]] <- cvfit$relaxed$gamma.1se
  }
}
save(o_relax_fit, o_relax_lambdamin, o_relax_gammamin, 
     o_relax_lambda1se, o_relax_gamma1se, file="models/step_o_relax_models.Rdata")