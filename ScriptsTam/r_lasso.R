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

layer <- data.frame(matrix(ncol = length(ynames), nrow = nobs))
layer_train <- layer[train_ind, ]
layer_test <- layer[-train_ind, ]
n_train <- dim(train)[1]
n_test <- dim(test)[1]

train_nz <- train[which(train[['Y86']]>1),]
dim(train_nz)[1]
r_lasso_fit <- list()
r_lasso_lambda <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>1),]
  if (dim(train_nz)[1] > 5) {
    cvfit <- cv.glmnet(data.matrix(train_nz[c(xnames)]), 
                       data.matrix(train_nz[[yname]]), family = "gaussian", nfolds=6)
    r_lasso_fit[[yname]] <- cvfit$glmnet.fit
    r_lasso_lambda[[yname]] <- cvfit$lambda.min
  }
}
save(r_lasso_fit, r_lasso_lambda, file="r_lasso_models.Rdata")
