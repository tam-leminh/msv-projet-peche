source("miseenformedonnees.R")
source("format_data.R")

ret <- format_data(binary=TRUE, month=TRUE, rect=TRUE)
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

c_lasso_fit <- list()
c_lasso_lambda <- list()
for (yname in ynames) {
  if (sum(train[[yname]]) >= 5) {
    cvfit <- cv.glmnet(data.matrix(data[c(xnames)]), data.matrix(data[[yname]]), 
                                             type.measure = "mse", family="binomial", nfolds = 5)
    c_lasso_fit[[yname]] <- cvfit$glmnet.fit
    c_lasso_lambda[[yname]] <- cvfit$lambda.min
  }
}
save(c_lasso_fit, c_lasso_lambda, file="c_lasso_models.Rdata")
