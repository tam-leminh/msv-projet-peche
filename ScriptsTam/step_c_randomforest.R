#Train random forests for C step
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

c_randomforest_fit <- list()
for (yname in ynames) {
  if (sum(train[[yname]]) > 6) {
    rf_mod <- randomForest(x=train[c(xnames)], y=as.factor(train[[yname]]), ntree=500, mtry=7)
    c_randomforest_fit[[yname]] <- rf_mod
  }
}
save(c_randomforest_fit, file="models/step_c_randomforest_models.Rdata")