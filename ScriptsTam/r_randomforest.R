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

r_randomforest_fit <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>0),]
  if (dim(train_nz)[1] > 5) {
    rf_mod <- randomForest(x=train_nz[c(xnames)], y=train_nz[[yname]], ntree=500, mtry=20)
    r_randomforest_fit[[yname]] <- rf_mod
    pred_train <- predict(rf_mod, train_nz[c(xnames)])
    print(yname)
    print(mean((pred_train-train_nz[[yname]])^2))
  }
}
save(r_randomforest_fit, file="r_randomforest_models.Rdata")
