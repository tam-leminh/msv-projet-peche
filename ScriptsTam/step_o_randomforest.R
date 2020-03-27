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

o_randomforest_fit <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>0),]
  if (dim(train_nz)[1] > 5) {
    rf_mod <- randomForest(x=train[c(xnames)], y=train[[yname]], ntree=500, mtry=length(xnames)/3, importance=TRUE)
    o_randomforest_fit[[yname]] <- rf_mod
  }
}
save(o_randomforest_fit, file="models/step_o_randomforest_models.Rdata")