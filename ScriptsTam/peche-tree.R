source("miseenformedonnees.R")
source("format_data.R")
library(rpart)
library(rpart.plot)

ret <- format_data(log=FALSE, month=TRUE, rect=TRUE)
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



tree_mod <- rpart(paste("Y89~",strsumx), data=train, control = rpart.control(minsplit = 20))
plotcp(tree_mod)
printcp(tree_mod)
prp(tree_mod)
text(tree_mod, use.n = TRUE)
