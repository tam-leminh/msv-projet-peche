#Simple trees
source("miseenformedonnees.R")
source("format_data.R")
library(rpart)
library(rpart.plot)

ret <- format_data(log=FALSE, month=TRUE, rect=TRUE)
data <- ret$data
xnames <- ret$xnames
ynames <- ret$ynames
strsumx <- paste(xnames, collapse= "+")

#Build train and test sets

set.seed(55)

ret <- create_train_test(data, 0.7)
train <- ret$train
test <- ret$test
train_ind <- ret$train_ind

k <- 89 #species number
tree_mod <- rpart(paste0("Y", k, "~", strsumx), data=train, control = rpart.control(minsplit = 20))

#Full tree
prp(tree_mod)

#CV pruning
plotcp(tree_mod)
pruned <- prune(tree_mod, cp=tree_mod$cptable[which.min(tree_mod$cptable[,'xerror'])])
prp(pruned)

