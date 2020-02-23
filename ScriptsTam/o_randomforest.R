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

o_randomforest_fit <- list()
for (yname in ynames) {
  train_nz <- train[which(train[[yname]]>1),]
  if (dim(train_nz)[1] > 5) {
    rf_mod <- randomForest(x=train[c(xnames)], y=train[[yname]], ntree=100, mtry=length(xnames))
    o_randomforest_fit[[yname]] <- rf_mod
    pred_train <- predict(rf_mod, train[c(xnames)])
    print(yname)
    print(mean((pred_train - train[[yname]])^2))
  }
}
save(o_randomforest_fit, file="o_randomforest_models.Rdata")

imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(o_randomforest_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(o_randomforest_fit)
for (yname in names(o_randomforest_fit)) {
  imp_matrix[,yname] <- o_randomforest_fit[[yname]]$importance
  print(dim(o_randomforest_fit[[yname]]$importance))
}
o_randomforest_fit[['Y86']]$importance[order(-o_randomforest_fit[['Y86']]$importance),][1:10]
o_randomforest_fit[['Y85']]$importance[order(-o_randomforest_fit[['Y85']]$importance),][1:10]

dim(o_randomforest_fit[['Y85']]$importance)
dev.off()
dev.new(height=12, width=5)
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", dendrogram="none", trace="none", 
          breaks=c(0.1, 1, 10, 100, 1000), key=FALSE, keysize=0.2, 
          col=brewer.pal(4,"OrRd"), cexRow=0.5)
