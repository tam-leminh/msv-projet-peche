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
c_lasso_lambdamin <- list()
c_lasso_lambda1se <- list()
for (yname in ynames) {
  if (sum(train[[yname]]) >= 5) {
    cvfit <- cv.glmnet(data.matrix(data[c(xnames)]), data.matrix(data[[yname]]), 
                                             type.measure = "mse", family="binomial", nfolds = 5)
    c_lasso_fit[[yname]] <- cvfit$glmnet.fit
    c_lasso_lambdamin[[yname]] <- cvfit$lambda.min
    c_lasso_lambda1se[[yname]] <- cvfit$lambda.1se
  }
}
save(c_lasso_fit, c_lasso_lambdamin, c_lasso_lambda1se, file="c_lasso_models.Rdata")

a <- coef(c_lasso_fit[['Y86']], s=c_lasso_lambda1se[['Y86']])
a <- predict(c_lasso_fit[['Y86']], s=c_lasso_lambda1se[['Y86']], type="coef")
a <- a[which(rownames(a)!="(Intercept)"),1]
a[a != 0] <- 1
a

imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(c_lasso_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(c_lasso_fit)
for (yname in names(c_lasso_fit)) {
  a <- predict(c_lasso_fit[[yname]], s=c_lasso_lambda1se[[yname]], type="coef")
  imp_matrix[,yname] <- a[rownames(a)!="(Intercept)",1]
}
imp_matrix[imp_matrix != 0] <- 1
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", 
          dendrogram="none", trace="none", key=FALSE, keysize=0.2, 
          col=brewer.pal(3,"OrRd"), cexRow=0.5, cexCol=0.5)
max(imp_matrix)


imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(c_lasso_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(c_lasso_fit)
for (yname in names(c_lasso_fit)) {
  a <- predict(c_lasso_fit[[yname]], s=c_lasso_lambdamin[[yname]], type="coef")
  imp_matrix[,yname] <- a[rownames(a)!="(Intercept)",1]
}
imp_matrix[imp_matrix != 0] <- 1
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", 
          dendrogram="none", trace="none", key=FALSE, keysize=0.2, 
          col=brewer.pal(3,"OrRd"), cexRow=0.5, cexCol=0.5)
max(imp_matrix)
