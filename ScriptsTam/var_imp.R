if (!file.exists("c_lasso_models.Rdata")) {
  source("c_lasso.R")
}
if (!file.exists("r_lasso_models.Rdata")) {
  source("r_lasso.R")
}
if (!file.exists("r_relax_models.Rdata")) {
  source("r_relax.R")
}
if (!file.exists("r_randomforest_models.Rdata")) {
  source("r_randomforest.R")
}
if (!file.exists("o_lasso_models.Rdata")) {
  source("o_lasso.R")
}
if (!file.exists("o_relax_models.Rdata")) {
  source("o_relax.R")
}
if (!file.exists("o_randomforest_models.Rdata")) {
  source("o_randomforest.R")
}

source("miseenformedonnees.R")
source("format_data.R")

load("c_lasso_models.Rdata")
load("r_lasso_models.Rdata")
load("r_relax_models.Rdata")
load("r_randomforest_models.Rdata")
load("o_lasso_models.Rdata")
load("o_relax_models.Rdata")
load("o_randomforest_models.Rdata")

ret <- format_data(log=TRUE, month=TRUE, rect=TRUE)
data <- ret$data
xnames <- ret$xnames
ynames <- ret$ynames

#Variable importance/selection in models

##1-step Random forest
imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(o_randomforest_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(o_randomforest_fit)
for (yname in names(o_randomforest_fit)) {
  imp_matrix[,yname] <- o_randomforest_fit[[yname]]$importance[,1]
}
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", dendrogram="none", trace="none", 
          breaks=c(0.001, 0.01, 0.1, 0.5, 1), key=FALSE, keysize=0.4, 
          col=brewer.pal(4,"OrRd"), cexCol=0.5, cexRow=0.5)
legend(0.02,0.95, legend=c("> 0.5", "0.1-0.5", "0.01-0.1","<0.01"), fill=brewer.pal(4,"OrRd"), cex=0.5)



##Classification LASSO lambda1se

imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(c_lasso_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(c_lasso_fit)
for (yname in names(c_lasso_fit)) {
  a <- predict(c_lasso_fit[[yname]], s=c_lasso_lambda1se[[yname]], type="coef")
  imp_matrix[,yname] <- a[rownames(a)!="(Intercept)",1]
}
imp_matrix[imp_matrix != 0] <- 1

heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", 
          dendrogram="none", trace="none", key=FALSE, keysize=0.4, 
          col=brewer.pal(3,"OrRd"), cexRow=0.5, cexCol=0.5)

##Classification LASSO lambdamin

imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(c_lasso_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(c_lasso_fit)
for (yname in names(c_lasso_fit)) {
  a <- predict(c_lasso_fit[[yname]], s=c_lasso_lambdamin[[yname]], type="coef")
  imp_matrix[,yname] <- a[rownames(a)!="(Intercept)",1]
}
imp_matrix[imp_matrix != 0] <- 1
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", 
          dendrogram="none", trace="none", key=FALSE, keysize=0.4, 
          col=brewer.pal(3,"OrRd"), cexRow=0.5, cexCol=0.5)

##Regression LASSO lambda1se

imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(r_lasso_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(r_lasso_fit)
for (yname in names(r_lasso_fit)) {
  a <- predict(r_lasso_fit[[yname]], s=r_lasso_lambda1se[[yname]], type="coef")
  imp_matrix[,yname] <- a[rownames(a)!="(Intercept)",1]
}
imp_matrix[imp_matrix != 0] <- 1
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", 
          dendrogram="none", trace="none", key=FALSE, keysize=0.4, 
          col=brewer.pal(3,"OrRd"), cexRow=0.5, cexCol=0.5)

##Regression LASSO lambdamin

imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(r_lasso_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(r_lasso_fit)
for (yname in names(r_lasso_fit)) {
  a <- predict(r_lasso_fit[[yname]], s=r_lasso_lambdamin[[yname]], type="coef")
  imp_matrix[,yname] <- a[rownames(a)!="(Intercept)",1]
}
imp_matrix[imp_matrix != 0] <- 1
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", 
          dendrogram="none", trace="none", key=FALSE, keysize=0.7, 
          col=brewer.pal(3,"OrRd"), cexRow=0.5, cexCol=0.5)

