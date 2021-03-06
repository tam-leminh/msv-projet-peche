#Variable importance plots

#Check all models
if (!file.exists("models/step_c_lasso_models.Rdata")) {
  source("step_c_lasso.R")
}
if (!file.exists("models/step_r_lasso_models.Rdata")) {
  source("step_r_lasso.R")
}
if (!file.exists("models/step_o_randomforest_models.Rdata")) {
  source("step_o_randomforest.R")
}

source("miseenformedonnees.R")
source("format_data.R")

load("models/step_c_lasso_models.Rdata")
load("models/step_r_lasso_models.Rdata")
load("models/step_o_randomforest_models.Rdata")

#Get X names
ret <- format_data(log=TRUE, month=TRUE, rect=TRUE)
xnames <- ret$xnames

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
legend(0.02,0.95, legend=c("<0.01", "0.01-0.1", "0.1-0.5", "> 0.5"), fill=brewer.pal(4,"OrRd"), cex=0.5)

##Step C LASSO lambda1se
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

##Step C LASSO lambdamin
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

frequency <- rowSums(imp_matrix)[order(rowSums(imp_matrix))]
a <- as.data.frame(frequency)
a$varnames <- rownames(a)
rownames(a) <- NULL

ggplot(a[a['frequency']>10,], aes(x=reorder(varnames, frequency), y=frequency)) + 
  geom_point(size=2, colour="goldenrod4") +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=frequency), size=1.5, colour="goldenrod3") +
  geom_point(size=3, colour="goldenrod4") +
  ylab("Times selected") +
  xlab("Variable") +
  coord_flip()

##Step R LASSO lambda1se
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

##Step R LASSO lambdamin
imp_matrix <- matrix(0, nrow=length(xnames), ncol=length(r_lasso_fit))
rownames(imp_matrix) <- xnames
colnames(imp_matrix) <- names(r_lasso_fit)
for (yname in names(r_lasso_fit)) {
  a <- predict(r_lasso_fit[[yname]], s=r_lasso_lambdamin[[yname]], type="coef")
  imp_matrix[,yname] <- a[rownames(a)!="(Intercept)",1]
}
imp_matrix[imp_matrix != 0] <- 1
heatmap.2(imp_matrix, Rowv = TRUE, Colv = TRUE, scale="none", 
          dendrogram="none", trace="none", key=FALSE, keysize=0.4, 
          col=brewer.pal(3,"OrRd"), cexRow=0.5, cexCol=0.5)