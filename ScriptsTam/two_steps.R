#2-steps model

#Check all models
if (!file.exists("models/step_c_lasso_models.Rdata")) {
  source("step_c_lasso.R")
}
if (!file.exists("models/step_c_randomforest_models.Rdata")) {
  source("step_c_randomforest.R")
}
if (!file.exists("models/step_r_lasso_models.Rdata")) {
  source("step_r_lasso.R")
}
if (!file.exists("models/step_r_relax_models.Rdata")) {
  source("step_r_relax.R")
}
if (!file.exists("models/step_r_randomforest_models.Rdata")) {
  source("step_r_randomforest.R")
}

source("miseenformedonnees.R")
source("format_data.R")

load("models/step_c_lasso_models.Rdata")
load("models/step_c_randomforest_models.Rdata")
load("models/step_r_lasso_models.Rdata")
load("models/step_r_relax_models.Rdata")
load("models/step_r_randomforest_models.Rdata")

#Models selection for C and R steps
c_method <- "lassomin"
#c_method <- "lasso1se"
#c_method <- "randomforest"
#r_method <- "lassomin"
r_method <- "lasso1se"
#r_method <- "relaxmin"
#r_method <- "relax1se"
#r_method <- "randomforest"

#Data formatting
ret <- format_data(log=TRUE, month=TRUE, rect=TRUE)
data <- ret$data
xnames <- ret$xnames
ynames <- ret$ynames
nobs = dim(data)[1]
strsumx = paste(xnames, collapse= "+")

#Build train and test sets
set.seed(55) #for reproducibility
ret <- create_train_test(data, 0.7)
train <- ret$train
test <- ret$test
train_ind <- ret$train_ind
n_train = dim(train)[1]
n_test = dim(test)[1]

#Compute means for B1
baseline1_train <- colMeans(train)
baseline1_test <- colMeans(train)

#Function for applying 2-steps to one species with a threshold
dec_loop <- function(yname, dec_thr, c_method, r_method) {
  layers_train <- list()
  layers_test <- list()
  
  #Step C and decision
  if (c_method == 'lassomin') {
    if (yname %in% names(c_lasso_fit)) {
      zero_pred_train <- predict(c_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                 type="response", s=c_lasso_lambdamin[[yname]])
      layers_train[[yname]] <- ifelse(zero_pred_train > dec_thr, 1, 0)
      zero_pred_test <- predict(c_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                type="response", s=c_lasso_lambdamin[[yname]])
      layers_test[[yname]] <- ifelse(zero_pred_test > dec_thr, 1, 0)
    } else {
      layers_train[[yname]] <- data.matrix(rep(0, n_train))
      layers_test[[yname]] <- data.matrix(rep(0, n_test))
    }
  } else if (c_method == 'lasso1se') {
    if (yname %in% names(c_lasso_fit)) {
      zero_pred_train <- predict(c_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                 type="response", s=c_lasso_lambda1se[[yname]])
      layers_train[[yname]] <- ifelse(zero_pred_train > dec_thr, 1, 0)
      zero_pred_test <- predict(c_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                type="response", s=c_lasso_lambda1se[[yname]])
      layers_test[[yname]] <- ifelse(zero_pred_test > dec_thr, 1, 0)
    } else {
      layers_train[[yname]] <- data.matrix(rep(0, n_train))
      layers_test[[yname]] <- data.matrix(rep(0, n_test))
    }
  } else if (c_method == 'randomforest') {
    if (yname %in% names(c_randomforest_fit)) {
      zero_pred_train <- predict(c_randomforest_fit[[yname]], newdata=train[c(xnames)], type="prob")[,2]
      layers_train[[yname]] <- ifelse(zero_pred_train > dec_thr, 1, 0)
      zero_pred_test <- predict(c_randomforest_fit[[yname]], newdata=test[c(xnames)], type="prob")[,2]
      layers_test[[yname]] <- ifelse(zero_pred_test > dec_thr, 1, 0)
    } else {
      layers_train[[yname]] <- data.matrix(rep(0, n_train))
      layers_test[[yname]] <- data.matrix(rep(0, n_test))
    }
  } else {
    stop("Don't know this classification method")
  }
  
  #Compute predicted positive mean for B2
  baseline2_train <- list()
  baseline2_test <- list()
  baseline2_train[[yname]] <- ifelse(layers_train[[yname]]==0, 0, mean(train[train[yname] > 0,][[yname]]))
  baseline2_test[[yname]] <- ifelse(layers_test[[yname]]==0, 0, mean(train[train[yname] > 0,][[yname]]))
  
  predicted_discards_train <- data.frame(matrix(ncol = 0, nrow = n_train))
  predicted_discards_test <- data.frame(matrix(ncol = 0, nrow = n_test))
  
  #Step R
  if (r_method == 'lassomin') {
    if (yname %in% names(r_lasso_fit)) {
      value_pred_train <- predict(r_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_lasso_lambdamin[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 0, value_pred_train)
      value_pred_test <- predict(r_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                  type="response", s=r_lasso_lambdamin[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 0, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (r_method == 'lasso1se') {
    if (yname %in% names(r_lasso_fit)) {
      value_pred_train <- predict(r_lasso_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_lasso_lambda1se[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 0, value_pred_train)
      value_pred_test <- predict(r_lasso_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=r_lasso_lambda1se[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 0, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (r_method == 'relaxmin') {
    if (yname %in% names(r_relax_fit)) {
      value_pred_train <- predict(r_relax_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_relax_lambdamin[[yname]], gamma=r_relax_gammamin[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 0, value_pred_train)
      value_pred_test <- predict(r_relax_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=r_relax_lambdamin[[yname]], gamma=r_relax_gammamin[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 0, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (r_method == 'relax1se') {
    if (yname %in% names(r_relax_fit)) {
      value_pred_train <- predict(r_relax_fit[[yname]], newx=data.matrix(train[c(xnames)]), 
                                  type="response", s=r_relax_lambda1se[[yname]], gamma=r_relax_gamma1se[[yname]])
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 0, value_pred_train)
      value_pred_test <- predict(r_relax_fit[[yname]], newx=data.matrix(test[c(xnames)]), 
                                 type="response", s=r_relax_lambda1se[[yname]], gamma=r_relax_gamma1se[[yname]])
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 0, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else if (r_method == 'randomforest') {
    if (yname %in% names(r_randomforest_fit)) {
      value_pred_train <- predict(r_randomforest_fit[[yname]], newdata=train[c(xnames)], type="response")
      predicted_discards_train[yname] <- ifelse(layers_train[[yname]]==0, 0, value_pred_train)
      value_pred_test <- predict(r_randomforest_fit[[yname]], newdata=test[c(xnames)], type="response")
      predicted_discards_test[yname] <- ifelse(layers_test[[yname]]==0, 0, value_pred_test)
    } else {
      predicted_discards_train[yname] <- data.matrix(rep(0, n_train))
      predicted_discards_test[yname] <- data.matrix(rep(0, n_test))
    }
  } else {
    stop("Don't know this regression method")
  }
  
  #Compute mean errors (log metric)
  scores <- list()
  scores[["mod_train"]] <- mean(abs(predicted_discards_train[[yname]] - train[[yname]]))
  scores[["b0_train"]] <- mean(abs(train[[yname]]))
  scores[["b1_train"]] <- mean(abs(baseline1_train[[yname]] - train[[yname]]))
  scores[["b2_train"]] <- mean(abs(baseline2_train[[yname]] - train[[yname]]))
  scores[["mod_test"]] <- mean(abs(predicted_discards_test[[yname]] - test[[yname]]))
  scores[["b0_test"]] <- mean(abs(test[[yname]]))
  scores[["b1_test"]] <- mean(abs(baseline1_test[[yname]] - test[[yname]]))
  scores[["b2_test"]] <- mean(abs(baseline2_test[[yname]] - test[[yname]]))
  return(scores)
}

scores <- data.frame(yname=character(), dec_thr=numeric(), mod_train=numeric(), b0_train=numeric(), b1_train=numeric(), 
                     b2_train=numeric(), mod_test=numeric(), b0_test=numeric(), b1_test=numeric(), b2_test=numeric(), stringsAsFactors=FALSE)

#Apply 2-steps for all species for a range of decision thresholds
k <- 0
d_range <- seq(0.,1.,0.02)
for (yname in ynames) {
  for (d in d_range) {
    k = k+1
    scores[k,] <- c(yname, d, as.list(dec_loop(yname, d, c_method, r_method)))
  }
}

#Single (joint) decision threshold
single_score <- aggregate(scores[,c(3:10)], list(dec_thr = scores$dec_thr), mean)

#Multiple (separate) decision threshold
thresholds <- numeric()
thresholds[ynames] <- as.numeric(setDT(scores)[, .SD[which.min(mod_train)], .SDcols='dec_thr', by = yname][['dec_thr']])

best_scores <- setDT(scores)[, .SD[which.min(mod_train)], 
              .SDcols=c('dec_thr', 'mod_train', 'b0_train', 'b1_train', 'b2_train', 'mod_test', 'b0_test', 'b1_test', 'b2_test'), by = yname]

colMeans(best_scores[,c('mod_train', 'b0_train', 'b1_train', 'b2_train', 'mod_test', 'b0_test', 'b1_test', 'b2_test')])
mult_train <- mean(best_scores$mod_train)
mult_test <- mean(best_scores$mod_test)

#Baselines
b1 <- mean(best_scores$b1_test)
b2 <- mean(best_scores$b2_test)

#Error plot
plot(single_score$dec_thr, single_score$mod_train, col='red', lty=1, type="o", cex=1, xlim=c(0.2,.8), ylim=c(0.1,0.42), 
     xlab="Decision threshold", ylab="Mean log error", main=paste0("Error of 2-step model (C=", c_method, ", R=", r_method, ")"))
lines(single_score$dec_thr, single_score$mod_test, col='blue', type="o", cex=1)
abline(h=mult_train, col="red", lty=2)
abline(h=mult_test, col="blue", lty=2)
abline(h=b1, col="brown", lty=1)
abline(h=b2, col="black", lty=2)
legend(0.60, 0.20, legend=c(paste("train (single), min =", round(min(single_score$mod_train),3)),
                            paste("test (single), min =", round(min(single_score$mod_test),3)), 
                            paste("train (multiple) =", round(mult_train,3)), 
                            paste("test (multiple) =", round(mult_test,3)), 
                            paste("b1_test =", round(b1,3)),
                            paste("b2_test =", round(b2,3))), 
       col=c("red", "blue", "red", "blue", "brown", "black"), lty=c(1,1,2,2,1,2), pch=list(1,1,"","","",""), cex = 0.75)

#Decision threshold histogram
h <- ggplot(data=best_scores, aes(x=dec_thr)) +
  geom_bar(stat="bin", fill="royalblue3", breaks=seq(0, 1, 0.04), colour='darkorange4')
plot(h)
