source("miseenformedonnees.R")
source("format_data.R")

means <- aggregate(disquant.co[,c(9:157)], list(month=disquant.co$month), mean)
means_nz <- aggregate(disquant.co[,c(9:157)], list(month=disquant.co$month), function(x) sum(x)/sum(x!=0))
count <- aggregate(disquant.co[,c(9:157)], list(month=disquant.co$month), function(x) sum(x!=0))
plot.ts(means_nz[c(2)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(10)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(17)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(21)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(36)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(39)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(60)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(69)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(70)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(75)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(76)], xlab="month", main="Moyenne des quantités rejetées")

plot.ts(count[c(77)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means[c(77)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(77)], xlab="month", main="Moyenne des quantités rejetées")

plot.ts(means_nz[c(84)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(86)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(89)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(123)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(141)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(145)], xlab="month", main="Moyenne des quantités rejetées")
plot.ts(means_nz[c(146)], xlab="month", main="Moyenne des quantités rejetées")

corCount <- cor(t(count[c(8:12,1:7),c(2:151)]))
heatmap.2(corCount, Rowv=NA, Colv=NA, dendrogram="none", xlab="month", ylab="month", 
          main="corr between months for count of non zero", trace="none", key=TRUE, 
          keysize=1, col=brewer.pal(9,"YlOrRd"))


means_nz[is.na(means_nz)] <- 0
corMeans <- cor(t(means_nz[c(8:12,1:7),c(2:151)]))
heatmap.2(corMeans, Rowv=NA, Colv=NA, dendrogram="none", xlab="month", ylab="month",
          main="corr between months for means of non zero", trace="none", key=TRUE, 
          keysize=1, col=brewer.pal(9,"YlOrRd"))
