#Month data for individual species
source("miseenformedonnees.R")
source("format_data.R")

means <- aggregate(disquant.co[,c(8:157)], list(month=disquant.co$month), mean)
means_nz <- aggregate(disquant.co[,c(8:157)], list(month=disquant.co$month), function(x) sum(x)/sum(x!=0))
count <- aggregate(disquant.co[,c(8:157)], list(month=disquant.co$month), function(x) sum(x!=0))

#Plots for 1 species
k <- 75 #Species number
plot.ts(count[c(k+1)], xlab="month", main="Nombre de rejets total")
plot.ts(means[c(k+1)], xlab="month", main="Moyenne des quantités rejetées/mois")
plot.ts(means_nz[c(k+1)], xlab="month", main="Moyenne des quantités rejetées/mois sachant rejet")

#Correlation matrices for months
corCount <- cor(t(count[c(8:12,1:7),c(2:151)]))
heatmap.2(corCount, Rowv=NA, Colv=NA, dendrogram="none", xlab="month", ylab="month", 
          main="corr between months for count of non zero", trace="none", key=TRUE, 
          keysize=1.5, col=brewer.pal(9,"YlOrRd"))

means_nz[is.na(means_nz)] <- 0
corMeans <- cor(t(means_nz[c(8:12,1:7),c(2:151)]))
heatmap.2(corMeans, Rowv=NA, Colv=NA, dendrogram="none", xlab="month", ylab="month",
          main="corr between months for means of non zero", trace="none", key=TRUE, 
          keysize=1.5, col=brewer.pal(9,"YlOrRd"))
