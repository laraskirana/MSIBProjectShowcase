library(readxl)
library(magrittr)
library(knitr)
library(ggplot2)
library(factoextra)
library(cluster)

HIREARKI <- read_excel("C:/Users/Laras Kirana/Downloads/HIREARKI.xlsx")
View(HIREARKI)

#Standardisasi Data
Datastand <- HIREARKI
Datastand <- scale(HIREARKI[2:5])

n <- dim()[1]
Data <- data.matrix(Datastand,1:4)
rownames(Data)=c(1:nrow(Data)) 

STANDARISASI <- read_excel("C:/Users/Laras Kirana/Downloads/STANDARISASI.xlsx")
View(STANDARISASI)

n <- dim(STANDARISASI)[1]
Data <- data.matrix(STANDARISASI,1:4)
rownames(Data)=c(1:nrow(Data)) 

korelasi = cor(STANDARISASI, method="pearson")
korelasi

d1=dist(STANDARISASI)
hc=eclust(d1, FUNcluster = "hclust", hc_metric = "euclidean",hc_method = "single")
d2=cophenetic(hc)
cor.sing=cor(d1,d2)
cor.sing

d1=dist(STANDARISASI)
hc=eclust(d1, FUNcluster = "hclust", hc_metric = "euclidean",hc_method = "average")
> d2=cophenetic(hc)
> cor.ave=cor(d1,d2)
> cor.ave

d1=dist(Data)
hc=eclust(d1, FUNcluster = "hclust", hc_metric = "euclidean",hc_method = "complete")
d2=cophenetic(hc)
cor.comp=cor(d1,d2)
cor.comp

> # Centroid
d1=dist(STANDARISASI)
hc=eclust(d1, FUNcluster = "hclust", hc_metric = "euclidean",hc_method = "centroid")
d2=cophenetic(hc)
cor.centr=cor(d1,d2)
cor.centr

 # Ward's Method
d1=dist(STANDARISASI)
hc=eclust(d1, FUNcluster = "hclust", hc_metric = "euclidean",hc_method = "ward.D")
d2=cophenetic(hc)
cor.ward=cor(d1,d2)
cor.ward
hier = hclust(dist(STANDARISASI), method = "single")
plot(hier)

fviz_nbclust(STANDARISASI, hcut, method = "silhouette")

clus_hier = eclust(STANDARISASI, FUNcluster = "hclust", k = 2, hc_method = "complete", graph= TRUE)
dend=fviz_dend(clus_hier, rect = TRUE, show_labels = TRUE, cex = 0.5)
idclus = clus_hier$cluster
idobs = as.numeric(names(idclus))
print(dend) 
idclus = clus_hier$cluster
c1 = c(); c2 = c(); 
  for (i in 1:n){
      if(idclus[i] == 1){c1 = c(c1,i)}
        else if (idclus[i] == 2){c2 = c(c2,i)}
     }
clustering = list(Cluster1 = c1, Cluster2 = c2)
clustering$Cluster1
clustering$Cluster2

#NON-HIREARKI
# Library
library(readxl)
library(gridExtra)
library(factoextra)
library(DT)

COVID19_1_ <- read_excel("C:/Users/Laras Kirana/Downloads/COVID19(1).xlsx")
View(COVID19_1_)

DataCovid <- COVID19_1_ 
distance<-get_dist(DataCovid[,2:6])
fviz_dist(distance, gradient=list(low="green", mid="white",high="red"))

kluster<-kmeans(DataCovid[,2:6], centers = 3, nstart = 25) 
fviz_cluster(kluster, data=DataCovid[,2:6])

letak= data.frame(DataCovid[,1],kluster$cluster)
colnames(letak)[colnames(letak)=="kluster.cluster"] = "Cluster"
letak
