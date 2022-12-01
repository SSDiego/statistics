# Como criar h cluster 

hc.complete <- hclust(dist(x), method = "complete")
hc.complete
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

# Lembrando como X foi criado
set.seed(2)
x <- matrix(rnorm (50*2) , ncol =2)
x[1:25 ,1]= x[1:25 ,1] + 3
x[1:25 ,2]= x[1:25 ,2] - 4

# Plot

par(mfrow = c(1 ,3))
plot(hc.complete ,main =" Complete Linkage ", xlab ="", sub ="", cex =.9)
plot(hc.average , main =" Average Linkage ", xlab ="", sub ="",cex =.9)
plot(hc.single , main =" Single Linkage ", xlab ="", sub ="",cex =.9)

# Nomes de acordo com posição de corte

cutree(hc.complete , 2)

cutree(hc.average , 2)

cutree(hc.single , 2)

cutree(hc.single , 4)

# Scaling / Normalizing

xsc = scale(x)
plot(hclust(dist(xsc), method ="complete") , main =" HierarchicalClustering with Scaled Features ")


# Correlação ao invés de distância euclidiana

x <- matrix (rnorm (30*3) , ncol =3)
dd <- as.dist(1- cor(t(x)))
plot(hclust (dd , method ="complete"), main =" Complete Linkage with Correlation - Based Distance ", xlab ="", sub ="")
