# Entendimento das dimensões

states <- row.names(USArrests)
states
names(USArrests)

# Entendimento do uso da função Apply
mean(USArrests$Murder)
mean(USArrests$Assault)
mean(USArrests$Murder[1])

apply(USArrests, 2, mean)
apply(USArrests,c(1,2), mean)

apply(USArrests , 2, var)

# Após analisar Média e Variância entedemos porquê devemos normalizar os dados

pr.out <- prcomp(USArrests, scale =TRUE)
pr.out

names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale=0)

pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot (pr.out, scale =0)

pr.out$sdev

pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var)
pve

plot(pve, xlab = " Principal Component ", ylab = " Proportion of
Variance Explained ",ylim=c(0 ,1) ,type='b')
plot(cumsum(pve), xlab = " Principal Component ", ylab = "
Cumulative Proportion of Variance Explained ",ylim = c(0 ,1), type='b')

# cumsum = soma acumulada