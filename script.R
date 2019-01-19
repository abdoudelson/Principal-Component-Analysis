villes <- read.delim("~/Bureau/BD_projet_2018_2019/Binome9/villes.txt", row.names=1)
#recuperation des données de 1994
tokeep=villes[,1]==2
villes=villes[tokeep,]
villes <- subset(villes, select = -c(annee))
#ainsi la variable année devien caduc
used_data <- subset(villes, select = -c(prxsl,	prxal,	salbrt,	salnet,	htrav	,vac	
            ,achbrt	,achnet	,pain	,hamb	,alim	,panier,	vetdam,	vethom	,appart4	,appart3,	loyer,	
            appmen,	bus	,taxi	,voiture,	resto	,hotel	,serv,	impots,salhor                                     
))

stats = summary(used_data)
boxplot(villes[28:39])
library(FactoMineR)
pca=PCA(villes[,c(1,28:39)],scale.unit = TRUE,ncp = 5,graph = T,quali.sup = 1)

#visualisation
library("factoextra")

fviz_screeplot(res.pca, ncp=5)

fviz_pca_var(res.pca)

fviz_pca_var(res.pca, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.7) + theme_light()


# Contributions of variables on PC1
fviz_pca_contrib(res.pca, choice = "var", axes = 1)


# Contributions of variables on PC2
fviz_pca_contrib(res.pca, choice = "var", axes = 2,top = 6)

# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=7.5) + theme_minimal()


# individus
fviz_pca_ind(res.pca, col.ind="cos2") +
  scale_color_gradient2(low="black", mid="blue", 
                        high="red", midpoint=0.50) + theme_minimal()

# Contributions of individuals to PC1
fviz_pca_contrib(res.pca, choice = "ind", axes = 1)

# Contributions of individuals to PC2
fviz_pca_contrib(res.pca, choice = "ind", axes = 2)

# Total contribution on PC1 and PC2
fviz_pca_contrib(res.pca, choice = "ind", axes = 1:2)