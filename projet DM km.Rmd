---
title: "Projet DM"
output: html_document
---
```{r}
library(readr)
train <- read_csv("train.csv")
View(train)
```

```{r}
library(readr)
test <- read_csv("test.csv")
View(test)
```

```{r}
library(readr) # Importer les données 
library(dplyr) # Manipuler les données
library(tidyverse) # Manipuler les données
library(caret) # Machine Learning
```
```{r}
full <- bind_rows(train,test) # Regrouper dans un seul dataframe
head(full)
```

```{r}
sum(is.na(full))
# Le nombre de valeurs manquantes est égale à 1698
colMeans(is.na(full))
# - 30% de valeurs manquantes dans survived >> Supprimer toutes les valeurs manquantes
# - 20% de valeurs manquantes dans Age >> Remplacer par la médiane de la varibale age
# Fare et Embarked : Le nombre de valeurs manquantes est très faible donc on va les supprimer
# PLus de 77% de valeurs manquantes pour Cabin >> Supprimer la variable Cabin
```
```{r}
full <- full[!is.na(full$Embarked),] # Supprimer les valeurs manquantes de la varibale Embarked
full <- full[!is.na(full$Survived),] # Supprimer les valeurs manquantes de la varibale Survived
full[is.na(full$Age),]$Age <- median(full$Age,na.rm=TRUE ) # Remplacer les valeurs manquantes de la variable age avec la mediane #na.rm = True : pour ignoer les valeurs nulles
```
 
 Sélectionner les données qu'on garde pour la suite de l'analyse
```{r}
full <- full %>% select(c("Survived","Pclass","Sex","Age","SibSp",
                          "Parch","Fare","Embarked"))
```

```{r}
set.seed(222) # Paraméter un seed aléatoire

# Redéviser les données en train (75%) et test
smp_size <- floor(0.75*nrow(full)) # floor pour avoir un nombre entier
train_ind <- sample(seq_len(nrow(full)), size= smp_size) # Avoir 666 individus au hasard
train <-  full[train_ind,]
test <-  full[-train_ind,]
```

```{r}
# Nous allons travailler sur les variables survived, pclass et age TRAIN
library(FactoMineR)
data("train_ind")
View(train)
str(train)
A=train[c(1:2,4,5,6,7)]#variables actives
d1=dist(scale(A),method="euclidian")

```

```{r}
# classification ascendante hierarchique
cah.train <- hclust(d1,method = "ward.D")
print(cah.train)
```

```{r}
train.dist <- dist(d1, method = "euclidian")
head(train.dist)
print(train.dist)
```


```{r}
# afficher le dendogramme avec materialisation des classes
library(clValid)
classes<-cutree(cah.train,k=3)
classes
table(classes)
plot(cah.train,hang=-1)
rect.hclust(cah.train,k=3,border="red")
# a priori on peut suggerer k=3
```

```{r}
# decoupage en k groupes de classes
groupe.cah <- cutree(cah.train,k=3)
table(groupe.cah)

```

```{r}
library(factoextra)
km <- kmeans(d1, centers = 3 , nstart = 100)
print(km)

```
```{r}
train.labels=train$Survived
km.clusters <- km$cluster
fviz_cluster(list(data=d1, cluster= km.clusters))



```

