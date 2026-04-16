#Le package rpart [TAR13] est dédié aux arbres de décision. Sur un ordinateur connecté à
#Internet, installer ce package en tapant dans une console R la commande suivante :
#install.packages("rpart", dep=TRUE)

#Ensuite, à chaque ouverture d’une session R, ce package devra comme d’habitude être chargé grâce à la commande 
library(rpart)

data = data(kyphosis)
data

# Visualiser et obtenir un résumé de ce jeu de données :
#   — la variable Kyphosis indique si l’enfant a subi une déformation de la colonne vertébrale
# après une opération chirurgicale ;
# — la variable Age donne l’âge de l’enfant (en mois) ;
# — la variable Number indique le nombre de vertèbres concernées par l’opération ;
# — la variable Start indique le numéro de la première vertèbre opérée.
# On peut par exemple se demander si l’âge de l’enfant, le nombre de vertèbres opérées et la
# position des vertèbres opérées permettent d’évaluer le risque de survenue d’une cyphose à l’issue
# de l’opération. Pour cela, on va construire un modèle d’arbre de décision grâce à la commande
# suivante :

arbre = rpart(Kyphosis ~ ., method="class", minsplit=20, xval=81, data=kyphosis)

# Quelques explications :
#   — arbre est le nom choisi pour l’objet R qui contiendra les résultats ;
# — rpart est le nom de la commande permettant de créer un arbre de décision ;
# — la définition du modèle passe par « Kyphosis  . », qui est une formule R. La tilde signifie
# « est à expliquer en fonction de » : on met donc à gauche de la tilde la variable que l’on
# souhaite expliquer (ici, Kyphosis) et à droite les variables explicatives séparées par des +.
# Ici, on a mis un point à droite : cela signifie « tout le reste ». On aurait donc tout aussi bien
# pu écrire : Kyphosis  Age + Number + Start, le modèle aurait été équivalent... mais
# beaucoup plus long à écrire ;
# — method = "class" est ici un argument totalement optionnel, mais il permet de rappeler
# à R que la variable à expliquer est qualitative (c’est une sécurité supplémentaire) ;
# — minsplit=20 signifie qu’il faut au moins 20 individus dans un nœud pour tenter un split ;
# — l’argument xval permet de régler la stratégie de validation croisée : on y indique le nombre
# de « portions » du jeu de données à créer pour effectuer la validation croisée. Par exemple,
# xval = 2 signifiera qu’on divisera le jeu de données en deux parties égales : l’une corres-
#   pondant à un jeu d’apprentissage, l’autre à un jeu de validation. Ici, l’argument xval = 81
# correspond à la stratégie classique de leave-one-out (LOOCV), puisqu’il y a 81 individus
# dans le jeu de données ;
# — enfin, l’argument data=kyphosis permet d’indiquer le nom du jeu de données à utiliser.


summary(arbre)


#Les commandes suivantes permettent d’afficher l’arbre de décision sous forme graphique :
plot(arbre, uniform=TRUE, margin=0.1, main="Arbre de décision")
text(arbre, fancy=TRUE, use.n=TRUE, pretty=0, all=TRUE)

#À ce stade, on peut souhaiter élaguer l’arbre de décision. Pour cela, il est conseillé d’exécuter la
#commande plotcp(arbre) pour déterminer la taille optimale (ou printcp(arbre) pour obtenir le même résultat mais sous la forme d’un tableau et non d’un graphique) 

plotcp(arbre)

# On y constate que l’erreur de prédiction commise en validation croisée est minimale si size of
# tree = 2, c’est-à-dire si l’arbre ne comporte que deux feuilles terminales. L’arbre correspondant
# a une complexité cp = 0.059 : c’est de cette façon que l’on va indiquer à R la taille d’arbre
# souhaitée, grâce à la commande suivante.

arbre2 = rpart(Kyphosis ~ ., cp=0.059, data=kyphosis)
plot(arbre2, uniform=TRUE, margin=0.1, main="Arbre de décision")
text(arbre2, fancy=TRUE, use.n=TRUE, pretty=0, all=TRUE)

inco = data.frame(c(60,30,90), c(1,2,3), c(5,12,16))
colnames(inco) = c("Age", "Number", "Start")
                   
inco                

# La prédiction de la variable Kyphosis pour ces trois individus s’opère alors par la commande
# predict (dont le premier argument est le nom du modèle utilisé, et le second, newdata, est le
#          nom du data frame contenant les nouveaux individus) :
predict(arbre2, newdata=inco, type="class")


# Selon le modèle, seul le premier enfant semble devoir développer une cyphose à l’issue de son
# intervention chirurgicale. Pour obtenir les probabilités associées aux modalités absent et present
# pour chaque individu, il suffit de retirer l’argument type="class"
