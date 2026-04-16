w = read.csv("https://chesneau.users.lmno.cnrs.fr/data-arbre.csv", header =T, sep = ";")
attach(w)
str(w)

pairs(w)

library(rpart)
arbre = rpart(Y ~ x1 + x2)
arbre

plot(arbre)
text(arbre)

library(rpart.plot)
prp(arbre, extra = 1)

rpart.plot(arbre)


# predict(arbre, newdata = data.frame(x1 = 3, x2 = 1))

plotcp(arbre)
