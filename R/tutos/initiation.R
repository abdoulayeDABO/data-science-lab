options(stringsAsFactors = F)
options(scipen = 100)
options(max.print = 1000)

data("Sacramento")                # Charge d'abord
sacramento_data <- Sacramento     # PUIS assigne à une variable

ls()
objects()

head(sacramento_data, 4)
summary(sacramento_data)
str(sacramento_data)

sacramento_data[1:10, ]
# Ouvre dans un viewer
# View(sacramento_data[100:200, ])
sacramento_data[sacramento_data$price > 800000 & !is.na(sacramento_data$price), ]


x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
x
1/x
y <- c(x, 0, x)
y ^ 2

min(x)
length(x)
prod(x)
sum(x)
mean(x)
var(x)


seq = 1:30 # seq(1,30)
seq

seq(-5, 5, by=.2) -> s
s

z <- c(1:3,NA); ind <- is.na(z)
ind

xy = c("X1", "Y2", "X3", "Y4", "X5", "Y6", "X7", "Y8", "X9", "Y10")
xy


# Fait un histogramme :
hist(sacramento_data$price, col="#333333", border="white", breaks=25) # amusez-vous avec ces paramètres
hist(log(sacramento_data$price), col="#333333", border="white", breaks=25) # vous ferez plus de tracés plus tard

plot(sacramento_data$price, col="#333333", lwd=3, xlab="Year", main="Number of important discoveries per year")
