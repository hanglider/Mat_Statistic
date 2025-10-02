#создание массива
vector <- c(1, 1, 2, 2, 2, 3, 3, 3, 4, 5)
vector

#точечный вариационный ряд
fr_table <- table(vector)

#относительная  частота
fr <- as.vector(fr_table)
fr
fr/length(vector)

#эмпирическая функция определения
ecdf(vector)
ecdf(vector)(1)
ecdf(vector)(2)
plot(ecdf(vector))

#выборочные числовые характеристики 
avg <- sum(vector)/length(vector)
avg 
mean(vector)
disp <- sum(vector^2)/length(vector) - mean(vector)^2 #выборочная дисперсия
S <- disp*length(vector)/(length(vector) - 1)
S #несмещенная выборочная дисперчия
var(vector)
sd(vector) #стандартное отклонение

#квантили
Q1 <- quantile(vector, 0.25)
Q1
Q2 <- quantile(vector, 0.5)
Q2
Q3 <- quantile(vector, 0.75)
Q3
summary(vector)
boxplot(vector, horizontal = TRUE)

#Метод Тьюхи (выбросы)
V <- Q3 + 1.5*(Q3 - Q1)
H <- Q1 - 1.5*(Q3 - Q1)
V/H
data <- read.table("/Users/ivan/IT/Mat_Statistic/Data/babyboom.dat.txt", quote="\"", comment.char="")

#интервальный ряд
table(cut(data$V3, breaks = 6))
mean(data$V3)
var(data$V3)
sd(data$V3)
plot(ecdf(data$V3))
boxplot(data$V3, horizontal = TRUE)
summary(data$V3)
U <- 3572 + 1.5 * (3572 - 3142)
U
L <- 3142 - 1.5 * (3572 - 3142)
L
subset(data, data$V3 <= L)
table(data$V2)
girl <- subset(data, data$V2 == 1)
girl
boys <- subset(data, data$V2 == 2)
boys
