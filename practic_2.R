data <- read.table("C:/Users/Admin/Documents/practic1/babyboom.dat.txt", quote="\"", comment.char="")

#проверка на нормальность 
#визуальная с помощью графиков

boxplot(data$V3, horizontal = TRUE)

#коэфицент асиметрии

install.packages("e1071")
library(e1071)
skewness(data$V3)

#коэфицент эксцесса

kurtosis(data$V3)
boxplot(data1$V3)
install.packages("car")

qqplot(data$V3)  ###эта хуета у меня не работает 

U <- 3572 + 1.5 * (3572 - 3142)
L <- 3142 - 1.5 * (3572 - 3142)

#база без выбросов

data1 <- subset(data, data$V3 >= L & data$V3 <= U)
data1
qqplot(data1$V3) ###аналогично
