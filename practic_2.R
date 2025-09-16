data <- read.table("C:/Users/Admin/Documents/practic1/babyboom.dat.txt", quote="\"", comment.char="")

# Проверка на нормальность 
# Визуальная с помощью графиков

boxplot(data$V3, horizontal = TRUE)

# Коэфицент асиметрии

install.packages("e1071")
library(e1071)
skewness(data$V3)

# Коэфицент эксцесса

kurtosis(data$V3)
boxplot(data1$V3)
install.packages("car")

qqplot(data$V3)  ###эта хуета у меня не работает 

U <- 3572 + 1.5 * (3572 - 3142)
L <- 3142 - 1.5 * (3572 - 3142)

# База без выбросов

data1 <- subset(data, data$V3 >= L & data$V3 <= U)
data1
qqplot(data1$V3) ###аналогично

# Критерии согласия
# 1) критейрий Пирсона

install.packages("nortest")
library(nortest)
pearson.test(data$V3)   #отклоняем
pearson.test(data1$V3)  #нормальность есть 

# 2) критерий Колмогорова

ks.test(data$V3, "pnorm", alternative = "greater")
ks.test(data1$V3, "pnorm", alternative = "greater")

# 3) критерий Шапиро-Уилка

shapiro.test(data$V3)   #отклоняем

# 4) критерий Лиллифорса

lillie.test(data$V3)    #отклоняем
lillie.test(data1$V3)   #принимаем

# 5) критерий Крамера фон Мезеса

cvm.test(data$V3)       #отклоняем
cvm.test(data1$V3)      #принимаем

# 6) критерий Андерсона-Дарлинга 

ad.test(data$V3)        #отклоняем
ad.test(data1$V3)[[2]]  #принимаем
ad.test(data1$V3)[2]
ad.test(data1$V3)$p.value

# Проверка гипотезы о математическом ожидании
avg <- mean(data$V3)
avg

# Критерий одновыборочный Стьюдента

t.test(data1$V3, mu = 3500, alternative = "t")
t.test(data1$V3, mu = 3200, alternative = "g")

# Проверка гипотезы о дисперсии 
install.packages("EnvStats")
library(EnvStats)
s2 <- var(data1$V3)
varTest(data1$V3, sigma.squared = s2, alternative = "t")
varTest(data1$V3, sigma.squared = s2, conf.level = 0.99, alternative = "t")

