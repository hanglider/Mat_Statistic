data <- read.table("/Users/ivan/IT/Mat_Statistic/Data/babyboom.dat.txt", quote="\"", comment.char="")

shapiro.test(data$V3)

girl <- subset(data, data$V2 == 1)
boy <- subset(data, data$V2 == 2)

shapiro.test(girl$V3) #  p-value = 0.01798 < 0.05 => отклоняем гиопотезу (не является норм распределением)
shapiro.test(boy$V3) # p-value = 0.2022 > 0.05 => нет оснований отклонять гипотезу 
t.test(data$V3, mu = 3500, conf.level = 0.95, alternative = 't')

# Критерий Вилкоксона для любых выборок даже не имеющих нормального распределения

wilcox.test(data$V3, mu = 3200, exact = TRUE, correct = TRUE, conf.level = 0.95, alternative = 't', conf.int = TRUE)# p-value = 0.07046 >0.05 принимаем гипотезу о медиане 

# Гипотеза о дисперсии 

install.packages("EnvStats")
library(EnvStats)
var(data$V3)
varTest(data$V3, sigma.squared = 300000) #(H0 дисперсия=300000, H1 дисперсия !=300000) p.value = 0.7925299, нет основания отклонить гипотезу 

# Доля девочек 
18/44

binom.test(18,44, p = 0.5, conf.level = 0.95, alternative = 't') #p-value = 0.2912
prop.test(18,44, p = 0.5, conf.level = 0.95, alternative = 't')

# Двухвыборочные критерии
# Проверка равентсва мат ожиданий
t.test(boy$V3, girl$V3, mu = 0, paired = FALSE, var.equal = TRUE)

# Проверка равенства дисперсий (критерий Фишера)
var.test(boy$V3, girl$V3, ratio = 1, conf.level = 0.95, alternative = "t")

# Критерий Краскела - Уолиса
kruskal.test(data$V3, data$V2)

iris <- read.csv("/Users/ivan/IT/Mat_Statistic/iris.txt", header=FALSE)
table(iris$V5)
kruskal.test(iris$V1, iris$V5)
boxplot(iris$V1~iris$V5)

# Зависимость между величинами
# 1) количественные переменные
cor.test(iris$V1, iris$V2, conf.level = 0.95, method = "p", alternative = "t")
shapiro.test(iris$V1) #не нормальное распределение
cor.test(iris$V1, iris$V2, conf.level = 0.95, method = "s", alternative = "t")

# 2) категориальные переменные 
prop.test(c(10, 9), c(26, 18))
# Критерий Фишера для таблиц сопряженности 2х2
matrix(c(10, 16, 9, 9), ncol = 2, byrow = TRUE)
fisher.test(matrix(c(10, 16, 9, 9), ncol = 2, byrow = TRUE))

            