#===========================================
# Лабораторная работа №3
# Проверка гипотез о равенстве параметров
#===========================================

#--------------------------------------------
# 1. Dataset Babyboom
#--------------------------------------------

data <- read.table("/Users/ivan/IT/Mat_Statistic/Data/babyboom.dat.txt", quote="\"", comment.char="")
colnames(data) <- c("Time", "Sex", "Weight", "MinutesAfterMidnight")

# Разделяем по полу
girl <- subset(data, Sex == 1)
boy <- subset(data, Sex == 2)

# Проверка нормальности
shapiro.test(girl$Weight)
shapiro.test(boy$Weight)

# Проверка равенства средних (двухвыборочный t-критерий Стьюдента)
# H0: средний вес девочек = среднему весу мальчиков
t.test(boy$Weight, girl$Weight, mu = 0, paired = FALSE, var.equal = TRUE)
# Если нормальность не подтверждена, используем непараметрический критерий Вилкоксона
wilcox.test(boy$Weight, girl$Weight, paired = FALSE, exact = TRUE, correct = TRUE, conf.int = TRUE)

# Проверка равенства дисперсий (критерий Фишера)
# H0: дисперсии равны
var.test(boy$Weight, girl$Weight, ratio = 1, conf.level = 0.95, alternative = "two.sided")

# Альтернатива при ненормальности — критерий Краскела-Уоллиса
kruskal.test(data$Weight, data$Sex)


#--------------------------------------------
# 2. Dataset Euroweight
#--------------------------------------------

euro <- read.table("/Users/ivan/IT/Mat_Statistic/Data/euroweight.dat.txt", header = FALSE)
colnames(euro) <- c("Weight", "Batch")

# Проверка нормальности веса (по каждому batch)
by(euro$Weight, euro$Batch, function(x) {
  if (length(x) >= 3) shapiro.test(x) else "Меньше 3 наблюдений"
})

# Проверка равенства средних
# (1) Однофакторный дисперсионный анализ (ANOVA)
# H0: средние веса монет во всех партиях равны
anova_res <- aov(Weight ~ as.factor(Batch), data = euro)
summary(anova_res)

# (2) Попарное сравнение между партиями
pairwise.t.test(euro$Weight, euro$Batch, p.adjust.method = "none")

# Проверка равенства дисперсий (критерий Бартлетта)
# Отбираем только батчи, где >= 2 наблюдений
euro_filtered <- subset(euro, Batch %in% names(table(euro$Batch)[table(euro$Batch) >= 2]))

# Проверка: сколько партий осталось
table(euro_filtered$Batch)

# Bartlett test (равенство дисперсий)
bartlett.test(Weight ~ Batch, data = euro_filtered)


# Альтернатива при ненормальности — критерий Краскела-Уоллиса
kruskal.test(Weight ~ as.factor(Batch), data = euro)


#--------------------------------------------
# 3. Dataset Iris
#--------------------------------------------

iris <- read.csv("/Users/ivan/IT/Mat_Statistic/Data/iris.txt", header = FALSE)
colnames(iris) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Class")

# Проверка гипотез о равенстве распределений (Краскел-Уоллис)
kruskal.test(iris$SepalLength, iris$Class)
kruskal.test(iris$SepalWidth,  iris$Class)
kruskal.test(iris$PetalLength, iris$Class)
kruskal.test(iris$PetalWidth,  iris$Class)

# Проверка равенства средних (ANOVA)
summary(aov(SepalLength ~ as.factor(Class), data = iris))
summary(aov(SepalWidth  ~ as.factor(Class), data = iris))
summary(aov(PetalLength ~ as.factor(Class), data = iris))
summary(aov(PetalWidth  ~ as.factor(Class), data = iris))

# Проверка равенства дисперсий (критерий Бартлетта)
bartlett.test(SepalLength ~ as.factor(Class), data = iris)
bartlett.test(SepalWidth  ~ as.factor(Class), data = iris)
bartlett.test(PetalLength ~ as.factor(Class), data = iris)
bartlett.test(PetalWidth  ~ as.factor(Class), data = iris)

# Графики для визуальной оценки различий
boxplot(iris$SepalLength ~ iris$Class, main = "Sepal Length по классам", horizontal = TRUE)
boxplot(iris$SepalWidth  ~ iris$Class, main = "Sepal Width по классам", horizontal = TRUE)
boxplot(iris$PetalLength ~ iris$Class, main = "Petal Length по классам", horizontal = TRUE)
boxplot(iris$PetalWidth  ~ iris$Class, main = "Petal Width по классам", horizontal = TRUE)


#--------------------------------------------
# 4. Dataset Sugery
#--------------------------------------------

# Читаем Excel
# install.packages("readxl")
library(readxl)

sug <- read_excel("/Users/ivan/IT/Mat_Statistic/Data/surgery.xlsx")

# Просмотр структуры
str(sug)
summary(sug)

# Для удобства — короткие имена
colnames(sug) <- c("Before_Left", "Before_Right", "After_Left", "After_Right")

# Определяем успешные случаи
success <- (sug$Before_Left < sug$After_Left) & (sug$Before_Right < sug$After_Right)
table(success)

success_count <- sum(success, na.rm = TRUE)
total <- length(success)

success_count
total

# Проверяем гипотезу о вероятности успеха (например p=0.7)
# H0: p = 0.7, H1: p != 0.7
prop.test(success_count, total, p = 0.7, conf.level = 0.95, alternative = "two.sided")

# Для вероятности 0.8:
prop.test(success_count, total, p = 0.8, conf.level = 0.95, alternative = "two.sided")

# Визуально — доля успешных операций
success_count / total