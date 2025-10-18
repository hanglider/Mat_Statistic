#===========================================
# Лабораторная №2 — Проверки распределений
#===========================================

#------------------------------------------------------------
# 1) BABYBOOM
#------------------------------------------------------------

baby <- read.table("/Users/ivan/IT/Mat_Statistic/Data/babyboom.dat.txt", header = FALSE)
colnames(baby) <- c("TimeHHMM", "Sex", "Weight", "MinutesAfterMidnight")

# Быстрый обзор
str(baby)
summary(baby)

#-----------------------
# 1.1 Нормальность веса
#   a) все дети
#   b) только девочки
#   c) только мальчики
#-----------------------

# Визуально
boxplot(baby$Weight, horizontal = TRUE, main = "Babyboom: вес (все)")

# Тест Шапиро-Уилка (как в твоей практике)
shapiro.test(baby$Weight)  # H0: нормальность

# Разделение по полу
girls <- subset(baby, Sex == 1)
boys  <- subset(baby, Sex == 2)

# Проверка нормальности отдельно
shapiro.test(girls$Weight)
shapiro.test(boys$Weight)

# Точечные оценки параметров (все, девочки, мальчики)
mean(baby$Weight);  sd(baby$Weight);  var(baby$Weight)
mean(girls$Weight); sd(girls$Weight); var(girls$Weight)
mean(boys$Weight);  sd(boys$Weight);  var(boys$Weight)

t.test(baby$Weight, conf.level = 0.95)   # all
t.test(girls$Weight, conf.level = 0.95)  # girls
t.test(boys$Weight,  conf.level = 0.95)  # boys

# install.packages("EnvStats")
library(EnvStats)

varTest(baby$Weight,  alternative = "two.sided", conf.level = 0.95)
varTest(girls$Weight, alternative = "two.sided", conf.level = 0.95)
varTest(boys$Weight,  alternative = "two.sided", conf.level = 0.95)

#-----------------------
# 1.2 Экспоненциальность интервалов между рождениями
# Идея: если моменты рождений — пуассоновский процесс, то интервалы ~ Exp(λ)
# λ оцениваем точечно: λ̂ = 1/mean(между-рождений)
# Проверка гипотезы: KS с теоретической CDF pexp(., rate = λ̂)
#-----------------------

# Строим интервалы как разности соседних времен (в минутах)
times_sorted <- sort(baby$MinutesAfterMidnight)
inter_arrival <- diff(times_sorted)  # интервалы между рождениями (мин)

# Точечная оценка λ
lambda_hat <- 1 / mean(inter_arrival)

# Визуально
hist(inter_arrival, main = "Интервалы между рождениями (мин)", xlab = "", col = "lightblue", border = "white")
boxplot(inter_arrival, horizontal = TRUE, main = "Inter-arrival (мин)")

# KS-тест на экспоненциальность с оценкой λ̂
ks.test(inter_arrival, "pexp", rate = lambda_hat)  # H0: Exp(λ̂)

#-----------------------
# 1.3 Пуассон для количества рождений по часам
# Подход: считаем число рождений в каждом часу (0..23).
# Если arrivals — пуассон-процесс, то эти счётчики ~ Pois(λ), где λ̂ = среднее по часам.
# Pearson χ² GOF: бины 0,1,2,3,4,5+ (чтобы ожидаемые >= 5).
#-----------------------

# Час из minutes: floor(min/60)
hours <- floor(baby$MinutesAfterMidnight / 60)

# Частоты по часам (убедимся, что все 0:23 представлены)
tab_hours <- table(factor(hours, levels = 0:23))
tab_hours

# λ̂ = среднее число рождений за час
lambda_hour <- mean(as.numeric(tab_hours))

# Биновые наблюдаемые частоты по значениям счётчика (0,1,2,3,4,5+)
# Сначала посчитаем сколько часов имели 0,1,2,3,4,≥5 рождений
obs_0  <- sum(tab_hours == 0)
obs_1  <- sum(tab_hours == 1)
obs_2  <- sum(tab_hours == 2)
obs_3  <- sum(tab_hours == 3)
obs_4  <- sum(tab_hours == 4)
obs_5p <- sum(tab_hours >= 5)

obs_vec <- c(obs_0, obs_1, obs_2, obs_3, obs_4, obs_5p)
names(obs_vec) <- c("0","1","2","3","4","5+")

# Теоретические вероятности по Пуассону с λ̂ для тех же бинов
p0  <- dpois(0, lambda_hour)
p1  <- dpois(1, lambda_hour)
p2  <- dpois(2, lambda_hour)
p3  <- dpois(3, lambda_hour)
p4  <- dpois(4, lambda_hour)
p5p <- 1 - ppois(4, lambda_hour)  # P(X>=5)

p_vec <- c(p0,p1,p2,p3,p4,p5p)

# Ожидаемые частоты = 24 часа * p_k
exp_vec <- 24 * p_vec
obs_vec; round(exp_vec, 2)

# Pearson χ² GOF (p в аргументе — вероятности бинов)
chisq.test(x = obs_vec, p = p_vec, rescale.p = TRUE)  # NB: df ~ #bins-1-#параметров(1)


#========================================================
# 2) EUROW E I G H T
# Переменные: weight, batch
# Проверка нормальности: (а) все монеты, (б) внутри каждого пакета
# ДИ для параметров нормального распределения
#========================================================

euro <- read.table("/Users/ivan/IT/Mat_Statistic/Data/euroweight.dat.txt", header = FALSE)
colnames(euro) <- c("weight", "batch")

str(euro)
summary(euro)

# Визуально
boxplot(euro$weight, horizontal = TRUE, main = "Euroweight: все батчи")

# Нормальность (вся выборка)
shapiro.test(euro$weight)

# Доверительные интервалы (считаем точечные оценки и строим CI)
mean(euro$weight); sd(euro$weight); var(euro$weight)
t.test(euro$weight, conf.level = 0.95)
varTest(euro$weight,  alternative = "two.sided", conf.level = 0.95)

# === По батчам ===
# Нормальность по батчам
by(euro$weight, euro$batch, shapiro.test)

# t-интервал для мат. ожидания по батчам
by(euro$weight, euro$batch, function(x) t.test(x, conf.level = 0.95))

# Интервал для дисперсии по батчам
by(euro$weight, euro$batch, function(x) varTest(x, alternative = "two.sided", conf.level = 0.95))


#========================================================
# 3) IRIS
#========================================================

iris <- read.csv("/Users/ivan/IT/Mat_Statistic/Data/iris.txt", header = FALSE)
colnames(iris) <- c("sepal_length","sepal_width","petal_length","petal_width","class")

str(iris)
table(iris$class)

# Визуально
boxplot(sepal_length ~ class, data = iris, main = "Iris: sepal length по классам", horizontal = TRUE)

# Нормальность sepal_length по классам (без for)
by(iris$sepal_length, iris$class, shapiro.test)

# Точечные оценки и ДИ по классам
by(iris$sepal_length, iris$class, function(x) c(mean = mean(x), sd = sd(x), var = var(x)))
by(iris$sepal_length, iris$class, function(x) t.test(x, conf.level = 0.95))                             # ДИ для μ
by(iris$sepal_length, iris$class, function(x) varTest(x, alternative = "two.sided", conf.level = 0.95)) # ДИ для σ^2