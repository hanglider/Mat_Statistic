# ==========================================
# ЛАБОРАТОРНАЯ РАБОТА №1
# Датасет: Auto MPG
# ==========================================

# install.packages("readr")
# install.packages("e1071")
# install.packages("PerformanceAnalytics")

library(readr)
library(e1071)
library(PerformanceAnalytics)

# ==========================================
# 1) ЗАГРУЗКА ДАННЫХ
# ==========================================

auto <- readr::read_csv("/Users/ivan/IT/Mat_Statistic/Data/auto-mpg.csv", na = c("", "NA", "?"), show_col_types = FALSE)
auto <- na.omit(auto)   # удаляем пропуски

str(auto)
summary(auto)

# ==========================================
# 2) ВЫБОР ИЗУЧАЕМЫХ ПЕРЕМЕННЫХ
# ==========================================

mpg         <- as.numeric(auto$mpg)
horsepower  <- as.numeric(auto$horsepower)    
weight      <- as.numeric(auto$weight)
acceleration<- as.numeric(auto$acceleration)

# ==========================================
# 3) ГИСТОГРАММЫ
# ==========================================

par(mfrow = c(2, 2))

hist(mpg, main = "Histogram of MPG", xlab = "Miles per Gallon", col = "lightblue", border = "grey40")
hist(horsepower, main = "Histogram of Horsepower", xlab = "Horsepower", col = "lightblue", border = "grey40")
hist(weight, main = "Histogram of Weight", xlab = "Weight (lbs)", col = "lightblue", border = "grey40")
hist(acceleration, main = "Histogram of Acceleration", xlab = "Acceleration", col = "lightblue", border = "grey40")

# mpg — правосторонняя асимметрия: много авто со средним расходом, немного очень экономичных;
# horsepower — выраженная правая асимметрия, мощные авто встречаются редко;
# weight — ярко выраженная правая асимметрия (большинство авто лёгкие);
# acceleration — почти симметричное, лёгкий хвост влево.

# ==========================================
# 4) BOXPLOT (ЯЩИКИ С УСАМИ)
# ==========================================

par(mfrow = c(2, 2))

boxplot(mpg, horizontal = TRUE, main = "Boxplot — MPG", col = "lightgreen")
boxplot(horsepower, horizontal = TRUE, main = "Boxplot — Horsepower", col = "lightgreen")
boxplot(weight, horizontal = TRUE, main = "Boxplot — Weight", col = "lightgreen")
boxplot(acceleration, horizontal = TRUE, main = "Boxplot — Acceleration", col = "lightgreen")

# mpg — симметричное распределение, без выбросов, значения в основном 18–30 (умеренно правый хвост).
# horsepower — правостороннее распределение с верхними выбросами (мощные авто).
# weight — слегка правостороннее, выбросов нет, большинство машин среднего веса.
# acceleration — близко к симметричному, но есть отдельные выбросы (очень быстрые и очень медленные авто).

# ==========================================
# 5) ОПИСАТЕЛЬНАЯ СТАТИСТИКА
# ==========================================

mean_mpg <- mean(mpg)
mean_hp <- mean(horsepower)
mean_weight <- mean(weight)
mean_acc <- mean(acceleration)

var_mpg <- var(mpg)
var_hp <- var(horsepower)
var_weight <- var(weight)
var_acc <- var(acceleration)

sd_mpg <- sd(mpg)
sd_hp <- sd(horsepower)
sd_weight <- sd(weight)
sd_acc <- sd(acceleration)

median_mpg <- median(mpg)
median_hp <- median(horsepower)
median_weight <- median(weight)
median_acc <- median(acceleration)

Q1_mpg <- quantile(mpg, 0.25)
Q3_mpg <- quantile(mpg, 0.75)
Q1_hp <- quantile(horsepower, 0.25)
Q3_hp <- quantile(horsepower, 0.75)
Q1_weight <- quantile(weight, 0.25)
Q3_weight <- quantile(weight, 0.75)
Q1_acc <- quantile(acceleration, 0.25)
Q3_acc <- quantile(acceleration, 0.75)

skew_mpg <- skewness(mpg)
skew_hp <- skewness(horsepower)
skew_weight <- skewness(weight)
skew_acc <- skewness(acceleration)

kurt_mpg <- kurtosis(mpg)
kurt_hp <- kurtosis(horsepower)
kurt_weight <- kurtosis(weight)
kurt_acc <- kurtosis(acceleration)

stats <- data.frame(
  Variable = c("MPG", "Horsepower", "Weight", "Acceleration"),
  Mean = c(mean_mpg, mean_hp, mean_weight, mean_acc),
  Variance = c(var_mpg, var_hp, var_weight, var_acc),
  SD = c(sd_mpg, sd_hp, sd_weight, sd_acc),
  Median = c(median_mpg, median_hp, median_weight, median_acc),
  Q1 = c(Q1_mpg, Q1_hp, Q1_weight, Q1_acc),
  Q3 = c(Q3_mpg, Q3_hp, Q3_weight, Q3_acc),
  Skewness = c(skew_mpg, skew_hp, skew_weight, skew_acc),
  Kurtosis = c(kurt_mpg, kurt_hp, kurt_weight, kurt_acc)
)

print(stats, digit = 3)

# ==========================================
# 6) МЕТОД ТЬЮКИ (ВЫБРОСЫ)
# ==========================================

Q1_hp <- quantile(horsepower, 0.25)
Q3_hp <- quantile(horsepower, 0.75)
IQR_hp <- Q3_hp - Q1_hp
lower_hp <- Q1_hp - 1.5 * IQR_hp
upper_hp <- Q3_hp + 1.5 * IQR_hp
outliers_hp <- horsepower[horsepower < lower_hp | horsepower > upper_hp]

length(outliers_hp)  # количество выбросов
outliers_hp          # список значений-выбросов

# ==========================================
# 7) КОРРЕЛЯЦИОННЫЙ АНАЛИЗ
# ==========================================

cor_matrix <- cor(cbind(mpg, horsepower, weight, acceleration))
round(cor_matrix, 3)

# Диаграмма корреляций
chart.Correlation(cbind(mpg, horsepower, weight, acceleration), histogram = TRUE, pch = 19)

# mpg и weight: сильная отрицательная корреляция (~ -0.83)
# mpg и horsepower: отрицательная (~ -0.78)
# horsepower и weight: положительная (~ +0.86)
# acceleration слабо связана с остальными

# 1. Расход топлива (mpg) близок к нормальному распределению, но имеет небольшой правый хвост.
# 2. Мощность и вес автомобилей имеют ярко выраженную правостороннюю асимметрию:
#    большинство машин лёгкие и маломощные, несколько — мощные и тяжёлые.
# 3. Acceleration распределён более симметрично.
# 4. По методу Тьюки выбросы наблюдаются в horsepower — это мощные спорткары.
# 5. Корреляция между переменными логична:
#    - Чем больше вес и мощность → тем меньше mpg (расход выше).
#    - Horsepower и weight сильно положительно связаны.
# 6. В целом распределения реалистичны, логично отражают взаимосвязь характеристик автомобиля.