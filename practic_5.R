ad <- read.csv("/Users/ivan/IT/Mat_Statistic/Data/Advertising.csv")
l4 <- lm(Sales ~ TV + Radio + Newspaper, data = ad)
summary(l4)

l5 <- lm(Sales ~ TV + Radio, data = ad)
summary(l5)

l6 <- lm(ad$Sales ~ ad$TV + ad$Radio)
summary(l6)
predict(l6, newdata = data.frame(TV = 300, Radio = 50))
predict(l6, newdata = data.frame(TV = 200, Radio = 100), interval = "p")
#predict(l6, newdata = data.frame(TV = c(200, 100), Radio = c(100, 50)), interval = "prediction")

# Проверка предположений регрессионного анализа
plot(ad$Sales, l6$residuals)
# Проверка на гомоскедастичность (тест Бройша-Пагана)
install.packages("lmtest")
library(LmTest)
bptest(l6)

# Проверка на автокорреляцию (тест Дарбина-Уотсона)
dwtest(l6)

# Проверка равенства нулю матожидания остатков 
t.test(l6$residuals, mu = 0)

# Проверка нормальности остатков
shapiro.test(l6$residuals)

# Средняя ошибка апрроксимации 
mean(abs((ad$Sales - l6$fitted.values)) / ad$Sales)
install.packages("Mlmetrics")
library(MlMetrics)
MAPE(ad$Sales, l6$fitted.values)

# Мультиколлинеарность (зависимость между факториалами)
install.packages("car")
vif(l6)

# Подбор лучшей модели (показатель Акаике)
install.packages("MASS")
library(MASS)
stepAIC(l4)

install.packages("leaps")
library(Leaps)
regsubsets(Sales ~ TV + Radio + Newspaper, data = ad, nvmax = 3)
summary(regsubsets(Sales ~ TV + Radio + Newspaper, data = ad, nvmax = 3))$
summary(regsubsets(Sales ~ TV + Radio + Newspaper, data = ad, nvmax = 3))$
summary(regsubsets(Sales ~ TV + Radio + Newspaper, data = ad, nvmax = 2), scale = "adjr2")


