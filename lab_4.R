#===========================================
# Лабораторная №4 — Регрессионный анализ
# Датасеты: Kuiper.xls и cigarettes.txt
#===========================================

#install.packages("readxl")
#install.packages("car")
#install.packages("lmtest")
#install.packages("MASS")

library(readxl)
library(car)
library(lmtest)
library(MASS)

#--------------------------------------------
# 1) KUIPER.XLS
# Переменные: Price, Mileage, Cylinder, Liter, Cruise
#--------------------------------------------

kuiper <- read_excel("/Users/ivan/IT/Mat_Statistic/Data/Kuiper.xls")

# Убедимся в названиях столбцов (на всякий случай)
colnames(kuiper)

# Приводим нужные типы
kuiper$Price    <- as.numeric(kuiper$Price)
kuiper$Mileage  <- as.numeric(kuiper$Mileage)
kuiper$Cylinder <- as.numeric(kuiper$Cylinder)
kuiper$Liter    <- as.numeric(kuiper$Liter)
kuiper$Cruise   <- as.numeric(kuiper$Cruise)

str(kuiper)
summary(kuiper)

# --- Корреляционный анализ ---
cor_kuiper <- cor(kuiper[, c("Price","Mileage","Cylinder","Liter","Cruise")], use = "pairwise.complete.obs")
cor_kuiper
pairs(~ Price + Mileage + Cylinder + Liter + Cruise, data = kuiper, main = "Scatterplot matrix (Kuiper)")

# --- Базовая линейная модель ---
# Модель: Price ~ Mileage + Cylinder + Liter + Cruise
m_base <- lm(Price ~ Mileage + Cylinder + Liter + Cruise, data = kuiper)

# Результаты модели (коэффициенты, t-test по каждому, F-test в целом)
summary(m_base)        # t для коэфф., F-тест модели, R^2
anova(m_base)          # дисперсионный анализ

# Уравнение регрессии (считать как подстановку коэффициентов из summary)
coef(m_base)
confint(m_base, level = 0.95)   # ДИ для коэффициентов

# --- График рассеяния + линия регрессии (простая) ---
# Для визуализации возьмём Price vs Mileage + линия однофакторной регрессии
plot(kuiper$Mileage, kuiper$Price, main = "Price vs Mileage (Kuiper)", xlab = "Mileage", ylab = "Price")
abline(lm(Price ~ Mileage, data = kuiper), col = "blue", lwd = 2)

# --- Диагностика базовой модели ---
par(mfrow = c(2,2))
plot(m_base)   # Residuals vs Fitted, Normal Q-Q, Scale-Location, Residuals vs Leverage

# Точечные проверки
res_base <- resid(m_base)
shapiro.test(res_base)            # Нормальность остатков
bptest(m_base)                    # Гетероскедастичность (Breusch-Pagan)
dwtest(m_base)                    # Автокорреляция остатков (Durbin-Watson)
vif(m_base)                       # Мультиколлинеарность (VIF)

# Важные наблюдения / выбросы влияния
outlierTest(m_base)               # Bonferroni p для выбросов
cooks <- cooks.distance(m_base)   # Cook's distance
hatv  <- hatvalues(m_base)        # leverage
cbind(cooks, hatv)[order(-cooks), ][1:5, ]  # топ-5 по Cook's (на вид)

# --- Улучшение модели: шаговый отбор ---
m_step <- stepAIC(m_base, direction = "both", trace = FALSE)
summary(m_step)
anova(m_step)
coef(m_step)
confint(m_step, level = 0.95)

# Диагностика step-модели
par(mfrow = c(2,2))
plot(m_step)
res_step <- resid(m_step)
shapiro.test(res_step)
bptest(m_step)
dwtest(m_step)
vif(m_step)
outlierTest(m_step)

# --- Box-Cox трансформация целевой переменной ---
# Подбираем λ
bc <- boxcox(m_base, plotit = TRUE)  # график поможет визуально
lambda_hat <- bc$x[which.max(bc$y)]
lambda_hat

# Если λ заметно отличается от 1 — строим модель на трансформированной цене
if (!is.na(lambda_hat) && abs(lambda_hat - 1) > 0.1) {
  if (abs(lambda_hat) < 1e-8) {
    y_bc <- log(kuiper$Price)
  } else {
    y_bc <- (kuiper$Price^lambda_hat - 1) / lambda_hat
  }
  m_bc <- lm(y_bc ~ Mileage + Cylinder + Liter + Cruise, data = kuiper)
  summary(m_bc)
  anova(m_bc)
  confint(m_bc, level = 0.95)
  
  par(mfrow = c(2,2))
  plot(m_bc)
  res_bc <- resid(m_bc)
  shapiro.test(res_bc)
  bptest(m_bc)
  dwtest(m_bc)
  vif(m_bc)
  outlierTest(m_bc)
}

#--------------------------------------------
# 2) CIGARETTES.TXT
# Переменные по столбцам: carbon monoxide (y), tar (x1), nicotine (x2), weight (x3)
#--------------------------------------------

cig <- read.table("/Users/ivan/IT/Mat_Statistic/Data/cigarettes.dat.txt",
                  header = FALSE, sep = "", fill = TRUE, stringsAsFactors = FALSE,
                  colClasses = c("character","numeric","numeric","numeric","numeric"))

# Имена столбцов: бренд + 4 числовых по ТЗ
colnames(cig) <- c("Brand","carbon_monoxide","tar","nicotine","weight")

# Берём только числовые переменные для анализа
cig <- cig[, c("carbon_monoxide","tar","nicotine","weight")]

str(cig)
summary(cig)

# --- Корреляции и scatter matrix ---
cor(cig, use = "pairwise.complete.obs")
pairs(cig, main = "Scatterplot matrix (Cigarettes)")

# --- Базовая модель: y ~ x1 + x2 + x3 ---
cig_m <- lm(carbon_monoxide ~ tar + nicotine + weight, data = cig)

# Результаты: t-тесты коэффициентов, F-тест модели, R^2
summary(cig_m)
anova(cig_m)
coef(cig_m)                     # коэффициенты
confint(cig_m, level = 0.95)    # ДИ для коэффициентов

# Наглядный график: y vs tar + линия простой регрессии
plot(cig$tar, cig$carbon_monoxide, main = "CO vs tar", xlab = "tar", ylab = "carbon monoxide")
abline(lm(carbon_monoxide ~ tar, data = cig), col = "blue", lwd = 2)

# --- Диагностика базовой модели ---
par(mfrow = c(2,2))
plot(cig_m)                     # Residuals vs Fitted, QQ, Scale-Location, Residuals vs Leverage

# Отдельные проверки
shapiro.test(resid(cig_m))      # нормальность остатков
bptest(cig_m)                   # гетероскедастичность (Breusch-Pagan)
dwtest(cig_m)                   # автокорреляция остатков (Durbin-Watson)
vif(cig_m)                      # мультиколлинеарность (VIF)
outlierTest(cig_m)              # выбросы (Bonferroni p)

# --- Шаговый отбор признаков (в обе стороны) ---
cig_step <- stepAIC(cig_m, direction = "both", trace = FALSE)
summary(cig_step)
anova(cig_step)
coef(cig_step)
confint(cig_step, level = 0.95)

# Диагностика для step-модели
par(mfrow = c(2,2))
plot(cig_step)
shapiro.test(resid(cig_step))
bptest(cig_step)
dwtest(cig_step)
vif(cig_step)
outlierTest(cig_step)

# --- Box–Cox трансформация (по желанию) ---
bc_cig <- boxcox(cig_m, plotit = TRUE)
lambda_cig <- bc_cig$x[which.max(bc_cig$y)]
lambda_cig

# Если λ заметно отличается от 1 — строим модель на трансформированном y
if (!is.na(lambda_cig) && abs(lambda_cig - 1) > 0.1) {
  y_bc <- if (abs(lambda_cig) < 1e-8) log(cig$carbon_monoxide) else (cig$carbon_monoxide^lambda_cig - 1) / lambda_cig
  cig_m_bc <- lm(y_bc ~ tar + nicotine + weight, data = cig)
  summary(cig_m_bc)
  anova(cig_m_bc)
  confint(cig_m_bc, level = 0.95)
  
  par(mfrow = c(2,2))
  plot(cig_m_bc)
  shapiro.test(resid(cig_m_bc))
  bptest(cig_m_bc)
  dwtest(cig_m_bc)
  vif(cig_m_bc)
  outlierTest(cig_m_bc)
}