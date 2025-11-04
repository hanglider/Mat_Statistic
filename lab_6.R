# ==========================================
# Лабораторная работа №6 — Расширенные методы регрессии
# 1) Ridge-регрессия  (blood.xlsx)
# 2) Квантильная регрессия (Kuiper.xls)
# 3) Нелинейные регрессии (cigarettes.dat.txt)
# ==========================================

# install.packages("readxl")
# install.packages("car")
# install.packages("MASS")
# install.packages("glmnet")
# install.packages("quantreg")
# install.packages("lmtest")

library(readxl)
library(car)
library(MASS)
library(glmnet)
library(quantreg)
library(lmtest)

# =========================================================
# 1) RIDGE-регрессия: blood.xlsx
# Модель: X1 ~ X2 + X3 (X1 — давление, X2 — возраст, X3 — вес)
# =========================================================

blood_raw <- read_excel("/Users/ivan/IT/Mat_Statistic/Data/blood.xlsx")
blood <- as.data.frame(lapply(blood_raw, function(x) as.numeric(gsub(",", ".", as.character(x)))))
colnames(blood) <- c("X1","X2","X3")
blood <- na.omit(blood)

str(blood)
summary(blood)

# --- Обычная линейная модель ---
ols_blood <- lm(X1 ~ X2 + X3, data = blood)
summary(ols_blood)
car::vif(ols_blood)

# --- Ridge через glmnet (устойчивый способ) ---
X <- as.matrix(blood[, c("X2","X3")])
y <- blood$X1

cv_ridge <- cv.glmnet(X, y, alpha = 0)
plot(cv_ridge)
cv_ridge$lambda.min
cv_ridge$lambda.1se

ridge_best <- glmnet(X, y, alpha = 0, lambda = cv_ridge$lambda.min)
coef(ridge_best)

# --- Сравнение с OLS по MSE ---
pred_ridge <- as.numeric(predict(ridge_best, newx = X))
mse_ridge  <- mean((y - pred_ridge)^2)
mse_ols    <- mean(resid(ols_blood)^2)
mse_ols; mse_ridge

# --- Диагностика OLS ---
par(mfrow = c(2,2))
plot(ols_blood)
bptest(ols_blood)
dwtest(ols_blood)

# =========================================================
# 2) Квантильная регрессия: Kuiper.xls
# Модель: Price ~ Mileage + Liter + Cruise + Sound + Leather
# =========================================================

kuiper <- read_excel("/Users/ivan/IT/Mat_Statistic/Data/Kuiper.xls")
kuiper <- kuiper[, c("Price", "Mileage", "Liter", "Cruise", "Sound", "Leather")]
kuiper$Price   <- as.numeric(kuiper$Price)
kuiper$Mileage <- as.numeric(kuiper$Mileage)
kuiper$Liter   <- as.numeric(kuiper$Liter)
kuiper$Cruise  <- as.numeric(kuiper$Cruise)
kuiper$Sound   <- as.numeric(kuiper$Sound)
kuiper$Leather <- as.numeric(kuiper$Leather)
kuiper <- na.omit(kuiper)

str(kuiper)
summary(kuiper)

# --- OLS для сравнения ---
ols_k <- lm(Price ~ Mileage + Liter + Cruise + Sound + Leather, data = kuiper)
summary(ols_k)
AIC(ols_k)

# --- Квантильная регрессия для τ = 0.25, 0.5, 0.75 ---
rq25 <- rq(Price ~ Mileage + Liter + Cruise + Sound + Leather, data = kuiper, tau = 0.25)
rq50 <- rq(Price ~ Mileage + Liter + Cruise + Sound + Leather, data = kuiper, tau = 0.50)
rq75 <- rq(Price ~ Mileage + Liter + Cruise + Sound + Leather, data = kuiper, tau = 0.75)

summary(rq25, se = "nid")
summary(rq50, se = "nid")
summary(rq75, se = "nid")

# --- График коэффициента Mileage по квантилям ---
taus <- c(0.25, 0.5, 0.75)
beta_mileage <- c(coef(rq25)["Mileage"], coef(rq50)["Mileage"], coef(rq75)["Mileage"])
plot(taus, beta_mileage, type = "b", xlab = "τ", ylab = "β_Mileage",
     main = "Коэффициент Mileage по квантилям")
abline(h = coef(ols_k)["Mileage"], col = "red", lwd = 2)

# --- График прямых квантильных регрессий ---
plot(kuiper$Mileage, kuiper$Price, xlab = "Mileage", ylab = "Price",
     main = "Квантильные линии vs OLS")
abline(rq(Price ~ Mileage, data = kuiper, tau = 0.25), col = "blue")
abline(rq(Price ~ Mileage, data = kuiper, tau = 0.50), col = "purple")
abline(rq(Price ~ Mileage, data = kuiper, tau = 0.75), col = "darkgreen")
abline(lm(Price ~ Mileage, data = kuiper), col = "red", lwd = 2)

# --- Сравнение OLS и медианной регрессии ---
AIC(ols_k)
AIC(rq50)

# =========================================================
# 3) Нелинейные регрессии: cigarettes.dat.txt
# y = carbon_monoxide, x1 = tar, x2 = nicotine, x3 = weight
# =========================================================

cig <- read.table("/Users/ivan/IT/Mat_Statistic/Data/cigarettes.dat.txt",
                  header = FALSE, sep = "", fill = TRUE, stringsAsFactors = FALSE,
                  colClasses = c("character","numeric","numeric","numeric","numeric"))
colnames(cig) <- c("Brand","carbon_monoxide","tar","nicotine","weight")
cig <- cig[, c("carbon_monoxide","tar","nicotine","weight")]
cig <- na.omit(cig)

str(cig)
summary(cig)

# --- Линейная модель ---
m_lin <- lm(carbon_monoxide ~ tar + nicotine + weight, data = cig)
summary(m_lin); AIC(m_lin)

# --- Квадратичная модель ---
cig$tar2 <- cig$tar^2
cig$nic2 <- cig$nicotine^2
cig$w2   <- cig$weight^2

m_quad <- lm(carbon_monoxide ~ tar + nicotine + weight + tar2 + nic2 + w2 +
               tar:nicotine + tar:weight + nicotine:weight, data = cig)
summary(m_quad); AIC(m_quad)

# --- Кубическая модель ---
cig$tar3 <- cig$tar^3
cig$nic3 <- cig$nicotine^3
cig$w3   <- cig$weight^3

m_cub <- lm(carbon_monoxide ~ tar + nicotine + weight + tar2 + nic2 + w2 +
              tar3 + nic3 + w3, data = cig)
summary(m_cub); AIC(m_cub)

# --- Сравнение моделей по Adj.R² и AIC ---
adjR2_lin  <- summary(m_lin)$adj.r.squared
adjR2_quad <- summary(m_quad)$adj.r.squared
adjR2_cub  <- summary(m_cub)$adj.r.squared
adjR2_lin; adjR2_quad; adjR2_cub
AIC(m_lin); AIC(m_quad); AIC(m_cub)

# --- Выбор лучшей модели ---
best_model <- m_lin; best_name <- "Linear"; best_aic <- AIC(m_lin)
if (AIC(m_quad) < best_aic) { best_model <- m_quad; best_name <- "Quadratic"; best_aic <- AIC(m_quad) }
if (AIC(m_cub)  < best_aic) { best_model <- m_cub;  best_name <- "Cubic";     best_aic <- AIC(m_cub) }
best_name

# --- Диагностика лучшей модели ---
par(mfrow = c(2,2))
plot(best_model)
bptest(best_model)
dwtest(best_model)