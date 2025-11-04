# ==========================================
# Лабораторная №5 — Логистическая регрессия
# Датасет: Doctor.xlsx (y, x1..x5)
# ==========================================

# install.packages("readxl")
# install.packages("MASS")
# install.packages("lmtest")
# install.packages("pROC")
# install.packages("caret")

library(readxl)
library(MASS)
library(lmtest)
library(pROC)
library(caret)

# -----------------------------
# 1) Чтение данных
# -----------------------------
doc <- read_excel("/Users/ivan/IT/Mat_Statistic/Data/Doctor.xlsx")

# Исправим возможные запятые на точки и убедимся, что всё numeric
doc$x2 <- as.numeric(gsub(",", ".", doc$x2))
doc$x3 <- as.numeric(gsub(",", ".", doc$x3))
doc$x5 <- as.numeric(gsub(",", ".", doc$x5))

str(doc)
summary(doc)

# -----------------------------
# 2) Зависимая переменная (0/1)
# -----------------------------
Y_num <- doc$y
Y_fac <- factor(ifelse(Y_num == 1, "1", "0"), levels = c("0","1"))

# -----------------------------
# 3) Базовая логистическая модель
# -----------------------------
m_logit <- glm(Y_num ~ x1 + x2 + x3 + x4 + x5,
               data = doc, family = binomial(link = "logit"))

summary(m_logit)
coef(m_logit)
confint(m_logit)
confint.default(m_logit)

# Уравнение: log(p/(1-p)) = β0 + β1*x1 + β2*x2 + β3*x3 + β4*x4 + β5*x5

# -----------------------------
# 4) Значимость модели целиком
# -----------------------------
m_null <- glm(Y_num ~ 1, data = doc, family = binomial(link = "logit"))
lrtest(m_null, m_logit)
waldtest(m_logit)

# -----------------------------
# 5) Пробит-модель
# -----------------------------
m_probit <- glm(Y_num ~ x1 + x2 + x3 + x4 + x5,
                data = doc, family = binomial(link = "probit"))
summary(m_probit)
AIC(m_logit); AIC(m_probit)

# -----------------------------
# 6) Прогнозы, матрица ошибок (порог 0.5)
# -----------------------------
p_hat <- predict(m_logit, type = "response")
pred_05 <- factor(ifelse(p_hat >= 0.5, "1", "0"), levels = c("0","1"))
confusionMatrix(pred_05, Y_fac, positive = "1")

# Функции для чувствительности/специфичности
sens_at <- function(actual01, prob, thr) mean(prob[actual01 == 1] >= thr)
spec_at <- function(actual01, prob, thr) mean(prob[actual01 == 0] <  thr)

sens_at(Y_num, p_hat, 0.5)
spec_at(Y_num, p_hat, 0.5)

# -----------------------------
# 7) Оптимальный порог (Youden)
# -----------------------------
roc_obj <- roc(response = Y_fac, predictor = p_hat, levels = c("0","1"), direction = "<")
plot(roc_obj, main = "ROC curve (logit)")
auc(roc_obj)

best <- coords(roc_obj, "best", ret = c("threshold","sensitivity","specificity"), best.method = "youden")
best

# Убираем NA перед вычислениями
valid_idx <- complete.cases(p_hat, Y_fac)
p_hat_valid <- p_hat[valid_idx]
Y_fac_valid <- Y_fac[valid_idx]

# Оптимальный порог из ROC
best <- coords(roc_obj, "best", ret = c("threshold","sensitivity","specificity"), 
               best.method = "youden", transpose = FALSE)
thr_opt <- as.numeric(best["threshold"])   # гарантированно число

# Предсказания при оптимальном пороге
pred_opt <- factor(ifelse(p_hat_valid >= thr_opt, "1", "0"), levels = c("0","1"))

# Таблица ошибок
confusionMatrix(pred_opt, Y_fac_valid, positive = "1")

sens_at(Y_num, p_hat, thr_opt)
spec_at(Y_num, p_hat, thr_opt)

# -----------------------------
# 8) Улучшение модели (stepAIC)
# -----------------------------
m_step <- stepAIC(m_logit, direction = "both", trace = FALSE)
summary(m_step)
AIC(m_logit); AIC(m_step)

# Повторим ROC для улучшенной модели
p_hat_step <- predict(m_step, type = "response")
roc_step <- roc(response = Y_fac, predictor = p_hat_step, levels = c("0","1"), direction = "<")
plot(roc_step, main = "ROC (stepAIC)")
auc(roc_step)

# -----------------------------
# 9) Диагностика
# -----------------------------
# Псевдо-R^2
1 - (m_logit$deviance / m_logit$null.deviance)

# Остатки, влияние
res_dev <- residuals(m_logit, type = "deviance")
cooksd <- cooks.distance(m_logit)
hatv <- hatvalues(m_logit)
cbind(cooksd, hatv)[order(-cooksd), ][1:10, ]