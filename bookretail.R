library(tidyverse)
library(car)
library(MASS)

# read data
orders = read.csv("C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/orders.csv")
target = read.csv("C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/target.csv")

# t in years
orders$t_year = orders$t / 365.25

# basic rfm variables
rfm1 = orders %>%
  group_by(id) %>%
  summarise(tof = max(t_year), 
    r = min(t_year), 
    fitem = n(), 
    ford = n_distinct(ordnum), 
    m = sum(price * qty),
    m_wgtd = sum(price * qty / (1.5 ** t_year)),
    fitem2 = sum(qty),
    fcat = n_distinct(category),
    fbrand = n_distinct(brandid)) %>%
  mutate(m_per_order = m / ford,
         m_per_item = m / fitem,
         tof_per_item = tof / fitem,
         tof_per_order = tof / ford,
         item_per_order = fitem / ford)

rfm1$active24 = ifelse(rfm1$r <= 2, 1, 0) 
rfm1$active12 = ifelse(rfm1$r <= 1, 1, 0) 
rfm1$active4 = ifelse(rfm1$r <= 0.3, 1, 0)

# cross tab for categories freq
cats = sort(unique(orders$category))
rfm2 = orders %>%
  group_by(id, category) %>%
  summarise(cf = n()) %>%
  spread(category, cf, fill = 0)  %>%
  setNames(c("id", paste("cf", cats, sep = "")))

# cross tab for brands freq
brands = sort(unique(orders$brandid))
rfm3 = orders %>%
  group_by(id, brandid) %>%
  summarise(bf = n()) %>%
  spread(brandid, bf, fill = 0)  %>%
  setNames(c("id", paste("bf", brands, sep = "")))

# cross tab for categories spend
rfm4 = orders %>%
  group_by(id, category) %>%
  summarise(cm = sum(price * qty)) %>%
  spread(category, cm, fill = 0)  %>%
  setNames(c("id", paste("cm", cats, sep = "")))

# cross tab for brand spend
rfm5 = orders %>%
  group_by(id, brandid) %>%
  summarise(bm = sum(price * qty)) %>%
  spread(brandid, bm, fill = 0)  %>%
  setNames(c("id", paste("bm", brands, sep = "")))

# join with target
master_data = left_join(target, rfm1, by = "id") %>%
  left_join(rfm2, by = "id") %>%
  left_join(rfm3, by = "id") %>%
  left_join(rfm4, by = "id") %>%
  left_join(rfm5, by = "id")

summary(master_data)
names(master_data)

for(i in 3:87) master_data[[i]] = log(master_data[[i]] + 1)

train = !is.na(master_data$logtarg)

library(Boruta)
set.seed(12345)
Boruta.train <- Boruta(logtarg ~ ., data = master_data[train, ], doTrace = 2, ntree = 700, maxRuns = 20)
print(Boruta.train)
features <- names(master_data)[(which(Boruta.train$finalDecision == "Confirmed"))]  
master_data2 <- master_data[, c(features)]
features

fit1 = lm(logtarg ~ . -id, data = master_data2, subset = train)
summary(fit1)
vif(fit1)

fit2 <- stepAIC(fit1, direction = c("backward"))
summary(fit2)
vif(fit2)

yhat2 = predict(fit2, master_data2[!train,])
ans2 = data.frame(id = master_data2$id[!train], logtarg = yhat2)
ans2$logtarg = ifelse(ans2$logtarg < 0, 0, ans2$logtarg)
write.csv(ans2, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer1.csv", row.names = F)

fit3 = lm(logtarg ~ 1, data = master_data2, subset = train)
master_data_flag = filter(master_data2[train, ], logtarg > 0)
final_formula <- formula(lm(logtarg ~ (.-id)^2, data = master_data_flag))
final_formula
fit4 <- step(fit3, direction = "both", scope = final_formula)

summary(fit4)
yhat3 = predict(fit4, master_data2[!train,])
ans3 = data.frame(id = master_data2$id[!train], logtarg = yhat3)
ans3$logtarg = ifelse(ans3$logtarg < 0, 0, ans3$logtarg)
write.csv(ans3, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer2.csv", row.names = F)

master_data3 = master_data2
master_data3$flag = as.numeric(master_data3$logtarg > 0)

logfit1 = glm(flag ~ 1 -logtarg, data = master_data3[train, ], family = binomial)
logformula = formula(glm(flag ~ (.-id -logtarg)^2, data = master_data3[train, ], family = binomial))
logfit2 = step(logfit1, direction = "both", scope = logformula)
summary(logfit2)

phat <- predict(logfit2, master_data3[!train,], type = "response")
ans4 = data.frame(id = master_data3$id[!train], logtarg = yhat3 * phat)
ans4$logtarg = ifelse(ans4$logtarg < 0, 0, ans4$logtarg)
write.csv(ans4, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer3.csv", row.names = F)

#############################################################
set.seed(12345)
Boruta.train2 <- Boruta(logtarg ~ ., data = master_data[train, ], doTrace = 2, ntree = 1000, maxRuns = 20)
print(Boruta.train2)
features2 <- names(master_data)[(which(Boruta.train2$finalDecision == "Confirmed"))]  
master_data4 <- master_data[, c(features2)]
features2
#best entry so far
gamfit1 = gam(logtarg ~ .-id, data = master_data4, subset = train)
gamfit2 = step.Gam(gamfit1, scope=list(
  "tof"=~1+tof+s(tof),
  "r"=~1+r+s(r),
  "fitem"=~1+fitem+s(fitem),
  "ford"=~1+ford+s(ford),
  "m"=~1+m+s(m),
  "m_wgtd"=~1+m_wgtd+s(m_wgtd),
  "fitem2"=~1+fitem2+s(fitem2),
  "fcat"=~1+fcat+s(fcat),
  "fbrand"=~1+fbrand,
  "m_per_order"=~1+m_per_order+(m_per_order),
  "m_per_item"=~1+m_per_item+(m_per_item),
  "tof_per_item"=~1+tof_per_item+(tof_per_item),
  "tof_per_order"=~1+tof_per_order+(tof_per_order),
  "active24"=~1+active24,
  "active12"=~1+active12,
  "active4"=~1+active4,
  "cf0"=~1+cf0+s(cf0),
  "cf1"=~1+cf1+s(cf1),
  "cf3"=~1+cf3+s(cf3),
  "cf6"=~1+cf6+s(cf6),
  "cf7"=~1+cf7+s(cf7),
  "cf9"=~1+cf9+s(cf9),
  "cf10"=~1+cf10+s(cf10),
  "cf12"=~1+cf12+s(cf12),
  "cf17"=~1+cf17+s(cf17),
  "cf19"=~1+cf19+s(cf19),
  "cf22"=~1+cf22+s(cf22),
  "cf23"=~1+cf23+s(cf23),
  "cf30"=~1+cf30+s(cf30),
  "cf31"=~1+cf31+s(cf31),
  "cf35"=~1+cf35+s(cf35),
  "cf36"=~1+cf36+s(cf36),
  "cm39"=~1+cm39+s(cm39),
  "cm40"=~1+cm40+s(cm40),
  "cm41"=~1+cm41+s(cm41),
  "cm44"=~1+cm44+s(cm44),
  "cm99"=~1+cm99+s(cm99),
  "bf1"=~1+bf1+s(bf1),
  "bf3"=~1+bf3+s(bf3),
  "bf4"=~1+bf4+s(bf4),
  "cm0"=~1+cm0+s(cm0),
  "cm1"=~1+cm1+s(cm1),
  "cm3"=~1+cm3+s(cm3),
  "cm6"=~1+cm6+s(cm6),
  "cm7"=~1+cm7+s(cm7),
  "cm9"=~1+cm9+s(cm9),
  "cm10"=~1+cm10+s(cm10),
  "cm12"=~1+cm12+s(cm12),
  "cm17"=~1+cm17+s(cm17),
  "cm19"=~1+cm19+s(cm19),
  "cm22"=~1+cm22+s(cm22),
  "cm23"=~1+cm23+s(cm23),
  "cm30"=~1+cm30+s(cm30),
  "cm31"=~1+cm31+s(cm31),
  "cm39"=~1+cm39+s(cm39),
  "cm40"=~1+cm40+s(cm40),
  "cm41"=~1+cm41+s(cm41),
  "cm44"=~1+cm44+s(cm44),
  "cm50"=~1+cm50+s(cm50),
  "cm99"=~1+cm99+s(cm99),
  "bm1"=~1+bm1+s(bm1),
  "bm3"=~1+bm3+s(bm3)
))

gamyhat = predict(gamfit2, master_data4[!train,])
gamans = data.frame(id = master_data4$id[!train], logtarg = gamyhat)
gamans$logtarg = ifelse(gamans$logtarg < 0, 0, gamans$logtarg)
write.csv(gamans, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer13.csv", row.names = F)



fit5 = lm(logtarg ~ .-id, data = master_data4, subset = train)
#master_data_flag = filter(master_data4[train, ], logtarg > 0)
final_formula <- formula(lm(logtarg ~ (.-id)^2, data = master_data4[train, ]))
final_formula
fit6 <- step(fit5, direction = "both", scope = final_formula)
summary(fit6)
yhat4 = predict(fit6, master_data4[!train,])
ans5 = data.frame(id = master_data4$id[!train], logtarg = yhat4)
ans5$logtarg = ifelse(ans5$logtarg < 0, 0, ans5$logtarg)
write.csv(ans5, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer4.csv", row.names = F)





# do not overwrite
master_data_flag2 = filter(master_data4[train, ], logtarg > 0)
final_formula2 <- formula(lm(logtarg ~ (.-id)^2, data = master_data_flag2))
final_formula2
fit6 <- stepAIC(logtarg ~ tof + r + ford + m_wgtd + fitem2 + fcat + fbrand + 
               m_per_order + m_per_item + tof_per_item + tof_per_order + 
               active24 + active4 + cf0 + cf1 + cf6 + cf7 + cf9 + cf17 + 
               cf19 + cf22 + cf23 + cf30 + cf31 + cf35 + cf36 + cf39 + cf40 + 
               cf41 + cf44 + cf99 + bf1 + bf3 + bf4 + cm1 + cm3 + cm6 + 
               cm7 + cm9 + cm10 + cm12 + cm17 + cm19 + cm22 + cm23 + cm30 + 
               cm31 + cm39 + cm40 + cm41 + cm44 + cm50 + cm99 + bm1 + bm3 + 
               r:tof_per_item + m_wgtd:bf4 + m_per_order:cm39 + cm7:cm31 + 
               cf39:cm50 + tof:cf17 + tof:bf1 + m_per_item:cf7 + cm12:bm3 + 
               bf3:cm17 + cm17:cm39 + cm6:bm1 + m_per_order:cm50 + cm30:cm31 + 
               cf36:cf40 + cf36:cm22 + fcat:cm99 + fbrand:cf22 + cf31:cm30 + 
               ford:cf19 + tof_per_order:bf4 + bf3:cm44 + cf0:cf44 + cm7:cm50 + 
               cf1:cm22 + cf22:cm44 + cm3:cm7 + cf40:cm3 + ford:tof_per_item + 
               cf6:bm3 + cf30:cm17 + cm30:cm41 + active4:cm50 + cf0:cm44 + 
               m_per_order:cf39 + cf17:cm1 + cm9:cm99 + active24:bf3 + cf19:cm30 + 
               active24:cm23 + cm9:cm10 + cm7:cm23 + active4:cf22 + cm31:bm3 + 
               tof_per_order:cf22 + cf0:cm40 + cm6:cm44 + cm10:cm41 + cm19:cm41 + 
               cf40:cm41 + cf1:cm50 + tof_per_item:cm50 + tof_per_order:cm50 + 
               cf23:cm50 + m_per_order:cm99 + m_per_item:cm99 + ford:fcat + 
               active24:cm31 + active24:cf7 + fcat:tof_per_order + tof:tof_per_order + 
               r:bf1 + active4:cf19 + active4:bf3 + m_wgtd:cm3 + cf17:cm3 + 
               ford:cf35 + cf0:cf35, direction = "backward")
summary(fit6)
yhat5 = predict(fit6, master_data4[!train,])
ans6 = data.frame(id = master_data4$id[!train], logtarg = yhat5)
ans6$logtarg = ifelse(ans6$logtarg < 0, 0, ans6$logtarg)
write.csv(ans6, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer5.csv", row.names = F)


fit = lm(logtarg ~ tof + r + ford + m_wgtd + fitem2 + fcat + fbrand + 
           m_per_order + m_per_item + tof_per_item + tof_per_order + 
           active24 + active4 + cf0 + cf1 + cf6 + cf7 + cf9 + cf17 + 
           cf19 + cf22 + cf23 + cf30 + cf31 + cf35 + cf36 + cf39 + cf40 + 
           cf41 + cf44 + cf99 + bf1 + bf3 + bf4 + cm1 + cm3 + cm6 + 
           cm7 + cm9 + cm10 + cm12 + cm17 + cm19 + cm22 + cm23 + cm30 + 
           cm31 + cm39 + cm40 + cm41 + cm44 + cm50 + cm99 + bm1 + bm3 + 
           r:tof_per_item + m_wgtd:bf4 + m_per_order:cm39 + cm7:cm31 + 
           cf39:cm50 + tof:cf17 + tof:bf1 + m_per_item:cf7 + cm12:bm3 + 
           bf3:cm17 + cm17:cm39 + cm6:bm1 + m_per_order:cm50 + cm30:cm31 + 
           cf36:cf40 + cf36:cm22 + fcat:cm99 + fbrand:cf22 + cf31:cm30 + 
           ford:cf19 + tof_per_order:bf4 + bf3:cm44 + cf0:cf44 + cm7:cm50 + 
           cf1:cm22 + cf22:cm44 + cm3:cm7 + cf40:cm3 + ford:tof_per_item + 
           cf6:bm3 + cf30:cm17 + cm30:cm41 + active4:cm50 + cf0:cm44 + 
           m_per_order:cf39 + cf17:cm1 + cm9:cm99 + active24:bf3 + cf19:cm30 + 
           active24:cm23 + cm9:cm10 + cm7:cm23 + active4:cf22 + cm31:bm3 + 
           tof_per_order:cf22 + cf0:cm40 + cm6:cm44 + cm10:cm41 + cm19:cm41 + 
           cf40:cm41 + cf1:cm50 + tof_per_item:cm50 + tof_per_order:cm50 + 
           cf23:cm50 + m_per_order:cm99 + m_per_item:cm99 + ford:fcat + 
           active24:cm31 + active24:cf7 + fcat:tof_per_order + tof:tof_per_order + 
           r:bf1 + active4:cf19 + active4:bf3 + m_wgtd:cm3 + cf17:cm3 + 
           ford:cf35 + cf0:cf35, data=master_data4, subset = train)
summary(fit)
fit2 <- stepAIC(fit, direction = c("backward"))
summary(fit2)
yhat5 = predict(fit2, master_data4[!train,])
ans6 = data.frame(id = master_data4$id[!train], logtarg = yhat5)
ans6$logtarg = ifelse(ans6$logtarg < 0, 0, ans6$logtarg)
write.csv(ans6, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer11.csv", row.names = F)

names(master_data4)

library(glmnet)
set.seed(12345)
lassofit <- cv.glmnet(data.matrix(master_data4[train, 3:64]), master_data4[train, ]$logtarg, alpha = 1, nfolds = 100, lambda = seq(0,1,0.001))
lassofit$lambda.min
coef(lassofit,s="lambda.min")

lassopred <- predict(lassofit, newx = data.matrix(master_data4[!train, 3:64]), s = "lambda.min")
lassopred
ans7 = data.frame(id = master_data4$id[!train], logtarg = lassopred)
ans7
ans7$X1 = ifelse(ans7$X1 < 0, 0, ans7$X1)
write.csv(ans7, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer10.csv", row.names = F)

names(master_data2)
set.seed(12345)
lassofit <- cv.glmnet(data.matrix(master_data2[train, 3:54]), master_data2[train, ]$logtarg, alpha = 1, nfolds = 100, lambda = seq(0,1,0.001))
lassofit$lambda.min
coef(lassofit,s="lambda.min")

lassopred <- predict(lassofit, newx = data.matrix(master_data2[!train, 3:54]), s = "lambda.min")
lassopred
ans7 = data.frame(id = master_data2$id[!train], logtarg = lassopred)
ans7
ans7$X1 = ifelse(ans7$X1 < 0, 0, ans7$X1)
write.csv(ans7, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer9.csv", row.names = F)

as.matrix(coef(lassofit, lassofit$lambda.min))

fit = lm(logtarg ~ tof_per_item+
         m_wgtd+
         bf1+
         active4+
         cm99+
         cf12+
         cf9+
         cf41+
         fbrand+
         cf19+
         ford+
         cf31+
         cm17+
         cm7+
         active12+
         cm12+
         cf0+
         cf17+
         fitem2+
         cm0-id
         , data=master_data4, subset = train)
library(splines)

splinefit = lm(logtarg ~ cf99 + bs(tof) + r + fitem + bs(m_wgtd) + fbrand + m_per_order + tof_per_item + 
                 tof_per_order + active24 + cf9 + cf23 + bs(cf35) +  bs(cm40) + cm99 + bf1 + bf3 + 
                 bf4 + cm1 + cm9 + bs(cm17) +      cm22 + cm23 + bs(cm30) + cm40 + cm50 ,
               data = master_data4, subset = train)
summary(splinefit)

splinefit2 <- stepAIC(splinefit, direction = c("backward"))
summary(splinefit2)
vif(splinefit2)

splineyhat2 = predict(splinefit2, master_data4[!train,])
splineans2 = data.frame(id = master_data2$id[!train], logtarg = splineyhat2)
splineans2$logtarg = ifelse(splineans2$logtarg < 0, 0, splineans2$logtarg)
write.csv(splineans2, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer14.csv", row.names = F)



gamfit3 = gam(logtarg ~ .-id, data = master_data4, subset = train)
gamfit4 = step.Gam(gamfit3, scope=list(
  "tof"=~1+tof+s(tof),
  "r"=~1+r+s(r),
  "fitem"=~1+fitem+s(fitem),
  "m_wgtd"=~1+m_wgtd+s(m_wgtd),
  "fbrand"=~1+fbrand,
  "active24"=~1+active24,
  "m_per_order"=~1+m_per_order+(m_per_order),
  "tof_per_item"=~1+tof_per_item+(tof_per_item),
  "cf9"=~1+cf9+s(cf9),
  "cf23"=~1+cf23+s(cf23),
  "cf35"=~1+cf35+s(cf35),
  "cf99"=~1+cf99+s(cf99),
  "cm40"=~1+cm40+s(cm40),
  "cm99"=~1+cm99+s(cm99),
  "bf1"=~1+bf1+s(bf1),
  "bf3"=~1+bf3+s(bf3),
  "bf4"=~1+bf4+s(bf4),
  "cm1"=~1+cm1+s(cm1),
  "cm9"=~1+cm9+s(cm9),
  "cm17"=~1+cm17+s(cm17),
  "cm22"=~1+cm22+s(cm22),
  "cm23"=~1+cm23+s(cm23),
  "cm30"=~1+cm30+s(cm30),
  "cm40"=~1+cm40+s(cm40),
  "cm50"=~1+cm50+s(cm50),
  "bm1"=~1+bm1+s(bm1),
  "bm3"=~1+bm3+s(bm3)
))

gamyhat2 = predict(gamfit4, master_data4[!train,])
gamans2 = data.frame(id = master_data4$id[!train], logtarg = gamyhat2)
gamans2$logtarg = ifelse(gamans2$logtarg < 0, 0, gamans2$logtarg)
write.csv(gamans2, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer15.csv", row.names = F)

splinefit5 = lm(logtarg ~ cf99 + bs(tof) + r + fitem + bs(m_wgtd) + fbrand + m_per_order +      tof_per_item + tof_per_order + active24 + cf9 + cf23 + bs(cf35) 
          +  bs(cm40) + cm99 + bf1 + bf3 + bf4 + cm1 + cm9 + bs(cm17) +  cm22 + cm23 + bs(cm30) + cm40 + cm50 -id, data = master_data4, subset = train)
master_data_flag = filter(master_data4[train, ], logtarg > 0)
splinefinal_formula <- formula(lm(logtarg ~ (cf99 + bs(tof) + r + fitem + bs(m_wgtd) + fbrand + m_per_order +      tof_per_item + tof_per_order + active24 + cf9 + cf23 + bs(cf35) 
                                             +  bs(cm40) + cm99 + bf1 + bf3 + bf4 + cm1 + cm9 + bs(cm17) +  cm22 + cm23 + bs(cm30) + cm40 + cm50 -id)^2, data = master_data_flag))
splinefinal_formula
splinefit6 <- step(splinefit5, direction = "both", scope = splinefinal_formula)
summary(splinefit6)
splineyhat4 = predict(splinefit6, master_data4[!train,])
splineans5 = data.frame(id = master_data4$id[!train], logtarg = splineyhat4)
splineans5$logtarg = ifelse(splineans5$logtarg < 0, 0, splineans5$logtarg)
write.csv(splineans5, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/Final/testanswer16.csv", row.names = F)
