# import libraries
library(dplyr)
library(MASS)
library(car)
library(glmnet)
library(gam)

# read data
orders = read.csv("C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/orders.csv")
target = read.csv("C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/target.csv")

# checking orders data
head(orders)

# add total price column to orders
orders = orders %>% 
  mutate(total_price = qty * price) # add total_price column

head(orders)

# feature engineering

# time on file (first order was when)
tof_df = orders %>%
  group_by(id) %>%
  summarise(tof = max(t))

# recency (latest order was when)
rec_df = orders %>%
  group_by(id) %>%
  summarize(rec = min(t))

# total number of books
tot_items_df = orders %>%
  group_by(id) %>%
  summarise(freq_item = sum(qty))

# frequency (number of orders)
freq_df = orders %>%
  group_by(id) %>%
  summarise(freq_order = n_distinct(ordnum))

# total money (overall for a customer)
total_spend_df = orders %>%
  group_by(id) %>%
  summarize(total_cust_spend = sum(total_price))

# monetary (avg order qty size and price)
mon_df = orders %>%
  group_by(id, ordnum) %>%
  summarise(sum_qty = sum(qty), sum_total_price = sum(total_price)) %>%
  group_by(id) %>%
  summarise(avg_qty = mean(sum_qty), avg_price = mean(sum_total_price))

# diversity of categories
div_cat_df = orders %>%
  group_by(id) %>%
  summarise(div_cat = n_distinct(category))

# entropy (diversity in amount of purchases from all categories (~0.5 means diverse))
entropy_df = orders %>%
  group_by(id, category) %>%
  summarise(total_qty = sum(qty)) %>% 
  mutate(ent_freq = total_qty / sum(total_qty)) %>%
  group_by(id) %>% 
  summarise(entropy = -sum(ent_freq * log(ent_freq)))

avg_cat_df = orders %>%
  group_by(id, ordnum) %>%
  summarise(cat_count_ord = n_distinct(category)) %>% 
  group_by(id) %>%
  summarise(avg_cat = mean(cat_count_ord))

# combining all data frames
list_df = list(tot_items_df, freq_df, mon_df, total_spend_df, div_cat_df, tof_df, rec_df, entropy_df, avg_cat_df)
rfm_df = Reduce(function(x, y) merge(x, y, all.x = TRUE), list_df)

# checking stats for rfm created
count(rfm_df)
summary(rfm_df)
head(rfm_df)
length(unique(orders[["id"]]))

rfm_df[, -c(1)] <- scale(rfm_df[, -c(1)])

# combining rfm df and logtarg
master_data = merge(rfm_df, target, by = "id", all.x = TRUE)

# create train and test set
train = !is.na(master_data$logtarg)

# fit simple linear regression
fit_1 = lm(logtarg ~ log(freq_item + 1) + freq_order + log(avg_qty + 1) + log(avg_price + 1) + log(total_cust_spend + 1) + div_cat + tof + rec + entropy + avg_cat, data = master_data, subset = train)
summary(fit_1)
vif(fit_1)
yhat_1 = predict(fit_1, master_data[!train,])
ans_1 = data.frame(id = master_data$id[!train], logtarg = yhat_1)
ans_1$logtarg = ifelse(ans_1$logtarg < 0, 0, ans_1$logtarg)
write.csv(ans_1, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/testanswer_3.csv", row.names = F)

# fit stepwise backward
fit_2 <- stepAIC(fit_1, direction = c("backward"))
summary(fit_2)
vif(fit_2)
yhat_2 = predict(fit_2, master_data[!train,])
ans_2 = data.frame(id = master_data$id[!train], logtarg = yhat_2)
ans_2$logtarg = ifelse(ans_2$logtarg < 0, 0, ans_2$logtarg)
write.csv(ans_2, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/testanswer_4.csv", row.names = F)

# lasso regression
options(na.action="na.pass")
x = model.matrix(logtarg ~ .-id, master_data)
fit.lasso = glmnet(x[train, ], master_data$logtarg[train], alpha = 1)
plot(fit.lasso, xvar="lambda")
fit_3.cv = cv.glmnet(x[train, ], master_data$logtarg[train], alpha = 1, nfold = 3, lambda = seq(0, 10, 0.01))
yhat_3 = predict(fit.lasso, s = fit_3.cv$lambda.min, newx = x[!train, ])
ans_3 = data.frame(id = master_data$id[!train], logtarg = yhat_3)
ans_3$X1 = ifelse(ans_3$X1 < 0, 0, ans_3$X1)
write.csv(ans_3, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/testanswer_5.csv", row.names = F)

# stepwise gams
library(gam)
fit_4 = gam(logtarg ~ 1, data = master_data, subset = train)
fit_5 = step.Gam(fit_4, scope=list(
  "freq_item"=~1+freq_item+s(freq_item),
  "avg_qty"=~1+avg_qty+s(avg_qty),
  "avg_price"=~1+avg_price+s(avg_price),
  "total_cust_spend"=~1+total_cust_spend+s(total_cust_spend),
  "freq_order"=~1+freq_order+s(freq_order),
  "div_cat"=~1+div_cat+s(div_cat),
  "tof"=~1+tof+s(tof),
  "rec"=~1+rec+s(rec),
  "entropy"=~1+entropy+s(entropy),
  "avg_cat"=~1+avg_cat+s(avg_cat)
))
summary(fit_5)
yhat_4 = predict(fit_5, master_data[!train,])
ans_4 = data.frame(id = master_data$id[!train], logtarg = yhat_4)
ans_4$logtarg = ifelse(ans_4$logtarg < 0, 0, ans_4$logtarg)
write.csv(ans_4, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/testanswer_6.csv", row.names = F)
