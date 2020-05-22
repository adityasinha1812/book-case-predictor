# import libraries
library(dplyr)
library(ggplot2)
library(MASS)

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

# total number of items
tot_items_df = orders %>%
  group_by(id) %>%
  summarise(freq_item = n())

# frequency (number of orders)
freq_df = orders %>%
  group_by(id) %>%
  summarise(freq_order = n_distinct(ordnum))

# monetary (avg order qty size and price)
mon_df = orders %>%
  group_by(id, ordnum) %>%
  summarise(sum_qty = sum(qty), sum_total_price = sum(total_price)) %>%
  group_by(id) %>%
  summarise(avg_qty = mean(sum_qty), avg_price = mean(sum_total_price))

# total money (overall for a customer)
total_spend_df = orders %>%
  group_by(id) %>%
  summarize(total_cust_spend = sum(total_price))

# diversity of categories
div_cat_df = orders %>%
  group_by(id) %>%
  summarise(div_cat = n_distinct(category))

# combining all data frames
list_df = list(tot_items_df, freq_df, mon_df, total_spend_df, div_cat_df, tof_df, rec_df)
rfm_df = Reduce(function(x, y) merge(x, y, all.x = TRUE), list_df)

# checking stats for rfm created
count(rfm_df)
summary(rfm_df)

# combining rfm df and logtarg
master_data = merge(rfm_df, target, by = "id", all.x = TRUE)

# create train and test set
train = !is.na(master_data$logtarg)

# fit simple linear regression
fit_1 = lm(logtarg ~ freq_item + freq_order + log(avg_qty + 1) + log(avg_price + 1) + log(total_cust_spend + 1) + div_cat + tof + rec, data = master_data, subset = train)
summary(fit_1)
vif(fit_1)
yhat_1 = predict(fit_1, master_data[!train,])
ans_1 = data.frame(id = master_data$id[!train], logtarg = yhat_1)
ans_1$logtarg = ifelse(ans_1$logtarg < 0, 0, ans_1$logtarg)
write.csv(ans_1, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/testanswer_1.csv", row.names = F)

# fit stepwise backward
fit_2 <- stepAIC(fit_1, direction = c("backward"))
summary(fit_2)
vif(fit_2)
yhat_2 = predict(fit_2, master_data[!train,])
ans_2 = data.frame(id = master_data$id[!train], logtarg = yhat_2)
ans_2$logtarg = ifelse(ans_2$logtarg < 0, 0, ans_2$logtarg)
write.csv(ans_2, "C:/Users/adity/Downloads/Courses/Spring 2020/MSIT 423 - Data Science/Kaggle Competition/testanswer_2.csv", row.names = F)
