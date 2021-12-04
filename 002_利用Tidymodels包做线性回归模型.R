#####################################
#Tidymodels包学习项目
#第2集 利用tidymodels包做线性回归模型
#####################################

# 01 线性回归模型推导

# 02 线性回归模型案例
library(tidyverse)
library(tidymodels)
library(vip)

# 加载数据集
advertising <- read_rds(url('https://gmudatamining.com/data/advertising.rds'))
home_sales <- read_rds(url('https://gmudatamining.com/data/home_sales.rds')) %>% 
  select(-selling_date)

advertising %>% glimpse()
home_sales %>% glimpse()

# 数据集划分
# 训练集和测试集
set.seed(314)

advertising_split <- initial_split(advertising, prop = 0.75, 
                                   strata = Sales)
# 训练集
advertising_training <- advertising_split %>% 
  training()
# 测试集
advertising_test <- advertising_split %>% 
  testing()

# 使用parsnip包，格式统一化
# Pick a model type
# Set the engine
# Set the mode (either regression or classification)

lm_model <- linear_reg() %>% 
  set_engine('lm') %>% # adds lm implementation of linear regression
  set_mode('regression')

lm_model

# 训练集拟合线性回归模型
# 使用parsnip包的fit函数
# 设置3个参数

# a parnsip model object specification
# a model formula
# a data frame with the training data

lm_fit <- lm_model %>% 
  fit(Sales ~ ., data = advertising_training)
lm_fit

# 分析或者探索训练集的结果
names(lm_fit)

summary(lm_fit$fit)

# 训练回归模型的诊断信息
par(mfrow=c(2, 2))
plot(lm_fit$fit, pch = 16, col = "#006EA1")

# 训练结果的整洁格式
# yardstick包的tidy函数
# 或者
# parsnip包的glance函数
yardstick::tidy(lm_fit)
parsnip::glance(lm_fit)

# 变量重要性分析
vip(lm_fit)

# 评价测试集上面的准确率
# 模型的泛化能力
# parnsip包的predict函数

# a trained parnsip model object
# new_data for which to generate predictions

predict(lm_fit, new_data = advertising_test)

advertising_test_results <- predict(lm_fit, new_data = advertising_test) %>% 
  bind_cols(advertising_test)
advertising_test_results

# 计算测试集的RMSE和R^2
# yardstick包的rmse函数

yardstick::rmse(advertising_test_results, 
     truth = Sales,
     estimate = .pred)

yardstick::rsq(advertising_test_results,
    truth = Sales,
    estimate = .pred)

# 效果的可视化
# R^2 Plot
# 理想情况下 y = x
ggplot(data = advertising_test_results,
       mapping = aes(x = .pred, y = Sales)) +
  geom_point(color = '#006EA1') +
  geom_abline(intercept = 0, slope = 1, color = 'orange') +
  labs(title = 'Linear Regression Results - Advertising Test Set',
       x = 'Predicted Sales',
       y = 'Actual Sales')

# 升级版
# 创建一个机器学习工作流
# 第一步：Split Our Data
set.seed(314)

# Create a split object
advertising_split <- initial_split(advertising, prop = 0.75, 
                                   strata = Sales)

# Build training data set
advertising_training <- advertising_split %>% 
  training()

# Build testing data set
advertising_test <- advertising_split %>% 
  testing()

# 第二步：特征工程
advertising_recipe <- recipe(Sales ~ ., data = advertising_training) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

# 第三步：Specify a Model
lm_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

# 第四步：创建工作流
# 使用workflow包
# we start with workflow() to create an empty workflow and then add out model and recipe with add_model() and add_recipe().
advertising_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(advertising_recipe)

# 第五步：执行工作流
advertising_fit <- advertising_workflow %>% 
  last_fit(split = advertising_split)

# 模型性能分析
advertising_fit %>% collect_metrics()

# 测试集预测的结果
# Obtain test set predictions data frame
test_results <- advertising_fit %>% 
  collect_predictions()
# View results
test_results







# 学习资料：
# https://www.gmudatamining.com/lesson-10-r-tutorial.html
