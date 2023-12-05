# 创建箱线图

# 加载ggplot2包
library(ggplot2)

# 加载Iris数据集
data(iris)

# 创建箱线图，并添加 jitter
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(fill = "lightblue", color = "blue", width = 0.2) +
  geom_jitter(position = position_jitter(0.2), color = "red", size = 2) +
  labs(title = "Sepal Length Box Plot with Jitter by Species",
       x = "Species",
       y = "Sepal Length")

################################################################################

# 加载必要的库
library(ggplot2)
library(reshape2)

# 加载Iris数据集
data(iris)

# 将数据集重塑为长格式
iris_long <- melt(iris, id.vars = "Species", variable.name = "Variable", value.name = "Value")

# 创建箱线图并添加 jitter，分面显示
ggplot(iris_long, aes(x = Species, y = Value)) +
  geom_boxplot(fill = "lightblue", color = "blue", width = 0.2) +
  geom_jitter(position = position_jitter(0.3), color = "purple", size = 1) +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title = "Box Plots with Jitter by Species",
       x = "Species",
       y = "") +
  theme(strip.text = element_text(size = 10, face = "bold"))

################################################################################

library(ggridges)

# 加载Iris数据集
data(iris)

# 创建Ridgeline Plot
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges() +
  labs(title = "Ridgeline Plot of Sepal Length by Species",
       x = "Sepal Length") +
  theme_ridges()  # 使用ggridges的主题风格






