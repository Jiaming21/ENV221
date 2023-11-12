library(ggridges)

data <- read.csv("/Users/jiaming/Desktop/ENV221/GroupWork/diabetes.csv")
data <- data[,-c(2,9)]\

# 创建Ridgeline Plot
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges() +
  labs(title = "Ridgeline Plot of Sepal Length by Species",
       x = "Sepal Length") +
  theme_ridges()  # 使用ggridges的主题风格




