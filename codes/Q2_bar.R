library(e1071)

data <- read.csv("/Users/jiaming/Desktop/diabetes.csv")
head(data)

svm_model <- svm(Outcome ~ ., data = data, kernel = "linear")
summary(svm_model)

feature_weights <- coef(svm_model)
top_features <- head(sort(abs(feature_weights), decreasing = TRUE), 8)
print(top_features)

top_features <- as.data.frame(top_features)
print(top_features)

rownames <- rownames(top_features)
value <- top_features[,1]

library(ggplot2)
ggplot(data = top_features, aes(x = rownames, y = value)) +
  geom_bar(stat = "identity", fill = "blue")
