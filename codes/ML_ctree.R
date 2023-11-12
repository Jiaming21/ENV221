################################################################################
##############################      加载R包      ###############################
################################################################################
library(e1071) #支持向量机（SVM）模型
library(ROCR) #R接收者操作特征（ROC）曲线
library(pROC) #机器学习算准确度要用的包
library(dplyr) #特征选择，数据框操作
library(m6ALogisticModel) #独立开发的机器学习包
library(SummarizedExperiment) #基因组数据处理
library(MLmetrics) #模型评估
library(caret) #交叉验证
#绘制箱线图
library(ggplot2)
################################################################################
###########################     设置工作目录      ##############################
################################################################################
setwd('/Users/jiaming/Desktop/ENV221/GroupWork/datas')
################################################################################
############################      数据分析      ################################
################################################################################
#读取原始数据
data <- read.csv('./diabetes.csv')
data <- data[, !(colnames(data) %in% c("Glucose", "BloodPressure"))]
################################################################################
########################      挑出正负样本数据      ############################
################################################################################
Nsample <- data[data$Outcome == 0, ][1:268, ]
nrow(Nsample) # 268 # 215
Nsample <- Nsample[, !(colnames(Nsample) %in% c("Outcome"))]

Psample <- data[data$Outcome == 1, ]
nrow(Psample) # 268 # 215
Psample <- Psample[, !(colnames(Psample) %in% c("Outcome"))]
################################################################################
#############################    搭建结果表    #################################
################################################################################
final_result <- as.data.frame(matrix(data = NA,10,10))
rownames(final_result) <- c("AUROC_5fcv","Sn_5fcv","Sp_5fcv","ACC_5fcv","MCC_5fcv","AUROC","Sn","Sp","ACC","MCC")
################################################################################
#############################     训练模型     #################################
################################################################################
for(i in 1:10) {
  print(paste0('round_',i,'_starts'))
  #将正样本80% (215) 作为训练集，20% (53) 作为测试集
  set.seed(i)
  train_P_indx <- sample(1:nrow(Psample),215) 
  train_P <- Psample[train_P_indx,]
  test_P_indx <- setdiff(1:nrow(Psample),train_P_indx)
  test_P <- Psample[test_P_indx,]
  #将负样本取80% (215) 作为训练集，20% (53) 作为测试集
  train_N_indx <- sample(1:nrow(Nsample),215) 
  train_N <- Nsample[train_N_indx,]
  test_N_indx <- setdiff(1:nrow(Nsample),train_N_indx)
  test_N <- Nsample[test_N_indx,]
  #将train_P和train_N合并起来，打上标签
  train_data <- rbind(train_P,train_N)
  label_train <- c(rep("diabetes",nrow(train_P)),rep("non_diabetes",nrow(train_N)))
  label_train <- factor(label_train,labels=c("diabetes","non_diabetes"))
  train_data$label <- label_train
  #将test_P和test_N合并起来，打上标签
  test_data <- rbind(test_P,test_N)
  label_test <- c(rep(1,(nrow(test_P))),rep(0,(nrow(test_N))))
  test_data$label <- label_test
  #超参设定
  fitControl <- trainControl(method = "cv",  # 使用的验证类型        
                             number = 5,     # 折叠数 
                             savePred=TRUE,  # 在训练过程中是否应保存预测
                             summaryFunction = twoClassSummary, # 指定了一个应用于总结交叉验证过程结果的函数
                             classProbs = TRUE) # 指定是否应计算并返回类概率
  conservation <- train(label ~ ., data = train_data, 
                        method = "ctree", #随机森林："rf" #朴素贝叶斯："nb" #通用线性模型："glm"
                        preProc = c("center", "scale"), # 代表自变量预处理方法,the data are centered and scaled
                        #数据的中心化是指数据集中的各项数据减去数据集的均值
                        #标准化是指中心化之后的数据在除以数据集的标准差，即数据集中的各项数据减去数据集的均值再除以数据集的标准差
                        trControl = fitControl) # trControl：定义函数运行参数的列表
  # saveRDS(conservation,file = './conservation.model.nb.deleted.rds')
  #评估模型（训练内评估：CV）
  # conservation = readRDS('./conservation.model.nb.deleted.rds')
  conf_matrix <- confusionMatrix(conservation$pred$pred, conservation$pred$obs) 
  Matt_Coef <- function (conf_matrix){
    TP <- conf_matrix$table[1,1]
    TN <- conf_matrix$table[2,2]
    FP <- conf_matrix$table[1,2]
    FN <- conf_matrix$table[2,1]
    
    mcc_num <- TP*TN - FP*FN
    mcc_den <- as.double((TP+FP))*as.double((TP+FN))*as.double((TN+FP))*as.double((TN+FN))
    
    mcc_final <- mcc_num/sqrt(mcc_den)
    return(mcc_final)
  }
  mcc <- Matt_Coef(conf_matrix)
  #填入结果表
  final_result[1,i] <- mean(conservation$results[2,]$ROC)
  final_result[2,i] <- mean(conservation$results[2,]$Sens)
  final_result[3,i] <- mean(conservation$results[2,]$Spec)
  final_result[4,i] <- conf_matrix$overall[1]
  final_result[5,i] <- mcc
  #评估模型（测试集评估）
  pred <- predict(conservation, newdata = test_data, type = "prob")
  # print(length(label_test) == nrow(pred))
  BIOmotifvsnon_testppred <- prediction(pred$diabetes,label_test)
  BIOmotifvsnon_testpppred_auc <- performance(BIOmotifvsnon_testppred,"auc")
  final_result[6,i] <- BIOmotifvsnon_testpppred_auc@y.values[[1]][1]
  
  result <- as.data.frame(matrix(data=NA,2,2))
  result[1,1] <- length(which(pred[1:(length(label_test)/2),1] > 0.5)) #前面一半数据是positive，得到预测positie的样本个数，TP
  result[1,2] <- length(which(pred[1:(length(label_test)/2),1] < 0.5)) #FN
  result[2,1] <- length(which(pred[((length(label_test)/2) + 1):length(label_test),1] > 0.5)) #FP
  result[2,2] <- length(which(pred[((length(label_test)/2) + 1):length(label_test),1] < 0.5)) #TN
  #填入结果表
  final_result[7,i] <- result[1,1]/( result[1,1] + result[1,2] )
  final_result[8,i] <- result[2,2]/( result[2,1] + result[2,2] )
  final_result[9,i] <- ( result[1,1] + result[2,2] )/ nrow(test_data)
  final_result[10,i] <- (result[1,1]*result[2,2]-result[1,2]*result[2,1])/
    (sqrt(as.numeric(result[1,1]+result[2,1])*(result[1,1]+result[1,2])*(result[2,2]+result[2,1])*(result[2,2]+result[1,2])))
  
  print(final_result)
  print(paste0("Round_",i,"_finished"))
}

library(pROC)
# Plot ROC Curve
roc_data <- roc(label_test, pred$diabetes)

sensitivities <- roc_data$sensitivities
specificities <- 1 - roc_data$specificities

FPR_ctree <- 1 - specificities
FNR_ctree <- 1 - sensitivities

write.csv(FPR_ctree, "../datas/FPR_ctree.csv")
write.csv(FNR_ctree, "../datas/FNR_ctree.csv")
write.csv(conservation$results[2,]$ROC, "../datas/auc_value_ctree.csv")

