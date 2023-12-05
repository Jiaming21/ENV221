grades <- read.csv("/Users/jiaming/Desktop/ENV221/GroupWork/datas/diabetes.csv",header=TRUE,sep=",")
target <- grades[,1:9]
target <- target[,-2]
target$Outcome <-  factor(target$Outcome, levels = c(0, 1))
# names(target) # 查看列名
change_target <- target

# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("RColorBrewer")
# install.packages("scale")

library(ggplot2)
library(reshape2)
library(RColorBrewer)
# library(scale)

newtarget <- melt(change_target, #需要转换的数据集名称
               id.vars=c("Outcome"), #保留的主字段
               variable.name="All_var", #转换后分类维度
               value.name="All_value") #转换后的度量值名称
options(digits=21)
newtarget$All_var <- factor(newtarget$All_var)
newtarget$P_value[newtarget$All_var == 'Pregnancies'] <- 5.07 * (10^-10)
newtarget$log <- log(newtarget$All_value)

newtarget$scale[newtarget$All_var == 'Pregnancies'] <- scale(newtarget$All_value[newtarget$All_var == 'Pregnancies'], center = TRUE, scale = TRUE)
newtarget$scale[newtarget$All_var == 'Glucose'] <- scale(newtarget$All_value[newtarget$All_var == 'Glucose'], center = TRUE, scale = TRUE)
newtarget$scale[newtarget$All_var == 'BloodPressure'] <- scale(newtarget$All_value[newtarget$All_var == 'BloodPressure'], center = TRUE, scale = TRUE)
newtarget$scale[newtarget$All_var == 'SkinThickness'] <- scale(newtarget$All_value[newtarget$All_var == 'SkinThickness'], center = TRUE, scale = TRUE)
newtarget$scale[newtarget$All_var == 'Insulin'] <- scale(newtarget$All_value[newtarget$All_var == 'Insulin'], center = TRUE, scale = TRUE)
newtarget$scale[newtarget$All_var == 'BMI'] <- scale(newtarget$All_value[newtarget$All_var == 'BMI'], center = TRUE, scale = TRUE)
newtarget$scale[newtarget$All_var == 'DiabetesPedigreeFunction'] <- scale(newtarget$All_value[newtarget$All_var == 'DiabetesPedigreeFunction'], center = TRUE, scale = TRUE)
newtarget$scale[newtarget$All_var == 'Age'] <- scale(newtarget$All_value[newtarget$All_var == 'Age'], center = TRUE, scale = TRUE)

newtarget$P_value[newtarget$All_var == 'Pregnancies'] <- 5.065 * (10^-10)
#newtarget$P_value[newtarget$All_var == 'Glucose'] <- 8.935 * (10^-43)
newtarget$P_value[newtarget$All_var == 'BloodPressure'] <- 0.072
newtarget$P_value[newtarget$All_var == 'SkinThickness'] <- 0.038
newtarget$P_value[newtarget$All_var == 'Insulin'] <- 2.862 * (10^-4)
newtarget$P_value[newtarget$All_var == 'BMI'] <- 1.230 * (10^-16)
newtarget$P_value[newtarget$All_var == 'DiabetesPedigreeFunction'] <- 1.255 * (10^-06)
newtarget$P_value[newtarget$All_var == 'Age'] <- 2.210 * 10^(-11)

#pdf("~/WMM1/diff_volcano.pdf", width = 20, height = 16)
ggplot(newtarget, aes(x = All_var, y = scale, fill=Outcome)) +
  geom_boxplot() + #outlier.shape = NA
  geom_text(aes(All_var, y = 6, label = paste0("P = ", P_value)), data = newtarget, inherit.aes = F) + 
  scale_fill_manual(values = c("darkgreen", "orange")) +
  scale_y_continuous(breaks = c(-1,0,1,2,3,4,5,6), labels = c(-1,0,1,2,3,4,5,6), limits = c(-1,6)) +
  xlab(NULL) +
  ylab("FPI") + 
  #coord_flip() +  
  theme_classic() 
  #facet_grid(All_var~.) 
#dev.off()

#pdf("~/WMM1/diff_volcano.pdf", width = 20, height = 16)
ggplot(newtarget, aes(x = All_var, y = scale, fill=Outcome)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(outlier.shape = NA) + #outlier.shape = NA
  geom_text(aes(All_var, y = 3.5, label = paste0("P = ", P_value)), data = newtarget, inherit.aes = F) + 
  scale_fill_manual(values = c("pink", "lightblue")) +
  #scale_fill_manual(values = c("blue", "red"))
  #scale_fill_brewer(palette = "Set3") +
  
  #scale_fill_viridis_d() +
  scale_y_continuous(breaks = c(-1,0,1,2,3,4), labels = c(-1,0,1,2,3,4), limits = c(-1,4)) +
  xlab(NULL) +
  ylab('Standardized Value') + 
  
  coord_flip() +  
  theme_classic() 
#facet_grid(All_var~.) 
#dev.off()

# ggplot(newtarget, aes(x = All_var, y = scale, fill=Outcome)) +
#   geom_violin(alpha = 0.2) +
#   #geom_boxplot(outlier.shape = NA) + #outlier.shape = NA
#   geom_text(aes(All_var, y = 3.5, label = paste0("P = ", P_value)), data = newtarget, inherit.aes = F) + 
#   scale_fill_manual(values = c("pink", "lightblue")) +
#   #scale_fill_manual(values = c("blue", "red"))
#   #scale_fill_brewer(palette = "Set3") +
#   
#   #scale_fill_viridis_d() +
#   scale_y_continuous(breaks = c(-1,0,1,2,3,4), labels = c(-1,0,1,2,3,4), limits = c(-1,4)) +
#   xlab(NULL) +
#   ylab('Standardized Value') + 
#   
#   coord_flip() +  
#   theme_classic() 

ggplot(newtarget, aes(x = All_var, y = scale, fill = Outcome)) +
  geom_violin(alpha = 0.2) +
  geom_boxplot(width = 0.3, position = position_dodge(width = 0.75), outlier.shape = NA) +
  geom_text(aes(All_var, y = 3.5, label = paste0("P = ", P_value)),
            data = newtarget, inherit.aes = FALSE) +
  scale_fill_manual(values = c("pink", "lightblue")) +
  scale_y_continuous(breaks = c(-1, 0, 1, 2, 3, 4), labels = c(-1, 0, 1, 2, 3, 4), limits = c(-1, 4)) +
  xlab(NULL) +
  ylab('Standardized Value') +
  coord_flip() +
  theme_classic()



################################################################################

ggplot(newtarget, aes(x = All_value, fill=Outcome)) +
  geom_density() +                   # Create density plot
  facet_grid(All_var ~ .) +          # Facet the plot based on All_var
  xlim(0, 200) +                     # Set x-axis limits
  ylim(0, 0.1) +                     # Set y-axis limits
  scale_y_continuous(
    breaks = seq(0, 0.1, 0.02),      # Set y-axis breaks
    labels = seq(0, 0.1, 0.02),      # Set y-axis labels
    limits = c(0, 0.1)                # Set y-axis limits
  )


#geom_violin(alpha = 0.2) +
#geom_boxplot(outlier.shape = NA) + #outlier.shape = NA
#geom_text(aes(All_var, y = 3.5, label = paste0("P = ", P_value)), data = newtarget, inherit.aes = F) + 
#scale_fill_manual(values = c("pink", "lightblue")) +
#scale_fill_manual(values = c("blue", "red"))
#scale_fill_brewer(palette = "Set3") +

#scale_fill_viridis_d() +http://127.0.0.1:30911/graphics/6c75f5fd-c95e-48ce-b5bd-e2030ce3988f.png
scale_y_continuous(breaks = c(-1,0,1,2,3,4), labels = c(-1,0,1,2,3,4), limits = c(-1,4)) +

  xlab(NULL) +
  ylab('Standardized Value') 

#coord_flip() +  
#theme_classic() 
