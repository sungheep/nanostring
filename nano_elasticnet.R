library(caret)
library(glmnet)
library(pROC)

# Split the data into training and test set
training.samples <- createDataPartition(m4$Miller_payne_grade,p = 0.7, list = FALSE)
train.data  <- m4[training.samples, ]
test.data <- m4[-training.samples, ]

train.data <- read.csv('/data/users/gb/Asan/survival/nanostring/Rawdata/tr-te-split/X_train_data.csv',header=T,sep = '\t')
train.info <- read.csv('/data/users/gb/Asan/survival/nanostring/Rawdata/tr-te-split/Y_train_info.csv',header=T,sep = '\t')
test.data <- read.csv('/data/users/gb/Asan/survival/nanostring/Rawdata/tr-te-split/X_test_data.csv',header=T,sep = '\t')
test.info <- read.csv('/data/users/gb/Asan/survival/nanostring/Rawdata/tr-te-split/Y_test_info.csv',header=T,sep = '\t')

rownames(train.data) <- train.data[,1]
rownames(train.info) <- train.info[,1]
rownames(test.data) <- test.data[,1]
rownames(test.info) <- test.info[,1]
train.data <- train.data[-1]
train.info <- train.info[-1]
test.data <- test.data[-1]
test.info <- test.info[-1]

train.data$pCR <- train.info$pCR
test.data$pCR <- test.info$pCR

x <- model.matrix(pCR ~., train.data)[,-1]
y <- train.data$pCR



########### lasso ##############

lasso = cv.glmnet(x=x,y=y, alpha=1,nfolds = 10)

#plot(lasso)#,xvar="lambda")

lasso_coef <- coef(lasso,s=min(lasso$lambda))

x.test <- model.matrix(pCR ~., test.data)[,-1]

predlasso=predict(lasso, newx = x.test, s = min(lasso$lambda))#, type = "class")

multiclass.roc(test.data$pCR, as.numeric(predlasso))


##################### elastic net #################
library(doParallel)
a <- seq(0.2, 0.4, 0.1)
registerDoParallel(cores = 4)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(x, y, nfold = 10, type.measure = "deviance", paralle = T, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}

min_v <- search[search$cvm == min(search$cvm), ]
min_v$alpha

elastic = cv.glmnet(x=x,y=y, alpha = 0.95)#min_v$alpha)

elastic_coef <- coef(elastic,s=min(elastic$lambda))


predelastic=predict(elastic, newx = x.test, s = min(elastic$lambda))#, type = "class")

multiclass.roc(test.data$pCR, as.numeric(predelastic))

###################### ridge ################

ridge = cv.glmnet(x=x,y=y, alpha=0)

plot(ridge)#,xvar="lambda")

coef(ridge,s=min(ridge$lambda.min))

x.test <- model.matrix(Miller.Payne_grade ~., test.data)[,-1]

predridge=predict(ridge, newx = x.test, s = min(ridge$lambda))#, type = "class")

multiclass.roc(test.data$Miller.Payne_grade, predridge)

ridge = cv.glmnet(x=x2,y=y2, alpha=0)

plot(ridge)#,xvar="lambda")

coef(ridge,s=min(ridge$lambda.min))

x.test <- model.matrix(RCB ~., test2.data)[,-1]

predridge=predict(ridge, newx = x.test, s = min(ridge$lambda))#, type = "class")

multiclass.roc(test2.data$RCB, predridge)
