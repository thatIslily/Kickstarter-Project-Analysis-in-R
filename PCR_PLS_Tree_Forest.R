data = read.csv("df_kickstarterFE.csv")

#remove nan
data = na.omit(data)

#remove char and state
rmvdData <- data[-c(1,6,8,13)]

#remove duplicated rows
library(dplyr)
deduped.data = unique(rmvdData [,1:ncol(rmvdData)])

#log values
deduped.data $usd_pledged = log1p(deduped.data $usd_pledged)
deduped.data $goal = log1p(deduped.data $goal)

newData=deduped.data 

# Divide training and test sets
set.seed(1)
training_id = sample(seq_len(nrow(newData)), size = nrow(newData)*0.5)
ntrain_id = setdiff(1:nrow(newData), training_id)
validation_id = sample(ntrain_id, nrow(newData)*0.25)
test_id = setdiff(ntrain_id,validation_id)

training = newData[training_id,]
validation = newData[validation_id,]
test = newData[test_id,]
summary(training)
y=newData$usd_pledged
y.validation = y[validation_id]
y.state = data$state[validation_id]

##################### Principal Component Regression (PCR) Using 10-fold-CV ################################
install.packages('pcr')
library(pcr)
install.packages("pls")
library(pls)
pcr.fit = pcr(usd_pledged~., data= training, scale=T, validation = "CV", segments=10)
# plot cross-validation scores
validationplot(pcr.fit,val.type="MSEP") #MSEP = mean squared error of prediction
#here we see that best number of components = 8
#in red -- bias-corrected cross-validation error

#obtain predictions using # comps = 8
x = model.matrix(usd_pledged~., data= validation)[,-9] #deletes column "usd_pledged" from matrix
pcr.pred = predict(pcr.fit, x, ncomp=8)
# calculate prediction errors (mean squared error)
mean((pcr.pred - y.validation)^2) #14869.63
# calculate accuracy
validation$pcr = ifelse(pcr.pred>validation$goal,1,0) 
mean(validation$pcr!= y.state) #0.5218444

############################calculate profits##########################
          #senario1 where users are wrong and we also give wrong prediction 
                       # this won't influnce user's gain/loss
s1 = (test$goal>=test$usd_pledged) & (pcr.pred>=test$usd_pledged)
#probability of s1
p1 = length(s1[s1 == TRUE])/nrow(test)

          #senario2 where users are wrong and we give correct suggestion
s2=(test$goal>=test$usd_pledged) & (pcr.pred<test$usd_pledged)
#probability of s2
p2 = length(s2[s2 == TRUE])/nrow(test)
#the gain for users are the values predicted 
gain1=sum(pcr.pred[s2])*1.064 #sum(pred_subset[gain])

          #senario3 where users are correct and we give incorrect suggestion
s3=(test$goal<test$usd_pledged) & (pcr.pred>=test$usd_pledged)
#probability of s3
p3 = length(s3[s3 == TRUE])/nrow(test)
#the loss for users are the value actually funded
loss1=sum(test$usd_pledged[s3])

          #senario4 where users are correct and we also predict correct direction 
                              #but the value are lower than user's goal 
s4=(test$goal<test$usd_pledged) & (pcr.pred<test$usd_pledged) 
#probability of s4
p4 = length(s4[s4 == TRUE])/nrow(test)
#calculate loss
loss2=sum(test$usd_pledged[s4]-pcr.pred[s4]*1.064)
p1+p2+p3+p4
  
#          #senario5 where users are correct and we also predict correct direction 
                              #but the value are higher than user's goal 
#s5=(test$goal<test$usd_pledged) & (pcr.pred>test$goal) 
##probability of s5
#p5 = length(s5[s5 == TRUE])/nrow(test)
##calcultae gain
#gain2 = sum(pcr.pred[s5]-test$goal[s5])

#total gain and loss
gain_sum = gain1
loss_sum = loss1+loss2
total = gain_sum - loss_sum
total -145421.2
##################### Partial Least Squares (PLS) Using 10-fold-CV ########################################
pls.fit = plsr(usd_pledged~., data= training, scale=T, validation = "CV", segments=10)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit,validation,ncomp=3)
mean((pls.pred - y.validation)^2) #8.378075
# calculate accuracy
validation$pls = ifelse(pls.pred>validation$goal,1,0)
mean(validation$pls!= y.state) # 0.4212695

############################calculate profits##########################
#senario1 where users are wrong and we also give wrong prediction 
# this won't influnce user's gain/loss
s1 = (test$goal>=test$usd_pledged) & (pls.pred>=test$usd_pledged)
#probability of s1
p1 = length(s1[s1 == TRUE])/nrow(test)

#senario2 where users are wrong and we give correct suggestion
s2=(test$goal>=test$usd_pledged) & (pls.pred<test$usd_pledged)
#probability of s2
p2 = length(s2[s2 == TRUE])/nrow(test)
#the gain for users are the values predicted 
gain1=sum(pls.pred[s2])*1.064 #sum(pred_subset[gain])

#senario3 where users are correct and we give incorrect suggestion
s3=(test$goal<test$usd_pledged) & (pls.pred>=test$usd_pledged)
#probability of s3
p3 = length(s3[s3 == TRUE])/nrow(test)
#the loss for users are the value actually funded
loss1=sum(test$usd_pledged[s3])

#senario4 where users are correct and we also predict correct direction 
#but the value are lower than user's goal 
s4=(test$goal<test$usd_pledged) & (pls.pred<test$usd_pledged) 
#probability of s4
p4 = length(s4[s4 == TRUE])/nrow(test)
#calculate loss
loss2=sum(test$usd_pledged[s4]-pls.pred[s4]*1.064)
p1+p2+p3+p4

#          #senario5 where users are correct and we also predict correct direction 
#but the value are higher than user's goal 
#s5=(test$goal<test$usd_pledged) & (pcr.pred>test$goal) 
##probability of s5
#p5 = length(s5[s5 == TRUE])/nrow(test)
##calcultae gain
#gain2 = sum(pcr.pred[s5]-test$goal[s5])

#total gain and loss
gain_sum = gain1
loss_sum = loss1+loss2
total = gain_sum - loss_sum
total #-26507.21

############################################ Decision Tree ###############################################
# Load rpart and rpart.plot
install.packages("tree")
library(tree)
library(MASS)
library(ISLR)
library(rpart)
tree.pldg=tree(usd_pledged~.,training)
cv.pldg=cv.tree(tree.pldg)
plot(cv.pldg$size,cv.pldg$dev,type='b')
prune.pldg=prune.tree(tree.pldg,best=cv.pldg$size[which.min(cv.pldg$dev)])

# Prediction
yhat=predict(prune.pldg,newdata=validation)
mean((yhat-y.validation)^2) #1.036756

# calculate accuracy
validation$tree = ifelse(yhat>validation$goal,1,0)
mean(validation$tree != y.state) #0.4681237

############################calculate profits##########################
#senario1 where users are wrong and we also give wrong prediction 
# this won't influnce user's gain/loss
s1 = (test$goal>=test$usd_pledged) & (yhat>=test$usd_pledged)
#probability of s1
p1 = length(s1[s1 == TRUE])/nrow(test)

#senario2 where users are wrong and we give correct suggestion
s2=(test$goal>=test$usd_pledged) & (yhat<test$usd_pledged)
#probability of s2
p2 = length(s2[s2 == TRUE])/nrow(test)
#the gain for users are the values predicted 
gain1=sum(yhat[s2])*1.064 #sum(pred_subset[gain])

#senario3 where users are correct and we give incorrect suggestion
s3=(test$goal<test$usd_pledged) & (yhat>=test$usd_pledged)
#probability of s3
p3 = length(s3[s3 == TRUE])/nrow(test)
#the loss for users are the value actually funded
loss1=sum(test$usd_pledged[s3])

#senario4 where users are correct and we also predict correct direction 
#but the value are lower than user's goal 
s4=(test$goal<test$usd_pledged) & (yhat<test$usd_pledged) 
#probability of s4
p4 = length(s4[s4 == TRUE])/nrow(test)
#calculate loss
loss2=sum(test$usd_pledged[s4]-yhat[s4]*1.064)
p1+p2+p3+p4

#          #senario5 where users are correct and we also predict correct direction 
#but the value are higher than user's goal 
#s5=(test$goal<test$usd_pledged) & (pcr.pred>test$goal) 
##probability of s5
#p5 = length(s5[s5 == TRUE])/nrow(test)
##calcultae gain
#gain2 = sum(pcr.pred[s5]-test$goal[s5])

#total gain and loss
gain_sum = gain1
loss_sum = loss1+loss2
total = gain_sum - loss_sum
total #-78960.19
########################################## Random Forrest #################################################
install.packages("randomForest")
library(randomForest)
rf.pldg=randomForest(usd_pledged~.,data=training,mtry=6,ntree = 10, importance=TRUE)
# Prediction
yhat.rf = predict(rf.pldg,newdata=validation)
mean((yhat.rf-y.validation)^2) #0.6997271
# Calculate accuracy
validation$rf = ifelse(yhat.rf>validation$goal,1,0)
mean(validation$rf != y.state) #0.495147
# Most important predictor
importance(rf.pldg)
varImpPlot(rf.pldg)

############################calculate profits##########################
#senario1 where users are wrong and we also give wrong prediction 
# this won't influnce user's gain/loss
s1 = (test$goal>=test$usd_pledged) & (yhat.rf>=test$usd_pledged)
#probability of s1
p1 = length(s1[s1 == TRUE])/nrow(test)

#senario2 where users are wrong and we give correct suggestion
s2=(test$goal>=test$usd_pledged) & (yhat.rf<test$usd_pledged)
#probability of s2
p2 = length(s2[s2 == TRUE])/nrow(test)
#the gain for users are the values predicted 
gain1=sum(yhat.rf[s2])*1.064 #sum(pred_subset[gain])

#senario3 where users are correct and we give incorrect suggestion
s3=(test$goal<test$usd_pledged) & (yhat.rf>=test$usd_pledged)
#probability of s3
p3 = length(s3[s3 == TRUE])/nrow(test)
#the loss for users are the value actually funded
loss1=sum(test$usd_pledged[s3])

#senario4 where users are correct and we also predict correct direction 
#but the value are lower than user's goal 
s4=(test$goal<test$usd_pledged) & (yhat.rf<test$usd_pledged) 
#probability of s4
p4 = length(s4[s4 == TRUE])/nrow(test)
#calculate loss
loss2=sum(test$usd_pledged[s4]-yhat.rf[s4]*1.064)
p1+p2+p3+p4

#          #senario5 where users are correct and we also predict correct direction 
#but the value are higher than user's goal 
#s5=(test$goal<test$usd_pledged) & (pcr.pred>test$goal) 
##probability of s5
#p5 = length(s5[s5 == TRUE])/nrow(test)
##calcultae gain
#gain2 = sum(pcr.pred[s5]-test$goal[s5])

#total gain and loss
gain_sum = gain1
loss_sum = loss1+loss2
total = gain_sum - loss_sum
total #-79460.37
