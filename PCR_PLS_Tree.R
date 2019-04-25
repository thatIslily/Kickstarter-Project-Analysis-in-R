data = read.csv("df_kickstarterFE.csv")
#remove nan
data = na.omit(data)

#log values
data$usd_pledged = log1p(data$usd_pledged)
data$goal = log1p(data$goal)

#remove char and state
newData <- data[-c(1,6,8,13)]
head(newData)

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
pcr.fit = pcr(usd_pledged~., data= training, scale=T, validation = "CV", segments=10)
# plot cross-validation scores
validationplot(pcr.fit,val.type="MSEP") #MSEP = mean squared error of prediction
#here we see that best number of components = 8
#in red -- bias-corrected cross-validation error

#obtain predictions using # comps = 8
x = model.matrix(usd_pledged~., data= validation)[,-9] #deletes column "usd_pledged" from matrix
pcr.pred = predict(pcr.fit, x, ncomp=8)
# calculate prediction errors (mean squared error)
mean((pcr.pred - y.validation)^2) #31541.57
# calculate accuracy
validation$pcr = ifelse(pcr.pred>validation$goal,1,0) 
mean(validation$pcr!= y.state) #0.8060296

##################### Partial Least Squares (PLS) Using 10-fold-CV ########################################
install.packages("pls")
library(pls)
pls.fit = plsr(usd_pledged~., data= training, scale=T, validation = "CV", segments=10)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit,validation,ncomp=3)
mean((pls.pred - y.validation)^2) #8.242222
# calculate accuracy
validation$pls = ifelse(pls.pred>validation$goal,1,0)
mean(validation$pls!= y.state) #0.5065027

############################################ Decision Tree###############################################
# Load rpart and rpart.plot
library(MASS)
tree.pldg=tree(usd_pledged~.,training)
cv.pldg=cv.tree(tree.pldg)
plot(cv.pldg$size,cv.pldg$dev,type='b')
prune.pldg=prune.tree(tree.pldg,best=7)

# Prediction
yhat=predict(prune.pldg,newdata=validation)
mean((yhat-y.validation)^2) #1.029845

# calculate accuracy
validation$tree = ifelse(yhat>validation$goal,1,0)
mean(validation$tree != y.state) #0.7620265
