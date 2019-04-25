data = read.csv("df_kickstarterFE.csv")
#remove nan
data = na.omit(data)
#remove char
data <- data[-c(1,8,13)]
data$usd_pledged = log1p(data$usd_pledged)
head(data)

# Divide training and test sets
set.seed(1)
training_id = sample(seq_len(nrow(data)), size = nrow(data)*0.5)
ntrain_id = setdiff(1:nrow(data), training_id)
validation_id = sample(ntrain_id, nrow(data)*0.25)
test_id = setdiff(ntrain_id,validation_id)

training = data[training_id,]
validation = data[validation_id,]
test = data[test_id,]
summary(training)
y=data$usd_pledged
y.validation = y[validation_id]

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
mean((pcr.pred - y.validation)^2) #15861603
validation$pcr = ifelse(pcr.pred>validation$goal,1,0) 
mean(validation$pcr != y.validation) #0.9220983

##################### Partial Least Squares (PLS) Using 10-fold-CV ########################################
install.packages("pls")
library(pls)
pls.fit = plsr(usd_pledged~., data= training, scale=T, validation = "CV", segments=10)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit,validation,ncomp=8)
mean((pls.pred - y.validation)^2) #4.305774
validation$pls = ifelse(pls.pred>validation$goal,1,0)
mean(validation$pls!= y.validation) #0.9219394
############################################ Randon Forrest ###############################################
