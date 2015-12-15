#Project 2: Power Performance
#Jason Cotrell
#Jay Buntemeyer
#Stephanie R Sheldon

#Read and check dataset
data.in=WindPACT1500kW
data.in$X <- NULL #Not a variable
head(data.in)
tail(data.in)
str(data.in)
dim(data.in)

#Load required packages
library(ggplot2)
library(tree)
library(randomForest)
library(survival)
library(splines)
library(lattice)
library(parallel)
library(gbm)

#Variable Descriptions:
  #Pimary Wind Characteristics
    #ws.HH  Wind speed at the turbine hub height
    #Ti.HH  Turbulence intensity (wind variability) at hub height
    #Shear  Wind shear exponent 

  #Other calculated wind characteristics that could be useful in predicting power
    #ws.eq  Wind speed equivalent that helps account for shear across the rotor
    #RSS    A metric that describes the difference between the measured velocity profile and an ideal power law profile
    #power.std Standard deviation of the power
  
  #Value we want to predict
   #power.mean Mean power over a 10 minute interval

#Exploratory EDA
    
  #It may be interesting to break the data into regions (II, III and possibly II.5) and do more EDA
    #Creating factors for the regions and making sure I did this right
    data.in$TOR = rep("I",NROW(data.in))  
    data.in$TOR[data.in$ws.HH <= 11.5] = "II"
    data.in$TOR[data.in$ws.HH > 11.5] = "III"
    data.in$TOR = as.factor(data.in$TOR)
    summary(data.in$TOR)
    str(data.in$TOR)
    head(data.in)
    qplot(data.in$ws.HH,data.in$TOR, main='TOR vs. Wind Speed')

    data.TOR.II=data.in[data.in$TOR == "II",]
    data.TOR.III=data.in[data.in$TOR == "III",] 
    
  #Univariate
    #Power
    summary(data.in$power.mean)
    qplot(data.in$power.mean) #Note it is running at rates alot of the time.
    boxplot(data.in$power.mean, main="Mean Power")
  
    qplot(data.in$power.std) #Note it is running at rates alot of the time.
    boxplot(data.in$power.std, main="Power Standard Deviation")
  
    #Wind Speed
    summary(data.in$ws.HH) #no negative values
    qplot(data.in$ws.HH, binwidth=.1) #most wind speeds are less than 20; there are spikes at 3 and 15 m/s
    boxplot(data.in$ws.HH, main="Wind Speed")
  
    qplot(data.in$Ti.HH,binwidth=.1)
    boxplot(data.in$Ti.HH, main="Turbulence Intensity")

  #Bivariate Plots looking for relationships to power
    #Windspeed
    qplot(data.in$ws.HH,data.in$power.mean, main='Power Vs. Wind Speed', color = data.in$TOR) #Substantial variation around the transition region
    qplot(data.in$ws.eq,data.in$power.mean, main='Power Vs. Wind Speed Equivalent') #Little variance until 10 m/s and after 19 m/s
    qplot(data.in$ws.HH,data.in$power.std, main='Power Std. Vs. Wind Speed')
    qplot(data.in$ws.eq,data.in$ws.HH, main='Wind Speed HH Vs. Wind Speed Equivalent', color = data.in$Shear); #At higher ws, ws.eq is higher for low shear 
  
    qplot(data.in$Ti.HH,data.in$power.mean, main='Power Vs. Turbulence', color = data.in$ws.HH) #Power does appear to drop off at high TI
    qplot(data.in$Ti.HH,data.in$power.mean, main='Power Vs. Turbulence', color = data.in$TOR)# Works for region III, but not II
    qplot(data.in$Shear,data.in$power.mean, main='Power Vs. Shear', color = data.in$TOR) # Does not appear to have a any effect
    qplot(data.in$RSS,data.in$power.mean, main='Power Vs. RSS', color = data.in$ws.HH) #Does appear to have an effect at higher RSS & High Wind Speeds
    qplot(data.in$RSS,data.in$power.mean, main='Power Vs. RSS', color = data.in$TOR)
    
    data.in$power.std <- NULL #Ultimately, not a predictor variable
    
    #Calc correlation coefficient and p-value for predictor variables
    panel.cor <- function(x, y, digits = 2, cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      
      # correlation coefficient
      r <- cor(x, y)
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste("r= ", txt, sep = "")
      text(0.5, 0.6, txt)
      
      # p-value calculation
      p <- cor.test(x, y)$p.value
      txt2 <- format(c(p, 0.123456789), digits = digits)[1]
      txt2 <- paste("p= ", txt2, sep = "")
      if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
      text(0.5, 0.4, txt2)
    }
    #View scatterplot matrix
    pairs(data.in[1:6], upper.panel = panel.cor, main='All Regions Scatterplot Matrix')
    data.in$ws.eq <- NULL #High corr with ws.HH
    pairs(data.in[1:5], upper.panel = panel.cor, main='All Regions Scatterplot Matrix')
    
    #View scatterplot matrix of region II only
    pairs(data.TOR.II[1:5], upper.panel = panel.cor, main='Region II Scatterplot Matrix')
    data.TOR.II$ws.eq <- NULL #High corr with ws.HH
    pairs(data.TOR.II[1:4], upper.panel = panel.cor, main='Region II Scatterplot Matrix')
    
    #View scatterplot matrix of region III only
    pairs(data.TOR.III[1:5], upper.panel = panel.cor, main='Region III Scatterplot Matrix')
    data.TOR.III$ws.eq <- NULL #High corr with ws.HH
    pairs(data.TOR.III[1:4], upper.panel = panel.cor, main='Region III Scatterplot Matrix')
    
    #Conclusions: 
      #1. In region III, Ti, RSS, and std appear to be related to power.mean at faster wind speeds  
      #2. Region II should in theory be a cubic function, a transformed linear regression may work very well
      #3. In region II RSS, TI, and power.std may also be useful.
      #4. It may be useful later to define a transiton region II.5 and use a different method there
  
#Binned Predictions
    
    #Create a training data set to be used for all prediction methods; I will use Andy's method for now
    set.seed(123)
    ntrain = floor(NROW(data.in)/2)
    train = rep(FALSE,NROW(data.in))
    train[sample(nrow(data.in), ntrain)] = TRUE
    data.train = data.in[train == TRUE,]
    data.test = data.in[train == FALSE,]
    
    #Create a binned power curve model for training data (based on IEC Standard 61400-12-1)
    # "The selected data sets
    # - shall at least cover a wind speed range extending from 1 m/s below cut-in (3/ms) to
    # 1.5 times the wind speed at 85 % of the rated power of the wind turbine" (11.5 m/s)
    # and..
    # "The databaase shall be considered complete when it has met the following criteria:
    # - each bin includes a minimum of 30 min of sampled data
    
    # WindPACT Turbine specs:
    cutin.ws = 3 #cut in wind speed m/s
    rated.ws = 11.5 #rated wind speed m/s
    cutout.ws = 27.6 #cutout wind speed (not used)
    
    #Establish power curve range 
    max(data.in$ws.HH)
    minbin = floor(-1 + min ( c( (cutin.ws), data.in$ws.HH))) #bin minimum (2)
    maxbin = ceiling(1+ max ( c( (1.5*.85*rated.ws), data.in$ws.HH))) #bin maximum (25)
    
    #create the bins for each training data point &  add vector to back to the training data
    bins = seq(minbin, maxbin, by = 1) #create the bins
    ws.bin = cut(data.train$ws.HH, breaks = bins) #cut the training data into bins
    data.train.bin = cbind(data.train, ws.bin)
    
    #Aggregate the windspeed bins and take the mean of all values
    binned.model <-aggregate(data.train.bin, by=list(ws.bin), FUN=mean, na.rm=TRUE)
    binned.model <-binned.model[,c("Group.1","power.mean")]
    colnames(binned.model)[2] = "vlookup.power.mean"
    qplot(binned.model$Group.1,binned.model$vlookup.power.mean) #Plot training data and bins
    
    #Predict the power for the training data
    training.predictions =  merge(data.train.bin, binned.model, by.x="ws.bin", by.y="Group.1") 
    ggplot(training.predictions, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=vlookup.power.mean, color="prediction"))+
      ggtitle("Binned Power Predictions for Training Data") +
      theme(legend.title=element_blank())
    
    #Compute fit error for binned method with training data
    mean((training.predictions$power.mean - 
            training.predictions$vlookup.power.mean)^2) #3354.831
    
    #Compute predictions
    #Add bins for the training data to enable it to be used for predictions
    ws.bin = cut(data.test$ws.HH, breaks = bins) #cut the training data into bins
    data.test.bin = cbind(data.test, ws.bin) #add the bins to the test data
    
    #Add predicted power to data.test.bin
    test.predictions =  merge(data.test.bin, binned.model, by.x="ws.bin", by.y="Group.1")       
    ggplot(test.predictions, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=vlookup.power.mean, color="prediction")) +
      ggtitle("Binned Power Predictions for Test Data") +
      theme(legend.title=element_blank())
    
    #Compute prediction+fit error for binned method with training data
    mean((test.predictions$power.mean - 
            test.predictions$vlookup.power.mean)^2) #3703.235
    
    #Create a simple decision tree model with all variables
    tree.model = tree(power.mean ~ ., data.train) #create tree on data.train
    tree.model #You can see how many observations go into each part of the split
    plot(tree.model) #Plot the tree model
    text(tree.model, pretty=0) #Add text for the tree model
    summary(tree.model) #Summary that corresponds to tree
    
    tree.model.predictions = predict(tree.model, data.test)
    mean((tree.model.predictions-data.test$power.mean)^2) #MSE=9775.94
    sqrt(mean((tree.model.predictions-data.test$power.mean)^2)) #RMSE=98.87
    
    #Plot predicted vs actual or plot the error(?)
    plot(tree.model.predictions, data.test$power.mean)
    plot(tree.model.predictions,(tree.model.predictions  - data.test$power.mean))
    abline(h=0, col=2)
    
    #********************************************************
    #Create a random forest model
    
    #mtry=2
    rf.model = randomForest(power.mean ~ ws.HH + Ti.HH + Shear + TOR, data = data.train, mtry=2, ntree=1000)
    print(rf.model)
    
    rf.model.predictions = predict(rf.model, data.test)
    mean((rf.model.predictions - data.test$power.mean)^2) #546.7355
    
    #Boosting**************************************************
    
    boost.model=gbm(power.mean~., data=data.train, distribution=
                      "gaussian", n.trees=50000, interaction.depth=6, shrinkage=.001)
    gbm.perf(boost.model, plot.it=FALSE) #Result: 9686 using OOB method...
    summary(boost.model) #ws.HH is by far the most important variable
    
    #Partial dependance plot
    plot(boost.model,i="ws.HH")
    
    #Use the boosted model to predict power.mean for the train set
    #Left n.trees@ 50000 as MSE was better and this is a mathmatical model
    boost.model.trn.predictions=predict(boost.model,newdata=data.train, n.trees=50000)
    mean((boost.model.trn.predictions-data.train$power.mean)^2) #5.442251
    sqrt(mean((boost.model.trn.predictions-data.train$power.mean)^2)) #2.332863
    
    #Use the boosted model to predict power.mean for the test set
    boost.model.tst.predictions=predict(boost.model,newdata=data.test, n.trees=50000)
    mean((boost.model.tst.predictions-data.test$power.mean)^2) #69.85723
    sqrt(mean((boost.model.tst.predictions-data.test$power.mean)^2)) #8.358064
    
    ggplot(data.test, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=boost.model.tst.predictions, color="prediction"))+
      ggtitle("Boosted Power Predictions") +
      theme(legend.title=element_blank())

#Post 12/02/2015: The following models were created after our last day of class and 
#are much better optimized. Thank you everyone!  

    #Load additional required packages    
    library(caret)
    library(bst)
    library(plyr)
    library(Cubist)
    
    data.in$TOR <- NULL #Decided to work only with given variables for this last part
    set.seed(42)
    ntrain = floor(nrow(data.in)/2) 
    train = rep(FALSE,nrow(data.in))
    train[sample(nrow(data.in), ntrain)] = TRUE
    data.train = data.in[train == TRUE,]
    data.test = data.in[train == FALSE,]
    
    fitControl = trainControl(method='CV', #use cross validation
                              number=10, #set the number of folds
                              summaryFunction = defaultSummary, 
                              classProbs = FALSE)
    
    #Created new linear model which was bar far the WORST of all methods, as expected.
    lm.model = train(power.mean ~ ., 
                     data = data.train, 
                     method = "lm", 
                     trControl=fitControl)
    print(lm.model)
    
    lm.model.pred <- predict(lm.model, newdata = data.test)
    mean((lm.model.pred-data.test$power.mean)^2) #42756.17
    sqrt(mean((lm.model.pred-data.test$power.mean)^2)) #206.7757
    
    ggplot(data.test, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=lm.model.pred, color="predictions")) +
      ggtitle("Linear Predictions") +
      theme(legend.title=element_blank())
    
    #Created new random forest model
    rf.model = train(power.mean ~ ., 
                     method = "rf",
                     data = data.train, 
                     trControl=fitControl)
    print(rf.model)
    #The final value used for the model was mtry = 3. 
    plot(rf.model)
    
    rf.model.pred <- predict(rf.model, newdata = data.test)
    mean((rf.model.pred-data.test$power.mean)^2) #256.756
    sqrt(mean((rf.model.pred-data.test$power.mean)^2)) #16.024
    
    ggplot(data.test, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=rf.model.pred, color="predictions")) +
      ggtitle("Random Forest Predictions") +
      theme(legend.title=element_blank())
    
    #Created new stochastic gradient boosting model
    gbm.model = train(power.mean ~ ., 
                      data = data.train, 
                      method = "gbm",
                      trControl=fitControl,
                      verbose = FALSE)
    print(gbm.model)
    #The final values used for the model were n.trees = 150, interaction.depth = 3, shrinkage = 0.1
    #and n.minobsinnode = 10. 
    plot(gbm.model)
    
    gbm.model.pred <- predict(gbm.model, newdata = data.test)
    mean((gbm.model.pred-data.test$power.mean)^2) #342.370
    sqrt(mean((gbm.model.pred-data.test$power.mean)^2)) #18.503
    #While MSE and RMSE can be improved upon by changing the final values above, the model becomes 
    #quite overfit.
    
    ggplot(data.test, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=gbm.model.pred, color="predictions")) +
      ggtitle("Stochastic Gradient Boosting Predictions") +
      theme(legend.title=element_blank())
    
    #Created new boosted tree model
    bt.model <- train(power.mean ~ ., 
                      data = data.train,
                      method='bstTree',
                      trControl=fitControl)
    print(bt.model)
    #The final values used for the model were mstop = 150, maxdepth = 3 and nu = 0.1.
    plot(bt.model)
    
    bt.pred <- predict(bt.model, newdata = data.test)
    mean((bt.pred-data.test$power.mean)^2) #241.578
    sqrt(mean((bt.pred-data.test$power.mean)^2)) #15.543
    
    ggplot(data.test, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=bt.pred, color="predictions")) +
      ggtitle("Boosted Tree Predictions") +
      theme(legend.title=element_blank())
    
    #Created new cubist model which is producing the BEST results.
    cb.model <- train(power.mean ~ ., 
                      data = data.train,
                      method='cubist', 
                      trControl=fitControl)
    print(cb.model)
    #The final values used for the model were committees = 20 and neighbors = 5. 
    plot(cb.model)
    
    cb.pred <- predict(cb.model, newdata = data.test)
    mean((cb.pred-data.test$power.mean)^2) #25.885
    sqrt(mean((cb.pred-data.test$power.mean)^2)) #5.088
    
    ggplot(data.test, aes(x=ws.HH)) + 
      geom_point(aes(y=power.mean, color="actual power")) + 
      geom_point(aes(y=cb.pred, color="predictions")) +
      ggtitle("Cubist Predictions") +
      theme(legend.title=element_blank())