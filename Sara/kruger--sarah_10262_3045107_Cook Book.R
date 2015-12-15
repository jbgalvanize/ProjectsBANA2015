#Load in the data. 
library(jsonlite)
train<-fromJSON("C:\\Users\\krugers\\Documents\\PERSONAL\\BANA 6660\\Project 2\\train.json",flatten=TRUE)

#Split data into test and training.
set.seed(1)
trainset = sample(1:nrow(train), nrow(train)/2)
training = train[trainset,]
validation = train[-trainset,]


#Make a frequency table of each cuisine type. 
diff_cuisines<-unique(train$cuisine)
ftable<-table(train$cuisine)
ftable<- sort(ftable, decreasing=TRUE)
par(las=3)
barplot(ftable, main="Types of Cuisine", xlab="Cuisine", ylab="Frequency", cex.names = .7)


#Find top 5 unique ingredients for each cuisine.
library(tm)
#Put into TM Function
SimpleIngredients<-Corpus(VectorSource(training$ingredients))
#Stemming to simplfy ingredients
SimpleIngredients <- tm_map(SimpleIngredients, stemDocument, language="english")
inspect(SimpleIngredients)
DTM<-DocumentTermMatrix(SimpleIngredients)
inspect(DTM[1:5,1:20])
FrequencyIngredients<- colSums(as.matrix(DTM))
length(FrequencyIngredients)
head(FrequencyIngredients)
OrderedFrequencyIngredients<-order(FrequencyIngredients,decreasing=TRUE)
TopIngredients<- FrequencyIngredients[OrderedFrequencyIngredients]
TopIngredients[1:10]

#Create function to run 10 ingredients 
Top10Ing <- function(training, CuisineName){
  #Create function to be able to tie it to cuisine type. Takes each cusine to work out top 10 ingrendients using the previous stemming fucntion to simplify ingredients.
  SubSetData<- subset(training,cuisine== CuisineName)
  SimpleIngredients<-Corpus(VectorSource(SubSetData$ingredients))
  SimpleIngredients <- tm_map(SimpleIngredients, stemDocument, language="english")
  DTM<-DocumentTermMatrix(SimpleIngredients)
  FrequencyIngredients<- colSums(as.matrix(DTM))
  OrderedFrequencyIngredients<-order(FrequencyIngredients,decreasing=TRUE)
  TopIngredients<- FrequencyIngredients[OrderedFrequencyIngredients]
  names(TopIngredients[1:10])
} 


#Table of Frequency of Ingredients
Top10Table<- matrix(data = NA, nrow=10, ncol=20)
i=0
for (Cuis in diff_cuisines){
  i=i+1
  
  Top10Table[,i]<- Top10Ing(training, Cuis)  
  print(Cuis)
  print(Top10Table[,i])
}

colnames(Top10Table)<- diff_cuisines
Top10Table<- data.frame(Top10Table)


#Guess Italian for all cuisine types. Create a baseline.
Guess <- rep("italian",nrow(validation))
truth<- validation$cuisine
NumCorrect<- sum(Guess==truth)
TotalGuess<- length(Guess)
Percent<-NumCorrect/TotalGuess
print(Percent)
#This is the baseline for all other prediction methods.

#Decision Tree
library(rpart)
library(rpart.plot)

SimpleIngredients <- c(Corpus(VectorSource(training$ingredients)), Corpus(VectorSource(validation$ingredients)))
SimpleIngredients <- tm_map(SimpleIngredients, stemDocument, language="english")
NEWData <- DocumentTermMatrix(SimpleIngredients)
NEWData<- removeSparseTerms(NEWData, 0.99) 
NEWData <- as.data.frame(as.matrix(NEWData))

NEWData$cuisine <- as.factor(c(training$cuisine, validation$cuisine))
trainDataset  <- NEWData[1:nrow(training), ]
testDataset <- NEWData[-(1:nrow(training)), ]


fit<-rpart(cuisine~., data=trainDataset, method="class")
prp(fit, type=1, extra=4)
prp(fit)
print(fit)

PredFit<-predict(fit, newdata=testDataset, type="class")
table(PredFit,testDataset$cuisine)

#See accuracy of decision tree to compare to the baseline model.
Guess <- PredFit
truth<- testDataset$cuisine
NumCorrect2<- sum(Guess==truth)
TotalGuess2<- length(Guess)
Percent2<-NumCorrect2/TotalGuess2
print(Percent2)
