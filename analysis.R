library(caret)

### Load the sets
trainingInput<-read.csv("data/pml-training.csv")
testing<-read.csv("data/pml-testing.csv")

set.seed(2015)

trainingTest<-data.frame()
training<-data.frame()
for ( user in unique(trainingInput$user_name) ) {
        trainingPart<-createDataPartition(trainingInput[trainingInput$user_name==user,"classe"], p=0.8, list=FALSE)
        user_set<-which(trainingInput$user_name==user)
        userTraining<-trainingInput[user_set[trainingPart],]
        userTesting<-trainingInput[user_set[-trainingPart],]
        training<-rbind(training,userTraining)
        trainingTest<-rbind(trainingTest,userTesting)
}

for ( i in (8:159)) {
        training[,i]<-as.double(training[,i])
}

for ( i in (8:159)) {
        testing[,i]<-as.double(testing[,i])
}

naCols<-c()

for ( i in (8:159)) {
        countNa = sum(is.na(training[,i]))
        if ( countNa > 0.95*dim(training)[1] ) {
                naCols<-c(naCols,i)        
        }
}

nzv<-nearZeroVar(training)
trainingNzv<-training[,-c(1:5,7,nzv,naCols,160)]
nzvCor<-cor(trainingNzv, use="na.or.complete")
highlyCorNzv <- findCorrelation(nzvCor, cutoff = .75)
trainingUncorrNzv<-trainingNzv[,-highlyCorNzv]
preObj <- preProcess(trainingUncorrNzv,method="knnImpute")
trainingUncorrNzvNoNA<-predict(preObj,trainingUncorrNzv)
hapPca<-preProcess(trainingUncorrNzvNoNA, method="pca", thresh = 0.8)
trainPC<-predict(hapPca, trainingUncorrNzvNoNA)
modelFit <- train(training$classe ~ .,method="rf",data=trainPC)

applyPreprocess<-function(dataset, naCols, nzv, highlyCorNzv, hapPca) {
        datasetnzv<-dataset[,-c(1:5,7,nzv,naCols,160)]
        datasetUncorNzv<-datasetnzv[,-highlyCorNzv]
        preObj <- preProcess(datasetUncorNzv,method="knnImpute")
        datasetUncorNzvNoNA<-predict(preObj,datasetUncorNzv)
        predict(hapPca, datasetUncorNzvNoNA)
}

trainTestPCA<-applyPreprocess(trainingTest, naCols, nzv, highlyCorNzv, hapPca)
testPCA<-applyPreprocess(testing, naCols, nzv, highlyCorNzv, hapPca)

message("Training error:",(sum(predict(modelFit,trainPC) != training$classe)/dim(training)[1]))
message("Train Test error:",(sum(predict(modelFit,trainTestPCA) != trainingTest$classe)/dim(trainingTest)[1]))
predict(modelFit,testPCA)

featurePlot(x=training[,columnsFor("belt")],
            y = training$classe,
            plot="pairs")

columnsFor<-function(name) {
        colnames(trainingInput)[grep(name,colnames(trainingInput))]
}
