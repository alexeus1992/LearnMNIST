# This sciprt file contains a frame for learning handwritten digitals from the MNIST dataset

# load training data from files
data <- loadMNISTData("D:/R/mnist/train-images.idx3-ubyte", "D:/R/mnist/train-labels.idx1-ubyte")
trainLabels <- data$labels
trainData <- data$data

print(dim(trainData))
print(dim(trainLabels))
# trainingData should be 60000x786,  60000 data and 784 features (28x28), tha matrix trainData has 60000 rows and 784 columns
# trainingLabels should have 60000x1, one class label \in {0,1,...9} for each data.

#uncomment the following 3 lines to see the nth training example and its class label.
#n = 10;
#image( t(matrix(trainData[n, ], ncol=28, nrow=28)), Rowv=28, Colv=28, col = heat.colors(256),  margins=c(5,10))
#print("Class label:"); print(trainLabels[n])

sigmoid <- function(h){
    return (1/(1+exp(-h)));
}

learnModel <- function(data,labels){
    X <- data
    y <- labels
    numbers_count <- 10
    m <- nrow(X)
    n <- ncol(X)
    thetas <- matrix(data=0,nrow=numbers_count,ncol=n+1)
    X <- X/255
    X <- cbind(1,X)
    
    
    for(i in 0:numebrs_count-1){
        print(i)        
        normY <- as.numeric(y==i) 
        theta <- learnNumber(X,normY)
        thetas[i+1,] <- theta
    }
    return (thetas)
}
learnModel(trainData,trainLabels)
learnNumber <- function(data,labels){
  
  X <- data
  y <- labels  
  m <- nrow(X)
  n <- ncol(X)
  
  theta <- matrix(runif(n, -100, 100),ncol=1,nrow=n)
  lambda <- 0.0  # regularization of trade-off.
  lambda <- 0.001
  mu <- 0.1 # learning rate
  term <- 0.0001 #Termination of the learning
  prevJ <-0 #previous error
  J <- term + 1 #actual error
  j <- 0
  while (abs(prevJ-J) > term) {  
    
    g <- sigmoid(X%*%theta)
    delta <- t(X)%*%(g-y)
    delta[-1] <- delta[-1] - 2*mu*lambda*theta[-1]      
    theta <- theta - mu*delta
    error1 <- sum(-y*log(g+ 0.0000001)-(1-y)*log(1-g+ 0.0000001))
    error2<-lambda*sum((theta[-1])^2)
    error <- (error1 + error2)/m
    accuracy <- sum(g == y) / m
    print(sprintf("errors: %f , %f", error1, error2))
    print(sprintf("%f , %f", error, accuracy))
    
    prevJ <- J
    J <- error  
  return (theta)
}
# train a model
classifier <- learnModel(data = trainData, labels = trainLabels)
predictedLabels <- testModel(classifier, trainData)


#calculate accuracy on training data
print("accuracy on training data:\t")
print(sum(predictedLabels == trainLabels)/length(trainLabels))

#calculate the following error metric for each class obtained on the train data:
#Recall, precision, specificity, F-measure, FDR and ROC for each class separately. Use a package for ROC. 


# test the model
#data <- loadMNISTData("D:/R/mnist/t10k-images.idx3-ubyte", "D:/R/mnist/t10k-labels.idx1-ubyte")
testLabels <- data$labels
testData <- data$data

print(dim(testData))
print(dim(testLabels))
#trainingData should be 10000x786,  10000 data and 784 features (28x28), tha matrix trainData has 10000 rows and 784 columns
#trainingLabels should have 10000x1, one class label \in {0,1,...9} for each data.

predictedLabels <- testModel(classifier, testData)

#1nn

#calculate accuracy
print("accuracy on test data:\t")
print(sum(predictedLabels == testLabels)/length(testLabels))

#calculate the following error metric for each class obtained on the test data:
#Recall, precision, specificity, F-measure, FDR and ROC for each class separately. Use a package for ROC. 

