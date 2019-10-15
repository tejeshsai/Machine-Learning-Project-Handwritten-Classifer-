
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  
  #show handwritten digit with show_digit(matriximage$x[n,]),n is any number below 60000.
  matriximage<-load_image_file("C:/Users/pswat/OneDrive/Desktop/AMRITA/Sem 7/ML/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/train-images.idx3-ubyte")
  show_digit <- function(arr784, col=gray(12:1/12), ...) {
    image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
  }
  #Prepare fore prediction visualization
  matrixtest<-as.list(load_image_file("C:/Users/pswat/OneDrive/Desktop/AMRITA/Sem 7/ML/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/t10k-images.idx3-ubyte"))
  show_number <- function(arr784, col=gray(12:1/12), ...) {
    image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
  }
  #save data as dataframe
  # though not sure what to do with label data set...now
  # convert labels in to categorial value
  imagetraining<-as.data.frame(load_image_file("C:/Users/pswat/OneDrive/Desktop/AMRITA/Sem 7/ML/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/train-images.idx3-ubyte"))
  imagetest<-as.data.frame(load_image_file("C:/Users/pswat/OneDrive/Desktop/AMRITA/Sem 7/ML/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/t10k-images.idx3-ubyte"))
  labeltraining<-as.factor(load_label_file("C:/Users/pswat/OneDrive/Desktop/AMRITA/Sem 7/ML/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/train-labels.idx1-ubyte"))
  labeltest<-as.factor(load_label_file("C:/Users/pswat/OneDrive/Desktop/AMRITA/Sem 7/ML/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/t10k-labels.idx1-ubyte"))
  library(e1071) # Support Vector Machine (SVM)
  
  # Combine label and image integer. Rebuild Training and Test.
  imagetraining[,1]<-labeltraining
  imagetest[,1]<-labeltest
  Training<-imagetraining
  Test<-imagetest
  
  samplenumber<-20000 # change sample size here since 60k is way too huge
  vec<-seq(from=1,to=60000,by=1)
  mysample<-sample(vec,samplenumber)
  mysampleTraining<-Training[mysample,]
  
  pt <- proc.time()
  svmmodel <- svm(formula=mysampleTraining$n~.,data = mysampleTraining,method="class",kernel="radial",scale=F, cost=10)
  proc.time() - pt
  # user system elapsed
  # 199.228 1.867 202.425
  svmp <- predict(svmmodel, newdata = Test, type = "class")
  svmtable<-table("Actual Value" = Test$n, "Predicted Value" = svmp)
  svmtable
  svmconfusion<-as.data.frame(svmtable)
  write.csv(svmconfusion,file="svmconufusion.csv")
  svmerror<-sum(Test$n!=svmp)/nrow(Test)
  print(paste0("Accuracy(Precision) of SVM: ",1-svmerror))
  #[1] "Accuracy(Precision) of SVM: 0.9099"
  
  # Predict with SVMMODEL
  row <- 666
  svmpe <- predict(svmmodel,newdata=Test[row,],type="class")
  print(paste0("Actual Digit: ", as.character(Test$n[row])))
  #[1] "Actual Digit: 6"
  print(paste0("Predicted Digit: ",svmpe))
  #[1] "Predicted Digit: 6"
  #Visualize the digit to see what it really look like.
  show_number(matrixtest$x[row,])