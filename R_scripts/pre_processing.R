load_image_file <- function(filename) {
  ret = list() #make list
  f = file(filename,'rb') #point file in read mode
  readBin(f,'integer',n=1,size=4,endian='big') #read integers
  ret$n = readBin(f,'integer',n=1,size=4,endian='big') 
  nrow = readBin(f,'integer',n=1,size=4,endian='big') #rows
  ncol = readBin(f,'integer',n=1,size=4,endian='big') #coloumns
  x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F) #read binary file
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T) #make matrix
  close(f) #close file
  ret #print 
}

load_label_file <- function(filename) {
  f = file(filename,'rb') #open file
  readBin(f,'integer',n=1,size=4,endian='big') #read in binary
  n = readBin(f,'integer',n=1,size=4,endian='big')
  y = readBin(f,'integer',n=n,size=1,signed=F)
  close(f) 
  y
}
getwd() #get working directory
train <- load_image_file("D:/Development/Machine Learning Project/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/train-images.idx3-ubyte")
#change to train dataset path
test <- load_image_file("D:/Development/Machine Learning Project/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/t10k-images.idx3-ubyte")
#change to test dataset path
train$y <- load_label_file("D:/Development/Machine Learning Project/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/train-labels.idx1-ubyte")
#accessing element y in the list
test$y <- load_label_file("D:/Development/Machine Learning Project/Machine-Learning-Project-Handwritten-Classifer-/Dataset/ubyte-version/t10k-labels.idx1-ubyte")  
#accessing element y in test list

class(train)
#returns the class of train
train
#prints train set
