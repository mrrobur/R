  # my_kNN<-function(train, test, color, k){}
#rm(list=ls())
load("Classification_Student_160412IV_16.RData")

my_distance<-function(point, matrix, type, e){
  dimensions=length(matrix[,1]) 
  dist<-matrix(,dimensions,2)  # empty matrix initialization, dimension from Training Set
  for(i in seq(matrix[,1])){ #loop - count distance of given 'point' along all points of Validation Set
    
    if(type=="Euclidean"){
      dist[i,1]<-sqrt((point[1]-matrix[i,1])^2+(point[2]-matrix[i,2])^2) 
    }
    
    if(type=="Minkowsky"){
      dist[i,1]<-((abs(point[1]-matrix[i,1]))^e+abs((point[2]-matrix[i,2]))^e)^(1/e)
    }
    
    if(type=="Manhattan"){
      dist[i,1]<-abs((point[1]-matrix[i,1]))+abs((point[2]-matrix[i,2]))
    }
    
    if(type=="Canberra"){
      dist[i,1]<-(abs((point[1]-matrix[i,1]))/(abs(point[1])+abs(matrix[i,1])))+
                    (abs((point[2]-matrix[i,2]))/(abs(point[2])+abs(matrix[i,2])))
    }
    
    dist[i,2]<-i
    
  }
  return(dist)
}

my_kNN<-function(TrainingSet, ValidationSet, k, distance, e){
color_vote<-matrix(,k,1) #initialize kX1 matrix
Results<-matrix(,length(ValidationSet[,1]),1) #initialize kX1 matrix
Results1<-matrix(,length(ValidationSet[,1]),1) #initialize kX1 matrix
Results2<-matrix(,length(ValidationSet[,1]),1) #initialize kX1 matrix
Results3<-matrix(,length(ValidationSet[,1]),1) #initialize kX1 matrix
Results4<-matrix(,length(ValidationSet[,1]),1) #initialize kX1 matrix

for(j in seq(along=ValidationSet[,1])){
  #count distances for each set of points and save in M1 matrix
  M1<-my_distance(ValidationSet[j,], TrainingSet[,], distance, e) # find the shortest distances
  M1_match<-M1[,1][order(M1[,1])] #order ascending and save dist values to M1_match
  M2_match<-M1[,2][order(M1[,1])] #order ascending and save dist points id to M2_match

  # find coresponding colors in Training Set
  for(c in seq(1,k)){
  color_vote[c]<-TrainingSet[M2_match[c],3] # save color numbers of first k points to matrix
  } 
  
  # Count number of occurance for each color  
  color_v<-table(color_vote) #count occurances of each value
  
  #vote
  colr=as.table(which.max(color_v)) #show only max number and number of occurance
  col=as.numeric(row.names(colr)) #return row name (color value) as number
  
  #choose color
  color<-switch(col,"red","green","blue","yellow","black","violet","purple")
  color_set<-switch(ValidationSet[j,3], "red","green","blue","yellow","black","violet","purple")
  
  # plot points with appropriate color (prediction)

 # par(mfrow=c(2,2))
   plot(ValidationSet[j,1],ValidationSet[j,2],type = "p",pch=8,col=color,xlim=c(-70,70),ylim=c(-70,70),main=distance,xlab="x",ylab="y")
     par(new=TRUE)
  plot(ValidationSet[j,1],ValidationSet[j,2],type = "p",cex=2,col=color_set,xlim=c(-70,70),ylim=c(-70,70),xlab="x",ylab="y")
     par(new=TRUE)

    #print results to matrix
   Results[j]<-col
}
#par(new=FALSE)

  return(Results)
}
my_Evaluation<-function(Test,Validation){
  error=0
  for(i in seq(along=Validation)){
    if(Validation[i]!=Test[i]){
      error=error+1
    }
  }
  return(error/length(Test))
}  


par(mfrow=c(2,2))
kk<-11
#k<-1
 error1<-matrix(,kk,1)
 error2<-matrix(,kk,1)
 error3<-matrix(,kk,1)
 error4<-matrix(,kk,1)
k<-1
# for(k in seq(3,10)){
Results1<-my_kNN(PointsTraining,PointsValidation,5,"Euclidean",0)
error1[k]<-my_Evaluation(Results1,PointsValidation[,3])
par(new=FALSE)

Results2<-my_kNN(PointsTraining,PointsValidation,5,"Minkowsky", 3)
error2[k]<-my_Evaluation(Results2,PointsValidation[,3])
par(new=FALSE)

Results3<-my_kNN(PointsTraining,PointsValidation,3,"Manhattan",0)
error3[k]<-my_Evaluation(Results3,PointsValidation[,3])
par(new=FALSE)

Results4<-my_kNN(PointsTraining,PointsValidation,7,"Canberra",0)
error4[k]<-my_Evaluation(Results4,PointsValidation[,3])
par(new=FALSE)

Ex1<-cbind(Results1,Results2,Results3,Results4)
colnames(Ex1)<-c("Euclidean","Minkowsky","Manhattan","Canberra")

save(Ex1, file="Ex1_output.RData")


# plot(1,xlim=c(-70,70),ylim=c(-70,70),xlab="x",ylab="y",sub=error1)
# par(new=FALSE)
# plot(1,xlim=c(-70,70),ylim=c(-70,70),xlab="x",ylab="y",sub=error2)
# par(new=FALSE)
# plot(1,xlim=c(-70,70),ylim=c(-70,70),xlab="x",ylab="y",sub=error3)
# par(new=FALSE)
# plot(1,xlim=c(-70,70),ylim=c(-70,70),xlab="x",ylab="y",sub=error4)
# par(new=FALSE)
# 
 # }