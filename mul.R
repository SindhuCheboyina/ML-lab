normalize <- function(X) {
  mean=array(ncol(X))
  sd=array(ncol(X))
  for(i in 1:ncol(X)){
    mean[i]=mean(X[,i])
    sd[i]=sd(X[,i])
    if(sd[i])
      X[,i]=(mean[i]-X[,i])/sd[i]
    else
      X[,i]=0
  }
  li=list(X,mean,sd)
}

mydata=read_xlsx("C:/Users/User/Desktop/ml/mul-linear/BostonHousing.xlsx")
X =data.matrix(mydata[1:14])
y= data.matrix(mydata[13])

m=nrow(y)
alpha=0.1
itr=500

theta=matrix(1,nrow=ncol(X)+1)
li=normalize(X)
x<-matrix(unlist(li[1]), ncol = ncol(X))
x=cbind(matrix(1,nrow=m),x)
p=ncol(x)
for (i in 1:itr) {
  theta_prev=theta
  for(j in 1:p){
    deriv=(t(x%*%theta_prev - y)%*%x[, j])/m
    theta[j]=theta_prev[j]-(alpha%*%deriv)
    
  }
}
cost=sum((x%*%theta-y)^2)/(2*m)
hyp=x%*%theta

sprintf("Alpha Value: %f",alpha)
sprintf("Iterations: %d",itr)
sprintf("cost: %f",cost)

plot(y,col="red",main="Actual Value and Result Value")
par(new=TRUE)
plot(hyp,col="blue")
legend("topleft",
       c("Actual","Hypothesis"),
       fill=c("red","blue")
)

