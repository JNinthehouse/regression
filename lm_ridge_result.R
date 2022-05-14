lm_ridge_result=function(data,method='HKB',lambda=NULL,k_range=c(0,1)){
n=nrow(data)
p=ncol(data)-1
y_bar=mean(data[,p+1])
y_sd=sd(data[,p+1])
x_bar=as.vector(apply(data[,1:p],2,mean))
x_sd=as.vector(apply(data[,1:p],2,sd))
datas=data.frame(scale(data,center = T,scale = T))
if (is.null(lambda)){
re=lm.ridge(y~.,data = datas,lambda = seq(k_range[1],k_range[2],0.001))
if (method=='HKB'){lambda=re$kHKB}
else if (method=='L-W'){lambda=re$kLW}
else{lambda=0}
}
re=lm.ridge(y~.,data = datas,lambda = lambda)
coef=as.vector(re$coef)/x_sd*y_sd
coef=c(y_bar-sum(as.vector(re$coef)*x_bar/x_sd)*y_sd,coef)
tmp=NULL
for (i in 1:p){
  tmp=c(tmp,paste('x',as.character(i),sep = ''))
}
tmp=c('(Intercept)',tmp)
names(coef)=tmp
SSE=sum((datas[,p+1]-as.vector(as.matrix(datas[,1:p])%*%re$coef))^2)
SST=sum((data[,p+1]-mean(data[,p+1]))^2)
R2=1-(SSE/(n-p))/(SST/(n-1))
return(list(coef=coef,R2=R2))
}
