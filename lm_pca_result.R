lm_pca_result=function(data,k=NULL){
  n=nrow(data)
  p=ncol(data)-1
  re=princomp(~.,data[,1:p],cor=T)
  if (is.null(k)){
    k=NULL
    for (i in 1:p){
      tmp=sum((re$sdev^2)[1:i])/sum(re$sdev^2)
      if (tmp>=0.9){
        k=i
        break
      }
    }
  }
  data_z=cbind(data.frame(re$scores[,1:k]),data[1+p])
  A=as.matrix(loadings(re)[,1:k])
  x_bar=re$center
  x_sd=diag(1/re$scale)
  re2=lm(y~.,data_z)
  coef_z=coef(re2)
  coef=as.vector(x_sd%*%A%*%as.vector(coef_z[-1]))
  coef=c(coef_z[1]-sum(x_bar*coef),coef)
  tmp=NULL
  for (i in 1:p){
    tmp=c(tmp,paste('x',as.character(i),sep = ''))
  }
  tmp=c('(Intercept)',tmp)
  names(coef)=tmp
  SSE=sum((data_z[,k+1]-as.vector(cbind(rep(1,n),as.matrix(data_z[,1:k]))%*%coef(re2)))^2)
  SST=sum((data_z[,k+1]-mean(data_z[,k+1]))^2)
  R2=1-(SSE/(n-p-1))/(SST/(n-1))
  return(list(coef=coef,R2=R2))
}
