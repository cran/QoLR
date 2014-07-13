scoring.QLQMY20 <-
function(X,id="",items=1:20){
if(sum(apply(X[,items],2,is.integer))<20){
stop("Items must be integer");
break
}

if(min(X[,items],na.rm=T)<1){
stop("Minimum possible value for items is 1");
break
}

if(max(X[,items],na.rm=T)>4){
stop("Maximum possible value for items is 4");
break
}
if(id!=""){
Y=matrix(nrow=nrow(X),ncol=5)
Y=as.data.frame(Y)
Y[,1]=X[,id]

colnames(Y)=c(id,"MYFP","MYBI","MYDS","MYSE")
}else{
Y=matrix(nrow=nrow(X),ncol=4)
Y=as.data.frame(Y)
colnames(Y)=c("MYFP","MYBI","MYDS","MYSE")}
DM_MYFP=apply(is.na(X[,items[18:20]]),1,sum)
rs_MYFP=apply(X[,items[18:20]],1,sum,na.rm=TRUE)
rs_MYFP=rs_MYFP/(3-DM_MYFP)
Y$MYFP[DM_MYFP<=1]=(1-(rs_MYFP[DM_MYFP<=1]-1)/3)*100
Y$MYBI[!is.na(X[,items[17]])]=(1-(X[!is.na(X[,items[17]]),items[17]]-1)/3)*100
DM_MYDS=apply(is.na(X[,items[1:6]]),1,sum)
rs_MYDS=apply(X[,items[1:6]],1,sum,na.rm=TRUE)
rs_MYDS=rs_MYDS/(6-DM_MYDS)
Y$MYDS[DM_MYDS<=3]=(rs_MYDS[DM_MYDS<=3]-1)/3*100
DM_MYSE=apply(is.na(X[,items[7:16]]),1,sum)
rs_MYSE=apply(X[,items[7:16]],1,sum,na.rm=TRUE)
rs_MYSE=rs_MYSE/(10-DM_MYSE)
Y$MYSE[DM_MYSE<=5]=(rs_MYSE[DM_MYSE<=5]-1)/3*100
Y
}
