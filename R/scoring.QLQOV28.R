scoring.QLQOV28 <-
function(X,id="",time=""){

items=paste("q",31:58,sep="")

if(length(which(is.element(items,colnames(X))))<28){
stop("At least one item is missing: items must be named q31 to q58");
break
}

if(length(which(match(items,colnames(X))==sort(match(items,colnames(X)))))<28){
stop("Items must be named q31 to q58 and presented on that order in the dataset");
break
}

if(sum(apply(X[,items],2,is.integer))<28){
stop("Items must be integers");
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

if((id!="")&(time!="")){
Y=matrix(nrow=nrow(X),ncol=10)
Y=as.data.frame(Y)
Y[,1]=X[,id]
Y[,2]=X[,time]
colnames(Y)=c(id,time,"OVBI","OVSE","OVAT","OVAG","OVPN","OVHM","OVCH","OVHL")
}

if((id!="")&(time=="")){
Y=matrix(nrow=nrow(X),ncol=9)
Y=as.data.frame(Y)
Y[,1]=X[,id]
colnames(Y)=c(id,"OVBI","OVSE","OVAT","OVAG","OVPN","OVHM","OVCH","OVHL")
}

if((id=="")&(time!="")){
Y=matrix(nrow=nrow(X),ncol=9)
Y=as.data.frame(Y)
Y[,1]=X[,time]
colnames(Y)=c(time,"OVBI","OVSE","OVAT","OVAG","OVPN","OVHM","OVCH","OVHL")
}

if((id=="")&(time=="")){
Y=matrix(nrow=nrow(X),ncol=8)
Y=as.data.frame(Y)
colnames(Y)=c("OVBI","OVSE","OVAT","OVAG","OVPN","OVHM","OVCH","OVHL")
}




MD_ABDOMINAL_GI=apply(is.na(X[,items[1:7]]),1,sum)
rs_ABDOMINAL_GI=apply(X[,items[1:7]],1,sum,na.rm=TRUE)
rs_ABDOMINAL_GI=rs_ABDOMINAL_GI/(7-MD_ABDOMINAL_GI)
Y$OVAG[MD_ABDOMINAL_GI<=3]=(rs_ABDOMINAL_GI[MD_ABDOMINAL_GI<=3]-1)/3*100

MD_PN=apply(is.na(X[,items[11:13]]),1,sum)
rs_PN=apply(X[,items[11:13]],1,sum,na.rm=TRUE)
rs_PN=rs_PN/(3-MD_PN)
Y$OVPN[MD_PN<=1]=(rs_PN[MD_PN<=1]-1)/3*100

MD_H=apply(is.na(X[,items[18:19]]),1,sum)
rs_H=apply(X[,items[18:19]],1,sum,na.rm=TRUE)
rs_H=rs_H/(2-MD_H)
Y$OVHM[MD_H<=1]=(rs_H[MD_H<=1]-1)/3*100

MD_BI=apply(is.na(X[,items[20:21]]),1,sum)
rs_BI=apply(X[,items[20:21]],1,sum,na.rm=TRUE)
rs_BI=rs_BI/(2-MD_BI)
Y$OVBI[MD_BI<=1]=(1-(rs_BI[MD_BI<=1]-1)/3)*100

MD_ADT=apply(is.na(X[,items[22:24]]),1,sum)
rs_ADT=apply(X[,items[22:24]],1,sum,na.rm=TRUE)
rs_ADT=rs_ADT/(3-MD_ADT)
Y$OVAT[MD_ADT<=1]=(1-(rs_ADT[MD_ADT<=1]-1)/3)*100

MD_CT=apply(is.na(X[,items[c(10,14:17)]]),1,sum)
rs_CT=apply(X[,items[c(10,14:17)]],1,sum,na.rm=TRUE)
rs_CT=rs_CT/(5-MD_CT)
Y$OVCH[MD_CT<=2]=(rs_CT[MD_CT<=2]-1)/3*100


Y$OVHL[X[,items[8]]==1]=0
MD_Hl=apply(is.na(X[,items[8:9]]),1,sum)
rs_Hl=apply(X[,items[8:9]],1,sum,na.rm=TRUE)
rs_Hl=rs_Hl/(2-MD_Hl)
Y$OVHL[intersect(which(MD_Hl<=1),which(is.na(Y$OVHL)))]=
(rs_Hl[intersect(which(MD_Hl<=1),which(is.na(Y$OVHL)))]-1)/3*100


MD_SF=apply(is.na(X[,items[25:26]]),1,sum)
rs_SF=apply(X[,items[25:26]],1,sum,na.rm=TRUE)
rs_SF=rs_SF/(2-MD_SF)
Y$OVSE[intersect(which(MD_SF<=1),which(X[,items[26]]==1))]=(rs_SF[intersect(which(MD_SF<=1),which(X[,items[26]]==1))]-1)/3*100

X[,items[28]]=5-X[,items[28]]
MD_SF=apply(is.na(X[,items[25:28]]),1,sum)
rs_SF=apply(X[,items[25:28]],1,sum,na.rm=TRUE)
rs_SF=rs_SF/(4-MD_SF)
Y$OVSE[intersect(which(MD_SF<=2),which(!X[,items[26]]==1))]=(rs_SF[intersect(which(MD_SF<=2),which(!X[,items[26]]==1))]-1)/3*100



Y
}
