library(party)
#library(partykit)
library(data.table)
library(ROSE)
library(sunburstR)
library(stringr)

dir = "C:/Users/bruno.opermanis/Desktop/PFA/"
#################### Modeliling part ###############

setwd(dir)
load("data_sim.Rdata")
#data[,(names(data)):=lapply(names(data),function(v) ifelse(class(get(v))=="numeric",as.integer(get(v)),get(v)) )]
x = names(data)
x = x[! x %in% c("id","state")]

claims = data[,as.integer(1 %in% state),id]
data = unique(data[,c(x,"id"),with=FALSE],by="id")
data = data.table(data,claims = claims[,V1])

data1 = copy(data)
data = data.table(ROSE(claims~., data=data, seed=3,p = 0.5,N = data[,.N]*3)$data)
data[,.N,claims]

y = "claims"
x = names(data)[! names(data) %in% c(y,"id")]

ff = paste0(y," ~ ",paste0(x,collapse=" + "))
ct = ctree(formula(ff),data = data)

data[,pred:=predict(ct,data)]
data


tr = seq(0.2,0.8,length=100)
acc = sapply(tr,function(k){
  data[,clas:=as.integer(pred>k)]
  return(data[get(y)==clas,.N]/data[,.N])
})

plot(tr,acc,type="l")
tr0 = tr[which.max(acc)]
tr0


####### funkcijas koka aiguushanai #######
jointab = function(x,B){
  if(is.na(B)){
    return(matrix(x,1,1))
  }else{
    cbind(x,B)
  }
}

treeToTab = function(tct){
  
  if(tct$terminal==TRUE){ #if this is a leaf
    return(NA)
  }else{
    
    #paarbaudu vai skaitliskais
    if(is.null(attr(tct$psplit$splitpoint,"levels"))){
      
      #skailtiskais
      v = tct$psplit$splitpoint
      
      crit_left = paste0(tct$psplit$variableName,"<=",v)
      crit_right = paste0(tct$psplit$variableName,">",v)
      
      if(tct$psplit$toleft!=1){
        crit_right = crit_left
        crit_left = crit_right
      }
      
    }else{
      
      #kategoriskais
      tct$psplit$variableName
      
      
      v = tct$psplit$splitpoint
      val_left = attr(v,"levels")[v==1]
      val_right = attr(v,"levels")[v==0]
      
      if(tct$psplit$toleft!=1){
        val_right = val_left
        val_left = val_right
      }
      
      crit_left = paste0(tct$psplit$variableName,
                         " %in% c(",paste0("'",val_left,"'",collapse=","),")")
      
      crit_right = paste0(tct$psplit$variableName,
                          " %in% c(",paste0("'",val_right,"'",collapse=","),")")
      
    }
    
    A = jointab(crit_left,treeToTab(tct$left)) #
    B = jointab(crit_right,treeToTab(tct$right))
    
    #adds columsn to matrixes, if necessary
    if(ncol(A)>ncol(B)) B = Reduce(cbind,c(list(B),lapply(1:(ncol(A)-ncol(B)),function(u) NA)))
    if(ncol(A)<ncol(B)) A =Reduce(cbind,c(list(A),lapply(1:(ncol(B)-ncol(A)),function(u) NA)))

    return(rbind(A,B))
  }
}


tct = slot(ct,"tree")

X = treeToTab(tct)

L= lapply(1:nrow(X),function(k) X[k,!is.na(X[k,])] )

#atdod katram likumam P(claims==1) un skaitu tajaa splitaa
stats = sapply(L,function(l){
  ex = paste0("V = data[",paste0(l,collapse=" & "),"]")
  eval(parse(text=ex))
  return(V[,.N])
})

### get P(claims==1) for all nodes
probs = c()
counts = c()
lapply(L,function(l){
  v = lapply(1:length(l),function(k){
    vec = l[1:k]
    ex = paste0("V = data[",paste0(vec,collapse=" & "),",list(.N,sum(claims==1))]")
    eval(parse(text=ex))
    return(round(c(V[,V2]/V[,N],V[,N]),2))
  })
  
  probs[length(probs)+1] <<- paste0(sapply(v,function(vec) vec[1]),collapse="-")
  counts[length(counts)+1] <<- paste0(sapply(v,function(vec) vec[2]),collapse="-")
  return()
})
#n_max = max(sapply(probs,length))
#probs = t(sapply(probs,function(v){
#  if(length(v)<n_max) v = c(v,rep(NA,n_max-length(v)))
#  return(v)
#}))


out = data.table(rule = sapply(L,function(l)
  paste0(gsub(" ","",l),collapse="-")),n = stats,n1 = counts,probs = unlist(probs))
fun = function(x){
  sk = as.numeric(str_match_all(x,"[0-9]+\\.[0-9]+")[[1]][,1])
  x = gsub("[0-9]+\\.[0-9]+","AAAAAAA",x)
  
  for(s in sk)  x= sub("AAAAAAA",round(s,2),x)
  return(x)
}

fun = Vectorize(fun)
out[,rule:=fun(rule)]
#out = out[probs>tr0]


prob2col = function(p){
  sk1 = trunc(p*256)
  sk1 = ifelse(sk1==256,255,sk1)
  sk = as.hexmode(sk1)
  return(paste0("#",sk,sk,sk))
}


out[,rule:=paste0(rule,"+",probs,"+",n1)]
#out[,probs:=NULL]

#out = out[,list(rule,n,probs,color = prob2col(probs))]
#out = out[,list(rule,n,probs)]
sunburst(out)


