library(survival)
library(synthpop)
library(data.table)

######################## lung cancer data ###################

data = data.table(lung)
data[,inst:=NULL]
data[,time:=NULL]

data[,(names(data)):=lapply(names(data),function(v) ifelse(is.na(get(v)),0,get(v)) )]
data[,delta:=status-1] #variable : 0 - lived, 1 - died
data[,status:=NULL]

###### dividing data into people that died and lived till the end
lived = data[delta==0]
died = data[delta==1]
lived[,delta:=NULL]
died[,delta:=NULL]
lived = data.table(Reduce(rbind,syn(lived,m = 4)$syn)) # simulates "healthy people" data m times more
#X1 = data.table(Reduce(rbind,syn(X1,m = 1)$syn))



B = 20 #number of time intervals for each person

### "sick people"
X1 = lapply(1:nrow(died),function(k){
  x = died[k] # concrete person
  set.seed(k)
  count = rpois(1,1/5) # number of claims 
  Xt = rep(0,B)
  Xt[sample.int(B,count)] = 1 # simulates B long history of claims
  V = Reduce(rbind,replicate(B,x,simplify=FALSE))
  V[,id:=k]
  V = cbind(V,state = Xt)
  return(V)
})


### "healthy people"
X0 = lapply(1:nrow(lived),function(k){
  x = lived[k] 
  set.seed(k+nrow(died))
  count = rpois(1,1/50) # number of claims 
  Xt = rep(0,B)
  Xt[sample.int(B,count)] = 1 # simulates B long history of claims
  V = Reduce(rbind,replicate(B,x,simplify=FALSE))
  V[,id:=k+nrow(died)]
  V = cbind(V,state = Xt)
  return(V)
})


data = rbindlist(c(X1,X0))
dim(data)

data[,.N,state] # total claims
data[,1 %in% state,id][,.N,V1] # how many person have made claims

##### OUTPUT data specification
# regressors: age sex ph.ecog ph.karno pat.karno meal.cal wt.loss 
# unique identifier: id
# dependent vairble (claims): state

save(data,file = "pfa_data_sim.Rdata")


