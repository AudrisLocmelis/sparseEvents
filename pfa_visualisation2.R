library(shiny)
library(MASS)
library(data.table)
library(ggplot2)
library(ROSE)

load("pfa_data_sim.Rdata")
#data[,(names(data)):=lapply(names(data),function(v) ifelse(class(get(v))=="numeric",as.integer(get(v)),get(v)) )]
x = names(data)
x = x[! x %in% c("id","state")]

claims = data[,as.integer(1 %in% state),id]
data = unique(data[,c(x,"id"),with=FALSE],by="id")
data = data.table(data,claims = claims[,V1])



# generating data according to ROSE: p=0.5 as default
#X <- data.table(ROSE(claims~., data=data, seed=3,p = 0.5,N = data[,.N]*2)$data)
#X[,age:=round(age)]
#data[,.N,claims]
#X[,.N,claims]
X = copy(data)
X2 <- data.table(ROSE(claims~., data=data, seed=3,p = 0.5,N = data[,.N]*2)$data)
X2[,age:=round(age)]



#get_plot = function(X,nam){
#  X1 = copy(X[,c(nam,"claims"),with=FALSE])
#  X1[,claims:=as.character(claims)]
#  if( class(X1[,nam,with=FALSE][[1]]) %in% c("numeric","integer") ){
#    ex = paste0("p <- ggplot(X, aes(factor(claims),",nam,"))")
#    eval(parse(text=ex))
#    p+geom_boxplot()
#  }else{
#    ex = paste0("ggplot(X, aes(claims, ..count..)) + geom_bar(aes(fill = ",nam,"), position = 'stack')")#, position = "dodge")
#    eval(parse(text=ex))
#  }
#}

#get_plot = function(X,nam){
#  V1 = X[,.N,list(get(nam),claims)]
#  setnames(V1,"get",nam)
#  V1[,claims:=ifelse(claims==1,"claimed","no claim")]
#  
#  ex = paste0("ggplot(V1,aes(x = ",nam,", y = N,fill = claims)) + 
#                geom_bar(position = 'fill',stat = 'identity') + 
#                scale_y_continuous(labels = percent_format())")
#  eval(parse(text=ex))
#}

get_plot = function(X,nam){
  #nam = "age"
  
  X3 = copy(X)

  #korigjeju faktorus
  if(class(X3[,get(nam)])=="factor") X3[,(nam):=as.character(get(nam))]
  
  if(nam=="age") X3[,age:=cut(age,breaks = seq(0,110,by=5))]
  
  #samazinu skailtisko mainiigo daudzumu
  if(class(X3[,get(nam)]) %in% c("numeric","integer") & X3[,uniqueN(get(nam))]>10){
    r = X3[,range(get(nam))]
    X3[,(nam):=cut(get(nam),breaks = seq(r[1]-0.01,r[2],length = 11))]
  }
  
  X1 = X3[,sum(claims)/.N,get(nam)][order(get)]
  g = ggplot(X1, aes(x = factor(get)))
  g + geom_bar(aes(weight = V1)) + ylab("procent") + xlab(nam)
}


plot_density = function(X,nam){
  
  X1 = copy(X)
  X1[,claims:=ifelse(claims==1,"claimed","no claims")]
  X2[,claims:=ifelse(claims==1,"claimed","no claims")]
  if(class(X1[,get(nam)]) %in% c("integer","numeric") ){
    ex = paste0("ggplot(X1, aes(",nam,")) + geom_density(aes(group=claims,color=claims,fill = claims), alpha=0.3)")
    eval(parse(text=ex))
  }else{
    ex= paste0("ggplot(X2, aes(claims, ..count..)) + geom_bar(aes(fill = ",nam,"), position = 'dodge')")
    eval(parse(text=ex))
  }
}


#get_plot(X,"age")


## 
do_tests = function(X,nam){
  
  if(class(X[,get(nam)]) %in% c("integer","numeric")){
    ## Kruskal Wallis test
    test = kruskal.test(X[,get(nam)],X[,claims])
    vec1 = round(c(test$statistic,test$p.value),3)
    
    ## classical anova
    ff = paste0(nam," ~ claims")
    test = anova(lm(formula(ff),data = X))
    vec2 = round(c(test$`F value`[1],test$`Pr(>F)`[1]),3)
    
    
    out = data.table(KW_test = vec1,ANOVA_test = vec2)
  }else{
    #Chi two sample homogeneity test (?)
    X1 = X[,c("claims",nam),with=FALSE]
    X0 = X1[claims==0,.N,get(nam)]
    X1 = X1[claims==1,.N,get(nam)]
    X0 = merge(X0,X1,by="get",all=TRUE)
    X0[is.na(N.x),N.x:=0]
    X0[is.na(N.y),N.y:=0]
    
    test = chisq.test(X0[[2]],X0[[3]]) #, simulate.p.value = TRUE, B = 1000)
    vec1 = c(test$statistic,test$p.value)
    out = data.table(Chi_test = vec1)
  }
  rownames(out) = c("statistic","p-value")
  return(out)
}
do_tests(X,"age")
do_tests(X,"sex")



######## uztaisu tabulu ar visaam p-veertiibaam #####

ptab = data.table(vars = x,pval = sapply(x,function(v) do_tests(X,v)[[1]][2]) )[order(pval)]
rownames(ptab) = NULL



############## SHINY part ########################

x_num = x[sapply(x,function(v) X[,class(get(v))] %in% c("numeric","integer"))]
x_cat = x[sapply(x,function(v) ! X[,class(get(v))] %in% c("numeric","integer"))]


server = function(input, output, session) {

  #lapply(x, function(v) {
  #  output[[paste0('graf_', v)]] <- renderPlot({
  #    get_plot(X,v)
  #  })
  #})
  output$ptab = renderTable({ptab})
  output$test = renderTable({do_tests(X,input$variable)})
  output$graf = renderPlot({get_plot(X,input$variable)})
  output$density = renderPlot({plot_density(X,input$variable)})
}

vec = x
names(vec) = x

ui = fluidPage(
  
  titlePanel("Descriptive modeling/marginal tests"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:",vec),
      tableOutput("ptab")
    ),
    mainPanel(
      
      plotOutput("graf"),
      plotOutput("density"),
      tableOutput("test")
    )
  )
)
shinyApp(ui, server)
