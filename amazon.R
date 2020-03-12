library(timeSeries)
library(PerformanceAnalytics)
library(FRAPO)
library(quantmod)
library(dplyr)

convertir<-function(tabla){
  dias=as.data.frame(index(tabla))
  nueva=as.data.frame(tabla)
  nueva=nueva[,-c(1,2,3,5,6)]
  nueva=as.data.frame(nueva)
  y=cbind(dias,nueva)
  y=as.data.frame(y)
  colnames(y)<-c("DATES","PRECIO")
  y$DATES=as.Date(y$DATES)
  u=y %>% distinct(DATES, .keep_all = TRUE)
  #y$PRECIO=as.numeric.factor(y$PRECIO)
  rownames(u)=u$DATES
  return(u)
}

### convertir a numerico de factor
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

### Dradowns de los datos seleccionados
drawdowns_j<-function(tabla){
  dias=tabla$DATES
  base_serie<-timeSeries(tabla$PRECIO, charvec = rownames(tabla))
  base_retornos<-na.omit(returnseries(base_serie,method = "discrete",
                                      percentage = FALSE, trim = FALSE))
  
  base_draw=PerformanceAnalytics:::Drawdowns(base_retornos)
  dias=dias[-1]
  tabla=cbind(dias,base_draw$TS.1)
  tabla=as.data.frame(tabla)
  colnames(tabla)=c("DATES","DRAWDOWNS")
  tabla$DATES=as.Date(tabla$DATES)
  return(tabla)
}

todos_datos<-function(vec){
  total=list()
  for(j in 1:length(vec)){
    tryCatch({a=getSymbols(vec[j],src="yahoo",from="2000-12-12",auto.assign=FALSE,env = NULL)
    },error=function(e)NA)
    a<-na.omit(a)
    total[[j]]<-a
    
  }
  precios=list()
  for (i in 1:length(total)){
    b=convertir(total[[i]])
    precios[[i]]<-b
  }
  drawdowns=list()
  for (u in 1:length(precios)){
    precios[[u]]=na.omit(precios[[u]])
    c=drawdowns_j(precios[[u]])
    drawdowns[[u]]<-c
  }
  return(drawdowns)
}

todos_numero<-function(vec,numero){
  total=list()
  for(j in 1:length(vec)){
    tryCatch({a=getSymbols(vec[j],src="yahoo",from="1927-12-30",auto.assign=FALSE,env = NULL)
    },error=function(e)NA)
    total[[j]]<-a
    
  }
  precios=list()
  for (i in 1:length(total)){
    b=convertir(total[[i]])
    precios[[i]]<-b
  }
  drawdowns=list()
  for (u in 1:length(precios)){
    precios[[u]]=na.omit(precios[[u]])
    c=drawdowns_j(precios[[u]])
    drawdowns[[u]]<-c
  }
  defini=list()
  for (u in 1:length(drawdowns)){
    drawdowns[[u]]=na.omit(drawdowns[[u]])
    c=dup_con_numero(drawdowns[[u]],numero)
    ultima_actua=medida(c)
    defini[[u]]<-ultima_actua
  }
  return(defini)
  
}


vec<-c("MSFT")

# vec<-c("MSFT","AAPL","AMZN","FB","XOM","JNJ","V","GOOG",
#        "PG","CVX","VZ","T","PFE","MA","UNH","DIS",
#        "CSCO","HD","CVX","KO","MRK","PEP","INTC","MCD","CMCSA",
#        "BA","WMT","BABA","NFLX","ABT","MDT","ORCL","ADBE","ACN",
#        "IBM","PM","PYPL","HON","UNP","COST","NEE","SBUX",
#        "LIN","TMO","LLY","CRM","AVGO","TXN","ABBV","UTX",
#        "DHR","NKE","MO","MMM","NVDA","OCOM","AMT","GE",
#        "GILD","ADP","TJX","SYK","BKNG","DUK","ENB","WM",
#        "BDX","LOW","SO","ANTM","CAT","MDLZ","D","UPS","CB",
#        "CNI","NEM","CVS","COP","CL","CHTR","BSX","CELG",
#        "CI","FISV","ED","ISRG","CSX","PGR","KMB","DD","RTN",
#        "PSA","SU","AGN")
lista_descarga=todos_datos(vec)
