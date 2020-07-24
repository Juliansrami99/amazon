library(timeSeries)
library(PerformanceAnalytics)
library(FRAPO)
library(quantmod)
library(dplyr)
library(DBI)
library(RMySQL)
library(readxl)
library(tidyverse)
require(reshape2)
library(data.table)
library(DT)

convertir<-function(tabla){
  dias=as.data.frame(index(tabla))
  nueva=as.data.frame(tabla)
  nueva=nueva[,-c(1,2,3,6)]
  nueva=as.data.frame(nueva)
  y=cbind(dias,nueva)
  y=as.data.frame(y)
  colnames(y)<-c("DATES","PRECIO","VOLUMEN")
  y$DATES=as.Date(y$DATES)
  u=y %>% distinct(DATES, .keep_all = TRUE)
  #y$PRECIO=as.numeric.factor(y$PRECIO)
  rownames(u)=u$DATES
  return(u)
}

### convertir a numerico de factor
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

### Precio, volumen y dradowns de los datos seleccionados
drawdowns_j<-function(tabla){
  dias=tabla$DATES
  base_serie<-timeSeries(tabla$PRECIO, charvec = rownames(tabla))
  base_retornos<-na.omit(returnseries(base_serie,method = "discrete",
                                      percentage = FALSE, trim = FALSE))
  
  base_draw=PerformanceAnalytics:::Drawdowns(base_retornos)
  tabla=tabla[-1,]
  tabla_nueva=cbind(tabla[,1],base_retornos,tabla[,3],base_draw$TS.1,tabla$PRECIO)
  tabla_nueva=as.data.frame(tabla_nueva)
  colnames(tabla_nueva)=c("DATES","RETORNOS","VOLUMEN","DRAWDOWNS","PRECIO")
  tabla_nueva$DATES=as.Date(tabla_nueva$DATES)
  return(tabla_nueva)
}

### Graficar precio con ploltly ####
grafica_precio<-function(tabla){
  g<-ggplot(tabla,aes(x=tabla$DATES,y=tabla$PRECIO))+
    geom_line()
  ggplotly(g)
}

### Crear drawups
drawups<-function(tabla){
  drawup=vector(,nrow(tabla))
  drawup[1]<-0
  for (i in 1:nrow(tabla)){
    pru=(1/(1-tabla$DRAWDOWNS[i]))-1 
  }
  total=cbind(tabla,drawup,mini,maxi,tem)
  colnames(total)=c("DATES","RETORNOS","VOLUMEN","DRAWDOWNS","PRECIO","DRAWUPS","MINIMOS",
                    "MAXIMOS","TEMP")
  return(total)
}

dup<-function(tabla){
  drawup=vector(,nrow(tabla))
  drawup[1]<-0
  temp=c(1)
  mini=vector(,nrow(tabla))
  maxi=vector(,nrow(tabla))
  tem=vector(,nrow(tabla))
  vector_logico=vector(,nrow(tabla))
  for (i in 2:nrow(tabla)){
    if (vector_logico[i]==FALSE){
      tem[i]<-temp[length(temp)]
      minimo=min(tabla$PRECIO[(temp[length(temp)]):i])
      mini[i]<-minimo
      j=i
      maximo=max(tabla$PRECIO[(temp[length(temp)]):j])
      maxi[j]<-maximo
      while(tabla$PRECIO[j]==maximo && j<nrow(tabla)){
        vector_logico[j]=TRUE
        drawup[j]=1-(minimo/tabla$PRECIO[j])
        j=j+1
        maximo=max(tabla$PRECIO[(temp[length(temp)]):j])
        maxi[j]<-maximo
      }
      if(tabla$PRECIO[j]<=minimo && j<nrow(tabla)){
        temp=c(temp,j)
        tem[j]<-temp[length(temp)]
        vector_logico[j]=TRUE
        drawup[j]<-0
      }
      else if(tabla$PRECIO[j]<maximo && tabla$PRECIO[j]>minimo && j<nrow(tabla)){
        n=j
        max_temp=tabla$PRECIO[n]
        while(max_temp<(maximo*1) && n<nrow(tabla) && tabla$PRECIO[n]>minimo){
          maxi[n]<-maximo
          vector_logico[n]=TRUE
          drawup[n]<-1-(minimo/tabla$PRECIO[n])
          n=n+1
          max_temp=max(tabla$PRECIO[j:n])
        }
        temp=c(temp,j)
        tem[j]<-temp[length(temp)]
      }
      if(j==nrow(tabla)){
        drawup[j]<-drawup[(j-1)]
      }
    }
  }
  total=cbind(tabla,drawup,mini,maxi,tem)
  colnames(total)=c("DATES","RETORNOS","VOLUMEN","DRAWDOWNS","PRECIO","DRAWUPS","MINIMOS",
                    "MAXIMOS","temp")
  return(total)
}

dup2<-function(tabla){
  drawup=vector(,nrow(tabla))
  drawup[1]<-0
  temp=c(1)
  mini=vector(,nrow(tabla))
  maxi=vector(,nrow(tabla))
  tem=vector(,nrow(tabla))
  vector_logico=vector(,nrow(tabla))
  for (i in 2:nrow(tabla)){
    if (vector_logico[i]==FALSE){
      tem[i]<-temp[length(temp)]
      minimo=min(tabla$PRECIO[(temp[length(temp)]):i])
      mini[i]<-minimo
      j=i
      maximo=max(tabla$PRECIO[(temp[length(temp)]):j])
      maxi[j]<-maximo
      while(tabla$PRECIO[j]==maximo && j<nrow(tabla)){
        vector_logico[j]=TRUE
        dnv
        j=j+1
        maximo=max(tabla$PRECIO[(temp[length(temp)]):j])
        maxi[j]<-maximo
      }
      if(tabla$PRECIO[j]<=minimo && j<nrow(tabla)){
        temp=c(temp,j)
        tem[j]<-temp[length(temp)]
        vector_logico[j]=TRUE
        drawup[j]<-0
      }
      else if(tabla$PRECIO[j]<maximo && tabla$PRECIO[j]>minimo && j<nrow(tabla)){
        n=j
        max_temp=tabla$PRECIO[n]
        #media_drawdowns=mean(tabla$DRAWDOWNS)-0*sd(tabla$DRAWDOWNS)
        while(max_temp<(maximo*1.3) && n<nrow(tabla) && tabla$PRECIO[n]>minimo){
          mini[n]<-minimo
          maxi[n]<-maximo
          vector_logico[n]=TRUE
          drawup[n]=1-(minimo/tabla$PRECIO[n])
          n=n+1
          max_temp=max(tabla$PRECIO[j:n])
        }
        temp=c(temp,j)
        tem[j]<-temp[length(temp)]
      }
      if(j==nrow(tabla)){
        drawup[j]<-drawup[(j-1)]
      }
    }
  }
  total=cbind(tabla,drawup,mini,maxi,tem)
  colnames(total)=c("DATES","RETORNOS","VOLUMEN","DRAWDOWNS","PRECIO","DRAWUPS","MINIMOS",
                    "MAXIMOS","temp")
  return(total)
}


## Funcion para obtener los drawups con diferentes drawups
dup_con_numero<-function(tabla,numero){
  if (numero==""){
    numero=1.3
    print("entro")
  }
  drawup=vector(,nrow(tabla))
  drawup[1]<-0
  temp=c(1)
  mini=vector(,nrow(tabla))
  maxi=vector(,nrow(tabla))
  tem=vector(,nrow(tabla))
  vector_logico=vector(,nrow(tabla))
  for (i in 2:nrow(tabla)){
    if (vector_logico[i]==FALSE){
      tem[i]<-temp[length(temp)]
      minimo=min(tabla$PRECIO[(temp[length(temp)]):i])
      mini[i]<-minimo
      j=i
      maximo=max(tabla$PRECIO[(temp[length(temp)]):j])
      maxi[j]<-maximo
      while(tabla$PRECIO[j]==maximo && j<nrow(tabla)){
        vector_logico[j]=TRUE
        drawup[j]=1-(minimo/tabla$PRECIO[j])
        j=j+1
        maximo=max(tabla$PRECIO[(temp[length(temp)]):j])
        maxi[j]<-maximo
      }
      if(tabla$PRECIO[j]<=minimo && j<nrow(tabla)){
        temp=c(temp,j)
        tem[j]<-temp[length(temp)]
        vector_logico[j]=TRUE
        drawup[j]<-0
      }
      else if(tabla$PRECIO[j]<maximo && tabla$PRECIO[j]>minimo && j<nrow(tabla)){
        n=j
        max_temp=tabla$PRECIO[n]
        #media_drawdowns=mean(tabla$DRAWDOWNS)-0*sd(tabla$DRAWDOWNS)
        while(max_temp<(maximo*as.numeric(numero)) && n<nrow(tabla) && tabla$PRECIO[n]>minimo){
          mini[n]<-minimo
          maxi[n]<-maximo
          vector_logico[n]=TRUE
          drawup[n]=1-(minimo/tabla$PRECIO[n])
          n=n+1
          max_temp=max(tabla$PRECIO[j:n])
        }
        temp=c(temp,j)
        tem[j]<-temp[length(temp)]
      }
      if(j==nrow(tabla)){
        drawup[j]<-drawup[(j-1)]
      }
    }
  }
  total=cbind(tabla,drawup,mini,maxi,tem)
  colnames(total)=c("DATES","RETORNOS","VOLUMEN","DRAWDOWNS","PRECIO","DRAWUPS","MINIMOS",
                    "MAXIMOS","temp")
  return(total)
}


### Pruebas para crear medida de division entre drawups y drawdowns

medida<-function(tabla){
  # Almaceno los colnames de la tabla
  nombres_tabla=colnames(tabla)
  # Medida para los draw
  med_draw<-vector(,nrow(tabla))
  med_draw[1]<-1
  # Rellenar vector
  for(i in 2:nrow(tabla)){
    dato_drawdowns=-(tabla$DRAWDOWNS[i]/tabla$DRAWDOWNS[i-1])
    dato_drawups=tabla$DRAWUPS[i]/tabla$DRAWUPS[i-1]
    if (dato_drawups>1 && !is.na(dato_drawups) && !is.infinite(dato_drawups) &&
        !is.na(dato_drawups)){
      med_draw[i]<-dato_drawups
    }
    else if(!is.infinite(dato_drawdowns) && !is.na(dato_drawdowns)){
      med_draw[i]<-dato_drawdowns
    }
    else{
      med_draw[i]<-0
    }
  }
  # Nuevos nombres
  nombres_nue_tabla=c(nombres_tabla,"drawmed")
  tabla=cbind(tabla,med_draw)
  colnames(tabla)<-nombres_nue_tabla
  return(tabla)
}

### Scale el indicador de los drawups+drawdowns

normal_indicador<-function(lista_tablas){
  lista_final=list()
  for (i in 1:length(lista_tablas)){
    nombre_tabla=colnames(lista_tablas[[i]])
    positivos=lista_tablas[[i]]$DRAWUPS+lista_tablas[[i]]$DRAWDOWNS+
      abs(min(lista_tablas[[i]]$DRAWUPS+lista_tablas[[i]]$DRAWDOWNS))
    escalados=scale(positivos)
    tot=cbind(lista_tablas[[i]],escalados)
    colnames(tot)<-c(nombre_tabla,"SCALE")
    lista_final[[i]]<-tot
  }
  return(lista_final)
}

### Graficar el scale

plot_normal_indicador<-function(lista_tablas,accion){
  x11()
  plot(lista_tablas[[1]]$DATES,lista_tablas[[1]]$SCALE,type="l",ylim=c(-5,5),
       ylab="indicator",xlab="years",xaxt='n')
  h<-seq(min(lista_tablas[[1]]$DATES),max(lista_tablas[[1]]$DATES),by="year")
  axis.Date(1,at=h,labels=format(h,"%Y"))
  lines(lista_tablas[[2]]$DATES,lista_tablas[[2]]$SCALE,type="l",col="red")
  lines(lista_tablas[[3]]$DATES,lista_tablas[[3]]$SCALE,type="l",col="orange")
  lines(lista_tablas[[4]]$DATES,lista_tablas[[4]]$SCALE,type="l",col="blue")
  lines(lista_tablas[[5]]$DATES,lista_tablas[[5]]$SCALE,type="l",col="green")
  legend("topright",legend = c("1","1.3","1.7","1.5","2"),col=c("black",
                                                                "red","orange",
                                                                "blue","green"),
         lty = 1)
  grid()
  abline(h=0,lwd=3)
  title(accion)
}


##### Relacion ganancias y drawdowns ####

drawup_regime<-function(tabla){
  nombres_tabla<-colnames(tabla)
  dato<-vector(,nrow(tabla))
  for (i in 1:nrow(tabla)){
    d=abs(tabla$DRAWDOWNS[i])/(1+tabla$DRAWDOWNS[i])
    dato[i]<-d
  }
  nueva_tabla=cbind(tabla,dato)
  colnames(nueva_tabla)<-c(nombres_tabla,"regime")
  return(nueva_tabla)
}

##### Funcion para ver maximos #####
cocientes<-function(tabla_stock,tabla_creada){
  coc=vector(,nrow(tabla_stock))
  logicos=vector(,nrow(tabla_stock))
  for(i in 1:nrow(tabla_stock)){
    if (logicos[i]==F){
      if(tabla_creada$PRECIO_MAX[i]!=0){
        logicos[i]=T
        j=i+1
        if (!is.null(tabla_creada$PRECIO_MAX[j]) && !is.na(tabla_creada$PRECIO_MAX[j])){
          for (u in j:nrow(tabla_stock)){
            if (tabla_creada$PRECIO_MAX[u]!=0){
              dato=tabla_creada$PRECIO_MAX[u]/tabla_creada$PRECIO_MAX[i]
              coc[u]<-dato
              break
            }
          }
        }
      } 
    }
  }
  nueva=cbind(tabla_creada,coc)
  nueva=as.data.frame(nueva)
  colnames(nueva)<-c(colnames(tabla_creada),"COCIENTES")
  return(nueva)
}

drawdowns_intermedios<-function(tabla_stock,tabla_creada){
  dra<-vector(,nrow(tabla_stock))
  logicos<-vector(,nrow(tabla_stock))
  for (i in 1:nrow(tabla_stock)){
    if (logicos[i]==F){
      if(tabla_creada$COCIENTES[i]!=0){
        logicos[i]=T
        j=i+1
        if (!is.null(tabla_creada$COCIENTES[j]) && !is.na(tabla_creada$COCIENTES[j])){
          for (u in j:nrow(tabla_stock)){
            if (tabla_creada$COCIENTES[u]!=0){
              break
            }
            else{
              dra[u]<-tabla_stock$DRAWDOWNS[u]
            }
          }
        }
      }
    }
  }
  g=cbind(tabla_creada,dra)
  g=as.data.frame(g)
  colnames(g)<-c(colnames(tabla_creada),"DRAW")
  return(g)
}

maximos_stock<-function(tabla,numero){
  if(numero<=min(tabla$DRAWDOWNS)){
    print("CAMBIE")
  }
  else{
    maximos_dias<-vector(,nrow(tabla))
    maximos<-c()
    logicos<-vector(,nrow(tabla))
    logicos[1]=T
    if (numero==0){
      maximos_dias[1]=tabla$PRECIO[1]
    }
    for(i in 1:nrow(tabla)){
      if (logicos[i]==F){
        if(tabla$DRAWDOWNS[i]>=numero){
          j=i+1
          while(!is.null(tabla$DRAWDOWNS[j]) && !is.na(tabla$DRAWDOWNS[j]) 
                && (tabla$DRAWDOWNS[j]>=tabla$DRAWDOWNS[(j-1)])){
            j=j+1
          }
          u=j-1
          maximos<-c(maximos,tabla$PRECIO[u])
          maximos_dias[u]<-tabla$PRECIO[u]
          logicos[u]<-T
        } 
      }
    }
    ba=data.frame("DATES"=tabla$DATES,"PRECIO"=tabla$PRECIO,"PRECIO_MAX"=maximos_dias)
    ja=cocientes(tabla,ba)
    ta=drawdowns_intermedios(tabla,ja)
    return(ta)
  }
}

drawdowns_minimos_intervalos<-function(tabla){
  draw_min=vector(,nrow(tabla))
  logicos=vector(,nrow(tabla))
  for (i in 1:nrow(tabla)){
    if (logicos[i]==F){
      dato=tabla$COCIENTES[i]
      if(dato!=0){
        logicos[i]<-T
        j=i+1
        if (j<=nrow(tabla)){
          for (b in j:nrow(tabla)){
            if (tabla$COCIENTES[b]!=0){
              dr=min(tabla$DRAW[i:b])
              a=which(tabla$DRAW==dr, arr.ind=TRUE)
              draw_min[a]<-dr
            }
          }
        }
      }
    }
  }
  nueva=cbind(tabla,draw_min)
  nueva=as.data.frame(nueva)
  colnames(nueva)<-c(colnames(tabla),"DRAW_MIN")
  return(nueva)
}

tiempo_entre_maximos<-function(tabla){
  nombres<-colnames(tabla)
  logicos<-vector(,nrow(tabla))
  dias_vector=vector(,nrow(tabla))
  for(i in 1:nrow(tabla)){
    if(logicos[i]==F){
      if (tabla$PRECIO_MAX[i]!=0 && !is.na(tabla$PRECIO_MAX[i])){
        logicos[i]<-T
        dias=0
        j=i+1
        if(j<=nrow(tabla)){
          for (h in j:nrow(tabla)){
            dias=dias+1
            if(tabla$PRECIO_MAX[h]!=0 && !is.na(tabla$PRECIO_MAX[h])){
              break
            }
          }
          dias_vector[i]<-dias
        }
      }
    }
  }
  for(i in 1:nrow(tabla)){
    if(dias_vector[i]==0){
      dias_vector[i]=NA
    }
  }
  tabla=cbind(tabla,dias_vector)
  tabla=as.data.frame(tabla)
  colnames(tabla)<-c(nombres,"DIAS")
  return(tabla)
}

reversion_media_scale<-function(tabla){
  media=mean(tabla$SCALE,na.rm = T)
  rev_dias=vector(,nrow(tabla))
  logicos=vector(,nrow(tabla))
  for (i in 1:nrow(tabla)){
    if (logicos[i]==F){
      if (tabla$SCALE[i]>media){
        logicos[i]=T
        j=i+1
        cont=1
        if (j<=nrow(tabla)){
          while(tabla$SCALE[j]>media & j<=nrow(tabla)){
            cont=cont+1
            j=j+1
          }
          rev_dias[i]<-cont
          if(j<=nrow(tabla)){
            nuev=cont
            for (h in (i+1):j){
              nuev=nuev-1
              rev_dias[h]=nuev
              logicos[h]=T
            }
          }
        }
      }
      else{
        logicos[i]=T
        j=i+1
        cont=1
        if (j<=nrow(tabla)){
          while(tabla$SCALE[j]<=media & j<=nrow(tabla)){
            cont=cont+1
            j=j+1
          }
          rev_dias[i]<-cont
          if(j<=nrow(tabla)){
            nuev=cont
            for (h in (i+1):j){
              nuev=nuev-1
              rev_dias[h]=nuev
              logicos[h]=T
            }
          }
        }
      }
    }
  }
  return(rev_dias)
}
##### todos: Funcion mas importante #####


todos<-function(vec){
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
    c=dup2(drawdowns[[u]])
    ultima_actua=medida(c)
    defini[[u]]<-ultima_actua
  }
  return(defini)
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


##########################################
# Creacion de maximos 
##########################################

prueba_maximos<-function(tabla){
  lista_total=list()
  for (j in 1:length(tabla)){
    pr=maximos_stock(tabla[[j]],0)
    # Drawdowns minimos entre cada maximo
    pr=drawdowns_minimos_intervalos(pr)
    for (i in 1:nrow(pr)){
      if (pr$PRECIO_MAX[i]==0){
        pr$PRECIO_MAX[i]=NA
      }
      # borrar cuando no necesite grafica dos
      # if (pr$COCIENTES[i]==0){
      #   pr$COCIENTES[i]=NA
      # }
      if(pr$DRAW_MIN[i]==0){
        pr$DRAW_MIN[i]=NA
      }
    }
    
    # Agrego a pr el tiempo que se demora en encontrar otro maximo
    pr=tiempo_entre_maximos(pr)
    lista_total[[j]]<-pr
  }
  return(lista_total)
  
}


##########################################
# Analisis kafka
##########################################


#### Mundo ####
tabla=read_xlsx("Mundo.xlsx")
vec<-as.vector(tabla$TICKERS)

tablas_original<-todos_numero(vec,1)

lista_accion<-normal_indicador(tablas_original)

con <- dbConnect(drv     = RMySQL::MySQL(),
                 username = "macro",
                 password = "macrowise",
                 host     = "macro.c8rej9nslv8v.us-east-2.rds.amazonaws.com",
                 port     = 3306,
                 dbname   = "macrowise")  


for (i in 1:length(lista_accion)){
   dbWriteTable(con,vec[i], lista_accion[[i]])
} 
dbDisconnect(con)
