library(timeSeries)
library(PerformanceAnalytics)
library(FRAPO)
library(quantmod)
library(dplyr)
library(DBI)
library(RMySQL)
library(readxl)
library(tidyverse)
#library(fredr)
#library(xlsx)
#library(plotly)
#library(readxl)
#library(writexl)
require(reshape2)
#library(e1071)
#library(poweRlaw)
library(data.table)
#library(tseries)
#library(mltools)
#library(gganimate)
#library(cluster)
#library(factoextra)
#library(NbClust)
#library(crosstalk)
library(DT)
#library(plyr)
#library(dplyr)
#library(fredr)



columna_agregar<-function(lista){
  for (i in 1:length(lista)){
    lista[[i]]<-lista[[i]] %>% mutate("EWH"=rep(NA,nrow(lista[[i]])))
  }
  return(lista)
}

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

tabla_tab<-function(tabla){
  #tab=reversion_media_scale(tabla)
  tab=tabla$SCALE
  hola=cbind(tabla$SCALE,tab)
  hola=cbind(hola,tabla$DATES)
  hola=as.data.frame(hola)
  hola$V3=as.Date(hola$V3)
  colnames(hola)<-c("SCALE","REVERSION","DATES")
  return(hola)
}

## Creacion lista de tablas tab

lista_tabla_tab<-function(lista){
  lista_final=list()
  for (j in 1:length(lista)){
    b=tabla_tab(lista[[j]])
    lista_final[[j]]<-b
  }
  return(lista_final)
}

## Para graficar tabla tab
graficar_tabla_tab<-function(tabla,media_rev){
  media=mean(tabla$REVERSION)
  w=lm(tabla$SCALE~tabla$DATES,tabla)
  x11()
  par(mfrow=c(1,2))
  plot(tabla$DATES,tabla$SCALE)
  abline(w,col="red")
  plot(tabla$REVERSION,tabla$SCALE)
  abline(v=media_rev,col="blue")
  abline(v=media,col="green")
  legend("topleft",legend = c("media muestra","media total"),col=c("green","blue"),bty='n',lty=2)
  print(w$coefficients)
  print(w$coefficients[2]>0.001 & w$coefficients[2]<0.001)
}

graficas_maximos<-function(tabla){
  numero_maximos=nrow(tabla %>% filter(!is.na(PRECIO_MAX)))
  mostrar=as.character(numero_maximos)
  x11()
  plot(tabla$DATES,tabla$PRECIO_MAX,pch=20,cex=1.5,col="red",ylab="",xlab="Dates",yaxt="n",
       ylim=c(min(tabla$PRECIO),max(tabla$PRECIO)))
  lines(tabla$DATES,tabla$PRECIO,type="l")
  grid()
  legend("topleft",legend = c(paste("numero maximos ",mostrar)))
}


analisis_maximos_datos<-function(tabla){
  d=split(tabla, format(as.Date(tabla$DATES), "%Y"))
  nombres=names(d)
  tama=vector(,length(d))
  for (i in 1:length(d)){
    largo=nrow(d[[i]])
    tama[i]=largo
  }
  nombres=as.data.frame(nombres)
  tama=as.data.frame(tama)
  defini=data.frame(nombres,tama)
  return(defini)
}

clasificacion_maximos<-function(tabla,tabla_maximos,fecha_inicio,fecha_final){
  # filtro la tabla historica del stock por fecha inicial y fecha final
  #tabla_temporal=tabla %>% filter(DATES>=fecha_inicio & DATES<=fecha_final)
  tabla_temporal_maximos=tabla_maximos %>% filter(DATES>=fecha_inicio & DATES<=fecha_final)

  # Crear una nueva variable que solo tenga el ao de los dias
  tabla_temporal_maximos=tabla_temporal_maximos %>% mutate("YEAR"=as.numeric(format(DATES,'%Y')))

  casos=vector(,8)

  ###### Paso uno: cuantos maximos hay ####
  num_maximos_historicos=tabla_maximos %>% filter(!is.na(PRECIO_MAX))
  num_maximos=tabla_temporal_maximos %>% filter(!is.na(PRECIO_MAX))
  n_maxi=nrow(num_maximos)
  n_maxi_historico=nrow(num_maximos_historicos)

  ##########################################
  if (n_maxi_historico==0){
    dato_an=0
    dato_an2=0
  }
  else{
    a=analisis_maximos_datos(num_maximos_historicos)
    if(length(a$tama)==1){
      dato_an=4
      dato_an2=4
    }
    else{
      dato_an=mean(a$tama,na.rm=T)+sd(a$tama,na.rm=T)
      dato_an2=mean(a$tama,na.rm=T)+sd(a$tama,na.rm=T)/2
    }
  }
  # Half year of the sample
  year_m=as.Date((as.numeric(as.Date(fecha_final))+as.numeric(as.Date(fecha_inicio)))/2)
  # Tabla de datos debajo de dicha fecha
  if (tabla_maximos$DATES[1]<fecha_inicio){
    tabla_inicial=tabla_temporal_maximos
    tabla_final=tabla_temporal_maximos
  }
  else{
    tabla_inicial=tabla_temporal_maximos %>% filter(DATES<year_m)
    # Tabla de datos despues de esa fecha
    tabla_final=tabla_temporal_maximos %>% filter(DATES>=year_m)
    # Numero de maximos de la tabla inicial
  }
  num_maximos_inicial=tabla_inicial %>% filter(!is.na(PRECIO_MAX))
  n_maxi_inicial=nrow(num_maximos_inicial)
  # Maximos segunda tabla
  num_maximos_final=tabla_final %>% filter(!is.na(PRECIO_MAX))
  n_maxi_final=nrow(num_maximos_final)
  if (n_maxi==0){
    casos[5]<-25
    casos[6]<-25
  }
  else{
    if (n_maxi_inicial==0 & n_maxi_final!=0){
      distancia=n_maxi_final
      if (distancia>=dato_an){
        casos[1]=35
        casos[8]=15
      }
      else if(distancia<dato_an & distancia>=dato_an2){
        casos[2]=35
        casos[8]=15
      }
      else{
        casos[7]=50
      }
    }
    else if(n_maxi_inicial!=0 & n_maxi_final==0){
      distancia=n_maxi_inicial
      if (distancia>=dato_an){
        casos[1]=15
        casos[2]=15
        casos[3]=20
      }
      else if(distancia<dato_an & distancia>=dato_an2){
        casos[3]=50
      }
      else if (distancia>mean(a$tama) & distancia<dato_an2){
        casos[3]=15
        casos[4]=35
      }
      else{
        casos[4]=50
      }
    }
    else{
      distancia=n_maxi
      if (n_maxi_final>=n_maxi_inicial){
        if (distancia>=dato_an){
          casos[1]=35
          casos[8]=15
        }
        else if(distancia<dato_an & distancia>=dato_an2){
          casos[1]=15
          casos[2]=35
        }
        else if (distancia>mean(a$tama) & distancia<dato_an2){
          casos[1]=10
          casos[2]=40
        }
      }
      else{
        if (distancia>=dato_an){
          casos[8]=20
          casos[1]=30
        }
        else if(distancia<dato_an & distancia>=dato_an2){
          casos[1]=15
          casos[2]=35
        }
        else if (distancia>mean(a$tama) & distancia<dato_an2){
          casos[1]=10
          casos[2]=25
          casos[3]=15
        }

      }
    }
  }
  return(casos)
}

clasificacion_reversion<-function(tabla,fecha_inicio,fecha_final){
  media_reversion=mean(tabla$REVERSION)
  tabla_temp=tabla %>% filter(DATES>=fecha_inicio & DATES<=fecha_final)
  numero_filas=nrow(tabla_temp)
  media_reversion_temp=mean(tabla_temp$REVERSION)
  positivos=tabla_temp %>% filter(SCALE>=0)
  negativos=tabla_temp %>% filter(SCALE<0)
  casos=vector(,8)
  # # Ao inicial
  # year_1=as.numeric(format(as.Date(fecha_inicio),'%Y'))
  # # Ao final
  # year_2=as.numeric(format(as.Date(fecha_final),'%Y'))
  # Ao mitad
  year_m=as.Date((as.numeric(as.Date(fecha_final))+as.numeric(as.Date(fecha_inicio)))/2)
  # Crear una nueva variable que solo tenga el ao de los dias
  #tabla_temp=tabla_temp %>% mutate("YEAR"=as.numeric(format(DATES,'%Y')))
  # Filtro por fecha inicial
  tabla_inicial=tabla_temp %>% filter(DATES<year_m)
  #Filtro por fecha final
  tabla_final=tabla_temp %>% filter(DATES>=year_m)
  # Positivos y negativos de cada de las tablas
  # positivos_inicial=tabla_inicial %>% filter(SCALE>=0)
  #
  # negativos_inicial=tabla_inicial %>% filter(SCALE<0)
  #
  # positivos_final=tabla_final %>% filter(SCALE>=0)
  #
  # negativos_final=tabla_final %>% filter(SCALE<0)
  # regresion de los dos aos
  if (tabla$DATES[1]>fecha_inicio){
    w=lm(tabla$SCALE~tabla$DATES)
    w1=lm(tabla$SCALE~tabla$DATES)
    w2=lm(tabla$SCALE~tabla$DATES)
  }
  else{
    w=lm(tabla_temp$SCALE~tabla_temp$DATES,tabla_temp)

    # regresion primer ao
    w1=lm(tabla_inicial$SCALE~tabla_inicial$DATES,tabla_temp)

    # regresion segundo ao
    w2=lm(tabla_final$SCALE~tabla_final$DATES,tabla_temp)
  }

  # todos los datos de la tabla son positivos?
  if (nrow(positivos)==nrow(tabla_temp)){
    # tabla_media=tabla_temp %>% mutate("MINORS"=ifelse(REVERSION<=media_reversion,1,0))
    # ceros_tabla_media=tabla_media %>% filter(MINORS==1)
    if(w$coefficients[2]<0.001 & w$coefficients[2]>(-0.001)){
      casos[1]=50+casos[1]
    }
    else if (w1$coefficients[2]<(-0.001) & w2$coefficients[2]<(-0.001)){
      casos[2]=50+casos[2]
    }
    else if (w1$coefficients[2]>(0.001) & w2$coefficients[2]>(0.001)){
      casos[8]=50+casos[8]
    }
    else if (w1$coefficients[2]>(0.001) & w2$coefficients[2]<(-0.001)){
      casos[2]=40+casos[2]
      casos[1]=10+casos[1]
    }
    else if(w1$coefficients[2]<(-0.001) & w2$coefficients[2]>(0.001)){
      casos[8]=40+casos[8]
      casos[1]=10+casos[1]
    }
  }
  else if(nrow(positivos)==0){
    # tabla_media=tabla_temp %>% mutate("MAYORES"=ifelse(REVERSION>=media_reversion,1,0))
    # ceros_tabla_media=tabla_media %>% filter(MAYORES==1)
    if (w$coefficients[2]<0.001 & w$coefficients[2]>(-0.001)){
      casos[5]=40+casos[5]
      casos[6]=10+casos[6]
    }
    else if (w1$coefficients[2]<0 & w2$coefficients[2]<0){
      casos[4]=10+casos[4]
      casos[5]=40+casos[5]
    }
    else if (w1$coefficients[2]>0 & w2$coefficients[2]>0){
      casos[7]=50+casos[7]
    }
    else if (w1$coefficients[2]>0 & w2$coefficients[2]<0){
      casos[5]=50+casos[5]
    }
    else if(w1$coefficients[2]<0 & w2$coefficients[2]>0){
      casos[6]=30+casos[6]
      casos[5]=20+casos[5]
    }
  }
  else{
    if (w1$coefficients[2]<0 & w2$coefficients[2]<0){
      casos[3]=35+casos[3]
      casos[4]=15+casos[4]
    }
    else if (w1$coefficients[2]>0 & w2$coefficients[2]>0){
      casos[7]=35+casos[7]
      casos[8]=15+casos[8]
    }
    else if (w1$coefficients[2]>0 & w2$coefficients[2]<0){
      casos[3]=40+casos[3]
      casos[7]=10+casos[7]
    }
    else if(w1$coefficients[2]<0 & w2$coefficients[2]>0){
      casos[3]=10+casos[3]
      casos[7]=40+casos[7]
    }
  }
  return(casos)
}

clasificacion_kafka<-function(tabla,tabla_maximos,fecha_inicio,fecha_final){
  primera_parte=clasificacion_maximos(tabla,tabla_maximos,fecha_inicio,fecha_final)
  segunda_parte=clasificacion_reversion(tabla,fecha_inicio,fecha_final)
  final=primera_parte+segunda_parte
  return(final)
}

crear_tabla_resumen<-function(lista,nombres){
  total=lista[[1]]
  if (length(lista)>1){
    for (i in 2:length(lista)){
      total=cbind(total,lista[[i]])
    }
  }
  total=as.data.frame(total)
  colnames(total)<-nombres
  return(total)
}


warning_tabla<-function(tabla){
  nombres=colnames(tabla)
  war=vector(,nrow(tabla))
  for (i in 1:nrow(tabla)){
    if (tabla$estados[i]!="icarus" & tabla$icarus[i]>30){
      war[i]<-"Icarus has a significant percentage into the algorithm, be careful"
    }
    else if(tabla$estados[i]!="zombie" & tabla$zombie[i]>30){
      war[i]<-"Zombie has a significant percentage into the algorithm, be careful"
    }
    else{
      war[i]<-"-"
    }
  }
  tabla_final=cbind(tabla,war)
  tabla_final=as.data.frame(tabla_final)
  colnames(tabla_final)=c(nombres,"warnings")
  return(tabla_final)
}



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




########### Proceso ###########
tabla=read_xlsx("spy.xlsx")
vec<-as.vector(tabla$TICKERS)

tablas_original<-todos_numero(vec,1)

lista_accion<-normal_indicador(tablas_original)

#pru=prueba_maximos(tablas_original)

### tablas reversion a la media ###
#listas_tab<-lista_tabla_tab(lista_accion)


### Ejecucion kafka ###
# lista_resultados=list()
# for (i in 1:length(tablas_original)){
#   print(i)
#   b=clasificacion_kafka(listas_tab[[i]],pru[[i]],"2018-05-25","2020-05-25")
#   lista_resultados[[i]]<-b
# }

#final=crear_tabla_resumen(lista_resultados,vec)


columna_estado=function(tabla){
  estados=c()
  for (i in 1:nrow(tabla)){
    maxi=which(tabla[i,]==max(tabla[i,]))
    nom=maxi[1]
    estados=c(estados,colnames(tabla)[nom])
  }
  tablaf=cbind(tabla,estados)
  tablaf=as.data.frame(tablaf)
  colnames(tablaf)<-c(colnames(tabla),"estados")
  return(tablaf)
}

cuadrantes<-function(tabla){
  tabla_fin=as.data.frame(seq(1,10))
  for (i in 1:ncol(tabla)){
    caso_maximo=which(tabla[,i]==max(tabla[,i]))
    if (length(caso_maximo)>1){
      caso_maximo=caso_maximo[1]
    }
    if(caso_maximo==1 || caso_maximo==3 || caso_maximo==5 || caso_maximo==7){
      if (caso_maximo==1){
        coordenadas=c(tabla[caso_maximo,i],tabla[caso_maximo,i])
      }
      else if (caso_maximo==3){
        coordenadas=c(-tabla[caso_maximo,i],tabla[caso_maximo,i])
      }
      else if (caso_maximo==5){
        coordenadas=c(-tabla[caso_maximo,i],-tabla[caso_maximo,i])
      }
      else if (caso_maximo==7){
        coordenadas=c(tabla[caso_maximo,i],-tabla[caso_maximo,i])
      }
    }
    else if(caso_maximo==2 || caso_maximo==6){
      if (caso_maximo==2){
        coordenadas=c(0,tabla[caso_maximo,i])
      }
      else if (caso_maximo==6){
        coordenadas=c(0,-tabla[caso_maximo,i])
      }
    }
    else if(caso_maximo==4 || caso_maximo==8){
      if (caso_maximo==4){
        coordenadas=c(-tabla[caso_maximo,i],0)
      }
      else if (caso_maximo==8){
        coordenadas=c(tabla[caso_maximo,i],0)
      }
    }
    fila=c(tabla[,i],coordenadas)
    colu=as.data.frame(fila)
    tabla_fin=cbind(tabla_fin,colu)
  }
  total=data.frame(t(tabla_fin))
  total=total[-1,]
  colnames(total)<-c("rockstar","rockstar-icarus","icarus","icarus-zombie","zombie",
                     "zombie-gem","gem","gem-rockstar","ejex","ejey")
  rownames(total)<-colnames(tabla)
  total=columna_estado(total)
  return(total)
}


#final=cuadrantes(final)

#final=warning_tabla(final)



con <- dbConnect(drv     = RMySQL::MySQL(),
                username = "macro",
                password = "macrowise",
                host     = "macro.c8rej9nslv8v.us-east-2.rds.amazonaws.com",
                port     = 3306,
		dbname   = "macrowise")

for (i in 1:length(lista_accion)){
   dbWriteTable(con,paste0(vec[i],"_spy"), lista_accion[[i]])
}


#dbWriteTable(con,"spy_kafka", final)
dbDisconnect(con)
