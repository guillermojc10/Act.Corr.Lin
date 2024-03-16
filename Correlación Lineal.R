'''{r, echo = FALSE}
knitr:opts_chunk$set(echo = TRUE)
'''

'''{r, echo = TRUE}
library(readxl)
data <- as.data.frame(read_excel("C:/Correlación lineal/data.xls"))
View(data)
print(data)
'''

#Act 1. teórica
#La correlación lineal funciona como una medida estádistica que se encarga
#de evalurar la relación lineal entre dos variables cuantitativas. Es decir, 
#actúa como medida de fuerza y de dirección en las relaciones de ambas variables. 
#En el caso de que dos variables tengan una correlación positiva: una variable aumenta
#y la otra también lo hace. Si es negativa: cuando una aumenta, la otra disminuye.
#La correlación lineal tiende a representarse mediante el coeficiente de correlación
#reconocido normalmente como "r", el cual va a variar entre -1 (negativa) y 1 (positiva). 
#El hecho de que dos variables esten correlacionadas no significa que une cause a la otra.

#Act 2. teórica
#La correlación lineal puede considerarse una prueba de correlación paramétrica, debido a
#que los datos asumidos siguen distribucion específica y tienen una relación lineal entre sí.
#Las que son pruebas paramétricas suelen requerir ciertas suposiciones sobre la población 
#subyacente la naturaleza de los datos empleados. Las pruebas que se consideran no paramétricas son las que no llegan 
#hacer estas suposiciones sobre la distribución de la población subyacente. Se suelen utilizar
#cuando los datos no cumplen con los supuestos de normalidad u otras suposiciones. 

#Act 3.
'''{r, echo = TRUE}
correl_datos <- cor(data)
print(correl_datos)

#Act 4. 
'''{r, echo = TRUE}
panel.cor <- function(x,y,digits = 2, prefix ="", cex.cor,...){
  usr<-par("usr")
  on.exit(par(usr))
  par(usr=c(0,1,0,1))
  Cor<-abs(cor(x,y))
  txt<-paste0(prefix,format(c(Cor, 0.123456789),digits=
                              digits)[1])
  if(missing(cexx.cor)) {
    cex.cor<-0.4/strwidth(txt)
  }
text(0.5,0.5, txt,
     cex=1+cex.cor*Cor)
}

#Act 5. 
'''{r,echo = TRUE}
library(correlation)
matriz<- correlation(data)
'''

#Act 6. 
''' {r,echo = TRUE}
library(ggpubr)
library(ggplot2)
ggscatter(data,x="altura", y="peso",
add= "reg.line", conf.int = TRUE,
cor.coef = TRUE, cor.method = &quot;pearson&quot;,
xlab = &quot;altura piezas (mm)&quot;, ylab = &quot;peso piezas (mg)&quot;)

#Act 7.
```{r, echo = TRUE}
library(corrplot)
corrplot(cor(data))
```

#Act 8.
#A
```{r, echo = TRUE}
distancia <- c( 1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)
n_piezas <- c(110,2,6,98,40,94,31,5,8,10)
datos_2 <- data.frame(distancia, n_piezas)
print(datos_2)
```

#B
```{r, echo = TRUE}
correlacion_datos_2 <- cor(datos_2)
print(correlacion_datos_2)
```

#C 
```{r, echo = TRUE}
significancia_datos_2 <- cor.test(datos_2$distancia,
datos_2$n_piezas)$p.value
print(significancia_datos_2)
```

#D
```{r, echo = TRUE}
intervaloconfianza_datos_2 <- cor.test(datos_2$distancia,
datos_2$n_piezas)$conf.int
print(intervaloconfianza_datos_2)
```
#E
La intensidad es alta ya que se acerca a 1

#F
Si

#G
No. Es necesario que haya más variabilidad de datos

#Act 9 y Act 10 no se hacerlas. 