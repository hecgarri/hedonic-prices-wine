if (!require(foreign)) install.packages("foreign"); require(foreign) ## Importar bases de datos en otros formatos 
if (!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)## Para gráficas más sofisticadas 
if (!require(MASS)) install.packages("MASS"); require(MASS)
if (!require(Hmisc)) install.packages("Hmisc"); require(Hmisc)
if (!require(reshape2)) install.packages("reshape2"); require(Hmisc) # Para dar formato a las data frames 
if (!require(dplyr)) install.packages("dplyr"); require(dplyr) # para el operador %>% 
if (!require(VGAM)) install.packages(VGAM); require(VGAM) # Modelos ordinales

rm(list = ls())
#dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
setwd("/home/hector/GoogleDrivePersonal/Docencia/Tesistas/Rodrigo Monje /")

data = read.csv("datos_vino_MEJORADA.csv") %>% filter(valle!="MENDOZA", valle!="ESPAÑA")
 
 # 80-84 : correcto o bueno 
 # 85-86: buenos aspirando a muy buenos 
 # 87-88: Muy buenos 
 # 89-90: Vinos que sorprenden 
 # 91-92: Excelente, con clase 
 # 93-94: Vinos sublimes 
 # >95: camino a la perfección 

data = mutate(data, tramos = ifelse(puntaje>=80 & puntaje<=84,1, 
                             ifelse(puntaje>=85 & puntaje<=86,2,
                             ifelse(puntaje>=87 & puntaje<=88,3, 
                             ifelse(puntaje>=89 & puntaje<=90,4,
                             ifelse(puntaje>=91 & puntaje<=92,5,
                             ifelse(puntaje>=93 & puntaje<=94,6, 
                             ifelse(puntaje>=95,7,NA)))))))) 

data = mutate(data, tramos2 = ifelse(tramos==1 | tramos==2,1, 
                              ifelse(tramos==3,2, 
                              ifelse(tramos==4,3, 
                              ifelse(tramos==5,4, 
                              ifelse(tramos==6 | tramos==7,5,NA))))))  
table(data$tramos2)

 # Reserva entre 4.000 a 6.999
 # Gran reserva 7.000  a 11.999
 # Premium de 12.000 a 19.999
 # Super premium 20.000 a 39.999
 # Ícono >40.000

data = mutate(data, precio2 = ifelse(pmercado>=2990 & pmercado<=6999,1,
              ifelse(pmercado>=7000 & pmercado<=11999,2,
              ifelse(pmercado>=12000 & pmercado<=19999,3,
              ifelse(pmercado>=20000 & pmercado<=39999,4,
              ifelse(pmercado>=40000,5,NA))))))


data = mutate(data, p_r = ifelse(precio2==1,1,0))
data = mutate(data, p_gr = ifelse(precio2==2,1,0))
data = mutate(data, p_p = ifelse(precio2==3,1,0))
data = mutate(data, p_sp = ifelse(precio2==4,1,0))
data = mutate(data, p_i = ifelse(precio2==5,1,0))


data$tramos2 = factor(data$tramos2, ordered=TRUE) 
data$precio2 = factor(data$precio2, ordered=TRUE) 


prop.table(table(data$tramos))


ftable(xtabs(~pmercado+tramos, data=data))

data = mutate(data, cs = ifelse(cepa=="CABERNET SAUVIGNON",1,0))
data = mutate(data, ca = ifelse(cepa=="CARMENERE",1,0))
data = mutate(data, me = ifelse(cepa=="MERLOT", 1,0))
data = mutate(data, sy = ifelse(cepa=="SYRAH",1,0))
data = mutate(data, et = ifelse(cepa=="ENSAMBLAJE TINTO",1,0))
data = mutate(data, oc = ifelse(cepa=="CABERNET FRANC" |
                                  cepa=="CARIGNAN" |
                                  cepa =="CINSAULT"| 
                                  cepa =="GEWURZTRAMINER" |
                                  cepa =="MALBEC" |
                                  cepa=="NEBBIOLO" | 
                                  cepa =="PAIS" |
                                  cepa=="PETIT VERDOT"|
                                  cepa=="PINOT NOIR" |
                                  cepa=="ZINFANDEL ",1,0))


data = mutate(data, csca = ifelse(cs==1 | ca==1,1,0))

data = mutate(data, came = ifelse(ca==1 | me==1,1,0))



data$precio2 = factor(data$precio2, ordered = TRUE)

data = mutate(data, NORTE=ifelse(is.na(NORTE),0,1))
data = mutate(data, CENTRO=ifelse(is.na(CENTRO),0,1))
data = mutate(data, SUR=ifelse(is.na(SUR),0,1))

#####################################################################################################
data$puntaje_sm = data$puntaje-mean(data$puntaje, na.rm=TRUE)
data = mutate(data, organolepticas = (astringencia+
                  frescor+ cuerpo+ dulzura)/4)

data = mutate(data, colchagua = ifelse(valle=="COLCHAGUA" |
                                         valle =="COLCHAGUA COSTA"|
                                         valle =="ALTO COLCHAGUA",1,0), 
              antigüedad = 2016-año)


m11 <- polr(precio2 ~ gran.reserva+super.premium+premium+icono+
            csca+me+sy+et+puntaje_sm+I(puntaje_sm^2)+
              astringencia+frescor+dulzura+cuerpo+medianas.bajo.precio+
              emergentes.exclusivas+valle.maipo+colchagua+
              antigüedad,
            data=data, Hess=TRUE)
m12 = polr(precio2~1, data=data, Hess=TRUE)

1-(logLik(m11)/logLik(m12))

(ctable1 <- coef(summary(m11)))

p1 <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE)*2

## combined table
(ctable1 <- cbind(ctable1, "p value" = round(p1, digits=2)))

data = mutate(data, precio_sm =  pmercado-14613.18)

m21 = polr(tramos2 ~ gran.reserva +premium+ super.premium + icono + csca + me + 
                   sy + et + astringencia + frescor + cuerpo + dulzura+
              medianas.bajo.precio+emergentes.exclusivas+
              antigüedad+I(antigüedad^2)+p_gr+p_p+p_sp+p_i+
              valle.maipo+colchagua,
                 data=data, Hess=TRUE)

phat = predict(m21, type="probs")

write.csv(phat, "simulacion_precios.csv")

(ctable <- coef(summary(m21)))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2

## combined table
(ctable <- cbind(ctable, "p value" = round(p, digits=2)))

m22 = polr(tramos2 ~ 1,
           data=data, Hess=TRUE)

r2 = 1-(logLik(m21)/logLik(m22))

#####################################################################################################

if (!require(quantreg)) install.packages("quantreg")
if (!require(stargazer)) install.packages("stargazer")


data = data %>% filter(oc!=1)

modelo = rq(log(pmercado) ~ gran.reserva+premium+super.premium+icono+
     cs+came+sy+et+
     medianas.bajo.precio+astringencia+frescor+cuerpo+dulzura+
     emergentes.exclusivas+valle.maipo+colchagua+
     antigüedad+I(antigüedad**2)+I(emergentes.exclusivas*super.premium)+
       I(medianas.bajo.precio*et)+
       I(medianas.bajo.precio*gran.reserva)+
       I(colchagua*icono)+I(colchagua*gran.reserva)+
       I(colchagua*premium),tau = .5, data=data)
summary(modelo, se = "boot", bsmethod = "xy", R = 1000)

stepwise = stepAIC(modelo, direction = c("both"))

summary(stepwise)

modelo1 = rq(formula(stepwise), tau = .5, data = data)
sumM1 = summary(modelo1, se = "boot", bsmethod = "xy", R = 1000)

modelo2 = rq(formula(stepwise),tau = .25, data=data)
sumM2 = summary(modelo2, se = "boot", bsmethod = "xy", R = 1000)

modelo3 = rq(formula(stepwise),tau = .75,data=data)
sumM3 = summary(modelo3, se = "boot", bsmethod = "xy", R = 1000)

QR = rq(formula(stepwise),tau = seq(0.05,0.95, by=0.05), data=data)
sumQR = summary(QR, se = "boot", bsmethod = "xy", R = 1000)

plot(sumQR, ols = FALSE)


############################################################
############################################################

modelo = rq(log(puntaje) ~ gran.reserva+premium+super.premium+icono+
              cs+came+sy+et+
              medianas.bajo.precio+astringencia+frescor+cuerpo+dulzura+
              emergentes.exclusivas+valle.maipo+colchagua+cuma+
              antigüedad+I(antigüedad**2)+I(emergentes.exclusivas*super.premium)+
              I(medianas.bajo.precio*et)+
              I(medianas.bajo.precio*gran.reserva)+
              I(colchagua*icono)+I(colchagua*gran.reserva)+
              I(colchagua*premium),tau = .5, data=data)
summary(modelo, se = "boot", bsmethod = "xy", R = 1000)

stepwise = stepAIC(modelo, direction = c("both"))

summary(stepwise)

form = "log(puntaje) ~ gran.reserva + premium + super.premium + icono + 
    sy + astringencia + frescor + cuerpo + antigüedad + I(antigüedad^2) + 
    I(colchagua * icono)+medianas.bajo.precio+emergentes.exclusivas+premium*et+
colchagua*premium"

modelo1 = rq(formula(stepwise), tau = .5, data = data)
sumM1 = summary(modelo1, se = "boot", bsmethod = "xy", R = 1000)

modelo2 = rq(formula(stepwise),tau = .25, data=data)
sumM2 = summary(modelo2, se = "boot", bsmethod = "xy", R = 100)

modelo3 = rq(formula(stepwise),tau = .75,data=data)
sumM3 = summary(modelo3, se = "boot", bsmethod = "xy", R = 100)

QR = rq(form,tau = seq(0.05,0.95, by=0.05), data=data)
sumQR = summary(QR, se = "boot", bsmethod = "xy", R = 100)

plot(sumQR, ols = FALSE)


modelo = lm(form, data = data)

require(car)






