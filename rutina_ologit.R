rm(list = ls())

setwd("/home/hector/GoogleDrivePersonal/Docencia/Tesistas/Rodrigo Monje /")


if (!require(foreign)) install.packages("foreign"); require(foreign) ## Importar bases de datos en otros formatos 
if (!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)## Para gráficas más sofisticadas 
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse) # para el operador %>% 
if (!require(quantreg)) install.packages("quantreg"); require(quantreg)
if (!require(stargazer)) install.packages("stargazer"); require(stargazer)
if (!require(sandwich)) install.packages("sandwich"); require(sandwich)
if (!require(lmtest)) install.packages("lmtest"); require(lmtest)
# Se quitan los datos de vinos de mendoza y de españa. 
data = read.csv("datos_vino_MEJORADA.csv") %>% filter(valle!="MENDOZA", valle!="ESPAÑA")
 

data = mutate(data,
      tramos = ifelse(puntaje>=80 & puntaje<=84,1,  # 80-84 : correcto o bueno 
      ifelse(puntaje>=85 & puntaje<=86,2, # 85-86: buenos aspirando a muy buenos 
      ifelse(puntaje>=87 & puntaje<=88,3,  # 87-88: Muy buenos  
      ifelse(puntaje>=89 & puntaje<=90,4,  # 89-90: Vinos que sorprenden 
      ifelse(puntaje>=91 & puntaje<=92,5,  # 91-92: Excelente, con clase 
      ifelse(puntaje>=93 & puntaje<=94,6,  # 93-94: Vinos sublimes 
      ifelse(puntaje>=95,7,NA))))))), # >95: camino a la perfección
      tramos2 = ifelse(tramos==1 | tramos==2,1, 
      ifelse(tramos==3,2, 
      ifelse(tramos==4,3, 
      ifelse(tramos==5,4, 
      ifelse(tramos==6 | tramos==7,5,NA))))),
      puntajesq = puntaje^2)   

data = mutate(data, 
              precio2 = ifelse(pmercado>=2990 & pmercado<=6999,1, # Reserva entre 4.000 a 6.999
                        ifelse(pmercado>=7000 & pmercado<=11999,2,  # Gran reserva 7.000  a 11.999
                        ifelse(pmercado>=12000 & pmercado<=19999,3,  # Premium de 12.000 a 19.999
                        ifelse(pmercado>=20000 & pmercado<=39999,4, # Super premium 20.000 a 39.999
                        ifelse(pmercado>=40000,5,NA))))))  # Ícono >40.000

data = mutate(data, p_r = ifelse(precio2==1,1,0)) # Precio reserva
data = mutate(data, p_gr = ifelse(precio2==2,1,0)) #Precio gran reserva
data = mutate(data, p_p = ifelse(precio2==3,1,0)) #Precio premium 
data = mutate(data, p_sp = ifelse(precio2==4,1,0)) #Precio super premium 
data = mutate(data, p_i = ifelse(precio2==5,1,0)) # Precio ícono

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

data = mutate(data, csca = ifelse(cs==1 | ca==1,1,0)) #cabernet y carmenere
data = mutate(data, came = ifelse(ca==1 | me==1,1,0)) #cabernet y merlot

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
              antigüedad = 2016-año, 
              antisq = antigüedad^2)

#Estamos excluyendo otras cepas 

data = data %>% filter(oc!=1)

form = "log(pmercado) ~ gran.reserva+premium+super.premium+icono+
  cs+came+sy+et+
  medianas.bajo.precio+emergentes.exclusivas+
  valle.maipo+colchagua+
  puntaje+puntajesq+
  antigüedad+antisq"

ols = lm(form, data = data)
coeftest(ols, vcov = vcovHC(ols, "HC1"))

modelo1 = rq(form,tau = .25, data=data, method = "br", 
            contrasts = TRUE)
modelo2 = rq(form,tau = .5, data=data, method = "br", 
             contrasts = TRUE)
modelo3 = rq(form,tau = .75, data=data, method = "br", 
             contrasts = TRUE)

rho = function(u, tau=.5) u*(tau-(u<0))

V_hat1 = sum(rho(modelo1$residuals, modelo1$tau)) 
V_tilde1 = sum(rho(rq(log(data$pmercado)~1, tau = .25,
                      method = "fn")$residuals,
                   modelo1$tau))
  
R1 = 1-(V_hat1/V_tilde1)  

V_hat2 = sum(rho(modelo2$residuals, modelo2$tau)) 
V_tilde2 = sum(rho(rq(log(data$pmercado)~1, tau = .5,
                      method = "fn")$residuals,
                   modelo2$tau))

R2 = 1-(V_hat2/V_tilde2)  


V_hat3 = sum(rho(modelo3$residuals, modelo3$tau)) 
V_tilde3 = sum(rho(rq(log(data$pmercado)~1, tau = .75,
                      method = "fn")$residuals,
                   modelo3$tau))

R3 = 1-(V_hat3/V_tilde3)  

sumM1 = summary(modelo1, se = "boot", bsmethod = "xy", R = 1000)
sumM2 = summary(modelo2, se = "boot", bsmethod = "xy", R = 1000)
sumM3 = summary(modelo3, se = "boot", bsmethod = "xy", R = 1000)

stargazer(sumM1$coefficients, type = "text")
stargazer(sumM2$coefficients, type = "text")
stargazer(sumM3$coefficients, type = "text")



QR = rq(form,tau = seq(0.05,0.95, by=0.05), data=data, method = "fn")
sumQR = summary(QR, se = "boot", bsmethod = "xy", R = 100)

plot(sumQR, parm = c(-1,-6,-7,-8,-9),ols = FALSE)

summary(ols$model)




modelo1 = rq(log(pmercado) ~ gran.reserva+premium+super.premium+icono+
               cs+came+sy+et+medianas.bajo.precio+emergentes.exclusivas+
               valle.maipo+colchagua+puntaje+puntajesq+antigüedad+antisq,
             tau = .25, data=data, method = "fn", 
             contrasts = FALSE)
modelo2 = rq(form,tau = .5, data=data, method = "fn", 
             contrasts = FALSE)
modelo3 = rq(form,tau = .75, data=data, method = "fn", 
             contrasts = FALSE)


anova(modelo1,modelo2, joint = FALSE,se = "ker")
anova(modelo2,modelo3, joint = FALSE,se = "ker")
anova(modelo1,modelo3, joint = FALSE,se = "ker")