if (!require('caret')) install.packages('caret')
if (!require('corrplot')) install.packages('corrplot')
if (!require('RColorBrewer')) install.packages('RColorBrewer')
if (!require('glmnet')) install.packages('glmnet')
if (!require('corrr')) install.packages('corrr')
if (!require('dplyr')) install.packages('dplyr')
if (!require('Metrics')) install.packages('Metrics')
library(readxl)
library(psych)
library("corrplot")
library(xlsx)
library(moments)
#Lectura e inspecci?n de estructura de los datos

df <- read.table('ventas_exam_model_muj1.txt', sep = '\t',header = TRUE, dec = ".")
str(df)
df

describe(df$ropamujer)
describe(df$correo)
describe(df$páginas)
describe(df$teléfono)
describe(df$impresa)
describe(df$nomina)
describe(df$servicio)

table(df$promo)
table(df$idmercado)
#carros<-within(carros,rm("ID","modelo","precio_basico","precio_equipado"))
#dim(carros)
#colnames(carros)

#crear variables dummy (binarias)
dfdumm<-dummyVars("~.",data=df, drop2nd = TRUE)
df_1<-as.data.frame(predict(dfdumm,newdata=df))
df_1
dff<-within(df_1,rm(tamañomer.Grande))
str(dff)
#veamos la dimension final de la base de datos
dim(dff)

colnames(df_1)

#retiro la variable a predecir
#predictores<-cbind(as.matrix(carrosfin2[,1:27]),as.matrix(carrosfin2[,29:58]))
predictores<-dff[,-1]
predictores

#Gr?fico de correlaciones
corpred <- cor(predictores)
corrplot(corpred, method="square",tl.cex = 0.7,col=brewer.pal(n=8, name="PuOr"),
         type = "upper", tl.pos = 'n')


corpred

#Filtrando variables altamente correlacionadas
corfil <- predictores %>% 
  correlate(use = "pairwise.complete.obs") %>% 
  shave() %>% 
  stretch(na.rm = TRUE) %>% 
  filter(between(r, 0.6, 1)|between(r,-1,-0.6))

View(corfil)
dff

hist(dff$servicio)


dff$idloc <- NULL
dff$servicio <- NULL
dff$idmercado <- NULL
dff$nomina <- scale(dff$nomina)
dff$correo <- scale(dff$correo)
dff$impresa <- scale(dff$impresa)
dff$páginas <- scale(dff$páginas)
dff$teléfono <- scale(dff$teléfono)
dff$servicio <- scale(dff$servicio)

summary(dff)
colnames(dff)
df.train <- dff



#############################
# haciendo el modelo
modeloaug<-lm(ropamujer~.,data=df.train)
modelocarstep<-step(modeloaug,direction="both",trace=0)
summary(modelocarstep)

df_test <- read.table('ventas_exam_qualify_muj1.txt', sep = '\t',header = TRUE, dec = ".")

# Predicciones sobre el test set

# Stepwise

df_taux <- df_test
df_taux$nomina <- scale(df_taux$nomina)
df_taux$correo <- scale(df_taux$correo)
df_taux$impresa <- scale(df_taux$impresa)
df_taux$páginas <- scale(df_taux$páginas)
df_taux$teléfono <- scale(df_taux$teléfono)

pred_step<-predict(modelocarstep,df_taux)

pred_step
