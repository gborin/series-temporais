#install.packages("graphics")

library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)
library(readxl)


# Exercicio 1 -------------------------------------------------------------
#getwd()
#setwd("C:/Users/ipandolfo/OneDrive/P?s Gradua??o/An?lise de S?ries Temporais/_Tarefas/Tarefa 01")

dados <- read_excel("Serie_Dados.xls", 
                    col_types = c("date", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric"))

# funcao pra trazer os retornos

retorno <- function (x) {
  ret <- NULL
  for (i in 1:length(x)) {
    ret[i] <- log(x[i+1]/x[i]) 
  }
  return(ret)
}


#calcula toda a matriz de retornos e remove a ultima linha (NAs)
retornos <- apply(dados[2:ncol(dados)], 2, retorno)
retornos <- as.data.frame(head(retornos,-1))

#calcula a media e desvio padrao
media <- apply(retornos, 2, mean)
desvio_padrao <- apply(retornos, 2, sd)
 
desc <- rbind(media, desvio_padrao)
 
#plota os histogramas

#transforma a matriz em long format
retornos_long <- gather(retornos, "nome")

#plota os histogramas (aqui quem manjar de estética ggplot pode ajudar)

ggplot(retornos_long, aes(x=value)) + xlim(-0.1,0.1) + geom_histogram(binwidth = 0.01) + facet_wrap(~nome)

 
#calcula o acf e pacf para a tabela toda
 
dados_2 <- dados[,2:ncol(dados)]
dados_ts <- apply(dados_2, 2, ts)

acf<-apply(dados_ts, 2, acf)
pacf<-apply(dados_ts, 2, pacf)


# Exercicio 2 -------------------------------------------------------------

#d	Serie aleatoria, observacoes iid da distribuicao N(0,1)

set.seed(42)

d<-rnorm(200, 0, 1)
d_ts <- ts(d)
plot.ts(d_ts)
ts.plot(d_ts, main='Série D - Aleatória ')
abline(h = mean(d_ts), col = "blue") 

plot(diff(d_ts))

acf(diff(d_ts))

pacf(diff(d_ts))

# A série temporal diff(d_ts) é estacionária e podemos fazer diff(d_ts(t))=alfa+beta1*diff(d_ts(t-1))+beta2*diff(d_ts(t-2))+beta3*diff(d_ts(t-3))

#e e)	Série com tendência estocástica, xt = xt-1 + N(1,5^2)

e <- NULL
e[1] <- rnorm(1)
for(i in 2:200){
  e[i] <- e[i-1] + rnorm(1,1,5)
}
e
e_ts <- ts(e)
ts.plot(e_ts, main='Série E - Tendência Estocástica ')
acf(e_ts)
pacf(e_ts)

# A série temporal e_ts é estacionário e possui apenas uma componente principal, assim e_ts(t)=alfa+beta1*e_ts(t-1)

#f Serie com correlação de curto-prazo
set.seed(42)
f <- NULL
f[1] <- rnorm(1)
for(i in 2:200){
  f[i] <- 0.7*f[i-1] + rnorm(1)
}
f
f_ts <- ts(f)
ts.plot(f_ts, main='Série F - Correlações de Curto Prazo')
acf(f)
pacf(f)

# A série temporal f_ts é estacionário e possui apenas uma componente principal, assim f_ts(t)=alfa+beta1*f_ts(t-1)

#g Serie com correlações negativas
g <- NULL
g[1] <- rnorm(1)
for(i in 2:200){
  g[i] <- -0.8*g[i-1] + rnorm(1)
}
g
g_ts <- ts(g)
ts.plot(g_ts, main='Série G - Correlações Negativas')
acf(g)
pacf(g)

# Assim como no exemplo anterior, a série temporal g_ts é estacionário e possui apenas uma componente principal, assim g_ts(t)=alfa+beta1*g_ts(t-1)

#h h)	Medias moveis 

set.seed(42)
ts.ma <- arima.sim(model=list(ma=0.6), n=200, innov = rnorm(200, 0, 1))
ts.plot(ts.ma, main='Série H - Médias Móveis')
acf(ts.ma)
pacf(ts.ma)

#A série temporarl ts.ma é estacionário e pode ser escrita como ts.ma(t)=alfa1*ts.ma(t-1)+alfa2*epsilon(t-1)+epsilon(t)


# Exercício 3 -------------------------------------------------------------
#a le o url e baixa os dados, retirando as 4 primeiras linhas
skirts <- read.table("https://robjhyndman.com/tsdldata/roberts/skirts.dat", header=TRUE, skip=4)
#cria a ts
skirts_ts <- ts(skirts, frequency = 1, start = c(1866))
plot.ts(skirts_ts)

#b Faça a decomposição da série do item (a): Sazonalidade, Tendência e Aleatória.
skirts_time_series=ts(skirts_ts,frequency = 8)
decompose_skirts=decompose(skirts_time_series,"additive")

plot(as.ts(decompose_skirts$seasonal))
plot(as.ts(decompose_skirts$trend))
plot(as.ts(decompose_skirts$random))
plot(decompose_skirts)

# Additive:  Time series = Seasonal + Trend + Random

install.packages("forecast")
library(forecast)
trend_skirts_ts = ma(skirts_ts, order = 4, centre = T)
plot(as.ts(skirts_ts))
lines(trend_skirts_ts)
plot(as.ts(trend_skirts_ts))

detrend_skirts_ts = skirts_ts - trend_skirts_ts
plot(as.ts(detrend_skirts_ts))

m_skirts = t(matrix(data = detrend_skirts_ts, nrow = 8))
seasonal_skirts = colMeans(m_skirts, na.rm = T)
plot(as.ts(rep(seasonal_skirts,16)))

random_skirts = skirts_ts - trend_skirts_ts - seasonal_skirts
plot(as.ts(random_skirts))

recomposed_skirts = trend_skirts_ts+seasonal_skirts+random_skirts
plot(as.ts(recomposed_skirts))
plot(as.ts(skirts_ts))

#c

acf(skirts_ts)
pacf(skirts_ts)

# faz a série das diferenças
dskirts_ts <- diff(skirts_ts)
acf(dskirts_ts)
pacf(dskirts_ts)


# Exercicio 4 -------------------------------------------------------------

#a) Processo AR(1) onde ??0=0, ??1=0.7

xa = arima.sim(model=list(ar=0.7), n=300)
acf(xa)
pacf(xa)

plot(xa,lty=1, bty='l,', col=1, main='',ylab=expression(paste(Y[t])))

ya = arima(xa, order = c(1,0,0))
summary(ya)

#b) Processo AR(1) onde θ0=0, θ1= -0.7

xb = arima.sim(model=list(ar=-0.7), n=300)
acf(xb)
pacf(xb)

plot(xb,lty=1, bty='l,', col=1, main='',ylab=expression(paste(Y[t])))

yb = arima(xb, order = c(1,0,0))
summary(yb)

#c) Processo AR(2) onde θ0=0, θ1=0.3 e θ2=0.5

xc = arima.sim(model=list(ar=0.5), n=300)
acf(xc)
pacf(xc)

plot(xc,lty=1, bty='l,', col=1, main='',ylab=expression(paste(Y[t])))

yc = arima(xc, order = c(2,0,0))
summary(yc)


#d) Processo MA(1) onde θ0=0, θ1=0.6
TT <- 300
am1 = arima.sim(model=list(am=0.6), n=TT)
plot(am1,main="AM(1) com theta = 0.6")


#e) Processo MA(1) onde θ0=0, θ1=-0.6
TT <- 300
am2 = arima.sim(model=list(am=-0.6), n=TT)
plot(am2,main="AM(1) com theta = -0.6")


# Exercício 5 -------------------------------------------------------------
#eu tratei o arquivo csv no excel pra conseguir carregar como números, o desafio aqui é fazer tudo pelo R.

pib<-read.csv2("STP-20180627212931685.csv", header = TRUE)

#a plota o gráfico da série (usei frequency = 4 pois os dados são trimestrais)

pib_ts<- ts(pib$pib, frequency = 4, start = c(1991,1))
plot.ts(pib_ts)

#b faz a decomposição
dec <- decompose(pib_ts)
plot(dec)   

#c faz a previsão
pib12_ts = ts(pib12$pib, frequency =4, start = c(2002,1))
plot(pib12_ts, 
     main = "Evolução do PIB brasileiro nos últimos 12 anos")
modelo_pib12 = auto.arima(pib12_ts)
modelo_pib12
forecast(modelo_pib12,2)
