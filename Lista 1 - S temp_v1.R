install.packages("graphics")

library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)
library(readxl)


# Exercicio 1 -------------------------------------------------------------
getwd()
setwd("C:/Users/ipandolfo/OneDrive/P?s Gradua??o/An?lise de S?ries Temporais/_Tarefas/Tarefa 01")


#carrega os dados (nao sei pq as tabelas desse prof carregam td cagado pra mim...)

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
ts.plot(d_ts, main='S?rie D - Aleat?ria ')
abline(h = mean(d_ts), col = "blue") 

plot(diff(d_ts))

acf(diff(d_ts))

acf(d)

pacf(d_ts)

#e e)	Série com tendência estocástica, xt = xt-1 + N(1,5^2)

e <- NULL
e[1] <- rnorm(1)
for(i in 2:200){
  e[i] <- e[i-1] + rnorm(1,1,5)
}
e
e_ts <- ts(e)
ts.plot(e_ts, main='S?rie E - Tend?ncia Estoc?stica ')
acf(e)
pacf(e)


#f Serie com correlação de curto-prazo
set.seed(42)
f <- NULL
f[1] <- rnorm(1)
for(i in 2:200){
  f[i] <- 0.7*f[i-1] + rnorm(1)
}
f
f_ts <- ts(f)
ts.plot(f_ts, main='S?rie F - Correla??es de Curto Prazo')
acf(f)
pacf(f)

#g Serie com correlações negativas
g <- NULL
g[1] <- rnorm(1)
for(i in 2:200){
  g[i] <- -0.8*g[i-1] + rnorm(1)
}
g
g_ts <- ts(g)
ts.plot(g_ts, main='S?rie G - Correla??es Negativas')
acf(g)
pacf(g)



#h h)	Medias moveis ??? - faltei na aula que o prof explicou isso e não entendi bem o que fazer aqui 

set.seed(42)
ts.ma <- arima.sim(model=list(ma=0.6), n=200, innov = rnorm(200, 0, 1))
ts.plot(ts.ma, main='S?rie H - M?dia M?veis')
acf(ts.ma)
pacf(ts.ma)





# Exercício 3 -------------------------------------------------------------
#a le o url e baixa os dados, retirando as 4 primeiras linhas
skirts <- read.table("https://robjhyndman.com/tsdldata/roberts/skirts.dat", header=TRUE, skip=4)
#cria a ts
skirts_ts <- ts(skirts, frequency = 1, start = c(1866))
plot.ts(skirts_ts)

#b não consegui decompor a série. O problema aqui é a frequência, pois os dados são anuais então deveria ser igual a 1, porém a funçaõ decompose só funciona para frequências maiores ou iguais a 2

dec <- decompose(skirts_ts)
plot(dec)

#c

acf(skirts_ts)
pacf(skirts_ts)

# faz a série das diferenças
dskirts_ts <- diff(skirts_ts)
acf(dskirts_ts)
pacf(dskirts_ts)


# Exercício 4 -------------------------------------------------------------
# Exercicio 4 -------------------------------------------------------------

#a) Processo AR(1) onde ??0=0, ??1=0.7

xa = arima.sim(model=list(ar=0.7), n=300)
acf(xa)
pacf(xa)

plot(xa,lty=1, bty='l,', col=1, main='',ylab=expression(paste(Y[t])))

ya = arima(xa, order = c(1,0,0))
summary(ya)

#b) Processo AR(1) onde ??0=0, ??1= -0.7

xb = arima.sim(model=list(ar=-0.7), n=300)
acf(xb)
pacf(xb)

plot(xb,lty=1, bty='l,', col=1, main='',ylab=expression(paste(Y[t])))

yb = arima(xb, order = c(1,0,0))
summary(yb)

#c) Processo AR(2) onde ??0=0, ??1=0.3 e ??2=0.5

xc = arima.sim(model=list(ar=0.5), n=300)
acf(xc)
pacf(xc)

plot(xc,lty=1, bty='l,', col=1, main='',ylab=expression(paste(Y[t])))

yc = arima(xc, order = c(2,0,0))
summary(yc)


#d) Processo MA(1) onde ??0=0, ??1=0.6
TT <- 300
am1 = arima.sim(model=list(am=0.6), n=TT)
plot(am1,main="AM(1) com theta = 0.6")


#e) Processo MA(1) onde ??0=0, ??1=-0.6
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
     main = "Evolu??o do PIB brasileiro nos ?ltimos 12 anos")
modelo_pib12 = auto.arima(pib12_ts)
modelo_pib12
forecast(modelo_pib12,2)