# series-temporais
1-  Utilizando o arquivo “Serie_Dados.csv” realize as seguintes etapas:
a)	 Crie a série temporal dos retornos Ln, ou seja, r=Ln(P_t+1 /P_t)

b)	Para cada ação construa o histograma dos retornos. Comente o resultado dos histogramas, verifique também o desvio padrão e a média de cada série

c)	Calcule o ACF e PACF de cada série de retornos. Comente os resultados.

2-   Para cada um dos processos abaixo gere 200 observações. Faça um gráfico da série, ACF e PACF. Comente os resultados.
d)	Série aleatória, observações iid da distribuição N(0,1)

e)	Série com tendência estocástica, 
 
f)	Serie com correlação de curto-prazo, 
 
g)	Serie com correlações negativas
 
h)	Medias moveis, 
 


3- Utilize a série abaixo para resolver cada item.
An example of a time series that can probably be described using an additive model with a trend and no seasonality is the time series of the annual diameter of women’s skirts at the hem, from 1866 to 1911. The data is available in the filehttp://robjhyndman.com/tsdldata/roberts/skirts.dat (original data from Hipel and McLeod, 1994).
a)	Faça a leitura da série de dados e os tratamentos necessários para considerar a mesma como uma série temporal
b)	Faça a decomposição da série do item (a): Sazonalidade, Tendência e Aleatória.
c)	Calcule a ACF e PACF da série e da primeira diferença

4- Usando a função arima.sim gere as seguintes simulações (300 ptos):
a) Processo AR(1) onde θ0=0, θ1=0.7
b) Processo AR(1) onde θ0=0, θ1= -0.7
c) Processo AR(2) onde θ0=0, θ1=0.3 e θ2=0.5
d) Processo MA(1) onde θ0=0, θ1=0.6
e) Processo MA(1) onde θ0=0, θ1=-0.6
Para cada simulação, plote o gráfico da série, calcule o ACF e PACF. Usando estes resultados conclua como deve ser o comportamento da ACF de PACF de um modelo autoregressivo( AR.)


5- Obtenha a série histórica do PIB Brasil no site: http://www.bcb.gov.br/pre/portalCidadao/cadsis/series.asp?idpai=PORTALBCB
Código da série: 1232
a)	Plote o gráfico da série usando o R
b)	Faça a decomposição da série em: Sazonalidade, Tendência e Aleatória.
c)	Usando o índice dos últimos 12 anos, encontre uma projeção para o PIB(índice) do próximo semestre usando um modelo AR(1).  Neste caso use a série das diferenças. 
