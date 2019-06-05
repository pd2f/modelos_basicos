# mostrar at� 2 casas decimais
options("scipen" = 2)

# Ler arquivo csv
Vinhos <- read.csv2("BaseWine_Red_e_White2018.csv", row.names=1)


#Base

View(Vinhos)


#mostrar as vari�veis
str(Vinhos)

#primeiros avalia��o das vari�veis]
summary(Vinhos)



attach(Vinhos)

## Criando label para cada coluna do dataset_##

attr(Vinhos$fixedacidity, 'label') <- 'acidez fixa'
attr(Vinhos$volatileacidity, 'label') <- 'acidez volatil'
attr(Vinhos$citricacid, 'label') <- 'acido citrico'
attr(Vinhos$residualsugar, 'label') <- 'acucar residual'
attr(Vinhos$chlorides, 'label') <- 'cloretos'
attr(Vinhos$freesulfurdioxide, 'label') <- 'dioxido de enxofre livre'
attr(Vinhos$totalsulfurdioxide, 'label') <- 'dioxido de enxofre total'
attr(Vinhos$density, 'label') <- 'densidade'
attr(Vinhos$pH, 'label') <- 'pH'
attr(Vinhos$sulphates, 'label') <- 'sulfatos'
attr(Vinhos$alcohol, 'label') <- 'alcool'
attr(Vinhos$quality, 'label') <- 'qualidade'
attr(Vinhos$Vinho, 'label') <- 'vinho'

##


sapply(Vinhos, function(x)all(is.na(x)))

# Frequ�ncia absoluta 
table(as.factor(Vinhos$quality), Vinhos$Vinho)


# Frequ�ncia relativa 
prop.table(table(as.factor(Vinhos$quality), Vinhos$Vinho),2)


attach(Vinhos)
aggregate(Vinhos,
          by = list(Vinho),
          FUN = mean)


#comando para gerar em 3 linhas e 4 colunas os histogramas
par (mfrow=c(3,4))
hist(fixedacidity)
hist(volatileacidity)
hist(citricacid )
hist(residualsugar)
hist(chlorides)
hist(freesulfurdioxide)
hist(totalsulfurdioxide)
hist(density)
hist(pH)
hist(sulphates)
hist(alcohol)
hist(quality)
par (mfrow=c(1,1))


hist(quality, col=c("pink"), col.main="darkgray", prob=T)


install.packages("plotly")
library(plotly)


plot_ly(x = Vinhos$volatileacidity, type = 'histogram')



attach(Vinhos)

#comando para gerar em 3 linhas e 4 colunas os histogramas
par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(residualsugar, main='residualsugar')
boxplot(chlorides, main='chlorides')
boxplot(freesulfurdioxide, main='freesulfurdioxide')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(Vinhos$quality, main='quality')
par (mfrow=c(1,1))




boxplot(quality ~ Vinho, main='quality')

boxplot(fixedacidity ~ Vinho, main='fixedacidity',col=c('red','blue'))
boxplot(volatileacidity ~ Vinho , main='volatileacidity',col=c('red','blue'))
boxplot(citricacid ~ Vinho, main='citricacid',col=c('red','blue'))
boxplot(residualsugar ~ Vinho, main='residualsugar',col=c('red','blue'))
boxplot(chlorides ~ Vinho, main='chlorides',col=c('red','blue'))
boxplot(freesulfurdioxide ~ Vinho, main='freesulfurdioxide' ,col=c('red','blue'))
boxplot(totalsulfurdioxide ~ Vinho, main='totalsulfurdioxide',col=c('red','blue'))
boxplot(density ~ Vinho, main='density',col=c('red','blue'))
boxplot(pH ~ Vinho, main='pH',col=c('red','blue'))
boxplot(sulphates ~ Vinho, main='sulphates',col=c('red','blue'))
boxplot(alcohol ~ Vinho, main='alcohol',col=c('red','blue'))

par (mfrow=c(1,1))


white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                                 chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                                 sulphates,alcohol))


red<- subset(Vinhos, Vinho=="RED", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                            chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                            sulphates,alcohol))


comparing_hist <- plot_ly(alpha = 0.6) %>% 
  add_histogram(x = ~red$volatileacidity, type = 'histogram', name='Vinho Tinto' ) %>%
  add_histogram(x = ~white$volatileacidity, name='Vinho Branco') %>%
  layout(barmode = 'overlay')
comparing_hist



# Gr�fico de dispers�o ( pch=caracter, lwd=largura)

plot(freesulfurdioxide~totalsulfurdioxide)
plot(freesulfurdioxide~totalsulfurdioxide, pch=1, lwd=3)

plot(freesulfurdioxide~totalsulfurdioxide)
abline(v=mean(freesulfurdioxide), col="red")
abline(h=mean(totalsulfurdioxide), col="green")



# Com base na an�lise explorat�ria inicial o que fazer?

# an�lise espec�fica - para vinho="WHITE" 


white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                                 chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                                 sulphates,alcohol))

# retirar o residualsugar, pois tem forte correlação com a densidadade além de possuir muitos outliers e grande variância
hist(white$quality)       

#Estat�sticas descritivas
summary(white)
head(white)

attach(white)

filter(white,
                      totalsulfurdioxide < 200 
                      & totalsulfurdioxide > 100
                      & sulphates < 0.69
                      & max(freesulfurdioxide) > min(totalsulfurdioxide) 
                      & citricacid < 0.54
                      & citricacid > 0.13
                      & fixedacidity < 8.5
                      & fixedacidity > 4.8
                      & chlorides < 0.07
                      & chlorides > 0.015
                      & volatileacidity <0.4
       & alcohol < 14
       & pH < 3.5
       & pH > 2.8
       & density < 1
                      ) %>% select(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol) -> 
  white_rmoutliers


hist(write_liers$quality)
boxplot(write_liers$fixedacidity)
summary(write_liers)
str(write_liers)
prop.table(table(as.factor(write_liers$volatileacidity)))



attach(write_liers)

#comando para gerar em 3 linhas e 4 colunas os histogramas
par (mfrow=c(3,4))
boxplot(fixedacidity, main='fixedacidity')
boxplot(volatileacidity , main='volatileacidity')
boxplot(citricacid , main='citricacid')
boxplot(chlorides, main='chlorides')
boxplot(totalsulfurdioxide, main='totalsulfurdioxide')
boxplot(density, main='density')
boxplot(pH, main='pH')
boxplot(sulphates, main='sulphates')
boxplot(alcohol, main='alcohol')
boxplot(Vinhos$quality, main='quality')
par (mfrow=c(1,1))


# Frequ�ncia absoluta 
table(as.factor(Vinhos$quality), Vinhos$Vinho)


# Frequ�ncia relativa 
prop.table(table(as.factor(Vinhos$quality), Vinhos$Vinho),2)

View(filter(white, density > 1) %>% 
       group_by(density,residualsugar) %>% 
       summarise(densidade = max(density), acucar = max(residualsugar), n = n()) %>% 
       arrange(desc(acucar)))

  
matcor <- cor(write_liers)
corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)
  

plot(write_liers$residualsugar, pch=1, lwd=3)

# matriz de correla��es
matcor <- cor(write_liers)
print(matcor, digits = 2)

install.packages("corrgram")
library(corrgram)

corrgram(write_liers, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

panel.cor <- function(x, y, digits=2, prefix ="", cex.cor,
                      ...)  {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y , use = "pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits) [1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor))
    cex <- 0.8/strwidth(txt)
  # abs(r) � para que na sa�da as correla��es ficam proporcionais
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
#pdf(file = "grafico.pdf")
pairs(write_liers, lower.panel=panel.smooth, upper.panel=panel.cor)



# fim


# h� outlies? Alguma sele�ao? Explique?




#Analise de simetria


#1
hist(white_rmoutliers$density, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$density), sd=sd(white_rmoutliers$density)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$density)

#2
hist(white_rmoutliers$volatileacidity, freq = FALSE, ylim = c(0,7))
curve(dnorm(x, mean=mean(white_rmoutliers$volatileacidity), sd=sd(white_rmoutliers$volatileacidity)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$volatileacidity)

#3
hist(white_rmoutliers$quality, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$quality), sd=sd(white_rmoutliers$quality)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$quality)

#4
hist(white_rmoutliers$fixedacidity, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$fixedacidity), sd=sd(white_rmoutliers$fixedacidity)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$fixedacidity)

#5
hist(white_rmoutliers$citricacid, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$citricacid), sd=sd(white_rmoutliers$citricacid)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$citricacid)

#6
hist(white_rmoutliers$chlorides, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$chlorides), sd=sd(white_rmoutliers$chlorides)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$chlorides)

#7
hist(white_rmoutliers$sulphates, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$sulphates), sd=sd(white_rmoutliers$sulphates)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$sulphates)


#8
hist(white_rmoutliers$totalsulfurdioxide, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$totalsulfurdioxide), sd=sd(white_rmoutliers$totalsulfurdioxide)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$totalsulfurdioxide)


#9
hist(white_rmoutliers$pH, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$pH), sd=sd(white_rmoutliers$pH)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$alcohol)

#10
hist(white_rmoutliers$alcohol, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$alcohol), sd=sd(white_rmoutliers$alcohol)), add=TRUE,  lwd=2)
curtose(white_rmoutliers$pH)


#calcular de assimetria e curtose

dec <- quantile(white_rmoutliers$totalsulfurdioxide,prob = seq(0.1, 1, length = 10), type = 5)
s <- summary(white_rmoutliers$totalsulfurdioxide)
kurtosis(white_rmoutliers$totalsulfurdioxide,type = 2)
curtose <- (s['3rd Qu.'] - s['1st Qu.']) / 2*(dec['90%'] - dec['0%'])
assimetria <- (s['Mean'] - s['Median']) / sd(white_rmoutliers$volatileacidity)

curtose <- function(amostra){
  dec = quantile(amostra,prob = seq(0.1, 1, length = 10), type = 3)
  sumario <- summary(amostra)
  c((s['3rd Qu.'] - s['1st Qu.']) / (2*(dec['90%'] - dec['10%'])))
  }
curtose(white_rmoutliers$totalsulfurdioxide)
hist(white_rmoutliers$volatileacidity)
rm(curtose2)


quantile(seq(3,100),prob = seq(0.1, 1, length = 10), type = 3)

quantile(white_rmoutliers$volatileacidity)

hist(white_rmoutliers$pH, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$pH), sd=sd(white_rmoutliers$pH)), add=TRUE,  lwd=2)
kurtosis(white_rmoutliers$pH)


hist(white_rmoutliers$density, freq = FALSE)
curve(dnorm(x, mean=mean(white_rmoutliers$density), sd=sd(white_rmoutliers$density)), add=TRUE,  lwd=2)
kurtosis(white_rmoutliers$density)
