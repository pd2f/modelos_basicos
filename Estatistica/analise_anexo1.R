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


hist(white$quality)       

#Estat�sticas descritivas
summary(white)

attach(white)

# matriz de correla��es
matcor <- cor(white)
print(matcor, digits = 2)

install.packages("corrgram")
library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

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
pairs(white, lower.panel=panel.smooth, upper.panel=panel.cor)



# fim


# h� outlies? Alguma sele�ao? Explique?


# Tem necessidade de fazer componentes principais? Explique?


