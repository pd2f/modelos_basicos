# limpar memória do R
rm(list=ls(all=TRUE))


# mostrar até 2 casas decimais
options("scipen" = 2)



# Ler arquivo xls.
install.packages("readxl")
library(readxl)
imoveis <- read_excel("C:/Users/Usuario/Documents/AI/Estatistica/Arquivo_Valorizacao_Ambiental_1.xlsx")

View(imoveis)


# trabalhar com as variáveis 
attach(imoveis)
#Verificando o formato das variáveis
str(imoveis)


#Estatísticas descritivas - Medidas resumo
summary(imoveis)

#comando para gerar em 3 linhas e duas colunas os histogramas
par (mfrow=c(3,3))
hist(imoveis$Valor)
hist(imoveis$Área)
hist(imoveis$IA)
hist(imoveis$Andar)
hist(imoveis$Suítes)
hist(imoveis$Vista)
hist(imoveis$DistBM)
hist(imoveis$Semruído)
hist(imoveis$AV200m)

par (mfrow=c(1,1))

boxplot(imoveis$Valor~imoveis$Vista, main='Valor Vs Vista',col=c('red','blue'))

boxplot(imoveis$Valor~imoveis$Semruído, main='Valor vs sem ruido',col=c('red','blue'))


# matriz de correlações
matcor <- cor(imoveis)
print(matcor, digits = 2)


#visualizar correlacao

install.packages("corrgram")
library(corrgram)

corrgram(matcor, type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

install.packages("corrplot")
library(corrplot)

corrplot::corrplot(matcor, method="circle", order="hclust")

#Gráfico de dispersao para a associação entre área m2 e valor
plot (x = imoveis$Área, y = imoveis$Valor,
      main = "Gráfico de dispersão",
      xlab = "Área",
      ylab = "Valor")

#Gráfico de dispersao para a associação entre IA e valor
plot (x = imoveis$IA, y = imoveis$Valor,
      main = "Gráfico de dispersão",
      xlab = "IA",
      ylab = "Valor")


# Opções de gráficos: Gráfico de dispersao com o plotly

install.packages("plotly")
library(plotly)
plot_ly (x=imoveis$Valor  , y=imoveis$Área, type="scatter")
plot_ly (x=imoveis$Valor  , y=imoveis$IA, type="scatter")


#Gráfico de dispersao com o ggplot2
install.packages("ggplot2")

library(ggplot2)
ggplot (data= imoveis, aes(x=imoveis$Área, y=imoveis$Valor )) + 
  geom_point(size=0.4) +
  geom_smooth(method="lm", color ="red", linetype=2) +
  labs(title = "Gráfico de dispersãoo, Valor e Área", x="Área", y="Valor")



attach(imoveis)
# Modelo de regressão linear simples


modelo1 <- lm(Valor ~ Área+Semruído+IA+Andar+Suítes+DistBM+AV200m+Vista)
summary(modelo1)

plot(modelo1$residuals)

stepwise<-step(modelo1,direction="both")

stepwise
summary(stepwise)



# Modelo final.
modelo_fim <- lm(Valor ~ Área+IA+Andar+Suítes+DistBM+Semruído+Vista)
summary(modelo_fim)


Val_pred <- predict(modelo_fim,interval = "prediction", level = 0.95) 
fix(Val_pred)
# intervalo de confianca - grafico para media
fit <- Val_pred[,1] # valores preditos
lower <- Val_pred[,2] # limite inferior
upper <- Val_pred[,3] # limite superior


mse <- mean((imoveis$Valor - fit)^2)
sqrt(mse)

erro_usando_media <- mean((imoveis$Valor - mean(imoveis$Valor))^2)
sqrt(erro_usando_media)


# grafico residuo
rs <- resid(modelo_fim)
plot(predict(modelo_fim), rs, xlab = "Preditor linear",ylab = "Residuos")
abline(h = 0, lty = 2)


#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal

qqnorm(residuals(modelo_fim), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(modelo_fim))

#Teste de Normalidade de Shapiro Wilk

# sE Valor P do teste é pequeno, rejeita-se a hipótese de normalidade dos resíduos e,
# por consequência, conclui-se que os erros não são normalmente distribuídos.

shapiro.test(residuals(modelo_fim))

install.packages("lmtest")
library(lmtest)
## perform Durbin-Watson test
dwtest(modelo_fim)

attach(imoveis)
Imoveis_Final<-cbind(imoveis,Val_pred)

fix(Imoveis_Final)







## Árvore de Regressão


install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

modelo_Valor_tree <- rpart (Valor ~ Área+IA+Andar+Suítes+DistBM+Semruído+AV200m+Vista, data=imoveis, 
                            cp = 0.001,minsplit = 5,maxdepth=10)




# Faz o Gráfico
rpart.plot(modelo_Valor_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE,
           snip=FALSE)

Val_pred_tree <- predict(modelo_Valor_tree,interval = "prediction", level = 0.95) 
str(Val_pred_tree)


mse_tree <- mean((imoveis$Valor - Val_pred_tree)^2)
sqrt(mse_tree)

erro_usando_media <- mean((imoveis$Valor - mean(imoveis$Valor))^2)
sqrt(erro_usando_media)

# grafico residuo
rs <- Val_pred_tree- imoveis$Valor
plot(predict(modelo_Valor_tree), rs, xlab = "Com Árvore de Regressão",ylab = "Residuos")
abline(h = 0, lty = 2)

