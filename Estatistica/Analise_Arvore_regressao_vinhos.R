## Árvore de Regressão

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


Vinhos <- read.csv2("BaseWine_Red_e_White2018.csv", row.names=1)
white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,residualsugar,
                                                 chlorides,freesulfurdioxide,totalsulfurdioxide,density,pH,
                                                 sulphates,alcohol))

filter(white,
       quality > 7
       | quality < 5
) %>% select(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol) ->
  white_rmoutliers

attach(white_rmoutliers)

# modelo inicial
modelo_Valor_tree <- rpart (quality ~
                              alcohol
                            +chlorides
                            +citricacid
                            +density
                            +fixedacidity
                            +pH
                            +sulphates
                            +totalsulfurdioxide
                            +volatileacidity
 ,  method = "anova",
 cp = 0.001,minsplit = 5,maxdepth=10)
 
summary(modelo_Valor_tree)
modelo_Valor_tree$cptable



# Plot para avaliar poda
plotcp(modelo_Valor_tree)

#Podando
modelo_Valor_tree <- prune(modelo_Valor_tree,mean(modelo_Valor_tree$cptable[09:10]))



# Faz o Gráfico
rpart.plot(modelo_Valor_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=1, varlen=-10, faclen=20,
           cex=0.4, tweak=1.2,
           compress=TRUE,
           snip=FALSE)

# par(mfrow=c(1,2))
# rsq.rpart(modelo_Valor_tree)
# par(mfrow=c(1,1))

# Validação através da raiz do erro médio quadrático 
val_amostra_tree <- predict(modelo_Valor_tree,interval = "prediction", level = 0.95) 
mse_tree <- mean((quality - val_amostra_tree)^2)
Rmse_tree <- sqrt(mse_tree)


plot(resid(modelo_Valor_tree))

# grafico residuo
rs <- Val_pred_tree- quality 
plot(predict(modelo_Valor_tree), rs, xlab = "Com Árvore de Regressão",ylab = "Residuos")
abline(h = 0, lty = 2)

# shapiro.test(resid(modelo_Valor_tree))
# Testando o modelo preditivo na amosta de vinhos Brancos
set.seed(20)
rd <- sample(1:length(white$quality),1000, replace = FALSE)
val_teste_tree <- predict(modelo_Valor_tree,white[rd,1:12],interval = "prediction", level = 0.95)
Amse_tree <- mean((white$quality[rd] - val_teste_tree)^2)
RAmse_tree <- sqrt(Amse_tree)

#limpando variáveis auxiliares
rm(Amse_tree)
rm(rd)
rm(val_teste_tree)
rm(mse_tree)
rm(val_amostra_tree)

r1 = RAmse_tree
