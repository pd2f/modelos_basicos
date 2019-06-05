Vinhos <- read.csv2("BaseWine_Red_e_White2018.csv", row.names=1)
white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol))



filter(white,
       quality > 7
       | quality < 5
) %>% select(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol) ->
  white_rmoutliers

attach(white_rmoutliers)

white_rmoutliers$fator_qualidade <- factor(quality,levels = c(3,4,5,7,8,9),labels = c("Ruim","Ruim","Ruim","Bom","Bom","Bom"))
white_rmoutliers$fator_qualidade_dummy <- factor(white_rmoutliers$fator_qualidade,levels = c("Ruim","Bom"),labels = c(0,1))



prt <- 2/3



set.seed(20)
treino <- sample(1:NROW(white_rmoutliers), as.integer(prt*NROW(white_rmoutliers)))

trainData <- white_rmoutliers[treino,]
testData  <- white_rmoutliers[-treino,]

attach(trainData)
modelo_log<-glm(fator_qualidade_dummy ~ alcohol
                # + chlorides
                +citricacid
                +density
                +fixedacidity
                #+pH
                #+sulphates
                #+totalsulfurdioxide
                +volatileacidity
                , data=trainData, family=binomial(link=logit))
summary(modelo_log)

predito<-fitted(modelo_log)

summary(predito)

hist(predito)
# Criar variável faixa probabilidade
fx_predito <- cut(predito, breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1), right=F)

plot(fx_predito , white_rmoutliers$fator_qualidade_dummy[treino])

attach(testData)
Predito_teste<-predict(modelo_log, testData)

### Matriz de confusão  

# contrasts(white_rmoutliers$fator_qualidade)


fx_predito1 <- cut(Predito_teste, 
                   breaks=c(0,0.50,1)
                   #breaks=c(0,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1)
                   , right=F)

MC <- table( white_rmoutliers$fator_qualidade_dummy[-treino],  fx_predito1 , deparse.level = 2) # montar a matriz de confus�o  
show(MC) # mostra os resultados  
ACC = sum(diag(MC))/sum(MC) # calcula a acurácia  
show(ACC) # mostra a acurrácia  
