install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

Vinhos <- read.csv2("BaseWine_Red_e_White2018.csv", row.names=1)
white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol))



filter(white,
       quality > 7
       | quality < 5
) %>% select(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol) ->
  white_rmoutliers
attach(white_rmoutliers)


white_rmoutliers$fator_qualidade <- factor(quality,levels = c(3,4,5,7,8,9),labels = c("Ruim","Ruim","Ruim","Bom","Bom","Bom"))
white_rmoutliers$fator_qualidade_dummy <- factor(white_rmoutliers$fator_qualidade,levels = c("Ruim","Bom"),labels = c(0,1), ordered = TRUE)

table(select(white_rmoutliers,quality,fator_qualidade))


# par (mfrow=c(3,3))
# plot(fixedacidity, quality,ylab="Fator Qualidade",xlab="Acidez Fixa",col=c('red','darkgreen'))
# plot(volatileacidity, fator_qualidade_dummy,ylab="Fator Qualidade",xlab="Acidez Volátil",col=c('red','darkgreen'))
# plot(citricacid, fator_qualidade,ylab="Fator Qualidade",xlab="Acidez Citrica",col=c('red','darkgreen'))
# plot(chlorides, fator_qualidade,ylab="Fator Qualidade",xlab="Cloreto",col=c('red','darkgreen'))
# plot(totalsulfurdioxide, fator_qualidade,ylab="Fator Qualidade",xlab="Dióxido de enxofre total",col=c('red','darkgreen'))
# plot(density, fator_qualidade,ylab="Fator Qualidade",xlab="Densidade",col=c('red','darkgreen'))
# plot(pH, fator_qualidade,ylab="Fator Qualidade",xlab="pH",col=c('red','darkgreen'))
# plot(sulphates, fator_qualidade,ylab="Fator Qualidade",xlab="Sulfatos",col=c('red','darkgreen'))
# par (mfrow=c(1,1))

prt <- 2/3



set.seed(20)
treino <- sample(1:NROW(white_rmoutliers), as.integer(prt*NROW(white_rmoutliers)))

trainData <- white_rmoutliers[treino,]
testData  <- white_rmoutliers[-treino,]

prop.table(table(trainData$fator_qualidade))
prop.table(table(testData$fator_qualidade))

modelo_tree  <- rpart(fator_qualidade ~
                              alcohol
                            + chlorides
                            +citricacid
                            +density
                            +fixedacidity
                            +pH
                            +sulphates
                            +totalsulfurdioxide
                            +volatileacidity
                            , data=trainData, cp = 0.001,maxdepth=10)

modelo_tree$variable.importance

summary(modelo_tree)

rpart.plot(modelo_tree, type=4, extra=104, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=10,
           cex=0.4, tweak=1.7,
           compress=TRUE,
           snip=FALSE)

yprob <- predict(modelo_tree,testData)
hist(yprob)


pred_class <- predict(modelo_tree ,testData , type = "class")
length(pred_class) 


Campanha.matriz.de.confusao<-table(white_rmoutliers$fator_qualidade[-treino],pred_class)
Campanha.matriz.de.confusao

diagonal <- diag(Campanha.matriz.de.confusao)
perc.erro <- 1 - sum(diagonal)/sum(Campanha.matriz.de.confusao)
perc.erro


round(1 - 0.1157025,4)
