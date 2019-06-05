Vinhos <- read.csv2("BaseWine_Red_e_White2018.csv", row.names=1)
white <- subset(Vinhos, Vinho=="WHITE", select=c(quality,
                                                 fixedacidity,
                                                 volatileacidity,
                                                 citricacid,
                                                 residualsugar,
                                                 chlorides,
                                                 freesulfurdioxide,
                                                 totalsulfurdioxide,
                                                 density,
                                                 pH,
                                                 sulphates,
                                                 alcohol))

# filter(qualidade,
# sulphates < 0.69
#        & citricacid < 0.54
#        & citricacid > 0.13
#        & fixedacidity < 8.5
#        & fixedacidity > 4.8
#        & chlorides < 0.07
#        & chlorides > 0.015
#        & volatileacidity <0.4
#        & alcohol < 14
#        & pH < 3.5
#        & pH > 2.8
#        & density < 1
#        
#        ) %>% select(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol) ->
#   white_rmoutliers

filter(white,
       quality > 7
       | quality < 5
) %>% select(quality,fixedacidity,volatileacidity,citricacid,chlorides,totalsulfurdioxide:alcohol) ->
  white_rmoutliers

attach(white_rmoutliers)
modelo_rl <- lm(quality ~ 
                  alcohol
                +density
                +volatileacidity
                +fixedacidity
                  )
summary(modelo_rl)

# Plot da dispersão em modelo linear
qplot(y=quality,x=
        alcohol
      +density
      +volatileacidity
      +fixedacidity, main = "Regressão Liner sobre a Qualidade do Vinho por suas características físico-químicas") +
  stat_smooth(method="lm", col="red", se=FALSE) 

#Plot dos residuais
plot(modelo_rl$residuals)
abline(h = 0, col="red", lwd = 3)

#teste shapiro
shapiro.test(residuals(modelo_rl))
hist(residuals(modelo_rl))

#idp <- matrix(c(scale(alcohol,scale = FALSE)
#                   ,scale(density,scale = FALSE)
#                   ,scale(volatileacidity,scale = FALSE)),ncol = 3)
# colnames(idp) <- c("alcohol_scl","density_scl","volatileacidity.scl")
# head(idp,1)
# 
# str(idp)
# str(quality_outliers)
# rm(idp)
# 
# 
# quality_outliers <- cbind(white_rmoutliers,idp)
# 

# Eliminação dos outliers após análise dos residuos
white_sresiduos <- white_rmoutliers[(residuals(modelo_rl) < 2)&(residuals(modelo_rl) > -2),]
attach(white_sresiduos)

#versão final do modelo
modelo_fnl <- lm(quality ~ 
                  alcohol
                +density
                +volatileacidity
                +fixedacidity)
summary(modelo_fnl)

#Plot do modelo de regressão linear
qplot(y=quality,x=
        alcohol
      +density
      +volatileacidity
      +fixedacidity, main = "Regressão Liner sobre a Qualidade do Vinho por suas características físico-químicas") +
  stat_smooth(method="lm", col="red", se=FALSE) 

#Plot dos residuais
plot(modelo_fnl$residuals)
abline(h = 0, col="red", lwd = 3)


# Validação através da raiz do erro médio quadrático 
val_modelo_ln <- predict(modelo_fnl,interval = "prediction", level = 0.95)
fit <- val_modelo_ln[,1] # valores preditos
lower <- val_modelo_ln[,2] # limite inferior
upper <- val_modelo_ln[,3] # limite superior

# Validação através da raiz do erro médio quadrático
mse_lin <- mean((quality - fit)^2)
Rmse_lin <- sqrt(mse_lin)


## Teste do modelo final com amostra da base de vinhos Brancos.
set.seed(20)
rd <- sample(1:length(white$quality),1000, replace = FALSE)
val_teste_rl <- round(predict(modelo_fnl,white[rd,c("quality", 
                                           "alcohol","density",
                                           "volatileacidity","fixedacidity")],
                     interval = "prediction", level = 0.95))
# fix(val_teste)
Vpredito <- val_teste_rl[,1] # valores preditos
RAmse_lin <-sqrt(mean((white$quality[rd] - Vpredito)^2))


#limpando variáveis auxiliares
rm(rd)
rm(mse_lin)
rm(Vpredito)
rm(fit)
rm(lower)
rm(upper)
