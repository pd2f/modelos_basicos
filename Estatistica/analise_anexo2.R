
summary(Vinhos)

Vinhos$quality

sort(unique(Vinhos$quality))

factor(Vinhos$quality, ordered=TRUE, 
        labels=c("Ruim","Ruim","Regular","Regular","Bom", "Bom", "Ótimo"))

install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
tintos <- Vinhos %>% filter(Vinho == "RED")
brancos <- Vinhos %>% filter(Vinho == "WHITE")

moda <- function(vetor) {
  valores = sort(unique(vetor))
  retorno = NULL
  for (valor in valores) {
    retorno <- c(retorno, length(vetor[vetor == valor]))
  }
  ret <-  as.matrix(retorno)
  rownames(ret) <- valores
  colnames(ret) <- c("frequência")
  ret
}

plot(moda(brancos$quality),main = 'Vinhos Brancos', type = 'l')

plot(moda(tintos$quality),main = 'Vinhos Tintos', type = 'l')
