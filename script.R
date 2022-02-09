#MÉDIA 
media <- function(dados){
  n <- length(dados) 
  soma <- 0
  for(i in 1:n){
    soma <- soma + dados[i]
  }
  return(soma/n)
}

#MEDIANA
mediana <- function(dados){
  n <- length(dados)
  dados <- sort(dados)
  if(n %% 2 == 0){
    termo <- (dados[n/2] + dados[(n/2) + 1]) / 2
  }else{
    termo <- dados[(n+1)/2]  
  }
  return(termo)
}

#MODA
moda <- function(dados){
  tabela <- table(dados)
  return(tabela[tabela == max(tabela)])
}

#QUARTIS Q1 e Q3
quartis <- function(dados){
  n <- length(dados)
  dados <- sort(dados)
  if(n %% 2 == 0){
    k <- n/2
    quartisdown <- mediana(dados[1:k])
    k <- k + 1
    quartisup <- mediana(dados[k:n])
  }else{
    k <- (n+1)/2
    quartisdown <- mediana(dados[1:k])
    quartisup <- mediana(dados[k:n])
  }
  valores <- c(quartisdown,quartisup)
  return(valores)
}

#VARIÂNCIA AMOSTRAL
varianciaAmostral <- function(dados){
  n <- length(dados) 
  soma <- 0
  x <- media(dados)
  for(i in 1:n){
    soma <- (dados[i] - x)**2 + soma
  }
  return(soma/(n-1))
}
#DESVIO PADRÃO AMOSTRAL
desvioAmostral <- function(dados){
  return(sqrt(varianciaAmostral(dados)))
}
