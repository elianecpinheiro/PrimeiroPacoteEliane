#'soma um
#'
#'essa funcao recebe um inteiro e retorna o mesmo numero adicionado de um
#'
#'@param x - valor a ser adicionado um
#'
#'@return x - valor adicinado de um
#'
#'@examples
#'
#'funcao(10)
#'z<-3
#'funcao(z)
#'\dontrun{funcao('blah')}
#'
#'@export
funcao<-function(x){
  x+1
}

#'mtcars2
#'
#'descricao descricao descricao
#'
#'@name mtcars2
#'@source mtcars
NULL

#' minha funcao que modela
#'
#' Ajusta um modelo aditivo generalizado
#' aos dados
#'
#' @param formula minha formula
#' @param dados meus dados
#'
#' @return objeto do tipo gam
#'
#' @export
modelar<-function(formula,dados){
  mgcv::gam(formula,data=dados)
}

#' funcao que compara tempo de processamento
#'
#'
#' @param dados meus dados
#'
#' @return tabela
#'
#' @export
tempoprocessamento<-function(dados){
#  library(microbenchmark)
  x <- dados
  microbenchmark::microbenchmark(sqrt(x),x^0.5,x^(1/2),(x)^(1/2))
}

#' funcao que compara tempo de acesso a dados
#'
#'
#' @param dados meus dados
#'
#' @return tabela
#'
#' @export
tempoacessodados<-function(dados){
  #  library(microbenchmark)
  x <- dados
  microbenchmark::microbenchmark(
    "[30,11]"=x[30,11],
    "$carb[30]"=x$carb[30],
    "[[c(11,30)"=x[[c(11,30)]],
    "[[11]][30]"=x[[11]][30],
    ".subset2"=.subset2(x,11)[30]
  )
}

#' funcao que verifica se um número x
#' está em um intervalo (a,b)
#'
#'
#' @param x valor a ser verificado
#' @param a minimo do intervalo
#' @param b maximo do intervalo
#'
#' @return tabela
#'
#' @export
pertenceintervalo<-function(x,a,b){
  #  library(microbenchmark)
  #Função 1
  f_ife = function(x,a,b){
    ifelse(x<=a,a,ifelse(x>=b,b,x))
  }
  #Função 2
  f_pmm = function(x,a,b){
    pmax(pmin(x,b),a)
  }
  #Função 3
  f_place = function(x,a,b){
    x[x<=a] = a
    x[x>=b] = b
    x
  }

  microbenchmark::microbenchmark(
f_ife(x,a,b),f_pmm(x,a,b),f_place(x,a,b)
  )
}
