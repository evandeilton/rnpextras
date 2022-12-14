% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{rnp_nsample_media}
\alias{rnp_nsample_media}
\title{Tamanho da amostra para media}
\usage{
rnp_nsample_media(N, s = NULL, z = NULL, e = NULL, infinito = FALSE)
}
\arguments{
\item{N}{Valor do tamanho da base de dados original}

\item{s}{E desvio padrao da variavel de interesse, ex. score medio.}

\item{z}{E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado. (ex. 25\%)}

\item{e}{E a margem de erro maximo que eu quero admitir (p.e. 5\%)}

\item{infinito}{se TRUE assume que a populacao e infinita e nao aplica fator de correcao}
}
\value{
vetor com tamanho da amostra n
}
\description{
Calcula tamanho da amostra ideal para estimar um parametrp de media
Considera caso em que a populacao original e finita ou infinita.
N = Tamanho do universo finita
Z = E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado.
Em funcao do nivel de confianca que buscamos, usaremos um valor determinado que e dado
pela forma da distribuicao de Normal. Os valores mais frequentes sao:
Nivel de confianca 90\% -> Z=1,645
Nivel de confianca 95\% -> Z=1,96
Nivel de confianca 99\% -> Z=2,575
e = E a margem de erro maximo que eu quero admitir (p.e. 5\%)
s = E o desvio padrao amostral da base variavel de interesse na base original. Se nao puder obter da base total gerar de uma amostra. Se ainda nao
tiver, faremo como raiz quadrada de N
}
\author{
LOPES, J. L
}
