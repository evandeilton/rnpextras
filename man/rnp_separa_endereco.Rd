% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{rnp_separa_endereco}
\alias{rnp_separa_endereco}
\title{Pega string de endereco e separa em rua, numero e complemento}
\usage{
rnp_separa_endereco(end, nespacos = 5, extraregex = NULL)
}
\arguments{
\item{end}{string unica com dados de um endereco inclundo nome da rua, numero e complemento}

\item{nespacos}{numero de epacos em branco fixos consecutivos utilizados par quebrar a string}

\item{extraregex}{pode passar sua propria regex para quebrar a string. Se for diferente de NULL 'nespacos' fica suprimida}
}
\value{
data.frame com 5 colunas de nomes:  endereco_entrada, nchar, rua, numero e complemento
}
\description{
Esta funcao e uma tentativa de organizar o texto de uma string onde em um unico bloco vem endereco, numero e complemento.
Este tipo de string e recebida em bases do Itau.
A logia e a seguinte: Seja a string "R ANTONIO MARCELLO                      24   APTO 34". Esta string tem tres partes:
(1) do "R" ate a esquerda do 24 e o endereco,
(2) o "24" e o numero e
(3) o APTO 34 e complemento.
O codigo contem regex que tentam identificar estes padroes e separa a string criando tres colunas novas
}
\examples{
str <- 'R GERVASIO MOREIRA MARQUES              172  TRAV 02'
rnp_separa_endereco(str, nespacos = 5) #quebra por 5 espacos em branco
rnp_separa_endereco(str, extraregex = "[[:space:]]{5}") #quebra com regex extra
}
\author{
LOPES, J. E.
}
