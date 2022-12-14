% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{rnp_tabulate}
\alias{rnp_tabulate}
\title{Tabula dados bivariados}
\usage{
rnp_tabulate(dados, vx, vy = NULL, bom = 0, perf = FALSE)
}
\arguments{
\item{dados}{data set contendos os campos para serem tabulados}

\item{vx}{string de nome da variavel categorica X}

\item{vy}{Se existir na base, a variavel target contendo 0 e 1}

\item{bom}{Se existir vy, definir qual a referencia. 1 ou 0 para bom?}

\item{perf}{Se existir dados de vy, decidir se tabula ou nao.}
}
\description{
This function takes a dataset and tabulates summary counts also as some statistical
Recebe um dataset e sumariza os dados criando tambem algumas estatisticas como
frequencias dos totais, percentuais, PSI/VDI, KS1, IV, GINI, WoE e odds
}
\details{
Os calculos nesta funcao sao baseados no pardigma de credit scoring, mas se a?plicam tambem no cenario de fraude.
}
\examples{
\dontrun{
ref <- rnp_tabulate(dados = da, "FX", "Y", bom = 0, perf = TRUE)
}
}
\author{
LOPES, J. E.
}
