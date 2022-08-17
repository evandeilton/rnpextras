
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rnpextras

<!-- badges: start -->
<!-- badges: end -->

O objetivo deste pacote é armazenar recusros extras tais como dados e
funções consumidos e/ou gerados em minhas práticas na linguarem R.
Tornar esses recusros públicos é uma forma de ajudar à comunidade de
alguma forma, ao disponibilizar códigos úteis e aplicáveis em situações
práticas da ciência de dados.

## Instalação

Rode os comandos a seguir

``` r
if(!require(devtools)){
  install.packages("devtools")
}
devtools::install_github("evandeilton/rnpextras")
```

# Exemplos

``` r
## Carregando pacotes
require(rnpextras)
if(!require("ISLR")){
  install.packages("ISLR")
  require(ISLR)
}
```

## Alguns dados

``` r
data("Credit", package = "ISLR");?Credit
da <- Credit
#data("mtcars", package = "datasets")
#data("iris", package = "datasets")
#data("penguins", package = "palmerpenguins")
```

## Descritiva dados numéricos

``` r
## Características de uma conjunto de dados
rnpextras::rnp_atributos(da) %>% 
  knitr::kable()
```

| classeBase | comprimento             | variaveis | classeVars |
|:-----------|:------------------------|:----------|:-----------|
| data.frame | 400 linhas e 12 colunas | ID        | integer    |
| data.frame | 400 linhas e 12 colunas | Income    | numeric    |
| data.frame | 400 linhas e 12 colunas | Limit     | integer    |
| data.frame | 400 linhas e 12 colunas | Rating    | integer    |
| data.frame | 400 linhas e 12 colunas | Cards     | integer    |
| data.frame | 400 linhas e 12 colunas | Age       | integer    |
| data.frame | 400 linhas e 12 colunas | Education | integer    |
| data.frame | 400 linhas e 12 colunas | Gender    | factor     |
| data.frame | 400 linhas e 12 colunas | Student   | factor     |
| data.frame | 400 linhas e 12 colunas | Married   | factor     |
| data.frame | 400 linhas e 12 colunas | Ethnicity | factor     |
| data.frame | 400 linhas e 12 colunas | Balance   | integer    |

## Média artmética simples, geométrica e harmônica

``` r
rnpextras::rnp_media(da$Income)  %>% 
  knitr::kable()
```

|  media_a |  media_g |  media_h |
|---------:|---------:|---------:|
| 45.21889 | 35.27731 | 28.37636 |

## Resumo descritivo

``` r
rnpextras::rnp_summary(da$Income) %>% 
  t() %>% knitr::kable()
```

|   N |     Soma | Nmis |    Min |      Q1 |   Media | Mediana |      Q3 |     Max |  DevPad |     IQR |     cv |
|----:|---------:|-----:|-------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|-------:|
| 400 | 18087.55 |    0 | 10.354 | 21.0072 | 45.2189 | 33.1155 | 57.4708 | 186.634 | 35.2443 | 36.4635 | 0.7794 |

``` r
rnpextras::rnp_summary_all(da)$num %>% 
  knitr::kable()
```

| variavel  |   N |       Soma | Nmis |     Min |        Q1 |     Media |   Mediana |        Q3 |       Max |    DevPad |       IQR |     cv |
|:----------|----:|-----------:|-----:|--------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|-------:|
| ID        | 400 |   80200.00 |    0 |   1.000 |  100.7500 |  200.5000 |  200.5000 |  300.2500 |   400.000 |  115.6143 |  199.5000 | 0.5766 |
| Income    | 400 |   18087.55 |    0 |  10.354 |   21.0072 |   45.2189 |   33.1155 |   57.4708 |   186.634 |   35.2443 |   36.4635 | 0.7794 |
| Limit     | 400 | 1894240.00 |    0 | 855.000 | 3088.0000 | 4735.6000 | 4622.5000 | 5872.7500 | 13913.000 | 2308.1988 | 2784.7500 | 0.4874 |
| Rating    | 400 |  141976.00 |    0 |  93.000 |  247.2500 |  354.9400 |  344.0000 |  437.2500 |   982.000 |  154.7241 |  190.0000 | 0.4359 |
| Cards     | 400 |    1183.00 |    0 |   1.000 |    2.0000 |    2.9575 |    3.0000 |    4.0000 |     9.000 |    1.3713 |    2.0000 | 0.4637 |
| Age       | 400 |   22267.00 |    0 |  23.000 |   41.7500 |   55.6675 |   56.0000 |   70.0000 |    98.000 |   17.2498 |   28.2500 | 0.3099 |
| Education | 400 |    5380.00 |    0 |   5.000 |   11.0000 |   13.4500 |   14.0000 |   16.0000 |    20.000 |    3.1252 |    5.0000 | 0.2324 |
| Balance   | 400 |  208006.00 |    0 |   0.000 |   68.7500 |  520.0150 |  459.5000 |  863.0000 |  1999.000 |  459.7589 |  794.2500 | 0.8841 |

## Correlação em forma tabular e vertical

``` r
rnpextras::rnp_correlacao(da[,c("Age","Income","Limit","Balance")])  %>% 
  knitr::kable()
```

|     | x      | y       | pearson | spearman | kendall |
|:----|:-------|:--------|--------:|---------:|--------:|
| 2   | Age    | Income  |  0.1753 |   0.1479 |  0.0985 |
| 3   | Age    | Limit   |  0.1009 |   0.0641 |  0.0414 |
| 1   | Age    | Balance |  0.0018 |  -0.0107 | -0.0074 |
| 5   | Income | Limit   |  0.7921 |   0.6574 |  0.4774 |
| 4   | Income | Balance |  0.4637 |   0.3613 |  0.2507 |
| 6   | Limit  | Balance |  0.8617 |   0.8894 |  0.7164 |

# Descritiva dados categóricos

## Tabelas de freqência simples e de dupla entrada

``` r
rnpextras::rnp_freq(da$Gender) %>% 
  knitr::kable()
```

| classe |  fa |     fr | faa |    fra |
|:-------|----:|-------:|----:|-------:|
| Male   | 193 | 0.4825 | 193 | 0.4825 |
| Female | 207 | 0.5175 | 400 | 1.0000 |

``` r
rnpextras::rnp_2freq(da$Gender, da$Ethnicity) %>% 
  knitr::kable()
```

| Tipo | Classe X/Y | African American | Asian | Caucasian | Total |
|:-----|:-----------|-----------------:|------:|----------:|------:|
| fa   | Male       |               49 |    47 |        97 |   193 |
| fa   | Female     |               50 |    55 |       102 |   207 |
| fa   | Total      |               99 |   102 |       199 |   400 |

``` r
rnpextras::rnp_2freq(da$Gender, da$Ethnicity, percents = TRUE) %>% 
  knitr::kable()
```

| Tipo   | Classe X/Y | African American |    Asian | Caucasian |    Total |
|:-------|:-----------|-----------------:|---------:|----------:|---------:|
| fa     | Male       |          49.0000 |  47.0000 |   97.0000 | 193.0000 |
| fr     | Male       |           0.1225 |   0.1175 |    0.2425 |   0.4825 |
| fr_col | Male       |           0.4949 |   0.4608 |    0.4874 |   0.4825 |
| fr_lin | Male       |           0.2539 |   0.2435 |    0.5026 |   1.0000 |
| fa     | Female     |          50.0000 |  55.0000 |  102.0000 | 207.0000 |
| fr     | Female     |           0.1250 |   0.1375 |    0.2550 |   0.5175 |
| fr_col | Female     |           0.5051 |   0.5392 |    0.5126 |   0.5175 |
| fr_lin | Female     |           0.2415 |   0.2657 |    0.4928 |   1.0000 |
| fa     | Total      |          99.0000 | 102.0000 |  199.0000 | 400.0000 |
| fr     | Total      |           0.2475 |   0.2550 |    0.4975 |   1.0000 |

## Sumarização com grupos

``` r
rnpextras::rnp_summary_by(da, variavel = "Income", grupos = "Gender") %>% 
  knitr::kable()
```

| Gender |   N |     Soma | Nmis |    Min |     Q1 |  Media | Mediana |     Q3 |     Max | DevPad |    IQR |    cv |
|:-------|----:|---------:|-----:|-------:|-------:|-------:|--------:|-------:|--------:|-------:|-------:|------:|
| Male   | 193 | 8802.791 |    0 | 10.354 | 20.088 | 45.610 |  33.437 | 58.063 | 182.728 | 35.638 | 37.975 | 0.781 |
| Female | 207 | 9284.763 |    0 | 10.363 | 21.917 | 44.854 |  32.164 | 57.270 | 186.634 | 34.955 | 35.352 | 0.779 |

``` r
rnpextras::rnp_summary_by(da, variavel = "Income", grupos = c("Gender","Student")) %>% 
  knitr::kable()
```

| Gender | Student |   N |     Soma | Nmis |    Min |     Q1 |  Media | Mediana |     Q3 |     Max | DevPad |    IQR |    cv |
|:-------|:--------|----:|---------:|-----:|-------:|-------:|-------:|--------:|-------:|--------:|-------:|-------:|------:|
| Male   | No      | 177 | 8058.153 |    0 | 10.354 | 20.088 | 45.526 |  34.480 | 58.063 | 182.728 | 35.567 | 37.975 | 0.781 |
| Male   | Yes     |  16 |  744.638 |    0 | 14.312 | 19.492 | 46.540 |  32.233 | 50.663 | 123.299 | 37.593 | 31.171 | 0.808 |
| Female | No      | 183 | 8137.719 |    0 | 10.363 | 21.917 | 44.468 |  31.861 | 57.605 | 186.634 | 34.349 | 35.688 | 0.772 |
| Female | Yes     |  24 | 1147.044 |    0 | 10.627 | 22.160 | 47.794 |  38.063 | 55.623 | 180.379 | 39.965 | 33.463 | 0.836 |
