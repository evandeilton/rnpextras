
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rnpextras

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/evandeilton/rnpextras.svg?branch=main)](https://travis-ci.com/evandeilton/rnpextras)
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
if(!require("fairml")){install.packages("fairml"); require(fairml)}
if(!require("ISLR")){install.packages("ISLR"); require(ISLR)}
```

## Alguns dados

``` r
data("german.credit", package = "fairml")
data("Credit", package = "ISLR")

da <- Credit
db <- german.credit
db$Target <- ifelse(db$Credit_risk == "BAD", 1, 0)
db$Credit_risk <- relevel(db$Credit_risk, ref = "BAD")

#data("mtcars", package = "datasets")
#data("iris", package = "datasets")
#data("penguins", package = "palmerpenguins")
```

## Descritiva dados numéricos

``` r
## Características de uma conjunto de dados
rnpextras::rnp_resumo(da) %>% 
  knitr::kable()
```

| var       | classe  | tipo    | nobs | ndis | nmis |  minimo |       q25 |   mediana |     media |       q75 |    maximo |    devpad |     cv |       iqr |
|:----------|:--------|:--------|-----:|-----:|-----:|--------:|----------:|----------:|----------:|----------:|----------:|----------:|-------:|----------:|
| ID        | integer | integer |  400 |  400 |    0 |   1.000 |  100.7500 |  200.5000 |  200.5000 |  300.2500 |   400.000 |  115.6143 | 0.5766 |  199.5000 |
| Income    | numeric | double  |  400 |  399 |    0 |  10.354 |   21.0072 |   33.1155 |   45.2189 |   57.4708 |   186.634 |   35.2443 | 0.7794 |   36.4635 |
| Limit     | integer | integer |  400 |  387 |    0 | 855.000 | 3088.0000 | 4622.5000 | 4735.6000 | 5872.7500 | 13913.000 | 2308.1988 | 0.4874 | 2784.7500 |
| Rating    | integer | integer |  400 |  283 |    0 |  93.000 |  247.2500 |  344.0000 |  354.9400 |  437.2500 |   982.000 |  154.7241 | 0.4359 |  190.0000 |
| Cards     | integer | integer |  400 |    9 |    0 |   1.000 |    2.0000 |    3.0000 |    2.9575 |    4.0000 |     9.000 |    1.3713 | 0.4637 |    2.0000 |
| Age       | integer | integer |  400 |   68 |    0 |  23.000 |   41.7500 |   56.0000 |   55.6675 |   70.0000 |    98.000 |   17.2498 | 0.3099 |   28.2500 |
| Education | integer | integer |  400 |   16 |    0 |   5.000 |   11.0000 |   14.0000 |   13.4500 |   16.0000 |    20.000 |    3.1252 | 0.2324 |    5.0000 |
| Gender    | factor  | integer |  400 |    2 |    0 |      NA |        NA |        NA |        NA |        NA |        NA |        NA |     NA |        NA |
| Student   | factor  | integer |  400 |    2 |    0 |      NA |        NA |        NA |        NA |        NA |        NA |        NA |     NA |        NA |
| Married   | factor  | integer |  400 |    2 |    0 |      NA |        NA |        NA |        NA |        NA |        NA |        NA |     NA |        NA |
| Ethnicity | factor  | integer |  400 |    3 |    0 |      NA |        NA |        NA |        NA |        NA |        NA |        NA |     NA |        NA |
| Balance   | integer | integer |  400 |  284 |    0 |   0.000 |   68.7500 |  459.5000 |  520.0150 |  863.0000 |  1999.000 |  459.7589 | 0.8841 |  794.2500 |

## Média artmética simples, geométrica e harmônica

``` r
rnpextras::rnp_media(da$Income)  %>% 
  knitr::kable(digits = 3)
```

| media_a | media_g | media_h |
|--------:|--------:|--------:|
|  45.219 |  35.277 |  28.376 |

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
  knitr::kable(digits = 3)
```

| variavel  |   N |       Soma | Nmis |     Min |       Q1 |    Media |  Mediana |       Q3 |       Max |   DevPad |      IQR |    cv |
|:----------|----:|-----------:|-----:|--------:|---------:|---------:|---------:|---------:|----------:|---------:|---------:|------:|
| ID        | 400 |   80200.00 |    0 |   1.000 |  100.750 |  200.500 |  200.500 |  300.250 |   400.000 |  115.614 |  199.500 | 0.577 |
| Income    | 400 |   18087.55 |    0 |  10.354 |   21.007 |   45.219 |   33.115 |   57.471 |   186.634 |   35.244 |   36.464 | 0.779 |
| Limit     | 400 | 1894240.00 |    0 | 855.000 | 3088.000 | 4735.600 | 4622.500 | 5872.750 | 13913.000 | 2308.199 | 2784.750 | 0.487 |
| Rating    | 400 |  141976.00 |    0 |  93.000 |  247.250 |  354.940 |  344.000 |  437.250 |   982.000 |  154.724 |  190.000 | 0.436 |
| Cards     | 400 |    1183.00 |    0 |   1.000 |    2.000 |    2.958 |    3.000 |    4.000 |     9.000 |    1.371 |    2.000 | 0.464 |
| Age       | 400 |   22267.00 |    0 |  23.000 |   41.750 |   55.667 |   56.000 |   70.000 |    98.000 |   17.250 |   28.250 | 0.310 |
| Education | 400 |    5380.00 |    0 |   5.000 |   11.000 |   13.450 |   14.000 |   16.000 |    20.000 |    3.125 |    5.000 | 0.232 |
| Balance   | 400 |  208006.00 |    0 |   0.000 |   68.750 |  520.015 |  459.500 |  863.000 |  1999.000 |  459.759 |  794.250 | 0.884 |

## Correlação em forma tabular e vertical

``` r
rnpextras::rnp_correlacao(da[,c("Age","Income","Limit","Balance")])  %>% 
  knitr::kable(digits = 3)
```

|     | x      | y       | pearson | spearman | kendall |
|:----|:-------|:--------|--------:|---------:|--------:|
| 2   | Age    | Income  |   0.175 |    0.148 |   0.098 |
| 3   | Age    | Limit   |   0.101 |    0.064 |   0.041 |
| 1   | Age    | Balance |   0.002 |   -0.011 |  -0.007 |
| 5   | Income | Limit   |   0.792 |    0.657 |   0.477 |
| 4   | Income | Balance |   0.464 |    0.361 |   0.251 |
| 6   | Limit  | Balance |   0.862 |    0.889 |   0.716 |

# Descritiva dados categóricos

## Tabelas de freqência simples e de dupla entrada

``` r
rnpextras::rnp_freq(da$Gender) %>% 
  knitr::kable(digits = 3)
```

| classe |  fa |    fr | faa |   fra |
|:-------|----:|------:|----:|------:|
| Male   | 193 | 0.482 | 193 | 0.482 |
| Female | 207 | 0.517 | 400 | 1.000 |

``` r
rnpextras::rnp_2freq(da$Gender, da$Ethnicity) %>% 
  knitr::kable(digits = 3)
```

| Tipo | Classe X/Y | African American | Asian | Caucasian | Total |
|:-----|:-----------|-----------------:|------:|----------:|------:|
| fa   | Male       |               49 |    47 |        97 |   193 |
| fa   | Female     |               50 |    55 |       102 |   207 |
| fa   | Total      |               99 |   102 |       199 |   400 |

``` r
rnpextras::rnp_2freq(da$Gender, da$Ethnicity, percents = TRUE) %>% 
  knitr::kable(digits = 3)
```

| Tipo   | Classe X/Y | African American |   Asian | Caucasian |   Total |
|:-------|:-----------|-----------------:|--------:|----------:|--------:|
| fa     | Male       |           49.000 |  47.000 |    97.000 | 193.000 |
| fr     | Male       |            0.122 |   0.117 |     0.242 |   0.482 |
| fr_col | Male       |            0.495 |   0.461 |     0.487 |   0.482 |
| fr_lin | Male       |            0.254 |   0.244 |     0.503 |   1.000 |
| fa     | Female     |           50.000 |  55.000 |   102.000 | 207.000 |
| fr     | Female     |            0.125 |   0.138 |     0.255 |   0.517 |
| fr_col | Female     |            0.505 |   0.539 |     0.513 |   0.517 |
| fr_lin | Female     |            0.242 |   0.266 |     0.493 |   1.000 |
| fa     | Total      |           99.000 | 102.000 |   199.000 | 400.000 |
| fr     | Total      |            0.248 |   0.255 |     0.498 |   1.000 |

## Sumarização com grupos

``` r
rnpextras::rnp_summary_by(da, variavel = "Income", grupos = "Gender") %>% 
  knitr::kable(digits = 3)
```

| Gender |   N |     Soma | Nmis |    Min |     Q1 |  Media | Mediana |     Q3 |     Max | DevPad |    IQR |    cv |
|:-------|----:|---------:|-----:|-------:|-------:|-------:|--------:|-------:|--------:|-------:|-------:|------:|
| Male   | 193 | 8802.791 |    0 | 10.354 | 20.088 | 45.610 |  33.437 | 58.063 | 182.728 | 35.638 | 37.975 | 0.781 |
| Female | 207 | 9284.763 |    0 | 10.363 | 21.917 | 44.854 |  32.164 | 57.270 | 186.634 | 34.955 | 35.352 | 0.779 |

``` r
rnpextras::rnp_summary_by(da, variavel = "Income", grupos = c("Gender","Student")) %>% 
  knitr::kable(digits = 3)
```

| Gender | Student |   N |     Soma | Nmis |    Min |     Q1 |  Media | Mediana |     Q3 |     Max | DevPad |    IQR |    cv |
|:-------|:--------|----:|---------:|-----:|-------:|-------:|-------:|--------:|-------:|--------:|-------:|-------:|------:|
| Male   | No      | 177 | 8058.153 |    0 | 10.354 | 20.088 | 45.526 |  34.480 | 58.063 | 182.728 | 35.567 | 37.975 | 0.781 |
| Male   | Yes     |  16 |  744.638 |    0 | 14.312 | 19.492 | 46.540 |  32.233 | 50.663 | 123.299 | 37.593 | 31.171 | 0.808 |
| Female | No      | 183 | 8137.719 |    0 | 10.363 | 21.917 | 44.468 |  31.861 | 57.605 | 186.634 | 34.349 | 35.688 | 0.772 |
| Female | Yes     |  24 | 1147.044 |    0 | 10.627 | 22.160 | 47.794 |  38.063 | 55.623 | 180.379 | 39.965 | 33.463 | 0.836 |

# Análise bivariada em dados de Crédito

## Sobre variáveis explicativas (features)

### Tabulação de dados

``` r
rnp_tabelao_variavel(dados = db, 
                     variavel_mau = "Credit_risk", 
                     variavel_categ = "Gender",
                     define_mau = "BAD") %>% 
  knitr::kable(digits = 3)
```

| Gender | total | p_total | t_maus | t_bons | p_maus | p_bons | p_maus_acum | p_bons_acum |  odds |    woe |    iv |    ks |  gini |
|:-------|------:|--------:|-------:|-------:|-------:|-------:|------------:|------------:|------:|-------:|------:|------:|------:|
| Female |   690 |    0.69 |    191 |    499 |  0.637 |  0.713 |       0.637 |       0.713 | 0.893 | -0.113 | 0.009 | 0.076 | 0.454 |
| Male   |   310 |    0.31 |    109 |    201 |  0.363 |  0.287 |       1.000 |       1.000 | 1.265 |  0.235 | 0.018 | 0.000 | 0.470 |

### VDI (Variable Deviate Index)

``` r
# Freqências Antes
tbref <- rnp_freq(db$Gender, digits = 3)

# Freqências Depois
tbrec <- rnp_freq(sample(db$Gender, size = 250), digits = 3)

# Freqências
tbref %>% knitr::kable(digits = 3)
```

| classe |  fa |   fr |  faa |  fra |
|:-------|----:|-----:|-----:|-----:|
| Female | 690 | 0.69 |  690 | 0.69 |
| Male   | 310 | 0.31 | 1000 | 1.00 |

``` r
tbrec %>% knitr::kable(digits = 3)
```

| classe |  fa |    fr | faa |   fra |
|:-------|----:|------:|----:|------:|
| Female | 177 | 0.708 | 177 | 0.708 |
| Male   |  73 | 0.292 | 250 | 1.000 |

``` r

# VDI
rnp_vdi(tbref, tbrec, total = FALSE) %>% 
  knitr::kable(digits = 3)
```

| classe | fa_esperado | fr_esperado | fa_atual | fr_atual |   vdi | inferencia |
|:-------|------------:|------------:|---------:|---------:|------:|:-----------|
| Female |         690 |        0.69 |      177 |    0.708 | 0.000 | G.Tudo bem |
| Male   |         310 |        0.31 |       73 |    0.292 | 0.001 | G.Tudo bem |

## Sobre variáveis resposta (label/target)

``` r
## Um modelo Nayve para score de crédito nos dados
id_amostra <- as.numeric(
  sample(row.names(db), size = nrow(db) * (2/3))
)
## Treino e teste
treino <- db[id_amostra, ]; treino$Target <- NULL
teste  <- db[-id_amostra, ]; treino$Target <- NULL

## Ajuste com redução de variáveis por stepwise

fit <- step(glm(Credit_risk ~ ., 
           family = binomial(link = "logit"),
           data = treino), direction = "both", trace = FALSE)

## Preditos
pred_treino <- predict(fit, newdata = treino, type = "response")
pred_teste  <- predict(fit, newdata = teste, type = "response")

## Junta os dados
treino_fill <- data.frame(treino, pred_treino)
teste_fill  <- data.frame(teste, pred_teste)
```

``` r
## Tabela de ganho (gains table) base de treino
rnp_tabelao_score(dados = treino_fill,
                  variavel_mau = "Credit_risk", 
                  variavel_score = "pred_treino", 
                  define_mau = "BAD",
                  credito = TRUE,
                  nquebras = 10) %>% 
  knitr::kable(digits = 3)
```

| quebras | total | p_total | min_score | max_score | t_maus | t_bons | p_maus | p_bons | v_total | p_maus_acum | p_bons_acum |  odds |    woe |    iv |    ks |   gini |
|--------:|------:|--------:|----------:|----------:|-------:|-------:|-------:|-------:|--------:|------------:|------------:|------:|-------:|------:|------:|-------:|
|       1 |    67 |   0.101 |     0.038 |     0.302 |     52 |     15 |  0.254 |  0.032 |       0 |       0.254 |       0.032 | 7.796 |  2.054 | 0.454 | 0.221 | -0.035 |
|       2 |    67 |   0.101 |     0.306 |     0.440 |     45 |     22 |  0.220 |  0.048 |       0 |       0.473 |       0.080 | 4.600 |  1.526 | 0.262 | 0.393 | -0.079 |
|       3 |    66 |   0.099 |     0.444 |     0.565 |     33 |     33 |  0.161 |  0.072 |       0 |       0.634 |       0.152 | 2.249 |  0.810 | 0.072 | 0.482 | -0.127 |
|       4 |    67 |   0.101 |     0.565 |     0.664 |     25 |     42 |  0.122 |  0.091 |       0 |       0.756 |       0.243 | 1.339 |  0.292 | 0.009 | 0.513 | -0.152 |
|       5 |    66 |   0.099 |     0.671 |     0.761 |     23 |     43 |  0.112 |  0.093 |       0 |       0.868 |       0.336 | 1.203 |  0.185 | 0.004 | 0.532 | -0.218 |
|       6 |    67 |   0.101 |     0.762 |     0.847 |     11 |     56 |  0.054 |  0.122 |       0 |       0.922 |       0.458 | 0.442 | -0.817 | 0.055 | 0.464 | -0.240 |
|       7 |    66 |   0.099 |     0.847 |     0.895 |      7 |     59 |  0.034 |  0.128 |       0 |       0.956 |       0.586 | 0.267 | -1.321 | 0.124 | 0.370 | -0.257 |
|       8 |    67 |   0.101 |     0.896 |     0.933 |      6 |     61 |  0.029 |  0.132 |       0 |       0.985 |       0.718 | 0.221 | -1.509 | 0.156 | 0.267 | -0.279 |
|       9 |    66 |   0.099 |     0.933 |     0.968 |      1 |     65 |  0.005 |  0.141 |       0 |       0.990 |       0.859 | 0.035 | -3.364 | 0.458 | 0.131 | -0.281 |
|      10 |    67 |   0.101 |     0.968 |     0.998 |      2 |     65 |  0.010 |  0.141 |       0 |       1.000 |       1.000 | 0.069 | -2.671 | 0.350 | 0.000 |  1.000 |

``` r

## 
## Tabela de ganho (gains table) base de teste
rnp_tabelao_score(dados = teste_fill,
                  variavel_mau = "Credit_risk", 
                  variavel_score = "pred_teste", 
                  define_mau = "BAD",
                  credito = TRUE,
                  nquebras = 10) %>% 
  knitr::kable(digits = 3)
```

| quebras | total | p_total | min_score | max_score | t_maus | t_bons | p_maus | p_bons | v_total | p_maus_acum | p_bons_acum |  odds |    woe |    iv |    ks |   gini |
|--------:|------:|--------:|----------:|----------:|-------:|-------:|-------:|-------:|--------:|------------:|------------:|------:|-------:|------:|------:|-------:|
|       1 |    34 |   0.102 |     0.037 |     0.237 |     22 |     12 |  0.232 |  0.050 |       0 |       0.232 |       0.050 | 4.612 |  1.529 | 0.277 | 0.181 | -0.034 |
|       2 |    33 |   0.099 |     0.239 |     0.345 |     21 |     12 |  0.221 |  0.050 |       0 |       0.453 |       0.100 | 4.403 |  1.482 | 0.253 | 0.352 | -0.076 |
|       3 |    33 |   0.099 |     0.346 |     0.498 |     16 |     17 |  0.168 |  0.071 |       0 |       0.621 |       0.172 | 2.368 |  0.862 | 0.084 | 0.450 | -0.135 |
|       4 |    34 |   0.102 |     0.500 |     0.656 |     10 |     24 |  0.105 |  0.100 |       0 |       0.726 |       0.272 | 1.048 |  0.047 | 0.000 | 0.454 | -0.161 |
|       5 |    33 |   0.099 |     0.656 |     0.774 |      8 |     25 |  0.084 |  0.105 |       0 |       0.810 |       0.377 | 0.805 | -0.217 | 0.004 | 0.434 | -0.190 |
|       6 |    33 |   0.099 |     0.775 |     0.829 |      6 |     27 |  0.063 |  0.113 |       0 |       0.874 |       0.490 | 0.559 | -0.582 | 0.029 | 0.384 | -0.225 |
|       7 |    34 |   0.102 |     0.832 |     0.894 |      4 |     30 |  0.042 |  0.126 |       0 |       0.916 |       0.615 | 0.335 | -1.092 | 0.091 | 0.301 | -0.221 |
|       8 |    33 |   0.099 |     0.894 |     0.937 |      5 |     28 |  0.053 |  0.117 |       0 |       0.968 |       0.732 | 0.449 | -0.800 | 0.052 | 0.236 | -0.254 |
|       9 |    33 |   0.099 |     0.938 |     0.969 |      2 |     31 |  0.021 |  0.130 |       0 |       0.990 |       0.862 | 0.162 | -1.818 | 0.198 | 0.128 | -0.275 |
|      10 |    34 |   0.102 |     0.971 |     0.999 |      1 |     33 |  0.011 |  0.138 |       0 |       1.000 |       1.000 | 0.076 | -2.574 | 0.328 | 0.000 |  1.000 |
