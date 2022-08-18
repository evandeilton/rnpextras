
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
                     variavel_categ = "Job",
                     define_mau = "BAD") %>% 
  knitr::kable(digits = 3)
```

| Job                                                              | total | p_total | t_maus | t_bons | p_maus | p_bons | p_maus_acum | p_bons_acum |  odds |    woe |    iv |    ks |  gini |
|:-----------------------------------------------------------------|------:|--------:|-------:|-------:|-------:|-------:|------------:|------------:|------:|-------:|------:|------:|------:|
| unskilled - resident                                             |   200 |   0.200 |     56 |    144 |  0.187 |  0.206 |       0.187 |       0.206 | 0.907 | -0.097 | 0.002 | 0.019 | 0.038 |
| skilled employee / official                                      |   630 |   0.630 |    186 |    444 |  0.620 |  0.634 |       0.807 |       0.840 | 0.978 | -0.023 | 0.000 | 0.033 | 0.630 |
| unemployed / unskilled - non-resident                            |    22 |   0.022 |      7 |     15 |  0.023 |  0.021 |       0.830 |       0.861 | 1.089 |  0.085 | 0.000 | 0.031 | 0.035 |
| management / self-employed / highly qualified employee / officer |   148 |   0.148 |     51 |     97 |  0.170 |  0.139 |       1.000 |       1.000 | 1.227 |  0.204 | 0.006 | 0.000 | 0.254 |

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

| classe |  fa |   fr | faa |  fra |
|:-------|----:|-----:|----:|-----:|
| Female | 180 | 0.72 | 180 | 0.72 |
| Male   |  70 | 0.28 | 250 | 1.00 |

``` r

# VDI
rnp_vdi(tbref, tbrec, total = FALSE) %>% 
  knitr::kable(digits = 3)
```

| classe | fa_esperado | fr_esperado | fa_atual | fr_atual |   vdi | inferencia |
|:-------|------------:|------------:|---------:|---------:|------:|:-----------|
| Female |         690 |        0.69 |      180 |     0.72 | 0.001 | G.Tudo bem |
| Male   |         310 |        0.31 |       70 |     0.28 | 0.003 | G.Tudo bem |

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
|       1 |    67 |   0.101 |     0.024 |     0.325 |     51 |     16 |  0.263 |  0.034 |       0 |       0.263 |       0.034 | 7.755 |  2.048 | 0.469 | 0.229 | -0.037 |
|       2 |    67 |   0.101 |     0.328 |     0.456 |     44 |     23 |  0.227 |  0.049 |       0 |       0.490 |       0.083 | 4.654 |  1.538 | 0.274 | 0.407 | -0.088 |
|       3 |    66 |   0.099 |     0.459 |     0.589 |     29 |     37 |  0.150 |  0.078 |       0 |       0.639 |       0.161 | 1.907 |  0.645 | 0.046 | 0.478 | -0.130 |
|       4 |    67 |   0.101 |     0.589 |     0.710 |     23 |     44 |  0.119 |  0.093 |       0 |       0.758 |       0.254 | 1.272 |  0.240 | 0.006 | 0.503 | -0.166 |
|       5 |    66 |   0.099 |     0.713 |     0.779 |     17 |     49 |  0.088 |  0.104 |       0 |       0.845 |       0.358 | 0.844 | -0.170 | 0.003 | 0.487 | -0.198 |
|       6 |    67 |   0.101 |     0.781 |     0.850 |     14 |     53 |  0.072 |  0.112 |       0 |       0.917 |       0.470 | 0.643 | -0.442 | 0.018 | 0.447 | -0.231 |
|       7 |    66 |   0.099 |     0.850 |     0.904 |      8 |     58 |  0.041 |  0.123 |       0 |       0.959 |       0.593 | 0.336 | -1.092 | 0.089 | 0.366 | -0.259 |
|       8 |    67 |   0.101 |     0.907 |     0.941 |      4 |     63 |  0.021 |  0.134 |       0 |       0.979 |       0.727 | 0.154 | -1.868 | 0.211 | 0.253 | -0.264 |
|       9 |    66 |   0.099 |     0.942 |     0.974 |      3 |     63 |  0.016 |  0.134 |       0 |       0.995 |       0.860 | 0.116 | -2.155 | 0.254 | 0.135 | -0.279 |
|      10 |    67 |   0.101 |     0.974 |     1.000 |      1 |     66 |  0.005 |  0.140 |       0 |       1.000 |       1.000 | 0.037 | -3.300 | 0.444 | 0.000 |  1.000 |

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
|       1 |    34 |   0.102 |     0.058 |     0.328 |     24 |     10 |  0.226 |  0.044 |       0 |       0.226 |       0.044 | 5.162 |  1.641 | 0.300 | 0.183 | -0.045 |
|       2 |    33 |   0.099 |     0.338 |     0.479 |     16 |     17 |  0.151 |  0.075 |       0 |       0.377 |       0.118 | 2.024 |  0.705 | 0.054 | 0.259 | -0.071 |
|       3 |    33 |   0.099 |     0.492 |     0.595 |     15 |     18 |  0.142 |  0.079 |       0 |       0.519 |       0.197 | 1.792 |  0.584 | 0.036 | 0.322 | -0.075 |
|       4 |    34 |   0.102 |     0.597 |     0.727 |     20 |     14 |  0.189 |  0.061 |       0 |       0.708 |       0.259 | 3.073 |  1.123 | 0.143 | 0.449 | -0.147 |
|       5 |    33 |   0.099 |     0.728 |     0.824 |     11 |     22 |  0.104 |  0.096 |       0 |       0.811 |       0.355 | 1.075 |  0.073 | 0.000 | 0.456 | -0.193 |
|       6 |    33 |   0.099 |     0.825 |     0.886 |      7 |     26 |  0.066 |  0.114 |       0 |       0.877 |       0.469 | 0.579 | -0.546 | 0.026 | 0.408 | -0.236 |
|       7 |    34 |   0.102 |     0.886 |     0.919 |      4 |     30 |  0.038 |  0.132 |       0 |       0.915 |       0.601 | 0.287 | -1.249 | 0.117 | 0.314 | -0.231 |
|       8 |    33 |   0.099 |     0.923 |     0.958 |      5 |     28 |  0.047 |  0.123 |       0 |       0.962 |       0.724 | 0.384 | -0.957 | 0.072 | 0.239 | -0.264 |
|       9 |    33 |   0.099 |     0.958 |     0.977 |      2 |     31 |  0.019 |  0.136 |       0 |       0.981 |       0.860 | 0.139 | -1.975 | 0.231 | 0.122 | -0.278 |
|      10 |    34 |   0.102 |     0.978 |     1.000 |      2 |     32 |  0.019 |  0.140 |       0 |       1.000 |       1.000 | 0.134 | -2.007 | 0.244 | 0.000 |  1.000 |
