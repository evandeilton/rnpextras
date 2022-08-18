
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
| Female | 166 | 0.664 | 166 | 0.664 |
| Male   |  84 | 0.336 | 250 | 1.000 |

``` r

# VDI
rnp_vdi(tbref, tbrec, total = FALSE) %>% 
  knitr::kable(digits = 3)
```

| classe | fa_esperado | fr_esperado | fa_atual | fr_atual |   vdi | inferencia |
|:-------|------------:|------------:|---------:|---------:|------:|:-----------|
| Female |         690 |        0.69 |      166 |    0.664 | 0.001 | G.Tudo bem |
| Male   |         310 |        0.31 |       84 |    0.336 | 0.002 | G.Tudo bem |

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

| quebras | total | p_total | min_score | max_score | t_maus | t_bons | p_maus | p_bons | v_total | p_maus_acum | p_bons_acum |   odds |    woe |    iv |    ks |   gini |
|--------:|------:|--------:|----------:|----------:|-------:|-------:|-------:|-------:|--------:|------------:|------------:|-------:|-------:|------:|------:|-------:|
|       1 |    67 |   0.101 |     0.024 |     0.287 |     55 |     12 |  0.278 |  0.026 |       0 |       0.278 |       0.026 | 10.833 |  2.383 | 0.601 | 0.252 | -0.038 |
|       2 |    67 |   0.101 |     0.287 |     0.431 |     44 |     23 |  0.222 |  0.049 |       0 |       0.500 |       0.075 |  4.522 |  1.509 | 0.261 | 0.425 | -0.097 |
|       3 |    66 |   0.099 |     0.432 |     0.571 |     26 |     40 |  0.131 |  0.086 |       0 |       0.631 |       0.160 |  1.536 |  0.429 | 0.020 | 0.471 | -0.132 |
|       4 |    67 |   0.101 |     0.574 |     0.694 |     22 |     45 |  0.111 |  0.096 |       0 |       0.742 |       0.256 |  1.156 |  0.145 | 0.002 | 0.486 | -0.144 |
|       5 |    66 |   0.099 |     0.694 |     0.785 |     24 |     42 |  0.121 |  0.090 |       0 |       0.864 |       0.346 |  1.351 |  0.301 | 0.010 | 0.517 | -0.213 |
|       6 |    67 |   0.101 |     0.785 |     0.864 |     11 |     56 |  0.056 |  0.120 |       0 |       0.919 |       0.466 |  0.464 | -0.767 | 0.049 | 0.453 | -0.236 |
|       7 |    66 |   0.099 |     0.864 |     0.905 |      7 |     59 |  0.035 |  0.126 |       0 |       0.955 |       0.592 |  0.280 | -1.271 | 0.115 | 0.363 | -0.253 |
|       8 |    67 |   0.101 |     0.906 |     0.945 |      6 |     61 |  0.030 |  0.130 |       0 |       0.985 |       0.722 |  0.232 | -1.459 | 0.146 | 0.263 | -0.267 |
|       9 |    66 |   0.099 |     0.946 |     0.976 |      3 |     63 |  0.015 |  0.135 |       0 |       1.000 |       0.857 |  0.113 | -2.184 | 0.261 | 0.143 | -0.286 |
|      10 |    67 |   0.101 |     0.977 |     1.000 |      0 |     67 |  0.000 |  0.143 |       0 |       1.000 |       1.000 |  0.000 |  0.000 | 0.000 | 0.000 |  1.000 |

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
|       1 |    34 |   0.102 |     0.043 |     0.272 |     23 |     11 |  0.226 |  0.047 |       0 |       0.226 |       0.047 | 4.756 |  1.559 | 0.278 | 0.178 | -0.038 |
|       2 |    33 |   0.099 |     0.280 |     0.427 |     19 |     14 |  0.186 |  0.060 |       0 |       0.412 |       0.108 | 3.087 |  1.127 | 0.142 | 0.304 | -0.072 |
|       3 |    33 |   0.099 |     0.430 |     0.549 |     16 |     17 |  0.157 |  0.073 |       0 |       0.569 |       0.181 | 2.141 |  0.761 | 0.064 | 0.388 | -0.110 |
|       4 |    34 |   0.102 |     0.549 |     0.685 |     14 |     20 |  0.137 |  0.086 |       0 |       0.706 |       0.267 | 1.592 |  0.465 | 0.024 | 0.439 | -0.150 |
|       5 |    33 |   0.099 |     0.689 |     0.790 |     10 |     23 |  0.098 |  0.099 |       0 |       0.804 |       0.366 | 0.989 | -0.011 | 0.000 | 0.438 | -0.194 |
|       6 |    33 |   0.099 |     0.794 |     0.859 |      6 |     27 |  0.059 |  0.116 |       0 |       0.863 |       0.483 | 0.505 | -0.682 | 0.039 | 0.380 | -0.234 |
|       7 |    34 |   0.102 |     0.859 |     0.915 |      3 |     31 |  0.029 |  0.134 |       0 |       0.892 |       0.616 | 0.220 | -1.514 | 0.158 | 0.276 | -0.241 |
|       8 |    33 |   0.099 |     0.916 |     0.951 |      2 |     31 |  0.020 |  0.134 |       0 |       0.912 |       0.750 | 0.147 | -1.919 | 0.219 | 0.162 | -0.240 |
|       9 |    33 |   0.099 |     0.951 |     0.986 |      3 |     30 |  0.029 |  0.129 |       0 |       0.941 |       0.879 | 0.228 | -1.481 | 0.148 | 0.062 | -0.234 |
|      10 |    34 |   0.102 |     0.986 |     1.000 |      6 |     28 |  0.059 |  0.121 |       0 |       1.000 |       1.000 | 0.487 | -0.719 | 0.044 | 0.000 |  1.000 |
