
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
                     variavel_mau = "Target", 
                     variavel_categ = "Purpose",
                     define_mau = 0) %>% 
  knitr::kable(digits = 3)
```

| Purpose               | total | p_total | t_maus | t_bons | p_maus | p_bons | p_maus_acum | p_bons_acum |  odds |    woe |    iv |    ks |  gini |
|:----------------------|------:|--------:|-------:|-------:|-------:|-------:|------------:|------------:|------:|-------:|------:|------:|------:|
| education             |    50 |   0.050 |     28 |     22 |  0.040 |  0.073 |       0.040 |       0.073 | 0.545 | -0.606 | 0.020 | 0.033 | 0.003 |
| others                |    12 |   0.012 |      7 |      5 |  0.010 |  0.017 |       0.050 |       0.090 | 0.600 | -0.511 | 0.003 | 0.040 | 0.002 |
| car (new)             |   234 |   0.234 |    145 |     89 |  0.207 |  0.297 |       0.257 |       0.387 | 0.698 | -0.359 | 0.032 | 0.130 | 0.091 |
| repairs               |    22 |   0.022 |     14 |      8 |  0.020 |  0.027 |       0.277 |       0.413 | 0.750 | -0.288 | 0.002 | 0.136 | 0.014 |
| business              |    97 |   0.097 |     63 |     34 |  0.090 |  0.113 |       0.367 |       0.527 | 0.794 | -0.230 | 0.005 | 0.160 | 0.073 |
| domestic appliances   |    12 |   0.012 |      8 |      4 |  0.011 |  0.013 |       0.379 |       0.540 | 0.857 | -0.154 | 0.000 | 0.161 | 0.010 |
| furniture / equipment |   181 |   0.181 |    123 |     58 |  0.176 |  0.193 |       0.554 |       0.733 | 0.909 | -0.096 | 0.002 | 0.179 | 0.180 |
| radio / television    |   280 |   0.280 |    218 |     62 |  0.311 |  0.207 |       0.866 |       0.940 | 1.507 |  0.410 | 0.043 | 0.074 | 0.294 |
| car (used)            |   103 |   0.103 |     86 |     17 |  0.123 |  0.057 |       0.989 |       0.997 | 2.168 |  0.774 | 0.051 | 0.008 | 0.105 |
| retrainin             |     9 |   0.009 |      8 |      1 |  0.011 |  0.003 |       1.000 |       1.000 | 3.429 |  1.232 | 0.010 | 0.000 | 0.007 |

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
| Female | 173 | 0.692 | 173 | 0.692 |
| Male   |  77 | 0.308 | 250 | 1.000 |

``` r

# VDI
rnp_vdi(tbref, tbrec, total = FALSE) %>% 
  knitr::kable(digits = 3)
```

| classe | fa_esperado | fr_esperado | fa_atual | fr_atual | vdi | inferencia |
|:-------|------------:|------------:|---------:|---------:|----:|:-----------|
| Female |         690 |        0.69 |      173 |    0.692 |   0 | G.Tudo bem |
| Male   |         310 |        0.31 |       77 |    0.308 |   0 | G.Tudo bem |

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
|       1 |    67 |   0.101 |     0.047 |     0.304 |     53 |     14 |  0.256 |  0.030 |       0 |       0.256 |       0.030 | 8.394 |  2.128 | 0.480 | 0.226 | -0.038 |
|       2 |    67 |   0.101 |     0.311 |     0.439 |     43 |     24 |  0.208 |  0.052 |       0 |       0.464 |       0.083 | 3.973 |  1.380 | 0.214 | 0.381 | -0.070 |
|       3 |    66 |   0.099 |     0.439 |     0.569 |     37 |     29 |  0.179 |  0.063 |       0 |       0.642 |       0.146 | 2.829 |  1.040 | 0.120 | 0.496 | -0.144 |
|       4 |    67 |   0.101 |     0.569 |     0.664 |     19 |     48 |  0.092 |  0.105 |       0 |       0.734 |       0.250 | 0.878 | -0.130 | 0.002 | 0.484 | -0.174 |
|       5 |    66 |   0.099 |     0.666 |     0.748 |     14 |     52 |  0.068 |  0.113 |       0 |       0.802 |       0.364 | 0.597 | -0.516 | 0.024 | 0.438 | -0.187 |
|       6 |    67 |   0.101 |     0.750 |     0.831 |     16 |     51 |  0.077 |  0.111 |       0 |       0.879 |       0.475 | 0.696 | -0.363 | 0.012 | 0.404 | -0.200 |
|       7 |    66 |   0.099 |     0.832 |     0.889 |     16 |     50 |  0.077 |  0.109 |       0 |       0.957 |       0.584 | 0.710 | -0.343 | 0.011 | 0.373 | -0.254 |
|       8 |    67 |   0.101 |     0.889 |     0.929 |      7 |     60 |  0.034 |  0.131 |       0 |       0.990 |       0.715 | 0.259 | -1.352 | 0.131 | 0.276 | -0.278 |
|       9 |    66 |   0.099 |     0.931 |     0.966 |      2 |     64 |  0.010 |  0.139 |       0 |       1.000 |       0.854 | 0.069 | -2.669 | 0.346 | 0.146 | -0.292 |
|      10 |    67 |   0.101 |     0.967 |     0.999 |      0 |     67 |  0.000 |  0.146 |       0 |       1.000 |       1.000 | 0.000 |  0.000 | 0.000 | 0.000 |  1.000 |

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
|       1 |    34 |   0.102 |     0.048 |     0.318 |     23 |     11 |  0.247 |  0.046 |       0 |       0.247 |       0.046 | 5.418 |  1.690 | 0.341 | 0.202 | -0.051 |
|       2 |    33 |   0.099 |     0.319 |     0.458 |     14 |     19 |  0.150 |  0.079 |       0 |       0.398 |       0.124 | 1.910 |  0.647 | 0.046 | 0.273 | -0.072 |
|       3 |    33 |   0.099 |     0.458 |     0.592 |     15 |     18 |  0.161 |  0.075 |       0 |       0.559 |       0.199 | 2.160 |  0.770 | 0.067 | 0.360 | -0.122 |
|       4 |    34 |   0.102 |     0.599 |     0.693 |     10 |     24 |  0.108 |  0.100 |       0 |       0.667 |       0.299 | 1.080 |  0.077 | 0.001 | 0.368 | -0.122 |
|       5 |    33 |   0.099 |     0.694 |     0.778 |     13 |     20 |  0.140 |  0.083 |       0 |       0.806 |       0.382 | 1.684 |  0.521 | 0.030 | 0.425 | -0.176 |
|       6 |    33 |   0.099 |     0.779 |     0.835 |      8 |     25 |  0.086 |  0.104 |       0 |       0.892 |       0.486 | 0.829 | -0.187 | 0.003 | 0.407 | -0.228 |
|       7 |    34 |   0.102 |     0.839 |     0.895 |      4 |     30 |  0.043 |  0.124 |       0 |       0.935 |       0.610 | 0.346 | -1.063 | 0.087 | 0.326 | -0.250 |
|       8 |    33 |   0.099 |     0.897 |     0.928 |      1 |     32 |  0.011 |  0.133 |       0 |       0.946 |       0.743 | 0.081 | -2.514 | 0.307 | 0.203 | -0.240 |
|       9 |    33 |   0.099 |     0.929 |     0.965 |      3 |     30 |  0.032 |  0.124 |       0 |       0.979 |       0.867 | 0.259 | -1.350 | 0.124 | 0.111 | -0.263 |
|      10 |    34 |   0.102 |     0.965 |     0.996 |      2 |     32 |  0.021 |  0.133 |       0 |       1.000 |       1.000 | 0.162 | -1.820 | 0.203 | 0.000 |  1.000 |
