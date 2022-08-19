
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
rnpextras::rnp_resumo(da)
#>          var  classe    tipo nobs ndis nmis  minimo       q25   mediana
#> 1         ID integer integer  400  400    0   1.000  100.7500  200.5000
#> 2     Income numeric  double  400  399    0  10.354   21.0072   33.1155
#> 3      Limit integer integer  400  387    0 855.000 3088.0000 4622.5000
#> 4     Rating integer integer  400  283    0  93.000  247.2500  344.0000
#> 5      Cards integer integer  400    9    0   1.000    2.0000    3.0000
#> 6        Age integer integer  400   68    0  23.000   41.7500   56.0000
#> 7  Education integer integer  400   16    0   5.000   11.0000   14.0000
#> 8     Gender  factor integer  400    2    0      NA        NA        NA
#> 9    Student  factor integer  400    2    0      NA        NA        NA
#> 10   Married  factor integer  400    2    0      NA        NA        NA
#> 11 Ethnicity  factor integer  400    3    0      NA        NA        NA
#> 12   Balance integer integer  400  284    0   0.000   68.7500  459.5000
#>        media       q75    maximo    devpad     cv       iqr
#> 1   200.5000  300.2500   400.000  115.6143 0.5766  199.5000
#> 2    45.2189   57.4708   186.634   35.2443 0.7794   36.4635
#> 3  4735.6000 5872.7500 13913.000 2308.1988 0.4874 2784.7500
#> 4   354.9400  437.2500   982.000  154.7241 0.4359  190.0000
#> 5     2.9575    4.0000     9.000    1.3713 0.4637    2.0000
#> 6    55.6675   70.0000    98.000   17.2498 0.3099   28.2500
#> 7    13.4500   16.0000    20.000    3.1252 0.2324    5.0000
#> 8         NA        NA        NA        NA     NA        NA
#> 9         NA        NA        NA        NA     NA        NA
#> 10        NA        NA        NA        NA     NA        NA
#> 11        NA        NA        NA        NA     NA        NA
#> 12  520.0150  863.0000  1999.000  459.7589 0.8841  794.2500
```

## Média artmética simples, geométrica e harmônica

``` r
rnpextras::rnp_media(da$Income)
#>    media_a  media_g  media_h
#> 1 45.21889 35.27731 28.37636
```

## Resumo descritivo

``` r
rnpextras::rnp_summary(da$Income) %>% t()
#>        N     Soma Nmis    Min      Q1   Media Mediana      Q3     Max  DevPad
#> [1,] 400 18087.55    0 10.354 21.0072 45.2189 33.1155 57.4708 186.634 35.2443
#>          IQR     cv
#> [1,] 36.4635 0.7794
rnpextras::rnp_summary_all(da)$num
#>    variavel   N       Soma Nmis     Min        Q1     Media   Mediana        Q3
#> 1        ID 400   80200.00    0   1.000  100.7500  200.5000  200.5000  300.2500
#> 2    Income 400   18087.55    0  10.354   21.0072   45.2189   33.1155   57.4708
#> 3     Limit 400 1894240.00    0 855.000 3088.0000 4735.6000 4622.5000 5872.7500
#> 4    Rating 400  141976.00    0  93.000  247.2500  354.9400  344.0000  437.2500
#> 5     Cards 400    1183.00    0   1.000    2.0000    2.9575    3.0000    4.0000
#> 6       Age 400   22267.00    0  23.000   41.7500   55.6675   56.0000   70.0000
#> 7 Education 400    5380.00    0   5.000   11.0000   13.4500   14.0000   16.0000
#> 8   Balance 400  208006.00    0   0.000   68.7500  520.0150  459.5000  863.0000
#>         Max    DevPad       IQR     cv
#> 1   400.000  115.6143  199.5000 0.5766
#> 2   186.634   35.2443   36.4635 0.7794
#> 3 13913.000 2308.1988 2784.7500 0.4874
#> 4   982.000  154.7241  190.0000 0.4359
#> 5     9.000    1.3713    2.0000 0.4637
#> 6    98.000   17.2498   28.2500 0.3099
#> 7    20.000    3.1252    5.0000 0.2324
#> 8  1999.000  459.7589  794.2500 0.8841
```

## Correlação em forma tabular e vertical

``` r
rnpextras::rnp_correlacao(da[,c("Age","Income","Limit","Balance")])
#>        x       y pearson spearman kendall
#> 2    Age  Income  0.1753   0.1479  0.0985
#> 3    Age   Limit  0.1009   0.0641  0.0414
#> 1    Age Balance  0.0018  -0.0107 -0.0074
#> 5 Income   Limit  0.7921   0.6574  0.4774
#> 4 Income Balance  0.4637   0.3613  0.2507
#> 6  Limit Balance  0.8617   0.8894  0.7164
```

# Descritiva dados categóricos

## Tabelas de freqência simples e de dupla entrada

``` r
rnpextras::rnp_freq(da$Gender)
#>   classe  fa     fr faa    fra
#> 1   Male 193 0.4825 193 0.4825
#> 2 Female 207 0.5175 400 1.0000
rnpextras::rnp_2freq(da$Gender, da$Ethnicity)
#>   Tipo Classe X/Y African American Asian Caucasian Total
#> 1   fa       Male               49    47        97   193
#> 2   fa     Female               50    55       102   207
#> 3   fa      Total               99   102       199   400
rnpextras::rnp_2freq(da$Gender, da$Ethnicity, percents = TRUE)
#>      Tipo Classe X/Y African American    Asian Caucasian    Total
#> 1      fa       Male          49.0000  47.0000   97.0000 193.0000
#> 2      fr       Male           0.1225   0.1175    0.2425   0.4825
#> 3  fr_col       Male           0.4949   0.4608    0.4874   0.4825
#> 4  fr_lin       Male           0.2539   0.2435    0.5026   1.0000
#> 5      fa     Female          50.0000  55.0000  102.0000 207.0000
#> 6      fr     Female           0.1250   0.1375    0.2550   0.5175
#> 7  fr_col     Female           0.5051   0.5392    0.5126   0.5175
#> 8  fr_lin     Female           0.2415   0.2657    0.4928   1.0000
#> 9      fa      Total          99.0000 102.0000  199.0000 400.0000
#> 10     fr      Total           0.2475   0.2550    0.4975   1.0000
```

## Sumarização com grupos

``` r
rnpextras::rnp_summary_by(da, variavel = "Income", grupos = "Gender")
#>   Gender   N     Soma Nmis    Min     Q1  Media Mediana     Q3     Max DevPad
#> 1   Male 193 8802.791    0 10.354 20.088 45.610  33.437 58.063 182.728 35.638
#> 2 Female 207 9284.763    0 10.363 21.917 44.854  32.164 57.270 186.634 34.955
#>      IQR    cv
#> 1 37.975 0.781
#> 2 35.352 0.779
rnpextras::rnp_summary_by(da, variavel = "Income", grupos = c("Gender","Student"))
#>   Gender Student   N     Soma Nmis    Min     Q1  Media Mediana     Q3     Max
#> 1   Male      No 177 8058.153    0 10.354 20.088 45.526  34.480 58.063 182.728
#> 2   Male     Yes  16  744.638    0 14.312 19.492 46.540  32.233 50.663 123.299
#> 3 Female      No 183 8137.719    0 10.363 21.917 44.468  31.861 57.605 186.634
#> 4 Female     Yes  24 1147.044    0 10.627 22.160 47.794  38.063 55.623 180.379
#>   DevPad    IQR    cv
#> 1 35.567 37.975 0.781
#> 2 37.593 31.171 0.808
#> 3 34.349 35.688 0.772
#> 4 39.965 33.463 0.836
```

# Análise bivariada em dados de Crédito

## Sobre variáveis explicativas (features)

### Tabulação de dados

``` r
rnp_tabelao_variavel(dados = db, 
                     variavel_mau = "Credit_risk", 
                     variavel_categ = "Purpose",
                     define_mau = "BAD")
#> # A tibble: 10 × 14
#>    Purpose       total p_total t_maus t_bons p_maus p_bons p_mau…¹ p_bon…²  odds
#>    <chr>         <dbl>   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl> <dbl>
#>  1 retrainin         9   0.009      1      8 0.0033 0.0114  0.0033  0.0114 0.292
#>  2 car (used)      103   0.103     17     86 0.0567 0.123   0.06    0.134  0.461
#>  3 radio / tele…   280   0.28      62    218 0.207  0.311   0.267   0.446  0.664
#>  4 furniture / …   181   0.181     58    123 0.193  0.176   0.46    0.621  1.10 
#>  5 domestic app…    12   0.012      4      8 0.0133 0.0114  0.473   0.633  1.17 
#>  6 business         97   0.097     34     63 0.113  0.09    0.587   0.723  1.26 
#>  7 repairs          22   0.022      8     14 0.0267 0.02    0.613   0.743  1.33 
#>  8 car (new)       234   0.234     89    145 0.297  0.207   0.91    0.95   1.43 
#>  9 others           12   0.012      5      7 0.0167 0.01    0.927   0.96   1.67 
#> 10 education        50   0.05      22     28 0.0733 0.04    1       1      1.83 
#> # … with 4 more variables: woe <dbl>, iv <dbl>, ks <dbl>, gini <dbl>, and
#> #   abbreviated variable names ¹​p_maus_acum, ²​p_bons_acum
```

### VDI (Variable Deviate Index)

``` r
# Freqências Antes
tbref <- rnp_freq(db$Purpose, digits = 3)

# Freqências Depois
tbrec <- rnp_freq(sample(db$Purpose, size = 250), digits = 3)

# Freqências
tbref
#>                   classe  fa    fr  faa   fra
#> 1               business  97 0.097   97 0.097
#> 2              car (new) 234 0.234  331 0.331
#> 3             car (used) 103 0.103  434 0.434
#> 4    domestic appliances  12 0.012  446 0.446
#> 5              education  50 0.050  496 0.496
#> 6  furniture / equipment 181 0.181  677 0.677
#> 7                 others  12 0.012  689 0.689
#> 8     radio / television 280 0.280  969 0.969
#> 9                repairs  22 0.022  991 0.991
#> 10             retrainin   9 0.009 1000 1.000
tbrec
#>                   classe fa    fr faa   fra
#> 1               business 19 0.076  19 0.076
#> 2              car (new) 62 0.248  81 0.324
#> 3             car (used) 25 0.100 106 0.424
#> 4    domestic appliances  4 0.016 110 0.440
#> 5              education 17 0.068 127 0.508
#> 6  furniture / equipment 41 0.164 168 0.672
#> 7                 others  4 0.016 172 0.688
#> 8     radio / television 67 0.268 239 0.956
#> 9                repairs  7 0.028 246 0.984
#> 10             retrainin  4 0.016 250 1.000

# VDI
rnp_vdi(tbref, tbrec, total = FALSE)
#>                   classe fa_esperado fr_esperado fa_atual fr_atual    vdi
#> 1               business          97       0.097       19    0.076 0.0051
#> 2              car (new)         234       0.234       62    0.248 0.0008
#> 3             car (used)         103       0.103       25    0.100 0.0001
#> 4    domestic appliances          12       0.012        4    0.016 0.0012
#> 5              education          50       0.050       17    0.068 0.0055
#> 6  furniture / equipment         181       0.181       41    0.164 0.0017
#> 7                 others          12       0.012        4    0.016 0.0012
#> 8     radio / television         280       0.280       67    0.268 0.0005
#> 9                repairs          22       0.022        7    0.028 0.0014
#> 10             retrainin           9       0.009        4    0.016 0.0040
#>    inferencia
#> 1  G.Tudo bem
#> 2  G.Tudo bem
#> 3  G.Tudo bem
#> 4  G.Tudo bem
#> 5  G.Tudo bem
#> 6  G.Tudo bem
#> 7  G.Tudo bem
#> 8  G.Tudo bem
#> 9  G.Tudo bem
#> 10 G.Tudo bem
```

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
                  nquebras = 10)
#> # A tibble: 10 × 17
#>    quebras total p_total min_score max_score t_maus t_bons p_maus p_bons v_total
#>      <dbl> <dbl>   <dbl>     <dbl>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1       1    67  0.101     0.0297     0.300     53     14 0.272  0.0297       0
#>  2       2    67  0.101     0.302      0.468     43     24 0.220  0.051        0
#>  3       3    66  0.0991    0.471      0.592     29     37 0.149  0.0786       0
#>  4       4    67  0.101     0.593      0.687     22     45 0.113  0.0955       0
#>  5       5    66  0.0991    0.687      0.783     18     48 0.0923 0.102        0
#>  6       6    67  0.101     0.784      0.857     13     54 0.0667 0.115        0
#>  7       7    66  0.0991    0.857      0.903      8     58 0.041  0.123        0
#>  8       8    67  0.101     0.903      0.940      6     61 0.0308 0.130        0
#>  9       9    66  0.0991    0.944      0.972      1     65 0.0051 0.138        0
#> 10      10    67  0.101     0.973      1.00       2     65 0.0103 0.138        0
#> # … with 7 more variables: p_maus_acum <dbl>, p_bons_acum <dbl>, odds <dbl>,
#> #   woe <dbl>, iv <dbl>, ks <dbl>, gini <dbl>

## 
## Tabela de ganho (gains table) base de teste
rnp_tabelao_score(dados = teste_fill,
                  variavel_mau = "Credit_risk", 
                  variavel_score = "pred_teste", 
                  define_mau = "BAD",
                  credito = TRUE,
                  nquebras = 10)
#> # A tibble: 10 × 17
#>    quebras total p_total min_score max_score t_maus t_bons p_maus p_bons v_total
#>      <dbl> <dbl>   <dbl>     <dbl>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
#>  1       1    34  0.102     0.0508     0.295     23     11 0.219  0.048        0
#>  2       2    33  0.0988    0.296      0.451     20     13 0.190  0.0568       0
#>  3       3    33  0.0988    0.460      0.606     15     18 0.143  0.0786       0
#>  4       4    34  0.102     0.616      0.706     13     21 0.124  0.0917       0
#>  5       5    33  0.0988    0.709      0.790      6     27 0.0571 0.118        0
#>  6       6    33  0.0988    0.793      0.859      6     27 0.0571 0.118        0
#>  7       7    34  0.102     0.861      0.902      4     30 0.0381 0.131        0
#>  8       8    33  0.0988    0.902      0.941      6     27 0.0571 0.118        0
#>  9       9    33  0.0988    0.941      0.970      6     27 0.0571 0.118        0
#> 10      10    34  0.102     0.970      0.998      6     28 0.0571 0.122        0
#> # … with 7 more variables: p_maus_acum <dbl>, p_bons_acum <dbl>, odds <dbl>,
#> #   woe <dbl>, iv <dbl>, ks <dbl>, gini <dbl>
```