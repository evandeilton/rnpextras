#' Pega string de endereco e separa em rua, numero e complemento
#' @author LOPES, J. E.
#' @description
#' Esta funcao e uma tentativa de organizar o texto de uma string onde em um unico bloco vem endereco, numero e complemento.
#' Este tipo de string e recebida em bases do Itau.
#' A logia e a seguinte: Seja a string "R ANTONIO MARCELLO                      24   APTO 34". Esta string tem tres partes:
#' (1) do "R" ate a esquerda do 24 e o endereco,
#' (2) o "24" e o numero e
#' (3) o APTO 34 e complemento.
#' O codigo contem regex que tentam identificar estes padroes e separa a string criando tres colunas novas
#' @param end string unica com dados de um endereco inclundo nome da rua, numero e complemento
#' @param nespacos numero de epacos em branco fixos consecutivos utilizados par quebrar a string
#' @param extraregex pode passar sua propria regex para quebrar a string. Se for diferente de NULL 'nespacos' fica suprimida
#' @return data.frame com 5 colunas de nomes:  endereco_entrada, nchar, rua, numero e complemento
#' @importFrom stringr str_trim  fixed str_trim
#' @importFrom stringi stri_split
#' @export
#' @examples
#' str <- 'R GERVASIO MOREIRA MARQUES              172  TRAV 02'
#' rnp_separa_endereco(str, nespacos = 5) #quebra por 5 espacos em branco
#' rnp_separa_endereco(str, extraregex = "[[:space:]]{5}") #quebra com regex extra
rnp_separa_endereco <- function(end, nespacos = 5, extraregex = NULL) {
  x <- end
  fix <- paste(rep("", nespacos+1), sep="", collapse = " ")
  # Remove espacos em branco da direita e da esquerda da string
  x <- stringr::str_trim(x, side = "both")

  # # Separa strings com numero junto a palavras
  x <- gsub('([0-9]+)([^0-9]+)', '\\1 \\2', x)

  # Quebra string seguindo o padrao de varios espacos em branco fixos consecutivos
  # remove espacos e elimina valores em branco dos vetores de strings
  y <- if(is.null(extraregex)){
    stringi::stri_split(x, regex = stringr::fixed(pattern = fix))
  } else {
    stringi::stri_split(x, regex = stringr::fixed(pattern = extraregex))
  }

  y <- lapply(y, function(x) {
    o <- as.character(stringr::str_trim(x, side = "both"))
    o <- o[o !=""]
    o
  })

  # Isola rua pegando a primeira parte da string. Se o vetor tiver apenas um elemento e rua sem numero
  rua <- sapply(y, function(x){
    if(length(x) > 1) {
        x[1]
    } else if (length(x) == 1){
      stringi::stri_split(x, regex = stringr::fixed(pattern = paste(rep("", nespacos-1), sep="", collapse = " ")))[1]
    } else {
      x
    }
  })

  # Isola numero pegando o primeiro numero que ocorre na posicao dois da string anterior
  # e quebrando por espaco em branco
  num <- sapply(y, function(x){
    if(length(x) >= 2){
      unlist(strsplit(x[2], " "))[1]
    } else if(length(x) == 1) {
      xx <- stringi::stri_split(x, regex = stringr::fixed(pattern = paste(rep("", nespacos-1), sep="", collapse = " ")))
      unlist(strsplit(as.character(xx[2]), " "))[1]
    } else {
      NULL
    }
  })

  # Isola o restante da string e trata seus valores como complemento removendo a parte que e do numero.
  com <- sapply(y, function(x){
    if(length(x) == 1){
      o <- 'NULL'
    } else {
      o <- paste(unlist(strsplit(x[2], " "))[-1], collapse = " ")
      o <- stringr::str_trim(o, side = "both")
      o <- o[o!=""]
      o <- ifelse(o == "character(0)", "NULL", o)
    }
    return(o)
  })

  O <- data.frame(endereco_entrada = end,
                  nchar = unlist(nchar(x)),
                  rua = as.character(rua),
                  numero = as.character(num),
                  complemento = ifelse(as.character(com) %in% c("character(0)","logical(0)"), "NULL", as.character(com)),
                  stringsAsFactors = FALSE, check.names = FALSE)

  return(O)
}


#' Tabela de frequencias simples
#'
#' @author LOPES, J. E.
#' @description Calcula estatisticas descritivas de variaveis categoricas como caractere
#' e fatores. Tambem funciona com numericas com valores repetidos. Como resultado tem-se
#' frequencias e percentuais simples e acumulados de cada categoria.
#' Se receber um data.frame tenta calcular as estatisticas para todas as variaveis da mesma.
#' Nao existe deduplicacao dos dados.
#'
#' @param x varivetor de entrada
#' @param digits arredondar as frequencias para...
#' @param percent se TRUE retorna os percentuais formatados como \%
#' @param sortclass ordenacao das classes.
#' @return data.frame com as estaisticas frequencia absoluta (fa), relativa (fr)
#' absoluta acumulada (faa) e relativa acumulada (fra)
#' @importFrom dplyr tibble count mutate mutate_if arrange
#' @export
#' @examples
#' rnp_freq(mtcars$cyl)
#' rnp_freq(mtcars$cyl, percent = TRUE)
#' rnp_freq(mtcars, percent = TRUE)
rnp_freq <- function(x, digits = 4, percent = FALSE, sortclass = TRUE) {
  if(all(is.na(x))){
    stop(cat("Todas as classes da variavel", deparse(substitute(x)), "estao missing!\n"))
  }

  aux_freq <- function(x, digits = 4, sortclass = sortclass, percent = percent){
    t2 <- x %>%
      dplyr::tibble(classe=x) %>%
      dplyr::count(classe, name = "fa") %>%
      dplyr::mutate(fr = if(percent) paste(round(100*(fa / sum(fa)), digits = digits), "%", sep="") else round(fa / sum(fa), digits = digits),
                    faa = cumsum(fa),
                    fra = if(percent) paste(round(100*(faa / sum(fa)), digits = digits), "%", sep="") else round(faa / sum(fa), digits = digits)
      ) %>%
      dplyr::mutate_if(.predicate = "is.integer", "as.numeric") %>%
      as.data.frame()

    if(sortclass){
      t2 <- t2 %>%
        dplyr::arrange(classe)
    }
    return(t2)
  }


  o <- if(is.null(dim(x))){
    aux_freq(x, sortclass = sortclass, digits = digits, percent = percent)
  } else {
    o <- lapply(X = x, FUN = function(i) aux_freq(x = i, sortclass = sortclass, digits = digits, percent = percent))

    ll <- c()
    for(i in names(o)) {
      ll[[i]] <- data.frame(variavel = i, o[[i]], stringsAsFactors = FALSE)
    }

    ll <- do.call(what = "rbind", ll)
    row.names(ll) <- NULL
    ll
  }
  return(as.data.frame(o))
}

#' Tabula dados bivariados
#'
#' @description This function takes a dataset and tabulates summary counts also as some statistical
#' Recebe um dataset e sumariza os dados criando tambem algumas estatisticas como
#' frequencias dos totais, percentuais, PSI/VDI, KS1, IV, GINI, WoE e odds
#'
#' @param dados data set contendos os campos para serem tabulados
#' @param vx string de nome da variavel categorica X
#' @param vy Se existir na base, a variavel target contendo 0 e 1
#' @param bom Se existir vy, definir qual a referencia. 1 ou 0 para bom?
#' @param perf Se existir dados de vy, decidir se tabula ou nao.
#' @details Os calculos nesta funcao sao baseados no pardigma de credit scoring, mas se a?plicam tambem no cenario de fraude.
#' @author LOPES, J. E.
#' @examples
#'\dontrun{
#' ref <- rnp_tabulate(dados = da, "FX", "Y", bom = 0, perf = TRUE)
#' }
#' @export
rnp_tabulate <- function(dados, vx, vy = NULL, bom = 0, perf = FALSE){

  if(perf & !is.null(vy)){
    formu <- formula(paste(vy, "~", vx))
    ag <- aggregate(formu,
                    data = dados,
                    FUN = function(x){
                      n <- length(x)
                      n1 <- sum(x)
                      n0 <- n - n1
                      o <- c("_n_1_"= n1, "_n_0_" = n0, "__n__" = n)
                    },
                    simplify = TRUE
    )
    ag <- data.frame(categoria = ag[1], as.data.frame(as.matrix(ag[2])))
  } else {
    ag <- table(dados[,vx])
    ag <- data.frame(unlist(ag))
    colnames(ag) <- c(vx, "__n__")
    ag$"_n_1_" <- ag$"_n_0_" <- NA
  }

  colnames(ag)[grep(pattern = "_n_1_", names(ag))] <- ifelse(bom == 1, "nbons", "nmaus")
  colnames(ag)[grep(pattern = "_n_0_", names(ag))] <- ifelse(bom == 0, "nbons", "nmaus")
  colnames(ag)[grep(pattern = "__n__", names(ag))] <- "total"
  ag <- subset(ag, select = c(colnames(ag)[1], "nbons", "nmaus", "total"))

  ## Frequencies
  ag$p_bons  <- ag$nbons / sum(ag$nbons, na.rm = TRUE)
  ag$p_maus  <- ag$nmaus / sum(ag$nmaus, na.rm = TRUE)
  ag$p_total <- ag$total / sum(ag$total, na.rm = TRUE)

  ag$cp_bons  <- cumsum(ag$p_bons)
  ag$cp_maus  <- cumsum(ag$p_maus)
  ag$cp_total <- cumsum(ag$p_total)

  ## Bad e Good rate
  ag$badrate  <- ag$nmaus / ag$total
  ag$goodrate <- 1 - ag$badrate

  ## KS, ODDS, WoE, IV, Gini
  ag$odds <- ag$p_bons / ag$p_maus
  ag$woe <- log(ag$odds)
  ag$fx_iv <- ag$woe * (ag$p_bons - ag$cp_maus)
  ag$fx_ks <- abs(ag$cp_bons - ag$cp_maus)

  mlag <- function(x,k) c(rep(0,k),head(x,-k))

  ag$lag_cp_bons <- mlag(ag$cp_bons, 1)
  ag$lag_cp_maus <- mlag(ag$cp_maus, 1)
  #ag$g1 <- (ag$cp_bons - ag$lag_cp_bons) * (ag$cp_maus + ag$lag_cp_maus)
  ag$fx_gini <- (ag$cp_bons + ag$lag_cp_bons) * (ag$cp_maus - ag$lag_cp_maus)
  ag$lag_cp_bons <- ag$lag_cp_maus <- NULL

  class(ag) <- c("mis","data.frame")
  return(ag)
}

#' Calcula Population Stability Index / Variable Deviation Index (PSI/VDI)
#'
#' @description
#' Recebe uma tabela contendo as distribuicoes de frequencias de aseline (x)
#' e recent (y) e calcula o PSI/VDI
#' @details
#' Note que existe um argumento i, isso a torna compativel com simulacoes por bootstrap
#' gerando intervalos de confianca para o indicador.
#' @param tab tabela contendo distribuicoes nomeadas baseline (x) e recente (y)
#' @param i parametro para bootstrap (uso interno)
#' @param cat O retorno pode ser total ou por categoria
#' @author LOPES, J. E.
#' @export
rnp_psi <- function(tab, i = TRUE, cat = FALSE){
  d <- tab[i,]
  if(cat){
    (d$x-d$y)*log(d$x/d$y)
  } else {
    sum((d$x-d$y)*log(d$x/d$y), na.rm = TRUE)
  }
}

#' Kolmogorov-Smirnov (KS)
#' @description
#' Recebe uma tabela contendo as distribuicoes de frequencias de aseline (x) e calcula o KS.
#' @details
#' Note que existe um argumento i, isso a torna compativel com simulacoes por bootstrap
#' gerando intervalos de confianca para o indicador.
#' @param tab tabela contendo distribuicoes nomeadas baseline (x) e recente (y)
#' @param i parametro para bootstrap (uso interno)
#' @param cat O retorno pode ser total ou por categoria
#' @export
rnp_psi <- function(tab, i = TRUE, cat = FALSE){
  d <- tab[i,]
  d$x <- cumsum(d$x)
  d$y <- cumsum(d$y)
  if(cat){
    abs(d$x-d$y)
  } else {
    max(abs(d$x-d$y), na.rm = TRUE)
  }
}

#' Coeficiente Gini
#'
#' @description
#' Recebe uma tabela contendo as distribuicoes de frequencias de aseline (x) e calcula o Gini
#' @details
#' Note que existe um argumento i, isso a torna compativel com simulacoes por bootstrap
#' gerando intervalos de confianca para o indicador.
#' @param tab tabela contendo distribuicoes nomeadas baseline (x) e recente (y)
#' @param i parametro para bootstrap (uso interno)
#' @param cat O retorno pode ser total ou por categoria
#' @export
rnp_gini <- function(tab, i = TRUE, cat = FALSE){
  # x = #bons, y = #maus
  fnlag <- function(x,k) c(rep(0,k),head(x,-k))

  d <- tab[i,]
  d$x <- cumsum(d$x)
  d$y <- cumsum(d$y)

  d$x_lag <- fnlag(d$x, 1)
  d$y_lag <- fnlag(d$y, 1)

  o <- (d$x + d$x_lag) * (d$y - d$y_lag)

  if(cat){
    abs(o)
  } else {
    abs(1-sum(o, na.rm = TRUE))
  }
}

#' Information Value (IV)
#'
#' @description
#' Recebe uma tabela contendo as distribuicoes de frequencias de aseline (x) e calcula o IV
#' @details
#' Note que existe um argumento i, isso a torna compativel com simulacoes por bootstrap
#' gerando intervalos de confianca para o indicador.
#' @param tab tabela contendo distribuicoes nomeadas baseline (x) e recente (y)
#' @param i parametro para bootstrap (uso interno)
#' @param cat O retorno pode ser total ou por categoria
#' @export
rnp_iv <- function(tab, i = TRUE, cat = FALSE){
  d <- tab[i,]
  if(cat){
    (d$x - d$y)*log(d$x / d$y)
  } else {
    sum((d$x - d$y)*log(d$x / d$y), na.rm = TRUE)
  }
}

#' Weight of Evidence (WoE)
#'
#' @description
#' Recebe uma tabela contendo as distribuicoes de frequencias de aseline (x) e calcula o WoE
#' @details
#' Note que existe um argumento i, isso a torna compativel com simulacoes por bootstrap
#' gerando intervalos de confianca para o indicador.
#' @param tab tabela contendo distribuicoes nomeadas baseline (x) e recente (y)
#' @param i parametro para bootstrap (uso interno)
#' @param cat O retorno pode ser total ou por categoria
#' @export
rnp_woe <- function(tab, i = TRUE, cat = FALSE){
  d <- tab[i,]
  if(cat){
    log(d$x / d$y)
  } else {
    # n?o tem WoE para total, apenas faixa
    log(d$x / d$y)
  }
}

#' Calcula os testes de Odds Shift e Qui-Quadrado
#'
#' @description
#' Recebe uma tabela contendo as distribuicoes de frequencias nomeadas
#' baseline (x) e recent (y) e calcula o teste chi-quadrado para homogeneidade
#' das classes de cada variavel sob as hipoteses:
#' H_0 = a distribuicao das frequencias sao iguais (nao diferem)
#' Se o p-value > 0.95 aceita H_0
#' @details
#' Note que existe um argumento i, isso a torna compativel com simulacoes por bootstrap
#' gerando intervalos de confianca para o indicador.
#' @param tab tabela contendo distribuicoes nomeadas baseline (x) e recente (y)
#' @param i parametro para bootstrap (uso interno)
#' @examples
#' \dontrun{
#' require(boot)
#' base <- mtcars[,c("gear","vs")]
#' colnames(base) <- c("var_fx", "y")
#' a <- rnp_tabulate(dados = base, vx = "var_fx", vy = "y", bom = 0, perf = TRUE)
#' a <- a[,c("p_bons", "p_maus")]
#' colnames(a) <- c("x","y")
#' rnp_odds_shift(a)
#' boot::boot(data = a, statistic = rnp_odds_shift, R = 50)
#' }
#' @export
rnp_odds_shift <- function(tab, i = TRUE){
  x <- tab[i,]
  total <- sum(x)
  rowsum <- apply(x,1,sum)
  colsum <- apply(x,2,sum)
  prop <- x/total
  rowprop <- sweep(x,1,rowsum,"/")
  colprop <- sweep(x,2,colsum,"/")
  expected <- (matrix(rowsum) %*% t(matrix(colsum))) / total
  dimnames(expected) <- dimnames(x)
  resid <- (x-expected)/sqrt(expected)
  df <- prod(dim(x)-1)
  X2 <- sum(resid^2)
  pvalue <- 1-pchisq(X2,df)
  return(c("X2" = X2, "gl" = df,"pvalor" = pvalue))
}

#' Limpa CPF
#' @description Recebe um vetor de numeros ou textos de CPF's remove qualquer
#' carcatere que nao seja numero e completa com zeros as esquerda se tiver
#' menos de 11 digitos. Caso o valor informado tenha mais de 11 digitos
#' trata como cnpf e completa formando um numero com 14 digitos
#' @param cpf valou ou vetor com os numeros de cpf
rnp_limpa_cpf <- function(cpf){
  resp <- stringr::str_replace_all(cpf, '[^[:digit:]]','')
  resp<- dplyr::if_else(stringr::str_length(resp)<=11,
                        stringr::str_pad(resp, width = 11, side = 'left', pad = '0'),
                        stringr::str_pad(resp, width = 14, side = 'left', pad = '0'))
  return(resp)
}

#' Corrige numero de telefone
#' @description Recebe um vetor com numeros de telefone e tentar ajustar
#' para o padrao limpo aceitavel. Forma adaptada para rodar tambem em data.frame
#' e matriz
#' @param tel vetor de numeros de celular
#' @details o objeto tel tera qualquer digito diferente de zero removido.
#' verifica-se o tamenho do numero e adicona DDI;
#' @importFrom stringr str_length str_sub
#' @importFrom dplyr case_when
#' @export
rnp_limpa_tel <- function(tel){

  resp <- tel %>%
    stringr::str_replace_all('[^0-9]','') %>%
      as.numeric()

  fn_aux <- function(resp){

    ddi <- dplyr::case_when(stringr::str_length(resp) == 13 ~ resp %>% stringr::str_sub(end = 2),
                            stringr::str_length(resp) == 11 ~ '55',
                            stringr::str_length(resp) == 10 ~ '55',
                            T ~ '')
    ddd <- dplyr::case_when(stringr::str_length(resp) == 13 ~ resp %>% stringr::str_sub(start = 3, end = 4),
                            stringr::str_length(resp) == 12 ~ resp %>% stringr::str_sub(end = 2),
                            stringr::str_length(resp) == 11 ~ resp %>% stringr::str_sub(end = 2),
                            stringr::str_length(resp) == 10 ~ resp %>% stringr::str_sub(end = 2),
                            T ~ '')
    telfinal <- dplyr::case_when(stringr::str_length(resp) %in% 8:9 ~ resp %>% stringr::str_sub(start = 1),
                                 stringr::str_length(resp) == 10 ~ resp %>% stringr::str_sub(start = 3),
                                 stringr::str_length(resp) == 11 ~ resp %>% stringr::str_sub(start = 3),
                                 stringr::str_length(resp) == 12 ~ resp %>% stringr::str_sub(start = 4),
                                 stringr::str_length(resp) == 13 ~ resp %>% stringr::str_sub(start = 5),
                                 T ~ '')
    telfinal <- dplyr::case_when(stringr::str_length(telfinal) == 8 ~ paste('9', telfinal, sep = ''),
                                 T ~ telfinal)
    fim <- data.frame(ddi=ddi,ddd=ddd,tel=telfinal)
    return(fim)
  }

  out <- if(is.vector(resp)){
    fn_aux(resp)
  } else {
    lapply(resp, fn_aux)
  }
  return(out)
}

#' Corrige CEP
#' @description Recebe um vetor com numeros de CEP e tentar ajustar
#' para o padrao limpo aceitavel.
#' @param cep vetor de numeros de CEP
#' @details o objeto cep tera qualquer digito diferente de zero removido.
#' verifica-se o tamanho do numero e se for 7 adiciona zeros a direita,
#' se for igual a 7 adiciona a esquerda, se for igual a 8 retorna o mesmo cep;
#' @importFrom stringr str_replace_all str_trim str_pad
#' @export

rnp_limpa_cep <- function (cep){
  resp <- cep %>%
    stringr::str_replace_all("[^0-9]","") %>%
    stringr::str_trim(side = "both")
  tamanho <- stringr::str_length(resp)
  resp <- dplyr::case_when(tamanho <= 6 ~ stringr::str_pad(resp, width = 8, side = "right", pad = "0"),
                           tamanho == 7 ~ stringr::str_pad(resp, width = 8, side = "left", pad = "0"),
                           T ~ resp)
  return(resp)
}

#' Limpa tel boost
#' @param tel vetor de numeros de tel
#' @details o objeto cep tera qualquer digito diferente de zero removido.
#' @importFrom stringr str_replace_all str_length
#' @importFrom dplyr case_when
#' @export
rnp_limpa_tel_boost <- function(tel){
  resp <- tel %>%
    stringr::str_replace_all('[^[:digit:]]','')
  telfinal <- dplyr::case_when(stringr::str_length(resp) == 13 ~ resp,
                               stringr::str_length(resp) == 11 ~ resp,
                        T ~ '')
  return(telfinal)
}

#' Limpa CPF
#' @description Recebe um vetor de numeros ou textos de CPF's remove qualquer
#' carcatere que nao seja numero e completa com zeros as esquerda se tiver
#' menos de 11 digitos. Caso o valor informado tenha mais de 11 digitos
#' trata como cnpf e completa formando um numero com 14 digitos. Versao vetorizada.
#' Se for passado um data.frame, trata cada coluna como uma variavel de CPF e
#' trata.
#' @param base base com coluna de cpf
#' @param cpf objetos com os numeros de cpf
#' @export
rnp_verifica_cpf <- function(base, cpf) {
  base <- as.data.frame(base)
  base$cpf_mexido = gsub('[^[:digit:]]', '', base[,cpf, drop=T])
  base$len_orig = nchar(base[,cpf, drop=T])
  base$valido = valida_doc(str_pad(base[,'cpf_mexido', drop=T],width = 11,side = 'left',pad = '0'),type='cpf')
  base$len_mexido = nchar(base[,'cpf_mexido'])
  base$mudou = ifelse(base[,'len_orig'] != base[,'len_mexido'],1,0)

  agrupado <- table(nchar(base$cpf_mexido))
  agrupado <- as.data.frame.table(agrupado, stringsAsFactors = FALSE)

  o = data.frame(categorias = c(agrupado[,1], 'Acentos?' ,'valido'),
                 prop = c(agrupado[,2]/sum(agrupado[,2], na.rm=T),
                          sum(base$mudou,na.rm=T)/sum(agrupado[,2], na.rm=T),
                          sum(base$valido, na.rm=T)/sum(agrupado[,2], na.rm=T)),
                 num_abs = c(agrupado[,2], sum(base$mudou,na.rm=T), sum(base$valido, na.rm=T)),
                 stringsAsFactors = F)
  return(o)
}

#' Limpa CNPJ
#' @description Recebe um vetor de numeros ou textos de CPF's remove qualquer
#' caractere que nao seja numero e completa com zeros as esquerda. Completa o numero
#' formando um numero com 14 digitos. Versao vetorizada.
#' @param base base com coluna de cnpj
#' @param cnpj objetos com os numeros de cpf
#' @importFrom validaRA valida_doc
#' @importFrom stringr str_pad
#' @export
rnp_verifica_cnpj <- function(base, cnpj) {
  base <- as.data.frame(base)
  base$cnpj_mexido = gsub('[^[:digit:]]', '', base[,cnpj, drop=T])
  base$len_orig = nchar(base[,cnpj, drop=T])
  base$valido = validaRA::valida_doc(stringr::str_pad(base[,'cnpj_mexido', drop=T],width = 11,side = 'left',pad = '0'),type='cnpj')
  base$len_mexido = nchar(base[,'cnpj_mexido'])
  base$mudou = ifelse(base[,'len_orig'] != base[,'len_mexido'],1,0)

  agrupado <- table(nchar(base$cnpj_mexido))
  agrupado <- as.data.frame.table(agrupado, stringsAsFactors = FALSE)

  o = data.frame(categorias = c(agrupado[,1], 'Acentos?' ,'valido'),
                 prop = c(agrupado[,2]/sum(agrupado[,2], na.rm=T),
                          sum(base$mudou,na.rm=T)/sum(agrupado[,2], na.rm=T),
                          sum(base$valido, na.rm=T)/sum(agrupado[,2], na.rm=T)),
                 num_abs = c(agrupado[,2], sum(base$mudou,na.rm=T), sum(base$valido, na.rm=T)),
                 stringsAsFactors = F)
  return(o)
}

#' Limpa celular
#' @param base base de dados
#' @param telefone vetor de numeros de tel
#' @details o objeto cep tera qualquer digito diferente de zero removido.
#' @export
rnp_verifica_celular <- function(base, telefone) {
  base <- as.data.frame(base)
  base$telefone_mexido = gsub('[^[:digit:]]', '', base[,telefone, drop=T])
  base$len_orig = nchar(base[,telefone, drop=T])
  base$len_mexido = nchar(base[,'telefone_mexido'])
  base$mudou = ifelse(base[,'len_orig'] != base[,'len_mexido'],1,0)

  agrupado <- table(nchar(dados$telefone_mexido))
  agrupado <- as.data.frame.table(agrupado, stringsAsFactors = FALSE)

  o = data.frame(categorias = agrupado[,1],
                 prop = agrupado[,2]/sum(agrupado[,2], na.rm=T),
                 num_abs = agrupado[,2],
                 stringsAsFactors = F)
  return(o)
}

#' Corrige CEP
#' @description Recebe um vetor com numeros de CEP e tentar ajustar
#' para o padrao limpo aceitavel.
#' @param base base de dados
#' @param cep vetor de numeros de CEP
#' @details o objeto cep tera qualquer digito diferente de zero removido.
#' verifica-se o tamanho do numero e se for menor que oito adiciona zeros a esquerda;
#' @export
rnp_verifica_cep <- function(base, cep){

  base <- as.data.frame(base)
  base$cep_mexido = gsub('[^[:digit:]]', '', base[,cep, drop=T])
  base$len_orig = nchar(base[,cep, drop=T])
  base$len_mexido = nchar(base[,'cep_mexido'])
  base$mudou = ifelse(base[,'len_orig'] != base[,'len_mexido'],1,0)

  agrupado <- table(nchar(base$cep_mexido))
  agrupado <- as.data.frame.table(agrupado, stringsAsFactors = FALSE)

  o = data.frame(categorias = agrupado[,1],
                 prop = agrupado[,2]/sum(agrupado[,2], na.rm=T),
                 num_abs = agrupado[,2],
                 stringsAsFactors = F)

  return(o)
}

#' VDI - Variable Deviation Index
#'
#' @description Serve para calcular o indice de desvio entre duas frequencias, sendo a referencia ou esperada versus a recente ou atual
#' Esta estatistica possui otimas propriedades que ajudam a identificar se esta havendo mudanca de publico olhando
#' para os mesmos dados antes e depois ao longo do tempo.
#'
#' @param tbref tabela de frequencias da base de referencia (esperado) calculada com a funcao rnp_freq()
#' @param tbrec tabela de frequencias da base de recente (atual) calculada com a funcao rnp_freq()
#' @param max_cat maximo de categorias devolvidas
#' @param digits digitos para arredondar
#' @param total se TRUE retorna o VDI somado, senao exibe por cateoria
#' @param sortclass se TRUE, ordena as categorias
#' @return se total == TRUE retorn data.frame com percentuais de cada classe da variavel mais estatistica VDI e inferencia do efeito.
#' Caso contrario, apenas um data.frame com estatistica VDI e inferencia do efeito.
#' @import dplyr
#' @author LOPES, J. E.
#' @examples
#' tbref <- rnp_freq(sample(mtcars$cyl, size = 20), digits = 6)
#' tbrec <- rnp_freq(sample(mtcars$cyl, size = 20), digits = 6)
#' rnp_vdi(tbref, tbrec, total = TRUE)
#' rnp_vdi(tbref, tbrec, total = FALSE)
#' @export
rnp_vdi <- function(tbref, tbrec, max_cat = 50, digits = 4, total = FALSE, sortclass = TRUE) {
  if(missing(tbref) | missing(tbrec)){
    stop(cat("Pelo menos uma tabela de frequencias esta ausente. Verifique se alguma variavel nao tem dados em P1 ou P2\n"))
  }
  tb <- tbref %>%
    dplyr::transmute(classe = as.character(classe),
                     fa_esperado = dplyr::if_else(fa %in% c(NA, Inf, 0), 0, fa),
                     fr_esperado = dplyr::if_else(fr %in% c(NA, Inf, 0), 0, fr)) %>%
    dplyr::arrange(classe) %>%
    dplyr::full_join(
      tbrec %>%  dplyr::transmute(classe = as.character(classe),
                                  fa_atual = dplyr::if_else(fa %in% c(NA, Inf, 0), 0, fa),
                                  fr_atual = dplyr::if_else(fr %in% c(NA, Inf, 0), 0, fr)),
      by = c("classe")
    ) %>%
    dplyr::mutate(vdi = round(dplyr::if_else(fr_esperado %in% c(NA, Inf, 0),
                                             0, (fr_atual-fr_esperado)*log(fr_atual/fr_esperado)), digits = digits),
                  inferencia = dplyr::if_else(vdi <= 0.05 | vdi %in% c(NA, Inf, 0), 'G.Tudo bem',
                                              dplyr::if_else(vdi <= 0.15, 'A.Atencao', 'R.Perigo'))) %>%
    dplyr::mutate_at(dplyr::vars(fa_esperado:vdi), ~replace(., is.na(.) | is.infinite(.), 0))

  out <- if(total) {
    tb %>%
      dplyr::transmute(vdi = sum(vdi, na.rm = TRUE),
                       inferencia = dplyr::if_else(vdi <= 0.05 | vdi %in% c(NA, Inf, 0), 'G.Tudo bem',
                                                   dplyr::if_else(vdi <= 0.15, 'A.Atencao', 'R.Perigo'))) %>%
      unique()
  } else {
    if(sortclass){
      if(length(tb$vdi) > max_cat){
        tb <- tb %>%
          mutate(dif = abs(fr_atual-fr_esperado),
                 metrica = abs(fr_atual-fr_esperado)/(fr_atual-fr_esperado)/2) %>%
          dplyr::top_n(max_cat, metrica)
      }
        tb %>%
          dplyr::arrange(classe)
      }
  }
  return(out)
}

#' VDI - Variable Deviation Index (Ponderado)
#'
#' @description Serve para calcular o indice de desvio entre duas frequencias, sendo a referencia ou esperada versus a recente ou atual
#' Esta estatistica possui otimas propriedades que ajudam a identificar se esta havendo mudanca de publico olhando
#' para os mesmos dados antes e depois ao longo do tempo.
#'
#' @param tbref tabela de frequencias da base de referencia (esperado) calculada com a funcao rnp_freq()
#' @param tbrec tabela de frequencias da base de recente (atual) calculada com a funcao rnp_freq()
#' @param max_cat maximo de categorias devolvidas
#' @param digits digitos para arredondar
#' @param ponderado se TRUE, pondera o VDI pelos pesoas das categorias.
#' @param total se TRUE retorna o VDI somado, senao exibe por cateoria
#' @param sortclass se TRUE, ordena as categorias
#' @return se total == TRUE retrna data.frame com percentuais de cada classe da variavel mais estatistica VDI e inferencia do efeito.
#' Caso contrario, apenas um data.frame com estatistica VDI e inferencia do efeito.
#' @import dplyr
#' @author LOPES, J. E.
#' @examples
#' tbref <- rnp_freq(sample(mtcars$cyl, size = 20), digits = 6)
#' tbrec <- rnp_freq(sample(mtcars$cyl, size = 20), digits = 6)
#' rnp_vdi2(tbref, tbrec, total = TRUE)
#' rnp_vdi2(tbref, tbrec, total = FALSE)
#' @export
rnp_vdi2 <- function(tbref, tbrec, max_cat = 50, digits = 4, ponderado = FALSE, total = FALSE, sortclass = TRUE) {
  if(missing(tbref) | missing(tbrec)){
    stop(cat("Pelo menos uma tabela de frequencoas esta ausente. Verifique se alguma variavel nao tem dados em P1 ou P2\n"))
  }
  tb <- tbref %>%
    dplyr::transmute(classe = as.character(classe),
                     fa_esperado = dplyr::if_else(fa %in% c(NA, Inf, 0), 0, fa),
                     fr_esperado = dplyr::if_else(fr %in% c(NA, Inf, 0), 0, fr)) %>%
    dplyr::arrange(classe) %>%
    dplyr::full_join(
      tbrec %>%  dplyr::transmute(classe = as.character(classe),
                                  fa_atual = dplyr::if_else(fa %in% c(NA, Inf, 0), 0, fa),
                                  fr_atual = dplyr::if_else(fr %in% c(NA, Inf, 0), 0, fr)),
      by = c("classe")
    )  %>%
    tidyr::replace_na(list(fa_esperado = 0, fr_esperado = 0, fa_atual = 0, fr_atual = 0)) %>%
    dplyr::mutate(vdi = round(dplyr::if_else(fr_esperado %in% c(NA, Inf, 0),
                                      0, (fr_atual-fr_esperado)*log(fr_atual/fr_esperado)), digits = digits),
                  inferencia = dplyr::if_else(vdi <= 0.05 | vdi %in% c(NA, Inf, 0), 'G.Tudo bem',
                                       dplyr::if_else(vdi <= 0.15, 'A.Atencao', 'R.Perigo')),
                  fr_conjunta = round((fa_esperado + fa_atual) / sum(fa_esperado + fa_atual), digits = digits),
                  vdip = round((-1/log10(fr_conjunta))*vdi, digits = digits),
                  inferencia2 = dplyr::if_else(vdip <= 0.05 | vdip %in% c(NA, Inf, 0), 'G.Tudo bem',
                                        dplyr::if_else(vdip <= 0.15, 'A.Atencao', 'R.Perigo')),
                  shiftprop = round(dplyr::if_else(fr_esperado > 0, (fr_atual / fr_esperado)-1, fr_atual - fr_esperado), digits = digits)
                  ) %>%
    dplyr::mutate_at(dplyr::vars(fa_esperado:vdip), ~replace(., is.na(.) | is.infinite(.), 0))

  out <- if(total) {
    if(ponderado) {
      tb %>% dplyr::transmute(vdi = sum(vdip, na.rm = TRUE),
                              inferencia = dplyr::if_else(vdi <= 0.05 | vdi %in% c(NA, Inf, 0), 'G.Tudo bem',
                                                          dplyr::if_else(vdi <= 0.15, 'A.Atencao', 'R.Perigo'))) %>%
        unique()
    } else {
      tb %>%
        dplyr::transmute(vdi = sum(vdi, na.rm = TRUE),
                         inferencia = dplyr::if_else(vdi <= 0.05 | vdi %in% c(NA, Inf, 0), 'G.Tudo bem',
                                                     dplyr::if_else(vdi <= 0.15, 'A.Atencao', 'R.Perigo'))) %>%
        unique()
    }
  } else {
    if(sortclass){
      if(length(tb$vdi) > max_cat){
        tb <- tb %>%
          dplyr::mutate(dif = abs(fr_atual-fr_esperado),
                        metrica = abs(fr_atual-fr_esperado)/(fr_atual-fr_esperado)/2) %>%
          dplyr::arrange(classe) %>%
          dplyr::top_n(max_cat, metrica)
      }
      tb %>%
        dplyr::arrange(classe)
    }
  }
  return(out)
}


#' KSP - KS de Proximidade
#'
#' @description Serve para calcular o indice de desvio entre duas frequencias, sendo a referencia ou esperada versus a recente ou atual
#' Esta estatistica e calculada com base no KS como conhecemos porem, adaptado para o caso em que
#' queremos analisar a distancia entre duas frequencias acumuladas e ajuda a identificar se esta havendo mudanca de publico olhando
#' para os mesmos dados antes e depois ao longo do tempo. A interpretacao e oposta ao KS de modelagem, ou seja, quanto menor-melhor.
#'
#' Calculamos o KSP como $max(abs(freq_relativa_ac_p1 - freq_relativa_ac_p2))$
#'
#' @param tbref tabela de frequencias da base de referencia (esperado) calculada com a funcao rnp_freq()
#' @param tbrec tabela de frequencias da base de recente (atual) calculada com a funcao rnp_freq()
#' @param max_cat maximo de categorias devolvidas
#' @param digits digitos para arredondar
#' @param total se TRUE retorna o KSP somado, se nao, exibe por cateoria
#' @param sortclass se TRUE, ordena as categorias
#' @return se total == TRUE retorn data.frame com percentuais de cada classe da variavel mais estatistica VDI e inferencia do efeito.
#' Caso contrario, apenas um data.frame com estatistica VDI e inferencia do efeito.
#' @import dplyr
#' @author LOPES, J. E.
#' @examples
#' tbref <- rnp_freq(sample(mtcars$cyl, size = 20), digits = 6)
#' tbrec <- rnp_freq(sample(mtcars$cyl, size = 20), digits = 6)
#' rnp_ksp(tbref, tbrec, total = TRUE)
#' rnp_ksp(tbref, tbrec, total = FALSE)
#' @export
rnp_ksp <- function(tbref, tbrec,  max_cat = 50, digits = 4, total = FALSE, sortclass = TRUE) {
  if(missing(tbref) || missing(tbrec)){
    stop(cat("Pelo menos uma tabela de frequencoas esta ausente. Verifique se alguma variavel nao tem dados em P1 ou P2\n"))
  }
  tb <- tbref %>%
    dplyr::transmute(classe = as.character(classe),
                     fa_esperado = dplyr::if_else(fa %in% c(NA, Inf, 0), 0, fa),
                     fr_esperado = dplyr::if_else(fr %in% c(NA, Inf, 0), 0, fr)) %>%
    dplyr::arrange(classe) %>%
    dplyr::full_join(
      tbrec %>%
        dplyr::transmute(classe = as.character(classe),
                                  fa_atual = dplyr::if_else(fa %in% c(NA, Inf, 0), 0, fa),
                                  fr_atual = dplyr::if_else(fr %in% c(NA, Inf, 0), 0, fr)) %>%
        dplyr::arrange(classe),
      by = c("classe")
    ) %>%
    tidyr::replace_na(list(fa_esperado = 0, fr_esperado = 0, fa_atual = 0, fr_atual = 0)) %>%
    dplyr::arrange(classe) %>%
    dplyr::mutate(kspe = purrr::accumulate(fr_esperado, sum),
                  kspa = purrr::accumulate(fr_atual, sum),
                  ksp = round(abs(kspe-kspa), digits = digits),
                  inferencia = dplyr::if_else(ksp <= 0.05 | ksp %in% c(NA, Inf, 0), 'G.Tudo bem',
                                              dplyr::if_else(ksp <= 0.15, 'A.Atencao', 'R.Perigo'))
                  )
    out <- if(total) {
    tb %>%
        dplyr::transmute(ksp = max(ksp, na.rm = TRUE),
                         inferencia = dplyr::if_else(ksp <= 0.05 | ksp %in% c(NA, Inf, 0), 'G.Tudo bem',
                                                     dplyr::if_else(ksp <= 0.15, 'A.Atencao', 'R.Perigo'))) %>%
      unique()
  } else {
    if(sortclass){
      if(length(tb$ksp) > 10){
        tb <- tb %>%
          dplyr::mutate(dif = max(ksp, na.rm = TRUE)) %>%
          dplyr::top_n(max_cat, ksp)
      }
      tb %>%
        dplyr::arrange(classe)
    }
  }
  return(out)
}

#' Analise descritiva
#' @description Calcula estatisticas descritivas para variaveis numericas.
#' @param x vetor numerico
#' @param digits cadas decimais no arredondamento
#' @export
rnp_descritiva <- function(x, digits = 4){
  qq <- unname(quantile(x, na.rm = TRUE))
  o <- c(minimo  = qq[1],
         q25     = qq[2],
         mediana = qq[3],
         media   = mean(x, na.rm = TRUE),
         q75     = qq[4],
         maximo  = qq[5],
         devpad  = sd(x, na.rm = TRUE),
         cv      = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
         iqr     = stats::IQR(x)
  )
  return(round(o, digits = digits))
}

#' Resumo de dados
#' @description
#' Recebe um objeto qualquer e retorna um resumo dos dados dentro deste objeto
#' @details
#' O resumo contém dados descritivos, classes, tipos, numero de missing, etc.
#' @param x vetor, data.frame ou lista de data.frames
#' @export
rnp_resumo <- function(x){
  if(inherits(x, "array")){
    x <- as.data.frame(x)
  }
  aux <- function(i){
    p1 <- p2 <- data.frame()
    if(inherits(i, c("numeric","integer","Date","POSIXct","POSIXt"))){
      p1 <- data.frame(
        classe = class(i),
        tipo = typeof(i),
        nobs = length(i),
        ndis = length(unique(i)),
        nmis = sum(is.na(i)),
        t(rnp_descritiva(i))
      )
    } else {
      p2 <- data.frame(
        classe = class(i),
        tipo = typeof(i),
        nobs = length(i),
        ndis = length(unique(i)),
        nmis = sum(is.na(i))
      )
    }
    dplyr::bind_rows(p1, p2)
  }
  if(!is.null(dim(x))){
    data.frame(var = names(x), purrr::map_df(x, aux))
  } else {
    if(inherits(x, c("list"))){
      data.frame(var = names(x), purrr::map_df(x, aux))
    } else {
      aux(x)
    }
  }
}

#' Estatisticas descritivas mais completas
#' @description Calcula estatisticas descritivas e intervalo de confianca para a media
#' @param x vetor numerico
#' @param digits cadas decimais no arredondamento
#' @param conf.level confianca para o intervalo
#' @export
rnp_stats <- function(x, digits = 4, conf.level = 0.95) {

  tt <- try_error(t.test(x, conf.level = conf.level))
  if(!inherits(tt, "try-error")) {
    ici <- tt$conf.int[1]
    ics <- tt$conf.int[2]
  } else {
    ici <- ics <- 0
  }

  o <- c(N = length(x),
         ICi = ici,
         media = mean(x, na.rm = TRUE),
         mediana = median(x, na.rm = TRUE),
         ICs = ics,
         desvpad = sd(x, na.rm = TRUE),
         pctmaiormediana = sum(na.exclude(x) > median(x, na.rm = TRUE)) / length(x),
         pctmaiormedia = sum(na.exclude(x) > mean(x, na.rm = TRUE))/ length(x),
         pctmaior1sd = sum(na.exclude(x) >= (mean(x, na.rm = TRUE) + 1*sd(x, na.rm = TRUE))) / length(x),
         pctmaior2sd = sum(na.exclude(x) >= (mean(x, na.rm = TRUE) + 2*sd(x, na.rm = TRUE))) / length(x),
         pctnulos = sum(is.na(x))/length(x)
  )

  o <- round(o, digits = digits)
  return(o)
}

#' Shift
#' @description Calcula o shift ou mudanca e pode ser aplicado a qualquer par de vetores.
#' @param x vetor x
#' @param y vetor y
#' @param digits total de digitos do arredondamento
#' @export
rnp_shift2 <- function(x, y, digits = 4) {
  psi <- ifelse(!y %in% c(NA, 0, NULL), (x - y) * log(x / y), 0)
  sft <- (x/y)-1
  oo  <- round(data.frame(shift = sft, psi = psi), digits = digits)
  return(oo)
}


#' Tamanho da amostra por proporcao
#'
#' @description
#' Calcula tamanho da amostra finita ou infinita baseado em proporcao
#' N = Tamanho do universo finita
#' Z = E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado.
#' Em funcao do nivel de confianca que buscamos, usaremos um valor determinado que e dado
#' pela forma da distribuicao de Normal. Os valores mais frequentes sao:
#' Nivel de confianca 90\% -> Z=1,645
#' Nivel de confianca 95\% -> Z=1,96
#' Nivel de confianca 99\% -> Z=2,575
#' e = E a margem de erro maximo que eu quero admitir (p.e. 5\%)
#' p = E a proporcao que esperamos encontrar. Este parametro tende confundir
#' bastante a primeira vista: Como vou saber qual proporcao espero,
#' se justamente estamos fazendo uma pesquisa para conhecer esta proporcao?
#' @param N Valor do tamanho da base de dados
#' @param p E a proporcao que esperamos encontrar. ex 50\% = 1/2 (padrao)
#' @param z E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado. (ex. 25\%)
#' @param e E a margem de erro maximo que eu quero admitir (p.e. 5\%)
#' @param infinito se TRUE assume que a populacao e infinita e nao aplica fator de correcao
#' @return vetor com tamanho da amostra n
#' @author LOPES, J. L
#' @export
rnp_tamamostra_prop <- function(N, p = NULL, z = NULL, e = NULL, infinito = FALSE) {
  if(is.null(N)) infinito <- TRUE
  if(is.null(z)) {
    z <- qnorm(p = 0.975, mean = 0, sd = 1)
  } else {
    z <- qnorm(p = z + (1-z)/2, mean = 0, sd = 1)
  }
  if(is.null(e)) e <- 0.05
  if(is.null(p)) p <- 1/2

  if(infinito) {
    n <- ceiling((z^2*(p*(1-p)))/e^2)
  } else {
    divid <- N*z^2*(p*(1-p))
    divis <- (N-1)*e^2 + z^2*(p*(1-p))
    n <- ceiling(divid/divis)
  }
  return(n)
}

#' Tamanho da amostra para proporcao
#'
#' @description
#' Calcula tamanho da amostra ideal para estimar um parametrp de proporcao.
#' Considera caso em que a populacao original e finita ou infinita.
#' N = Tamanho do universo finita
#' Z = E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado.
#' Em funcao do nivel de confianca que buscamos, usaremos um valor determinado que e dado
#' pela forma da distribuicao de Normal. Os valores mais frequentes sao:
#' Nivel de confianca 90\% -> Z=1,645
#' Nivel de confianca 95\% -> Z=1,96
#' Nivel de confianca 99\% -> Z=2,575
#' e = E a margem de erro maximo que eu quero admitir (p.e. 5\%)
#' p = E a proporcao que esperamos encontrar. Este parametro tende confundir
#' bastante a primeira vista: Como vou saber qual proporcao espero,
#' se justamente estamos fazendo uma pesquisa para conhecer esta proporcao?
#' @param N Valor do tamanho da base de dados
#' @param p E a proporcao que esperamos encontrar. ex 50\% = 1/2 (padrao)
#' @param z E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado. (ex. 25\%)
#' @param e E a margem de erro maximo que eu quero admitir (p.e. 5\%)
#' @param infinito se TRUE assume que a populacao e infinita e nao aplica fator de correcao
#' @return vetor com tamanho da amostra n
#' @author LOPES, J. L
#' @export
rnp_nsample_prop <- function(N, p = NULL, z = NULL, e = NULL, infinito = FALSE) {
  if(is.null(N)) infinito <- TRUE
  if(is.null(z)) {
    z <- qnorm(p = 0.975, mean = 0, sd = 1)
  } else {
    z <- qnorm(p = z + (1-z)/2, mean = 0, sd = 1)
  }
  if(is.null(e)) e <- 0.025
  if(is.null(p)) p <- 1/2

  if(infinito) {
    n <- ceiling((z^2*(p*(1-p)))/e^2)
  } else {
    divid <- N*z^2*(p*(1-p))
    divis <- (N-1)*e^2 + z^2*(p*(1-p))
    n <- ceiling(divid/divis)
  }
  return(n)
}

#' Tamanho da amostra para media
#'
#' @description
#' Calcula tamanho da amostra ideal para estimar um parametrp de media
#' Considera caso em que a populacao original e finita ou infinita.
#' N = Tamanho do universo finita
#' Z = E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado.
#' Em funcao do nivel de confianca que buscamos, usaremos um valor determinado que e dado
#' pela forma da distribuicao de Normal. Os valores mais frequentes sao:
#' Nivel de confianca 90\% -> Z=1,645
#' Nivel de confianca 95\% -> Z=1,96
#' Nivel de confianca 99\% -> Z=2,575
#' e = E a margem de erro maximo que eu quero admitir (p.e. 5\%)
#' s = E o desvio padrao amostral da base variavel de interesse na base original. Se nao puder obter da base total gerar de uma amostra. Se ainda nao
#' tiver, faremo como raiz quadrada de N
#' @param N Valor do tamanho da base de dados original
#' @param s E desvio padrao da variavel de interesse, ex. score medio.
#' @param z E o desvio do valor medio que aceitamos para alcancar o nivel de confianca desejado. (ex. 25\%)
#' @param e E a margem de erro maximo que eu quero admitir (p.e. 5\%)
#' @param infinito se TRUE assume que a populacao e infinita e nao aplica fator de correcao
#' @return vetor com tamanho da amostra n
#' @author LOPES, J. L
#' @export
rnp_nsample_media <- function(N, s = NULL, z = NULL, e = NULL, infinito = FALSE) {
  if(is.null(N)) infinito <- TRUE
  if(is.null(z)) {
    z <- qnorm(p = 0.975, mean = 0, sd = 1)
  } else {
    z <- qnorm(p = z + (1-z)/2, mean = 0, sd = 1)
  }
  if(is.null(e)) e <- 0.025
  if(is.null(s)) s <- 3.322*log(N)+1

  n_ <- ceiling((z*s/e)^2)

  n <- if(infinito) {
    n_
  } else {
    ceiling((N*n_)/(N+n_))
  }
  return(n)
}


#' Calcula shift do coeficiente de variacao
#'
#' @description
#' Dados dois vetores numericos com dados de dois periodos, p1 e p2 fazemos o coeficiente de variacao (cv)
#' de cada vetor e cria uma estatistica de shift entre os dois cv's.
#' O cv de cada daso e dado por cvp1 = sd(p1) / mean(p1) e cvp1 = sd(p2) / mean(p2). O shift e dado por
#' shift = (cvp2-cvp1) / cvp1. Se shift <= 0.01, alerta = 0, se > 0.05 e <= 0.10, alerta = 1,
#' se > 0.10 e <= 0.15, alerta = 2 e segue ate o 4.
#' Testar se alertas a partir de 1 fazem sentido. Esta regua pode ser calibrada.
#' @param p1 Vetor de dados do periodo 1
#' @param p2 Vetor de dados do periodo 2
#' @param digits Arredondamento do numero
#' @param gatilho Se preferiri, passar manualmente qual o gatilho para dizer se um shift e alerta. ex. 0.22
#' @author LOPES, J. L.
#' @export
rnp_shift_cv <- function(p1, p2, digits = 4, gatilho = NULL) {
  if(missing(p1) | missing(p2)) {
    stop("Informe vetor de dados do periodo 1 e do periodo 2")
  }
  x <- as.numeric(p1)
  y <- as.numeric(p2)

  sx <- sd(x, na.rm = TRUE) +.Machine$double.eps
  mx <- mean(x, na.rm = TRUE)

  sy <- sd(y, na.rm = TRUE) +.Machine$double.eps
  my <- mean(y, na.rm = TRUE)

  cvx <- ifelse(!mx %in% c(0, -Inf, Inf, NA), sx / mx, 0)
  cvy <- ifelse(!my %in% c(0, -Inf, Inf, NA), sy / my, 0)

  sft <- ifelse(!cvx %in% c(0, -Inf, Inf, NA), (cvy-cvx) / cvx, 0)

  fla <- if(is.null(gatilho)){
    ifelse(abs(sft) <= 0.05, 0, ifelse(abs(sft) <= 0.10, 1, ifelse(abs(sft) <= 0.15, 2, ifelse(abs(sft) <= 0.20, 3, 4))))
  } else {
    ifelse(abs(sft) <= gatilho, 0, 1)
  }

  return(round(c(cvp1 = cvx, cvp2 = cvy, cvshift = sft, alerta = fla), digits = digits))

}

#' Calcula medias de varios tipos
#'
#' @description Esta funcao e uma chamada master paras as funcoes \code{\link{media_aritmetica}},
#' \code{\link{media_geometrica}} e \code{\link{media_harmonica}}. Serve para calcular médias
#' de vetores numéricos e de matrizes/data.frames formados por colunas numericas.
#' @details Atraves desta funcao e possivel calcular medias aritmeticas, geometricas e harmonicas
#' simples e ponderadas. Se for passado uma matriz ela varre tdoas as colunas e determina as medias
#' para cada variavel. Se for passado argumento peso, as medias sao ponderadas pelos seus respectivos
#' pesos. No caso de matrizes, a matriz de peso precisa ter as mesmas dimensoes da matriz de origem
#' e o mesmo tipo de dados.
#' Quando existir peso, o nome das colunas do data.frame serao nomeadas como: media_ap, media_gp e media_hp.
#' Caso contrario, media_a, media_g e media_h.
#' @param x Vetor, matrix ou data.frame de dados numericos
#' @param peso Opcional. Vetor, matrix ou data.frame de dados numericos com pesos com as mesmas
#' caracteristcias de comprimento ou dimensoes do argumento x
#' @param remove.na Se TRUE remove os NA's, caso existam nos dados
#' @return Vetor ou data.frame com as medias de uma ou mais variaveis. No caso de data.frames, a
#' saida e um data.frame com quatro colunas, sendo uma o nome das variaveis e as outras tres as
#' colunas de medias aritmetica, geometrica e harmonica respectivamente. No caso de vetor numerico,
#' a saida e um data.frame simples com tres colunas e uma linha contendo as medias.
#' @author LOPES, J. E.
#' @examples
#' require(rnpextras)
#' # Exemplos com vetores
#' x <- mtcars$mpg;x[3:5] <- NA
#' p <- mtcars$cyl;p[5:7] <- NA
#' rnp_media(x = x, peso = NULL, remove.na = TRUE)
#' rnp_media(x = x, peso = NULL, remove.na = FALSE)
#' rnp_media(x = x, peso = p, remove.na = TRUE)
#' rnp_media(x = x, peso = p, remove.na = FALSE)
#' # Exemplos com matrizes (de mesmo tipo)
#' X <- mtcars
#' Y <- data.frame(matrix(runif(prod(dim(mtcars)), 5, 10), nrow = nrow(mtcars), ncol = ncol(mtcars)));
#' class(X); class(Y)
#' names(Y) <- names(X)
#' dim(X) == dim(Y)
#' rnp_media(X)
#' rnp_media(X, Y)
#' # Com dados reais da base rnpextras::dm_ies
#' X <- as.data.frame(rnpextras::dm_ies)
#' X <- X[,c(sapply(X, class) %in% c("integer", "numeric"))]
#' rnp_media(X[,-c(1:6)])
#' a <- sapply(X[,-c(1:6)], mean)
#' b <- rnp_media(X[,-c(1:6)])$media_a
#' all(a == b)
#' @export
rnp_media <- function(x, peso = NULL, remove.na = TRUE){

  aux <- function(x, peso, remove.na) {
    c(media_aritmetica(x, peso, remove.na),
      media_geometrica(x, peso, remove.na),
      media_harmonica(x, peso, remove.na))
  }
  out <- if(is.null(peso)){
    if(is.null(dim(x))){
      o <- data.frame(rbind(aux(x = x, peso = peso, remove.na = remove.na)))
      names(o) <- c("media_a","media_g","media_h")
      o
    } else {
      o <- sapply(x, function(i){
        rbind(aux(x = i, peso = peso, remove.na = remove.na))
      })
      o <- data.frame(variavel = row.names(t(o)), as.data.frame.matrix(t(o)), stringsAsFactors = FALSE)
      names(o)[-1] <- c("media_a","media_g","media_h")
      rownames(o) <- NULL
      o
    }
  } else {
    if(is.null(dim(x)) && is.null(dim(peso))){
      o <- data.frame(rbind(aux(x = x, peso = peso, remove.na = remove.na)))
      names(o) <- c("media_ap","media_gp","media_hp")
      o
    } else if(all(dim(x) == dim(peso))){
      x <- data.frame(x, stringsAsFactors = FALSE)
      peso <- data.frame(peso, stringsAsFactors = FALSE)
      o <- sapply(seq_along(names(x)), function(i){
        rbind(aux(x = x[,i], peso = peso[,i], remove.na = remove.na))
      })
      o <- data.frame(variavel = row.names(t(x)), as.data.frame.matrix(t(o)), stringsAsFactors = FALSE)
      names(o)[-1] <- c("media_ap","media_gp","media_hp")
      rownames(o) <- NULL
      o
    } else {
      stop(cat("No caso de media ponderada, para matriz ou data.frame, os dois objetos precisam ter os mesmo nomes para cada variavel x e para cada peso. Verifique seus dados!\n"))
    }
  }
  return(out)
}


#' Converte e categoriza numerico
#' @description Verifica se uma variavel tem muitas classes, converte para numerica e categoriza
#' @param x vetor de valores
#' @param min_categ minimo de categorias para que a variavel seja recategorizada
#' @param nfaixas numero de faixas desejadas
#' @param coarse_class se TRUE tenta converter x para numerico antes de recategorizar.
#' @importFrom Hmisc cut2
#' @export
rnp_check_class_vars <- function(x, min_categ = 50, nfaixas = 20, coarse_class = TRUE){
  x <- if(length(table(x)) > min_categ){
    if (coarse_class & is.factor(x)) {
      as.numeric(as.character(x))
    } else {
      as.numeric(x)
    }
  } else {
    x
  }

  o <- if(class(x) %in% c("numeric","integer")) {
    as.character(Hmisc::cut2(x, g = nfaixas, digits = 4))
  } else {
    x
  }
  return(o)
}

#' Corrige UTF-8 encoding
#' @description Recebe um vetor string de tipos em formado UTF-8 e tentar corrigir para latin-1
#' desta forma, conservando letras como c cidilhado e acentos. Outros formatos de encoding de
#' saida sao suportados.
#' @seealso \code{\link{Encoding}}
#' @param x vetor string
#' @param enc flag destino do encoding.
#' @export
rnp_try_fix_encoding <- function(x, enc = "latin1"){
  if(inherits(x, c("character","factor"))){
    x <- as.character(x)
    y <- iconv(x = enc2utf8(x), from = "UTF-8", to = enc)
    y <- iconv(x = y, from = "UTF-8", to = enc)

  }else{
    y <- x
  }
  return(y)
}

#' Gerador de senha forte (ou absurdas)
#'
#' @details Dado o total de senhas e o tamanho delas, o programa calcula aleatoreamente
#' as senhas com as seguintes regras:
#' 1. Tem letras maiusculas
#' 2. Tem letras minusculas
#' 3. Tem numeros de 0 a 9
#' 4. Tem caracteres especiais ex:  "#","$","*",".","!","?"
#' @param nsenhas total de senhas. Se colocar 20 saem vinte senhas
#' @param tamanho tamanho da senha em caracteres
#' @param seed numerico para pode repetir as senhas geradas antes. Se nao for informado usara smotragem por decimal uniforme entre 0 e 1
#' @param md5 Padrao TRUE, Devolve a senha tambem em MD5. Se FALSE devolve apenas as senhas
#' @return Vetor ou data.frame com as senhas
#' @examples
#' # 10 senhas tamanho 8 com senha de seed de rastreamento 1234 mais MD5
#' gerador_de_senha(10, 8, 1234, TRUE)
#' # O mesmo sem seed
#' gerador_de_senha(10, 8)
#' # Agora sem seed nem md5 e com tamanho 12
#' gerador_de_senha(10, 12, NULL, FALSE)
#' @export
gerador_de_senha <- function(nsenhas = 1, tamanho = 16, seed = NULL, md5 = TRUE){
  if(tamanho > 37) {
    stop("Pra que voce quer uma senha tao grande? O maximo e 37 caracteres!\n")
  } else if(nsenhas > 50000){
    cat("Quer mesmo gerar mais de 50000 senhas? Sugiro gerar em lotes menores. Vou rodar, mas o processo pode ficar lento.\n")
  }

  {
    if(is.null(seed)){
      set.seed(ceiling(runif(1, 0, 1) * 10^5))
    } else {
      set.seed(seed)
    }
    o <- sapply(seq_len(nsenhas), function(i, ...){
      paste0(sample(x = c(sample(LETTERS, 13),
                        sample(letters, 13),
                        sample(0:9, 5),
                        sample(c("#","$","@","*",".","!","?","[","]","{","}","(",")"), 6)),
                  tamanho,
                  replace = FALSE,
                  prob = c(rep(0.2/13,13), rep(0.3/13,13), rep(0.3/5,5), rep(0.2/6,6))), collapse = "")
    })

    if(md5) {
    o <- data.frame(senha = o,
                    senha_md5 = sapply(o, function(i) {digest::digest(i, "md5")}))
    rownames(o) <- NULL
    }
  }
  return(o)
}

#' Salvar data frame como json
#' @description Recebe um dataframe transforma em json e salva no caminho especiicado
#' @param x data.frame
#' @param file destino do arquivo, incluindo extensao .json
#' @param df_type salvar no formato linha, coluna ou valor
#' @param raw_type tipo dos dados brutos de saida
#' @param POSIXt formato de datetime
#' @param factor tratar fator como string?
#' @param trace se TRUE, exibe estatisticas do arquivo salvo
#' @importFrom jsonlite toJSON
#' @importFrom readr write_lines
#' @importFrom dplyr mutate
#' @export
converte_to_json <- function(x, file, df_type = "rows", raw_type = "mongo", POSIXt = "mongo", factor =  'string', trace = FALSE){
  out <- x %>%
    jsonlite::toJSON(dataframe = df_type, raw = raw_type, POSIXt = POSIXt, factor = factor) %>%
    readr::write_lines(file)
  if(trace) {
    lo <- file.info(file) %>%
      dplyr::mutate(arquivo = rownames(.), size_mb = size / (1024*1024))
    print(file.info(file))
  }
  return(invisible(out))
}


#' Dia da semana
#' @description Funcao para determinar o dia da semana e separar os periodos de extracao de dados
#' @param x vetor de datas
#' @param en se quer ver em ingles
#' @export
rnp_check_day <- function(x, en = TRUE){
  o <- iconv(tolower(weekdays(as.Date(as.character(x)), abbreviate = TRUE)), to='ASCII//TRANSLIT')
  o <- if(en) {
    dplyr::case_when(o %in% c("sun","dom") ~ "sun",
                     o %in% c("sat","sab") ~ "sat",
                     o %in% c("mon","seg") ~ "mon",
                     o %in% c("tue","ter") ~ "tur",
                     o %in% c("wed","qua") ~ "wed",
                     o %in% c("thu","qui") ~ "thu",
                     o %in% c("fri","sex") ~ "fri", TRUE ~ "nan")
  } else {
    dplyr::case_when(o %in% c("sun","dom") ~ "dom",
                     o %in% c("sat","sab") ~ "sab",
                     o %in% c("mon","seg") ~ "seg",
                     o %in% c("tue","ter") ~ "ter",
                     o %in% c("wed","qua") ~ "qua",
                     o %in% c("thu","qui") ~ "qui",
                     o %in% c("fri","sex") ~ "sex", TRUE ~ "nan")
  }
  return(o)
}


#' Estatisticas de associacao
#' @description Funcao para calcular as estatisticas, Qui-Quadrado, V de Cramer e C de contingencia para tabelas de dupla entrada.
#' @param x variavel 1
#' @param y variavel 1
#' @export
rnp_associacao <- function (x, y) {
  oo <- table(x, y)
  ch <- chisq.test(oo)
  rr <- sum(ch$expected) * (min(dim(ch$expected)) - 1)
  vv <- sqrt(ch$statistic/rr)
  cc <- sqrt((min(dim(ch$observed)))/(min(dim(ch$observed)) -
                                        1)) * sqrt(ch$statistic/(ch$statistic + sum(ch$observed)))
  out <- unname(c(ch$statistic, vv, cc))
  names(out) <- c("Qui-quadrado", "V-Cramer", "C-Contingencia")
  return(out)
}


#' Tabelao score
#' @description Tabelao para faixas de score visao volume de pedidos.
#' @param dados base de dados com dados com campos necessarios para construir o tabelcao
#' @param variavel_mau nome da variavel que contem os codigo de status dos pedidos. E.: pedido_status_id. Deve ser numerico.
#' @param variavel_score nome do campo que possui o score do pedido. Deve ser numerico.
#' @param variavel_valor nome do campo com valor total do pedido. Deve ser numerico. Se nao tiver deixe NULL, padrao.
#' @param nquebras total de quebras da base de dados com base na quantidade de pedidos (volume)
#' @param digits total de digitos para as estatisticas na tabela de saida
#' @param credito Se TRUE, gera o tabelao em ordem inversa, ou seja maiores escores sao melhores que menores scores. O processo inverso do tabelao de scores de fraude.
#' @param completa TRUE para determinar o tabelao com informacoes de valores monetarios
#' @param exclusoes vetor numerico de codigos de status a excluir da base de dados
#' @param define_mau vetor de codigos de status a considerar como mau. Padrao c(0, "M", "MAU", "BAD")
#' @import dplyr purrr
#' @export
rnp_tabelao_score <- function(dados, variavel_mau = "status_id_atual", variavel_score = 'score',
                              variavel_valor = NULL, nquebras = 20,
                              define_mau = c(0, "M", "MAU", "BAD"), digits = 4, credito = FALSE, completa = TRUE, exclusoes = NULL){

  if(missing(variavel_mau) || missing(variavel_score)||
     is.null(variavel_mau) || is.null(variavel_score)){
    stop("log: informe as duas variaveis que marcam: status do pedido e valor do score.")
  }

  mlag <- function(x, k) c(rep(0, k), head(x, -k))

  define_mau <- if(is.null(define_mau)) {
    c(0, "M", "MAU", "BAD")
  } else {
    define_mau
  }

  vx <- rlang::sym(variavel_score)
  vy <- rlang::sym(variavel_mau)

  out <- if(is.null(variavel_valor)){
    variavel_valor <- "valor"
    vz <- rlang::sym(variavel_valor)
    dados %>%
      dplyr::mutate(valor = 0) %>%
      dplyr::select({{vx}}, {{vy}}, {{vz}}, dplyr::everything()) %>%
      dplyr::arrange({{vx}}) %>%
      dplyr::filter(!({{vy}}) %in% exclusoes) %>%
      #purrr::set_names(tolower(colnames(.))) %>%
      dplyr::mutate(s_decisao = {{vx}} + rnorm(nrow(.), sd = 0.000000001),
                    quebras = cut(s_decisao,
                                  breaks = quantile(s_decisao, probs = seq(0,1,length.out = nquebras+1)),
                                  labels = 1:(length(quantile(s_decisao,probs = seq(0,1,length.out = nquebras+1)))-1), include.lowest = T),
                    fl_mau = dplyr::if_else({{vy}} %in% define_mau, 1, 0),
                    fl_tan = 1
      ) %>%
      dplyr::filter(!is.na(quebras))
  } else {
    vz <- rlang::sym(variavel_valor)
    dados %>%
      dplyr::select({{vx}}, {{vy}}, {{vz}}, dplyr::everything()) %>%
      dplyr::arrange({{vx}}) %>%
      dplyr::filter(!({{vy}}) %in% exclusoes) %>%
      #purrr::set_names(tolower(colnames(.))) %>%
      dplyr::mutate(s_decisao = {{vx}} + rnorm(nrow(.), sd = 0.000000001),
                    quebras = cut(s_decisao,
                                  breaks = quantile(s_decisao, probs = seq(0,1,length.out = nquebras+1)),
                                  labels = 1:(length(quantile(s_decisao,probs = seq(0,1,length.out = nquebras+1)))-1), include.lowest = T),
                    fl_mau = dplyr::if_else({{vy}} %in% define_mau, 1, 0),
                    fl_tan = 1
      ) %>%
      dplyr::filter(!is.na(quebras))
  }

  totais <- out %>%
    dplyr::summarise(total_maus = sum(fl_mau, na.rm = TRUE),
                     total_bons = sum(fl_tan, na.rm = TRUE) - total_maus,
                     total_geral = sum(fl_tan, na.rm = TRUE),
                     total_dinheiro = sum({{vz}}, na.rm = TRUE)
    )

  tab <- out %>%
    dplyr::group_by(quebras) %>%
    dplyr::summarise(
      total = sum(fl_tan, na.rm = TRUE),
      p_total = dplyr::if_else(totais$total_geral > 0, total /totais$total_geral, 0),
      min_score = min(s_decisao, na.rm = TRUE),
      max_score = max(s_decisao, na.rm = TRUE),
      t_maus = sum(fl_mau, na.rm = TRUE),
      t_bons = total-t_maus,
      p_maus = dplyr::if_else(totais$total_maus > 0, sum(fl_mau, na.rm = TRUE)/totais$total_maus, 0),
      p_bons = dplyr::if_else(totais$total_bons > 0, (total-sum(fl_mau, na.rm = TRUE))/totais$total_bons, 0),
      v_total = sum({{vz}}, na.rm = TRUE),
      .groups = "drop"
    )

  ## Corrige total de quebras
  fn_isinf <- function(x, fill = 0){
    x[which(is.infinite(x) | is.na(x))] <- fill
    x
  }

  t1 <- data.frame(quebras = as.character(seq_len(nquebras)))
  t2 <- dplyr::full_join(tab %>%
                           dplyr::mutate(quebras = as.character(quebras)),
                         t1, by = "quebras") %>%
    dplyr::mutate(quebras = as.numeric(quebras)) %>%
    dplyr::arrange(quebras)

  t2[is.na(t2)] <- 0

  t3 <- t2 %>%
    dplyr::arrange(dplyr::desc(quebras)) %>%
    dplyr::mutate(p_maus_acum = if(credito) purrr::accumulate(p_maus, sum, .dir = "backward") else purrr::accumulate(p_maus, sum),
                  p_bons_acum = if(credito) purrr::accumulate(p_bons, sum, .dir = "backward") else purrr::accumulate(p_bons, sum),
                  odds = p_maus / p_bons,
                  woe = log(odds),
                  iv = woe * (p_maus - p_bons),
                  ks = abs(p_maus_acum - p_bons_acum),
                  gini = (p_maus_acum + mlag(p_maus_acum, 1)) * (p_bons_acum - mlag(p_bons_acum, 1))
    ) %>%
    dplyr::arrange(quebras) %>%
    dplyr::mutate_if(is.numeric, .funs = "round", digits = digits) %>%
    dplyr::mutate_all(.funs = "fn_isinf")

  out <- if(completa){
    t3
  } else {
    t3 %>%
      dplyr::select(quebras, min_score, max_score, total, p_total, t_bons, p_bons, t_maus, p_maus, p_maus_acum,
                    p_bons_acum, odds, woe, iv, ks)
  }
  return(out)
}

#' Tabelao variavel
#' @description Tabelao para categorias de variaveis presentes ou nao em modelos.
#' @param dados base de dados com dados com campos necessarios para construir o tabelcao
#' @param variavel_mau nome da variavel que contem os codigo de status dos pedidos. E.: pedido_status_id. Deve ser numerico.
#' @param variavel_categ nome da variavel de interesse
#' @param digits total de digitos para as estatisticas na tabela de saida
#' @param exclusoes vetor numerico de codigos de status a excluir da base de dados
#' @param define_mau vetor de codigos de status a considerar como mau. Padrao e c(0, "M", "MAU", "BAD")
#' @import dplyr purrr
#' @export
rnp_tabelao_variavel <- function(dados, variavel_mau = "status_id_atual",
 variavel_categ = 'reg_combin', digits = 4, exclusoes = NULL, define_mau = NULL){

  vx <- rlang::sym(variavel_categ)
  vy <- rlang::sym(variavel_mau)

  mlag <- function(x, k) c(rep(0, k), head(x, -k))
  isna <- function(x) {
    x <- if(is.factor(x)) as.character(x) else x
    x[which(is.na(x)|is.null(x))] <- -999
    return(x)
  }

  define_mau <- if(is.null(define_mau)) {
    c(0, "M", "MAU", "BAD")
  } else {
    define_mau
  }

  out <- dados %>%
    dplyr::select({{vx}}, {{vy}}) %>%
    dplyr::arrange({{vx}}) %>%
    dplyr::filter(!({{vy}}) %in% exclusoes) %>%
    #purrr::set_names(tolower(colnames(.))) %>%
    dplyr::mutate(fl_mau = dplyr::if_else({{vy}} %in% define_mau, 1, 0),
                  fl_tan = 1
    ) %>%
    dplyr::mutate_at(.vars = paste(vx), .funs = "isna")

  totais <- out %>%
    dplyr::summarise(total_maus = sum(fl_mau, na.rm = TRUE),
                     total_bons = sum(fl_tan, na.rm = TRUE) - total_maus,
                     total_geral = sum(fl_tan, na.rm = TRUE))

  tab <- out %>%
    dplyr::group_by({{vx}}) %>%
    dplyr::summarise(
      total = sum(fl_tan),
      p_total = dplyr::if_else(totais$total_geral > 0, total /totais$total_geral, 0),
      t_maus = sum(fl_mau),
      t_bons = total-t_maus,
      p_maus = dplyr::if_else(totais$total_maus > 0, sum(fl_mau)/totais$total_maus, 0),
      p_bons = dplyr::if_else(totais$total_bons > 0, (total-sum(fl_mau))/totais$total_bons, 0)
    ) %>%
    dplyr::arrange(log(p_maus / p_bons)) %>%
    dplyr::mutate(p_maus_acum = purrr::accumulate(p_maus, sum),
                  p_bons_acum = purrr::accumulate(p_bons,sum),
                  odds = p_maus / p_bons,
                  woe = log(odds),
                  iv = woe * (p_maus - p_bons),
                  ks = abs(p_maus_acum - p_bons_acum),
                  gini = (p_maus_acum + mlag(p_maus_acum, 1)) * (p_bons_acum - mlag(p_bons_acum, 1))
    ) %>%
    dplyr::mutate_if(is.numeric, .funs = "round", digits = digits)
  return(tab)
}

# ----------------------------------------------------------------------------------------
# Funcoes para tratamento de dados
# ----------------------------------------------------------------------------------------

#' Remove acentos de caracteres.
#'
#' @description Remove acentos agudos, circunflexos, tremas, crases e cedilhas de Caracteres e strings.
#' @param str vetor de characters.
#' @return Vetor de characters sem acentos e sem cedilhas.
#' @examples
#' \dontrun{
#' palavras <- c("\u00e1gua", "op\u00e7\u00e3o", "\u00ff\u00c0\u00ea")
#' rnp_rm_accent(palavras)
#' }
#' @importFrom stringi stri_unescape_unicode stri_escape_unicode
#' @export
rnp_rm_accent <- function(str) {
  if(!is.character(str)) {
    str <- as.character(str)
  }
  str <- stringi::stri_escape_unicode(str)
  symbols <- c(
    acute = stringi::stri_unescape_unicode('\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00fd\u00dd'),
    grave = stringi::stri_unescape_unicode('\u00e0\u00e8\u00ec\u00f2\u00f9\u00c0\u00c8\u00cc\u00d2\u00d9'),
    circunflex = stringi::stri_unescape_unicode('\u00e2\u00ea\u00ee\u00f4\u00fb\u00c2\u00ca\u00ce\u00d4\u00db'),
    tilde = stringi::stri_unescape_unicode('\u00e3\u00f5\u00c3\u00d5\u00f1\u00d1'),
    umlaut = stringi::stri_unescape_unicode('\u00e4\u00eb\u00ef\u00f6\u00fc\u00c4\u00cb\u00cf\u00d6\u00dc\u00ff'),
    cedil = stringi::stri_unescape_unicode('\u00e7\u00c7')
  )
  nudeSymbols <- c(acute = 'aeiouAEIOUyY', grave = 'aeiouAEIOU', circunflex = 'aeiouAEIOU', tilde = 'aoAOnN', umlaut = 'aeiouAEIOUy', cedil = 'cC')
  return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), stringi::stri_unescape_unicode(str)))
}

#' Substitui NA numerico ou char.
#'
#' @description Substitui NA por algum valor fornecido ou padroniza com NA.
#' @param x Vetor qualquer
#' @param from Qual o codigo que quer remover.
#' @param to Qual o valor a substituir
#' @details Quando from = NULL, padrao, busca em numerico os valores  'NA, NaN, NULL, -Inf, Inf' e
#' quando character busca por '','.','NA','na','N/A','n/a','NaN','nan' e troca pelo valor de
#' to. Quando to = NULL, padrao, substitui pelo padrao do R que e NA.
#' @return Vetor x com as devidas alteracoes
#' @export
rnp_rm_na <- function(x, from = NULL, to = NULL){
  i <- x
  codes <- if(is.numeric(i)){
    c(NA, NaN, NULL, -Inf, Inf)
  } else if(!is.numeric(i)) {
    if(is.logical(i)){
      return(i)
    } else {
      c('','.','NA','na','N/A','n/a','NaN','nan',NA)
    }
  }
  if(is.null(from)){
    if(is.null(to)){
      i[which(i %in% codes)] <- NA
    } else {
      i[which(i %in% codes)] <- to
    }
  } else {
    if(is.null(to)){
      i[which(i %in% from)] <- NA
    } else {
      i[which(i %in% from)] <- to
    }
  }
  return(i)
}


#' Limpa texto de e-mail baseado em regex
#'
#' @description Aplica-se uma regex bem formatada para checar se as strings contendo dados de e-mails
#' estao corretas. Se houver alguma alguma string que nao bate com a regex entao esta e removida
#' e tratada como nao e-mail ou e-mail invalido.
#' @param email vetor de characters.
#' @param rm_accent logico indicando se deve buscar e remover acentos em email
#' @param fix_na se tiver algum tipo de na no vetor "email" sera removido
#' @return Vetor de characters detectados como e-mail
#' @importFrom stringr str_squish
#' @export
rnp_limpa_email <- function(email, rm_accent = TRUE, fix_na = TRUE){
  if(rm_accent) {
    x <- rnp_rm_accent(email)
  }
  if(fix_na) {
    x <- rnp_rm_na(x)
  }

  l <- grep("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE, value = TRUE)
  x[!x %in% l] <- NA

  return(stringr::str_squish(x))
}

#' Limpa texto textos em geral
#'
#' @description Util para limpar espacos em branco no meio das palavras, quebras de linha,
#' pagina entre outros, se definido, remove acentos e devolve em maiuscula ou minucula.
#' @param text vetor de characters.
#' @param rm_accent logico indicando se deve buscar e remover acentos em email
#' @param fix_na se tiver algum tipo de na no vetor "email" sera removido
#' @param char_size 'n' para normal, nada (nothing), 'l' para minuscula (lower) e 'u' para maiuscula (upper).
#' @param regex_from Aceita regex para buscar e substituir em text.
#' @param regex_to Valor para substituir em regex_from. Se NULL, substitui por vazio "".
#' @return Vetor de characters detectados como e-mail
#' @importFrom stringi stri_replace_all_charclass stri_trim_both
#' @export
rnp_limpa_text <- function(text, char_size = 'n', regex_from = NULL, regex_to = NULL, rm_accent = TRUE, fix_na = TRUE){
  char_size <- match.arg(char_size, c("n","l","u"))

  x <- if(rm_accent) {
    rnp_rm_accent(text)
  } else {
    text
  }

  if(fix_na) {
    x <- rnp_rm_na(x)
  }

  x <-   if(!is.null(regex_from) & !is.null(regex_to)){
    stringi::stri_replace_all_charclass(x, pattern = regex_from, replacement = regex_to)
  } else if(!is.null(regex_from) & is.null(regex_to)){
    stringi::stri_replace_all_charclass(x, pattern = regex_from, replacement = "")
  } else if(is.null(regex_from)  & !is.null(regex_to)){
    stop("log: preciso de regex_from diferente de NULL.\n")
  } else {
    x
  }

  out <- stringi::stri_replace_all_charclass(x, "\\p{WHITE_SPACE}", " ", merge=TRUE)
  out <- if(char_size == 'l'){
    tolower(out)
  } else if(char_size == 'u'){
    toupper(out)
  } else {
    out
  }
  return(stringi::stri_trim_both(out))
}


#' Verifica se a string eh email
#'
#' @description Aplica-se uma regex bem formatada para checar se as strings contendo dados de e-mails
#' esta correta. Se houver alguma alguma que nao bate com a regex entao esta e removida
#' e tratada como nao e-mail ou e-mail invalido.
#' @param email vetor de characters.
#' @param rm_accent logico indicando se deve buscar e remover acentos em email
#' @param fix_na se tiver algum tipo de na no vetor sera removido
#' @return Vetor de characters detectados como e-mail
#' @importFrom stringr str_squish
#' @export
is_email <- function(email, rm_accent = TRUE, fix_na = TRUE) {
  x <- stringr::str_squish(email)
  if(rm_accent) {
    x <- rnp_rm_accent(x)
  }
  if(fix_na) {
    x <- rnp_rm_na(x)
  }
  return(grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE))
}


#' Testa se o valor eh int64 ou logico
#'
#' @param i vetor qualqer
#' @return TRUE se o vetor i e de classe integer64
is_int64 <- function(i) {
  if( inherits(i, c("integer64","logical"))) TRUE else FALSE
}

#' Testa se o valor eh data
#'
#' @description Permite avaliar, atraves do tipo ou do conteudo, se um vetor de dados recebido e ou nao no tipo data
#' @param i vetor qualqer
#' @param check se TRUE (padao) avalia o conteudo de i e infere se a variavel total e de data
#' @param fix_na se tiver algum tipo de na no vetor sera removido
#' @return TRUE se o vetor e das classes de data
#' @export
is_date <- function(i, check = TRUE, fix_na = TRUE) {

  if(fix_na){
    i <- rnp_rm_na(i)
  }

  check_structure <- function(i){
    if(sapply(i, function(x) !all(is.na(as.Date(as.character(x), format = c("%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%Y-%m-%d")))))) TRUE else FALSE
  }
  if(check) {
    if(all(sapply(i, function(x) check_structure(x)))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if(inherits(i, c("POSIXlt","POSIXct","POSIXt","Date"))) {
      return(TRUE)
    } else {
      FALSE
    }
  }
}

#' Testa se o valor eh numerico
#'
#' @description Permite avaliar, atraves do tipo ou do conteudo, se um vetor de dados recebido e ou nao no tipo numerico
#' @param i vetor qualqer numerico
#' @param check se TRUE (padao) avalia o conteudo de i e infere se a variavel e de data
#' @param fix_na se tiver algum tipo de NA no vetor sera removido
#' @return TRUE se o vetor i e das classes numericas
#' @importFrom stringr str_squish str_replace_all
#' @export
is_num <- function(i, check = TRUE, fix_na = FALSE){

  if(fix_na){
    i <- rnp_rm_na(i)
  }

  i <- if(check){
    stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  } else {
    if(inherits(i, c("factor","character","integer64"))) {
      as.character(i)
    } else {
      i
    }
  }
  is.numeric(as.numeric(i))
}

#' Testa se o valor eh numerico. Se nao for, converte.
#'
#' @description Verifica se o conteudo do vetor passado e de numeros. Quando check = TRUE, a estrutura do dado
#' e testada via regex e se o padrao for detectado, o vetor e definido como numerico.
#' @param i vetor qualquer
#' @param check TRUE checa a estrutura do dado
#' @param fix_na vetor de strings a serem removidas, caso existam.
#' @return vetor numerico tratado a partir de i
#' @importFrom stringr str_squish
#' @export
as_num <- function(i, check = TRUE, fix_na = TRUE){
  i <- if(inherits(i, c("factor","character","integer64"))) {
    as.character(i)
  } else {
    i
  }
  if(fix_na) {
    i <- rnp_rm_na(i)
  }
  if(check){
   l <- stringr::str_squish(grepl("^[\\-\\+]?[0-9]+[\\.]?[0-9]*$|^[\\-\\+]?[0-9]+[L]?$|^[\\-\\+]?[0-9]+[\\.]?[0-9]*[eE][0-9]+$",i,perl=TRUE))
   i[which(l %in% FALSE)] <- NA
  }
  as.numeric(i)
}

#' Testa se o valor eh CPF
#'
#' @description Com apoio do pacote "validaRA" testa se o vetor recebido e um cpf.
#' @param i vetor de CPF. Ver \code{\link[validaRA]{valida_doc}}
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @import validaRA
#' @importFrom stringr str_squish str_replace_all
#' @export
is_cpf <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rnp_rm_na(i)
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  validaRA::valida_doc(as.numeric(out), type = "cpf")
}

#' Testa se o valor eh CNPJ
#'
#' @description Com apoio do pacote "validaRA" testa se o vetor recebido e um CNPJ.
#' @param i vetor de CNPJ. Ver \code{\link[validaRA]{valida_doc}}
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @import validaRA
#' @importFrom stringr str_squish str_replace_all
#' @export
is_cnpj <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rnp_rm_na(i)
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  validaRA::valida_doc(as.numeric(out), type = "cnpj")
}

#' Testa se o valor eh PIS
#'
#' @description Com apoio do pacote "validaRA" testa se o vetor recebido e um PIS
#' @param i vetor de numeros PIS. Ver \code{\link[validaRA]{valida_doc}}
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @import validaRA
#' @importFrom stringr str_squish str_replace_all
#' @export
is_pis <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rnp_rm_na(i)
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))
  validaRA::valida_doc(as.numeric(out), type = "pis")
}


#' Testa se o valor eh CEP
#'
#' @description Via regex e limpeza de dados, verifica se i e um cep.
#' @details Olha-se apenas a estrutura. Nao validamos se o cep e valido nos correios.
#' @param i vetor de numeros CEP
#' @param fix_na vetor de strings a serem removidas caso existam.
#' @importFrom stringr str_extract str_replace_all str_squish
#' @export
is_cep <- function(i, fix_na = TRUE){
  if(fix_na){
    i <- rnp_rm_na(i)
  }
  out <- i %>%
    format(scientific = FALSE) %>%
    stringr::str_extract(pattern = "[:digit:]{5}[-][:digit:]{3}|[:digit:]+") %>%
    stringr::str_replace_all("[^[:digit:]]", "") %>%
    stringr::str_squish() %>%
    as_num()
  return(nchar(out) <= 8)
}

#' Limpa Data
#'
#' @description Valida 27 padroes de combinacoes de datas incluindo data simples e datetime.
#'
#' @details Esta funcao trabalha com timezone = UTC: hora universal. A localizacao e definida
#' pelo padrao do sistema. Ver \code{\link[lubridate]{parse_date_time}} e \code{\link[base]{locales}}.
#'
#' @param date Vetor de data generico.
#' @param fix_na Padroes de NA removidos do numero ou string i
#' @param format_out Se desejar, pode informar um formato de data para a saida. Veja \code{\link[base]{format.Date}}
#' @param char_out Se TRUE, retorna a data formatada como char.
#' @importFrom lubridate parse_date_time
#' @examples
#' \dontrun{
#' x <- c('February 20th 1973',
#' 'Sunday, May 1, 2000',
#' 'february  14, 04',
#' 'Feb 20th 73',
#' 'January 5 1999 at 7pm',
#' 'jan 3 2010',
#' 'Jan 1, 1999',
#' 'jan 3   10',
#' '01 3 2010',
#' '1 3 10',
#' '1 13 89',
#' '5/27/1979',
#' '12/31/99',
#' 'DOB:12/11/00',
#' 'Thu, 1 July 2004 22:30:00',
#' 'Thu, 1st of July 2004 at 22:30:00',
#' 'Thu, 1July 2004 at 22:30:00',
#' 'Thu, 1July2004 22:30:00',
#' 'Thu, 1July04 22:30:00',
#' '21 Aug 2011, 11:15:34 pm',
#' '1979-05-27 05:00:59',
#' '1979-05-27',
#' '3 jan 2000',
#' '17 april 85',
#' '27/5/1979',
#' '20 01 89',
#' '00/13/10',
#' '14 12 00',
#' '03:23:22 pm')
#' rnp_limpa_date(x)
#' }
#' @export
rnp_limpa_date <- function(date, fix_na = TRUE, format_out = NULL, char_out = FALSE) {

  date <- if(fix_na){
    rnp_rm_na(date)
  } else {
    date
  }
  formats <- c(c("ymd","ydm",'myd','mdy','dmy','dym'),
               c("HMS","HM","MS"),
               paste0(rep(c("ymd","ydm",'myd','mdy','dmy','dym'), each = 3), c("HMS","HM","MS")))

  out <- if(!is.null(format_out)){
    format(lubridate::parse_date_time(date, orders = formats), format_out, tz = "UTC", locale = Sys.getlocale("LC_TIME"))
  } else {
    lubridate::parse_date_time(date, orders = formats, tz = "UTC", locale = Sys.getlocale("LC_TIME"))
  }

  return(if(char_out) as.character(out) else out)
}

#' Limpa CEP
#'
#' @description Via regex e limpeza de dados, verifica-se a estrutura do vetor informado, removendo tudo que nao for
#' numero e completando com zeros a esquerda casos com menos de 8 digitos. Se o CEP for da forma
#' 99999-999, o traco e removido e o numero retorna limpo.
#' nao validamos se o cep e valido nos correios.
#' @param cep vetor de numeros ou strings de CEP
#' @param fix_na padroes de NA removidos do numero ou string i
#' @importFrom stringr str_extract str_replace_all str_squish str_pad
#' @export
rnp_limpa_cep2 <- function(cep, fix_na = TRUE){
  i <- if(fix_na){
    rnp_rm_na(cep)
  } else {
    cep
  }
  i %>%
    format(scientific = FALSE) %>%
    stringr::str_extract(pattern = "[:digit:]{5}[-][:digit:]{3}|[:digit:]+") %>%
    stringr::str_replace_all("[^[:digit:]]", "") %>%
    stringr::str_squish() %>%
    stringr::str_pad(width = 8, side = "left", pad = "0")
}

#' Limpa CPF/CNPJ
#'
#' @description Com apoio do pacote "validaRA", regexes e avaliacao do comprimento das strings no campo de
#' CPF/CNPJ recebido, verifica a validade do campo segundo regras de construcao do CPF/CNPJ e acerta o tamanho.
#' Pontos, barras e tracos sao removidos e apenas um vetor numerico em forma de string e retornado.
#' Ver \code{\link[validaRA]{valida_doc}} para mais detalhes.
#' @param doc vetor de numeros ou strings de CPF/CNPJ.
#' @param fix_na padroes de NA removidos do numero ou string i
#' @importFrom stringr str_replace_all str_squish str_pad str_length
#' @importFrom dplyr tibble mutate case_when select
#' @import validaRA
#' @return tibbla com tres colunas, uma com o doc outras com logico se CPF = TRUE ou CNPJ = TRUE
#' @export
rnp_limpa_cpf_cnpj <- function(doc, fix_na = TRUE) {
  i <- if(fix_na){
    rnp_rm_na(doc)
  } else {
    doc
  }
  out <- stringr::str_squish(stringr::str_replace_all(i, "[^[:digit:]]", ""))

  out <- dplyr::tibble(doc_ = as_num(out),
                       cpf = is_cpf(doc_),
                       cnpj = is_cnpj(doc_))  %>%
    dplyr::mutate(doc = dplyr::case_when( cpf & !cnpj  ~ stringr::str_pad(doc_, width = 11, side = "left", pad = "0"),
                                         !cpf & cnpj ~ stringr::str_pad(doc_, width = 14, side = "left", pad = "0"),
                                          cpf & cnpj ~ as.character(doc_),
                                         TRUE ~ NA_character_)
                  ) %>%
    dplyr::select(doc, cpf, cnpj)
  return(out)
}

#' Leitura de dados com pacote rio
#'
#' @description Este e apenas um wraper que deixa a funcao \code{\link[rio]{import}}
#' melhor formatada para passar na funcao \code{\link{rnp_read}}
#' @param file Nome do arquivo a ser importado
#' @param ... Argumentos extras
#' @importFrom rio import
#' @export
rnp_read_bf_rio <- function(file, ...){
  #requireNamespace("rio")
  rio::import(file, ...)
}


#' Leitura de dados com pacote data.table
#'
#' @description Este e apenas um wraper que deixa a funcao \code{\link[data.table]{fread}}
#' melhor formatada para passar na funcao \code{\link{rnp_read}}
#' @param file Nome do arquivo a ser importado.
#' @param control Controle com todos argumentos necessarios para importar dados para uso inteno com a rnp_read()
#' @importFrom data.table fread
#' @export
rnp_read_bf_dt <- function(file, control = rnp_read_control(type = "fread")){
  #requireNamespace("data.table")
  data.table::fread(input = file, sep = control$delim, dec = control$dec, nrows = control$nrows, quote = control$quote,
                    encoding = control$encoding, header = control$header, skip = control$skip, select = control$select,
                    colClasses = control$colClasses
  )
}

#' Leitura de dados com pacote readr
#'
#' @description Este e apenas um wraper que deixa as funcoes \code{\link[readr]{read_csv2}} e
#' \code{\link[readr]{read_delim}} conveniente para passar na funcao \code{\link{rnp_read}}
#' @param file Nome do arquivo a ser importado.
#' @param control Controle com todos argumentos necessarios para importar dados para uso inteno com a rnp_read()
#' @importFrom readr read_csv2 read_delim
#' @export
rnp_read_bf_rd <- function(file, control = rnp_read_control(type = "readr")){
  #requireNamespace("readr")
  if(is.null(control$delim)){
    readr::read_csv2(file = file, col_names = control$col_names, col_types = control$col_types, locale = control$locale,
                     na = control$na, quote = control$quote, comment = control$comment, trim_ws = control$trim_ws,
                     skip = control$skip, n_max = control$n_max, guess_max = control$guess_max,
                     skip_empty_rows = control$skip_empty_rows)
  } else {
    readr::read_delim(file = file, delim = control$delim, quote = control$quote, escape_backslash = control$escape_backslash,
                      guess_max = control$guess_max, escape_double = control$escape_double, col_names = control$col_names,
                      col_types = control$col_types, locale = control$locale, na = control$na, trim_ws = control$trim_ws,
                      skip = control$skip, n_max = control$n_max
    )
  }
}

#' Leitura de dados com pacote vroom
#'
#' @description Este e apenas um wraper que deixa a funcao \code{\link[vroom]{vroom}}
#' melhor formatada para passar na funcao \code{\link{rnp_read}}
#' @param file Nome do arquivo a ser importado.
#' @param control Controle com todos argumentos necessarios para importar dados para uso inteno com a rnp_read()
#' @importFrom vroom vroom
#' @export
rnp_read_bf_vr <- function(file, control = rnp_read_control(type = "vroom")){
  #requireNamespace("vroom")
  vroom::vroom(file = file, delim = control$delim, quote = control$quote, escape_backslash = control$escape_backslash,
               escape_double = control$escape_double, col_names = control$col_names, col_types = control$col_types,
               locale = control$locale, na = control$na, trim_ws = control$trim_ws, skip = control$skip, col_select = control$col_select,
               n_max = control$n_max, altrep = control$altrep, guess_max = control$guess_max
  )
}

#' @title Controle para leitura de arquivos grandes (Big Files)
#'
#' @description Esta funcao e uma chave para passar argumentos para a funcao rnp_read
#' @param type Nome do mecanisco de leitura. Podendo sem um em "fread","readr","vroom" ou "rio". Veja os detalhes abaixo para saber mais.
#' @param delim Delimitador de colunas, se nao souber deixe NULL. Pode ser, ",", ";", "|" e outros conforme seu arquivo.
#' @param dec Separador decimal.
#' @param try_append Se TRUE (padrao) tenta apendar (empilhar) os dados. Se FALSE retorna lista de tabelas, uma para cada arquivo importado.
#' @param sep Agrupador de campos para formatacao de numeros
#' @param quote Regex para quotation. Serve para nao confundir com aspas que delimita strings.
#' @param escape_backslash Utilizar contrabarra para saltar Caracteres especiais
#' @param escape_double Se TRUE o valor '""' representara um unico \"
#' @param col_names TRUE, FALSE, um vetor de nomes ou numeros que serao tratados como indices para selecionar colunas
#' @param col_types Se NULL trabalha com os padroes do pacote readr. Se data.table, utiliza os padroes da funao \code{\link[data.table]{fread}}
#' @param col_select Vetor de nomes ou indices a selecionar.
#' @param encoding Encoding do arquivo. Como padrao pega as configuracoes do Sistema Operacional
#' @param na Tipo de valor para substituir NA
#' @param comment Caractere a ser tratado como comentario. padrao e hashtag
#' @param trim_ws Limpar espacos em branco a direita e esquerda das colunas
#' @param skip numero de linhas a saltar na leitura dos dados
#' @param n_max numero de linhas que quer extrair. Se Inf retorna o maximo de linhas
#' @param guess_max Total de linhas por partes lidas (chunksize)
#' @param altrep Controle dos tipos de dados na leitura por \code{\link[vroom]{vroom}}
#' @param progress Mostrar progresso da leitura dos dados
#' @param skip_empty_rows Ignorar linhas vazias
#' @param ... Argumentos extras
#' @export
rnp_read_control <- function(type = "fread", delim = NULL, dec = NULL, try_append = TRUE,
                             sep = NULL, quote = "\"", escape_backslash = FALSE,
                             escape_double = TRUE, col_names = TRUE, col_select = NULL,
                             col_types = NULL, encoding = NULL, na = c("", "NA"),
                             comment = "", trim_ws = FALSE, skip = 0, n_max = Inf,
                             guess_max = min(1000, n_max), altrep = TRUE,
                             progress = NULL, skip_empty_rows = TRUE, ...){

  type <- match.arg(type, c("fread","readr","vroom","rio"))

  control <- switch(type,
                    fread = list(dec = if(is.null(dec)) "." else dec,
                                 delim = if(is.null(delim)) "auto" else delim,
                                 quote = quote,
                                 nrows = n_max,
                                 header = if(col_names) "auto" else col_names,
                                 skip = if(skip == 0) "__auto__" else skip,
                                 select = if(is.null(col_select)) NULL else col_select,
                                 colClasses = if(is.null(col_types)) NULL else col_types,
                                 encoding =  if(is.null(encoding)){
                                   "unknown"
                                 }  else {
                                   if(encoding == "latin1") "Latin-1" else encoding
                                 }
                    ),
                    readr = list(delim = delim,
                                 quote = quote,
                                 escape_backslash = escape_backslash,
                                 escape_double = escape_double,
                                 col_names = col_names,
                                 col_types = col_types,
                                 locale = if(is.null(encoding)) {
                                   readr::default_locale()
                                 } else {
                                   readr::locale(encoding = if(is.null(encoding)) "UTF-8" else encoding,
                                                 decimal_mark = if(is.null(dec)) "." else dec
                                   )
                                 },
                                 na = na,
                                 comment = comment,
                                 trim_ws = trim_ws,
                                 skip = skip,
                                 n_max = n_max,
                                 guess_max = guess_max,
                                 progress = readr::show_progress(),
                                 skip_empty_rows = skip_empty_rows
                    ),
                    vroom = list(delim = delim,
                                 quote = quote,
                                 escape_backslash = escape_backslash,
                                 col_select = col_select,
                                 escape_double = escape_double,
                                 col_names = col_names,
                                 col_types = col_types,
                                 locale = if(is.null(encoding)) {
                                   vroom::default_locale()
                                 } else {
                                   vroom::locale(encoding = if(is.null(encoding)) "UTF-8" else encoding,
                                                 decimal_mark = if(is.null(dec)) "." else dec
                                                 )
                                 },
                                 na = na,
                                 comment = comment,
                                 trim_ws = trim_ws,
                                 skip = skip,
                                 n_max = n_max,
                                 guess_max = guess_max,
                                 altrep = altrep,
                                 progress = vroom::vroom_progress(),
                                 skip_empty_rows = skip_empty_rows
                    ),
                    rio = list(format = delim,
                               dec = if(is.null(dec)) "." else dec,
                               header = col_names)
  )
}

#' @title Leitura de arquivos grandes (Big Files)
#' @description Esta funcao e um ensemble que junta muitas funcoes otimizadas de pacotes especiais do
#' R. Entre eles, readr, data.table, vroom e rio.
#' @param file Vetor ou lista de strings com nomes de arquivos a serem importados da maioria das extensoes. Arquivos copactados em .gz e .bgz
#' @param type Nome do mecanisco de leitura. Podendo sem um em "fread","readr","vroom" ou "rio". Veja os detalhes abaixo para mais detalhes.
#' @param delim Delimitador de colunas, se nao souber deixe NULL. Pode ser, ",", ";", "|" e outros conforme seu arquivo.
#' @param dec Deparador decimal.
#' @param try_append Se TRUE (padrao) tenta apendar (empilhar) os dados. Se FALSE retorna lista de tabelas, uma para cada arquivo importado.
#' @param sep Agrupador de campos para formatacao de numeros
#' @param quote Regex para quotation. Serve para nao confundir com aspas que delimita strings.
#' @param escape_backslash Utilizar contrabarra para saltar Caracteres especiais
#' @param escape_double Se TRUE o valor '""' representara um unico \"
#' @param col_names TRUE, FALSE, um vetor de nomes ou numeros que serao tratados como indices para selecionar colunas
#' @param col_types Se NULL trabalha com os padroes do pacote readr. Se data.table, utiliza os padroes da funao \code{\link[data.table]{fread}}
#' @param col_select Vetor de nomes ou indices a selecionar.
#' @param encoding Encoding do arquivo. Como padrao pega as configuracoes do Sistema Operacional
#' @param na Tipo de valor para substituir NA
#' @param comment Caractere a ser tratado como comentario. padrao e hashtag
#' @param trim_ws Limpar espacos em branco a direita e esquerda das colunas
#' @param skip numero de linhas a saltar na leitura dos dados
#' @param n_max numero de linhas que quer extrair. Se Inf retorna o maximo de linhas
#' @param guess_max Total de linhas por partes lidas (chunksize)
#' @param altrep Controle dos tipos de dados na leitura por \code{\link[vroom]{vroom}}
#' @param progress Mostrar progresso da leitura dos dados
#' @param skip_empty_rows Ignorar linhas vazias
#' @param ... Outros argumentos de funcoes quando trabalhando com o pacote \code{\link[rio]{import}}
#' @details Para obter detalhes completos destas funcoes e seus argumentos
#' consulte a documentacao original dos seus desenvolvedores conforme lista abaixo:
#' \itemize{
#'     \item Para type = "fread" veja \code{\link[data.table]{data.table}} e \code{\link[data.table]{fread}}.
#'     \item Para type = "readr" veja \code{\link[readr]{readr}} e \code{\link[readr]{read_delim}}.
#'     \item Para type = "vroom" veja \code{\link[vroom]{vroom}}.
#'     \item Para type = "rio"   veja \code{\link[rio]{rio}} e \code{\link[rio]{import}}.
#' }
#' @importFrom data.table fread setDT is.data.table
#' @importFrom vroom vroom
#' @importFrom readr read_csv read_csv2
#' @importFrom rio import
#' @importFrom dplyr bind_rows
#' @importFrom purrr map map_df
#' @examples
#' \dontrun{
#' require("rnpextras")
#' f2 <- list.files(pattern = "1-c000.csv.gz$", full.names = TRUE)
#' a <- rnp_read(file = f2, type = "fread")
#' b <- rnp_read(file = f2, type = "readr", delim = ",", col_names = FALSE)
#' d <- rnp_read(file = f2, type = "vroom", delim = ",", col_names = FALSE)
#' e <- rnp_read(file = "/teste_02.xls", type = "rio", sheet = 1, range = "a3:j23")
#' }
#' @export
rnp_read <- function(file, type = "fread", delim = NULL, dec = NULL, try_append = TRUE,
                     sep = NULL, quote = "\"", escape_backslash = FALSE,
                     escape_double = TRUE, col_names = TRUE, col_select = NULL,
                     col_types = NULL, encoding = NULL, na = c("", "NA"),
                     comment = "", trim_ws = FALSE, skip = 0, n_max = Inf,
                     guess_max = min(1000, n_max), altrep = TRUE,
                     progress = NULL, skip_empty_rows = TRUE, ...){

  control <- rnp_read_control(type = type, delim = delim, dec = dec, try_append = try_append,
                              sep = sep, quote = quote, escape_backslash = escape_backslash,
                              escape_double = escape_double, col_names = col_names,
                              col_select = col_select, col_types = col_types, encoding = encoding,
                              comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max,
                              guess_max = guess_max, altrep = altrep, progress = progress,
                              skip_empty_rows = skip_empty_rows)

  try_read_fb <- switch(type,
                        fread = try(purrr::map(.x = as.list(file), .f = function(i){
                          rnp_read_bf_dt(i, control, ...)
                        })),
                        readr = try(purrr::map(.x = as.list(file), .f = function(i){
                          rnp_read_bf_rd(i, control, ...)
                        })),
                        vroom = try(purrr::map(.x = as.list(file), .f = function(i){
                          rnp_read_bf_vr(i, control, ...)
                        })),
                        rio = try(purrr::map(.x = as.list(file), .f = function(i){
                          rnp_read_bf_rio(i, ...)
                        }))
  )
  out <- if(try_append){
    if(col_names == FALSE){
      dplyr::bind_rows(try_read_fb) %>%
        data.table::setDT() %>%
        purrr::set_names(., nm = paste0("X", seq_along(colnames(.))))
    } else {
      dplyr::bind_rows(try_read_fb) %>%
        data.table::setDT()
    }
  } else {
    if(col_names == FALSE){
      purrr::map(try_read_fb, function(i) data.table::setDT(i)) %>%
        purrr::set_names(., nm = paste0("X", seq_along(colnames(.))))
    }
    purrr::map(try_read_fb, function(i) data.table::setDT(i))
  }
  return(out)
}

#' Particao de dados
#'
#' @description Ajuda a quebrar um conjunto de dados com base no total de linhas desejado.
#' @param n Total de linhas da base total
#' @param nrows Total de linhas por bloco
#' @export
rnp_cut <- function(n, nrows = 1000){
  x <- seq_len(n)
  n <- length(x)
  p <- nrows / n

  x <- as.character(cut(x, breaks = seq(1, n, by = round(p*n)), dig.lab = 10))

  y <- gsub(pattern = "\\[|\\]|\\(|\\)", replacement = "", x = x)
  y <- gsub(pattern = ",", replacement = "-", x = y)
  u <- sum(is.na(y))
  if(u > 0){
    ultimo = max(as.numeric(unlist(strsplit(y, "-"))), na.rm = TRUE)
    y <- c(na.exclude(y), rep(paste0(ultimo, "-", n), u))
  }
  return(y)
}


#' Remove colchetes, parentesis e chaves
#' @param x string de dados
rnp_clean_brackets <- function(x){
  trimws(gsub(pattern = '\\[|\\]|\\(|\\)|\\{|\\}|\\\\|"\"', replacement = "", x))
}

#' Grava dados em um banco via ODBC
#'
#' @description Recebe parametros de uma conexao odbc ativa e exporta para o respectivo banco de dados
#' uma tabela de dados do R como data.frame, tibbla, matriz ou data.table subindo inteira ou por partes.
#'
#' @details Quando o argumento column_types e NULL, padrao, a configuracao dos tipos de dados e feita
#' automaticamente atraves de metodos do pacote DBI, dplyr e dbplyr. Porem, o usuario pode passar
#' manualmente um vetor nomeado com os tipos compativeis com SQL. Cada valor e um tipo e cada nome
#' e uma coluna da tabela. Para obter as tipagens de forma otimizada, criamos a funcao \code{\link{rnp_db_types}}.
#' Esta funcao pode ser muito lenta, pois ela analisa a fundo a estrutura dos dados de toda as colunas
#' para determinar o tipo exato, mas permite paralelismo. Optamos por manter esta funcao fora das rotinas
#' da \code{\link{rnp_db_write}} devido seu carater de computacao custoso, mas recomendamos trabalhar com
#' ela para detectar corretamente so tipos exatos antes de criar a tabela fisica no banco de dados.
#' Existem duas formas de salvar dados no banco de dados: se a escolha for
#' method = 'dbi' o R tentara salvar a base de dados em partes utilizando methodos do pacote DBI,
#' caso method = 'copy' ele tentara salvar os dados de uma unica vez sem quebra em partes.
#' Este metodo pode evitar problemas com tipos de dados, mas costuma ser custoso computacionalmente.
#' Para mais detalhes ver \code{\link[DBI]{dbWriteTable}}, \code{\link[dplyr]{copy_to}} e
#' \code{\link[dbplyr]{in_schema}}.
#' Caso o usuario passe uma tipagem manual, ele precisa passar para todas as variaveis da tabela.
#' Exemplo, se a tabela tiver 3 colunas, column_types = c(mpg = 'DECIMAL(3,1)', cyl = 'VARCHAR(20)', logic = 'BIT') .
#'
#' @param con Conexao ativa odbc. Veja \code{\link[DBI]{dbConnect}} para mais detalhes
#' @param data Objeto de dados. Pdoe ser data.frame ou qualquer objeto que se posssa converter em data.frame
#' @param name String de nome da tabela que vai ser grabada no banco com os dados do objeto 'data'
#' @param schema String de nome do schema do banco de dados que deseja salvar. padrao e 'dbo'
#' @param method String com um entre os valoes 'dbi' ou 'copy'. Vaje details para mais informacoes.
#' @param chunk_size Define o total de linhas que cada lote salvara no banco por vez.
#' @param verbose Se TRUE que mostra todo o log do processo ou apenas algumas partes.
#' @param append Se TRUE e a tabela ja existir no banco de dados ele tenta apendar (empilhar) os dados.
#' @param column_types Tipagem das colunas. Se 'auto' salva com padroes do pacote DBI, se 'otima' busca
#' padros conforme estrutura dos dados. Pode ser muito demorado sem bases grandes. Se 'manual',
#' o usuario deve informar um vetor nomeado onde cada nome e uma coluna da base e o valor e o
#' formato compativel com o SQL Server.
#' @importFrom DBI dbWriteTable dbIsValid dbExistsTable
#' @importFrom dplyr copy_to
#' @importFrom dbplyr in_schema
#' @importFrom data.table `:=` setDT data.table
#' @importFrom purrr map map_df
#' @importFrom tibble as_tibble
#' @export
rnp_db_write <- function(con, data, name, column_types = NULL, schema = "dbo", method = "dbi", chunk_size = 24999, verbose = TRUE, append = FALSE){
  method <- match.arg(method, c("dbi","copy"))
  query <- paste0(schema, ".", name)
  nm_base <- colnames(data)
  ini0 <- Sys.time()

  if(!is.null(column_types)){
    if(!all(hasName(column_types, names(column_types)))){
      stop("log: preciso que o vetor de tipos seja nomeado com os nomes de cada coluna.\n")
    }
    if(length(column_types) != ncol(data)) {
      stop("log: o vetor de tipos precisar ser do tamanho do numero de colunas da base.\n")
    }
  }

  aux_method_dbi_write <- function(con, da, append, ...){
    query = paste0(schema, ".", name)
    DBI::dbWriteTable(conn = con, name = DBI::SQL(query), value = da,
                      field.types = column_types,
                      overwrite = FALSE, append = append,
                      encoding = "latin1", row.names = FALSE, ...)
  }

  aux_method_dbi_append <- function(con, da, append = TRUE, ...){
    query = paste0(schema, ".", name)
    DBI::dbWriteTable(conn = con, name = DBI::SQL(query), value = da,
                      #field.types = column_types,
                      overwrite = FALSE, append = append,
                      encoding = "latin1", row.names = FALSE, ...)
  }

  aux_method_copy_write <- function(con, da, ...){
    dplyr::copy_to(dest = con, df = da, field.types = column_types,
                   name = dbplyr::in_schema(schema, name),
                   temporary = FALSE, overwrite = FALSE, ...)
  }

  # aux_split_data <- function(da, chunk_size){
  #   nm_base <- colnames(da)
  #   da[, temp_quebra__ := rnp_cut(.N, nrows = chunk_size)]
  #   splt <- purrr::map(split(da, da$temp_quebra__), function(i){
  #     i$temp_quebra__ <- NULL
  #     tibble::as_tibble(i)
  #   })
  #   rm(da)
  #   return(splt)
  # }

  if(!DBI::dbIsValid(con)){
    stop(cat("log: Conexao perdida, favor reconectar!\n"))
  }

  if(DBI::dbExistsTable(conn = con, name = DBI::SQL(query)) & append == FALSE){
    stop(cat("log: A tabela (", query, ") ja existe no banco.",
             con@info$dbname, "Se nao for possivel dropar defina outro nome.\n"))
  }

  if(chunk_size >= nrow(data) | method == "copy"){
    chunk_size <- nrow(data) -1
    split_data <- FALSE
  } else {
    chunk_size <- chunk_size
    split_data <- TRUE
  }

  if(missing(schema) || is.null(schema)){
    schema <- "dbo"
  }

  # Quebrando os dados de entrada em pedacos de acordo com o total de linhas a subir em lotes.
  # Parte 1. cria uma base tamanho nrows com os tipos detectados na base total.
  # Parte 2. cria lista com restante da tabelas para subir via insert em lotes pequenos.
  if(!inherits(data, "data.table")){
    data.table::setDT(data)
  }

  if(!split_data | method == "copy"){
    ini <- Sys.time()
    cat(timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                  prefix =  "log: ", suffix = " Processando dados e checando comunicacao com o banco ...\n", quiet = TRUE))

    out <- if(method == "copy"){
      cat(timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                    prefix =  "log: ", suffix = " Sem quebras definidas. Subindo tabela completa ...\n", quiet = TRUE))
      try(aux_method_copy_write(con, data))
    } else {
      try(aux_method_dbi_write(con, data, append))
    }
    if(inherits(out, "try-error")){
      stop(FALSE)
    } else {
      if(verbose){
        verb <- paste0(" Duracao: ", round(as.numeric(Sys.time() -ini, units = "secs"), 4),
                       " secs; linhas: ", nrow(data),  ";",
                       " upload total: ", format(object.size(data), units = "Mb", digits = 4), " ...\n")
        cat(timestamp(prefix = "log: ",  suffix = verb, quiet = TRUE))
      }
      return(invisible(out))
    }
  } else {
    cat(timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                  prefix =  "log: ", suffix = " Processando quebras e preparando para subida ...\n", quiet = TRUE))
    out <- c()

    #splt <- aux_split_data(data, chunk_size)
    splt <- rnp_split_data_table(data, nrows = chunk_size)

    # nm <- names(sort(sapply(names(splt),
    #                         function(i){max(as.numeric(unlist(strsplit(i, "-"))))})))

    P1 <- splt[[1]]
    PN <- splt[-1]
    #rm(data)
    # Gravar parte 01
    {
      query <- paste0(schema, ".", name)
      cat(timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                    prefix = "log: ",  suffix = paste0(" Iniciando upload de (", length(splt), ") partes ...\n"), quiet = TRUE))
      ini <- Sys.time()
      lote <- 1

      try(aux_method_dbi_write(con, P1, append))

      if(verbose){
        linhas <- nrow(P1)
        obsize <- object.size(P1)
        fim <- Sys.time()
        verb <- paste0(" Duracao lote (", formatC(lote, digits = 4, width = 4), "): ", format(round(as.numeric(fim -ini, units = "secs"), 3), nsmall = 3),
                       " secs; linhas: ", formatC(linhas, digits = 8, width = 8),  ";",
                       " upload: ", format(obsize, units = "Mb", digits = 3), "...\n")
        cat(timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                      prefix = "log: ",  suffix = verb, quiet = TRUE))
      }
    }

    # Gravar restante das bases em loop por insert.
    {
      for(i in seq_along(PN)){
        ini <- Sys.time()
        lote <- lote + 1
        linhas <- linhas + nrow(PN[[i]])
        try(aux_method_dbi_append(con, PN[[i]], append = TRUE))

        if(verbose) {
          obsize <- obsize + object.size(PN[[i]])
          fim <- Sys.time()
          verb <- paste0(" Duracao lote (", formatC(lote, digits = 4, width = 4), "): ", format(round(as.numeric(fim-ini, units = "secs"), 3), nsmall = 3),
                         " secs; linhas: ", formatC(linhas, digits = 8, width = 8), ";",
                         " upload: ", format(obsize, units = "Mb", digits = 3), "...\n")
          out <- rbind(out, verb)
          cat(timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"), prefix = "log: ",  suffix = verb, quiet = TRUE))
        }
      }
    }
    verb <- paste0(" Tempo da carga: ", format(round(as.numeric(Sys.time()-ini0, units = "mins"), 3), nsmall = 3), " minutos;", " upload total: ", format(obsize, units = "Mb", nsmall = 4))

    if(verbose) cat(timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                              prefix =  "log: ", suffix = verb, quiet = TRUE), "... \n")

    return(invisible(out))
  }
}


#' Conta total de digitos antes ou sepois da virgula
#'
#' @param x Vetor numerico
#' @param depois Se TRUE, retorna total de digitos depois da virgula, caso contrario retorna total de digitos da parte inteira.
#' @export
count_numeric_digits <- function(x, depois = TRUE) {
  stopifnot(inherits(x, c("numeric","integer")))
  if ((x %% 1L) != 0) {
    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")[[1]]
    if(length(strs) > 1){
      n <- if(depois){
        nchar(strs[2])
      } else {
        nchar(strs[1])
      }
    } else {
      n <- if(depois){
        0L
      } else {
        nchar(x)
      }
    }
  } else {
    n <- if(depois){
      0L
    } else {
      nchar(x)
    }
  }
  return(n)
}


#' Calculates the biggest precision and scale that occurs in a numeric vector
#'
#' @description CREDITOS: https://stackoverflow.com/questions/42122215
#' The scale of a numeric is the count of decimal digits in the fractional part (to the right of the decimal point).
#' The precision of a numeric is the total count of significant digits in the whole number,
#' that is, the number of digits to both sides of the decimal point.
#'
#' To create a suitable numeric data type in a SQL data base use the returned \code{SQL.precision} which
#' is defined by \code{max(precision, non.fractional.precision + scale)}.
#'
#' @param x numeric vector
#'
#' @return A list with four elements:
#'         precision (total number of significant digits in the whole number),
#'         scale (number of digits in the fractional part),
#'         non.fractional.precision (number of digits at the left side and SQL precision.
#'
#' @details NA will be counted as precision 1 and scale 0!
#' @importFrom stringi stri_length
#' @examples
#' x <- c(0, 7654321, 54321.1234, 321.123, 321.123456789, 54321.1234, 100000000000, NA)
#' numeric_precision_and_scale(x)
#' numeric_precision_and_scale(c(10.0, 1.2))
#' @export
numeric_precision_and_scale <- function(x) {
  old.scipen <- getOption("scipen")
  # Overwrite options
  options(scipen = 999)   # avoid scientific notation when converting numerics to strings
  # Extract the decimal point character of the computer's current locale
  decimal.sign <- substr( 1 / 2, 2, 2)
  x.string <- as.character(x[!is.na(x)])
  if (length(x.string) > 0) {
    # calculate
    precision <- max(stringi::stri_length(sub(decimal.sign, "", x.string, fixed = TRUE)))
    scale <- max(stringi::stri_length(sub(paste0("\\d+\\", decimal.sign, "?(.*)$"), "\\1", x.string)))
    non.fractional.precision <- max(trunc(log10(abs(x))) + 1, na.rm = TRUE)
    SQL.precision <- max(precision, non.fractional.precision + scale)
    # Reset changed options
    options(scipen = old.scipen)
  } else {
    precision <- 1
    scale <- 0
    non.fractional.precision <- 1
    SQL.precision <- 1
  }
  return(list(precision = precision,
              scale = scale,
              non.fractional.precision = non.fractional.precision,
              SQL.precision = SQL.precision)
  )
}


#' Detecta tipos e formata para o SQL Server
#'
#' @description Esta funcao e um trator que vasculha todos os dados do objeto
#'  informado buscando identificar os tipos de dados e seus tamanhos maximos
#'  com base no total de caracteres retoando a formatacao ideal para o SQL.
#'
#' @details O objetivo principal dela e informar para a funcao \code{\link{rnp_db_write}} ou
#' qualquer outra da familia DBI que possua o argumento field.types ou data.types,
#' por exemplo \code{\link[DBI]{dbWriteTable}}, o formato correto para o comando de CREATE TABLE do SQL.
#' Isso e uma mao na roda para que o SQL crie a tabela ja tipada com os formatos corretos
#' evitando uso excessivo de banco e posterior trabalho de tipagem em SQL que
#' nem sempre e amigavel.
#' Se os dados sao numericos o R busca a quantidade de caracteres antes da virgula
#' (escala) e o total depois da virgula (precisao) e cria a formatacao correspondente
#' ao SQL Server. Se os dados sao do tipo charactere com mais de 7999 caracteres
#' o formato SQL sera \code{VARCHAR(MAX)}. O processo de scan dos dados pode ser lento
#' em tabelas grandes em linhas ou colunas. Para tentar melhorar a performance nestes
#' casos, foi inserido na funcao a possibilidade de rodar em paralelo com apoio do
#' pacote future. Veja \code{\link[future]{multiprocess}} para mais detalhes.
#' @param con Conexao criada via \code{\link[DBI]{dbConnect}}.
#' @param data Objeto para obter tipos dos dados.
#' @param parallel Se TRUE, ativa processamento em paralelo via future. Veja mais em \code{\link[future]{plan}}.
#' @param aloc_cpu Percentual de toda a CPU utilizada no processamento em paralelo. Valor entre 0.01 e 0.80. Ex. se tiver 10 cores e aloc_cpu = 0.10, roda apenas com um core.
#' @param sample_size Para detectar tipos com base em uma amostra definida pelo usuario. Aceita quantidade de linhas (valor entre 1 e nrow(data)) ou percentual da base (valor entre 0.01 e 1.00).
#' @param table Se TRUE, retorna tabela com dados dos tipos detecados para o objeto data.
#' @param verbose Se TRUE, exibe historico de execussao.
#' @param max_mem_size Total maximo de memoria fisica alocada para trabalhar em paralelo. Padrao 2Gb (2*1024^3). Veja \code{\link[future]{future.options}} para mais detalhes.
#' @importFrom utils timestamp memory.limit
#' @importFrom dplyr tibble as_tibble all_of select left_join mutate case_when transmute
#' @importFrom stringr str_replace_all
#' @importFrom future plan sequential availableCores multiprocess
#' @importFrom purrr map_chr
#' @importFrom furrr future_map_dbl future_map_chr future_map
#' @examples \dontrun{
#' rnp_db_types(con, mtcars)
#' rnp_db_types(con, mtcars, parallel = TRUE, table = TRUE)
#' }
#' @export
rnp_db_types <- function(con, data, sample_size = Inf, parallel = FALSE, aloc_cpu = 0.2, table = FALSE, verbose = TRUE, max_mem_size = 2*1024^3){

  obs <- object.size(data)
  if(as.numeric(obs / 1024^3) > 3*max_mem_size){
    if(as.numeric(obs / 1024^3)*3 >= memory.limit()/1024^2 || 3*max_mem_size >= memory.limit()/1024^2){
      stop("log: sua tabela eh muito grande para ser processada. Quebre em partes menores e tente novamente!\n")
    } else {
      options(future.globals.maxSize = ceiling(as.numeric(obs / 1024^3)*3))
    }
  } else {
    options(future.globals.maxSize = max_mem_size)
  }

  stopifnot(sample_size > 0)
  if (!DBI::dbIsValid(con) || missing(con)) {
    stop(cat("log: Conexao inativa ou nao informada, favor reconectar!\n"))
  }

  if(verbose){
    utils::timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"), prefix = "log: ", suffix = " inicio.")
  }

  if(aloc_cpu > 0.8){
    if(verbose){
      cat("log: O maximo de cpu eh 0.8. Precisa mesmo disso tudo?\n")
    }
    aloc_cpu <- 0.8
  } else if(aloc_cpu < 0 | is.na(aloc_cpu)){
    stop(cat("log: Quanto de cpu quer utilizar para rodar em paralelo, valor entre [0.01-0.80]?\n"))
  }

  sample_size_n <- sample_size_p <- c()

  if(sample_size >= length(data)){
    sample_size_n <- if(!is.null(dim(data))){
      nrow(data)
    } else if(is.null(dim(data))){
      length(data)
    }
  } else if(sample_size %% 1 == 0){
    sample_size_n <- sample_size
  } else if(sample_size %% 1 != 0){
    sample_size_p <- sample_size
  }

  data <- if(is.null(dim(data))){
    dplyr::tibble(X = data)
  } else {
    dplyr::as_tibble(data)
  }

  ## Amostra
  data <- if(!is.null(sample_size_n)){
    dplyr::sample_n(data, size = sample_size_n)
  } else if(!is.null(sample_size_p)){
    dplyr::sample_frac(data, size = sample_size_p)
  } else {
    data
  }

  if(verbose){
    utils::timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                     prefix = "log: ",
                     suffix = paste0(" processando ", nrow(data), " linhas e ", ncol(data), " colunas."))
  }

  if(parallel){
    # Compromete aloc_cpu% do CPU para a operacao
    n_workers <- ceiling(aloc_cpu*future::availableCores())
    future::plan(future::multiprocess(), workers = n_workers)
  }

  ## Detecta tipos dos dados considerando a codificacao do banco, obtido via con
  ## purrr::map_chr
  tipos_sql <- purrr::map_chr(data, function(i) {
    x <- try(DBI::dbDataType(con, i))
    y <- if(inherits(x, "try-error")){
      try(DBI::dbDataType(con, iconv(i, to = "ASCII//TRANSLIT")))
    } else {
      x
    }
    return(y)
  })

  ## Detecta tamanhos dos dados com base no total de caracteres
  ##  purrr::map_dbl
  tamanhos_r <- furrr::future_map_dbl(data, function(i){
    x <- rnp_rm_na(iconv(i, to = "ASCII//TRANSLIT"))
    y <- if(all(is.na(x))){
      5
    } else {
      er <- try(max(nchar(x), na.rm = TRUE))
      if(inherits(er, "try-error")){
        255
      } else {
        er
      }
    }
    return(y)
  }, .progress = TRUE)

  ## Detecta classes com base nos criterios do R
  classes_r <- furrr::future_map_chr(data, function(i){
    out <- class(i)[1]
    if(out == "integer64") "numeric" else out
  })

  ## Detecta numeros de acordo com tipos especificos do R
  vars_num <- names(data)[sapply(data, function(x) {
    inherits(x, c("numeric","integer64"))})]

  if(length(vars_num) == 0){
    precisao_r <- dplyr::tibble(variavel = NA_character_, precisao = NA_integer_, escala = NA_integer_)
  } else {
    ## Detecta precisao e escala compativel com os tipos aceitos no SQL Server
    precisao_r <- tibble::as_tibble(data) %>%
      dplyr::select(dplyr::all_of(vars_num)) %>%
      as.list()

    precisao_r <- furrr::future_map(precisao_r, .f = function(i){
      temp <- numeric_precision_and_scale(i)
      tibble::tibble(precisao = temp$SQL.precision,
                     escala = temp$scale)
    }, .progress = TRUE)  %>%
      dplyr::bind_rows(.id = "variavel")
  }

  ## Variaveis do tipo char maiores que 799 carateres soa tratadas como varchar(max)
  out <- data.frame(variavel = names(tipos_sql),
                    classes_r = unname(classes_r),
                    tamanhos_r = unname(tamanhos_r),
                    tipos_sql = unname(tipos_sql), stringsAsFactors = FALSE) %>%
    dplyr::left_join(precisao_r, by = "variavel") %>%
    dplyr::mutate(tipos_sql_fix = tolower(stringr::str_replace_all(tipos_sql, "[:digit:]|[:punct:]", "")),
                  fix = toupper(dplyr::case_when(tipos_sql_fix == "varchar" & tamanhos_r >= 7999 ~ paste0(tipos_sql_fix, "(MAX)"),
                                                 tipos_sql_fix == "varchar" ~ paste0(tipos_sql_fix, "(", tamanhos_r, ")"),
                                                 tipos_sql_fix == "float" ~ paste0("decimal(", precisao, ",", escala, ")"),
                                                 TRUE ~ tipos_sql
                                                 )
                                )
                  ) %>%
    dplyr::transmute(variavel, nchar_r = tamanhos_r, classe_r = classes_r,
                     classe_sql = tipos_sql, classe_sql_fix = fix)

  # Volta para sequancia e destroi os processos utilizados
  if(parallel){
    future::plan(future::sequential())
  }
  if(verbose){
    utils::timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"), prefix = "log: ", suffix = " fim.")
  }
  on.exit(future::plan(future::sequential()))
  if(table){
    return(out)
  } else {
    types <- out$classe_sql_fix
    names(types) <- out$variavel
    return(types)
  }
  on.exit(options(future.globals.maxSize = 1024^3))
}


#' Preparacao de dados para backtestes
#'
#' @description Esta funcao trata dados como CPF, CNPJ, CEP, TELEFONE, etc.
#' com base nas colunas informadas pelo usuario, para salvar no banco de dados.
#'
#' @details A funcao trata CPF e CNPJ validando os numeros e fixando quantidade de digitos.
#' Para os telefones, a funcao separa DDI, DDD e Numero criando campos com sufixo nos nomes.
#' Campos de texto sao limpos removendo-se espacos em branco ou acentos. Campos de data
#' sao validados para o formado ano-med-dia padrao do SQL. Mais de 27 tipos de padrao de data
#' sao checados. Veja \code{\link{rnp_limpa_date}} e \code{\link[lubridate]{parse_date_time}}
#' para mais detalhes. Se todos os argumentos <list_xxx_cols> forem NULL, a funcao tenta buscar CPF/CNPJ.
#'
#' @param data Objeto de dados para obter tipagem.
#' @param list_doc_cols Documento: Lista de documentos. Sao aceitos CPF e CNPJ.
#' @param list_nam_cols Nomes: Lista de campos de nomes. Aceita qualquer string.
#' @param list_tel_cols Telefone: Lista de campos de telefone. Aceita numeros e strings.
#' @param list_ema_cols E-mails: Lista de campos de emais. Aceita qualquer string.
#' @param list_cep_cols CEPs: Lista de campos de CEP. Aceita numeros e strings.
#' @param list_end_cols Enderecos: Lista de campos de enderecos. Aceita qualquer string.
#' @param list_dat_cols Datas: Lista de campos de data. Aceita qualquer tipo de data ou string de data conhecida.
#' @param list_num_cols Numeros: Lista de campos numericos ou que se parecam numeros. O R tratara como numerico.
#' @param return_all Se TRUE, Retorna dados corrigods mais dados originais. Caso contrario, somente colunas afetadas.
#' @seealso \code{\link{rnp_limpa_date}}, \code{\link{rnp_limpa_cep2}}, \code{\link{rnp_limpa_cpf_cnpj}} e \code{\link{rnp_limpa_tel}}.
#' @examples
#' \dontrun{
#' list_doc_cols <- c("customer_cpf","merchant_cnpj") # NULL
#' list_tel_cols <- c("customer_phone") # NULL
#' list_cep_cols <- c("address_zipcode","merchant_address_zipcode") # NULL
#' list_nam_cols <- c("customer_name","card_name") # NULL
#' list_end_cols <- c("address_street","address_complement","address_neighbor","address_city") # NULL
#' list_ema_cols <- c("customer_email") # NULL
#' list_dat_cols <- c("customer_created_date","order_date_utc3","dt_partition") #NULL
#' da_fix <- rnp_prep_data(da,
#'                         list_doc_cols = list_doc_cols,
#'                         list_nam_cols = list_nam_cols,
#'                         list_tel_cols = list_tel_cols,
#'                         list_ema_cols = list_ema_cols,
#'                         list_cep_cols = list_cep_cols,
#'                         list_end_cols = list_end_cols,
#'                         list_dat_cols = list_dat_cols, return_all = FALSE)
#' }
#' @export
rnp_prep <- function(data,
                     list_doc_cols = NULL,
                     list_nam_cols = NULL,
                     list_tel_cols = NULL,
                     list_ema_cols = NULL,
                     list_cep_cols = NULL,
                     list_end_cols = NULL,
                     list_dat_cols = NULL,
                     list_num_cols = NULL,
                     return_all = TRUE) {

  #colnames(data) <- tolower(colnames(data))
  columns_data <- colnames(data)
  columns_info <- unique(unlist(c(list_doc_cols, list_nam_cols, list_tel_cols,
                                  list_ema_cols, list_cep_cols, list_end_cols,
                                  list_dat_cols, list_num_cols)))

  if(!all(columns_info %in% columns_data == TRUE)){
    stop("log: As colunas (", paste0(columns_info[!(columns_info %in% columns_data)], collapse = ";"), ") nao existem na base. Verifique os nomes informados.\n")
    columns_info[!(columns_info %in% columns_data)]
  }

  aux_summary  <- function(x) sum(x, na.rm = TRUE)/length(na.exclude(x))
  aux_treshold <- function(x, treshold = 0.95) x >=  treshold

  ## Busca CPF/CNPJ, se nao for informado nenhuma coluna
  if(is.null(columns_info) | length(columns_info) == 0){
    nm_cpf <- purrr::map(data, ~is_cpf(.x), fix_na = TRUE) %>%
      purrr::map_dbl(~aux_summary(.x)) %>%
      aux_treshold(., treshold = 0.95)

    nm_cnpj <- purrr::map(data, ~is_cnpj(.x), fix_na = TRUE) %>%
      purrr::map_dbl(~aux_summary(.x)) %>%
      aux_treshold(., treshold = 0.95)
    list_doc_cols <- columns_info <-  unique(c(columns_data[nm_cpf], columns_data[nm_cnpj]))
    if(is.null(list_doc_cols) | length(columns_info) == 0) {
      stop("log: sem dados para tratar. Favor especificar na chamada da funcao.\n")
      on.exit(debuggingState(on = FALSE))
    }
  }

  db_doc <- if(!is.null(list_doc_cols)){
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_doc_cols))) %>%
      dplyr::mutate_all(.fun = ~rnp_limpa_cpf(.x))
  }

  db_names <- if(!is.null(list_nam_cols)){
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_nam_cols))) %>%
      dplyr::mutate_all(.fun = ~rnp_limpa_text(.x))
  } else {
    NULL
  }

  db_tel <- if(!is.null(list_tel_cols)){
    names_tel <- paste0(rep(list_tel_cols, each = 3), c("_ddi","_ddd","_tel"))
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_tel_cols))) %>%
      purrr::map(~rnp_limpa_tel(.x)) %>%
      dplyr::bind_cols() %>%
      purrr::set_names(., nm = names_tel)
  } else {
    NULL
  }

  db_email <- if(!is.null(list_ema_cols)){
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_ema_cols))) %>%
      dplyr::mutate_all(.fun = ~rnp_limpa_text(rnp_limpa_email(.x), char_size = "l"))
  } else {
    NULL
  }

  db_cep <- if(!is.null(list_cep_cols)){
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_cep_cols))) %>%
      dplyr::mutate_all(.fun = ~rnp_limpa_cep2(.x))
  } else {
    NULL
  }

  db_endereco <- if(!is.null(list_end_cols)){
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_end_cols))) %>%
      dplyr::mutate_all(.fun = ~rnp_limpa_text(.x))
  } else {
    NULL
  }

  db_data <- if(!is.null(list_dat_cols)){
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_dat_cols))) %>%
      dplyr::mutate_all(.fun = ~rnp_limpa_date(.x, format_out = "%Y-%m-%d", char_out = TRUE))
  } else {
    NULL
  }

  db_num <- if(!is.null(list_num_cols)){
    data %>%
      dplyr::select(dplyr::all_of(unlist(list_num_cols))) %>%
      dplyr::mutate_all(.fun = ~as_num(.x, check = TRUE, fix_na = TRUE))
  } else {
    NULL
  }

  out <- if(return_all){
    dplyr::bind_cols(
      dplyr::bind_cols(db_doc, db_names, db_tel, db_email, db_cep, db_endereco, db_data, db_num),
      data %>%
        dplyr::select(dplyr::all_of(columns_data[!(columns_data %in% columns_info)]))
    )
  } else {
    dplyr::bind_cols(db_doc, db_names, db_tel, db_email, db_cep, db_endereco, db_data, db_num)
  }
  return(out)
}

#' Leitura de dados do banco via ODBC
#'
#' @description Recebe uma conexao odbc criada por \code{\link[DBI]{dbConnect}} e passa uma query
#' para ober os resultados. Aceita todo tipo de consulta compativel com SQL server, inclusive
#' chamadas de execucao de procedures que retornam dados.
#'
#' @param con Conexao ativa odbc. Veja \code{\link[DBI]{dbConnect}} para mais detalhes.
#' @param query Consulta SQl que retorna dados.
#' @param chunk_size Total de linhas extraidas por batch.
#' @param progress Acompanha log da extracao.
#' @export
rnp_db_read <- function(con, query, chunk_size = 24999, progress = TRUE){
  if (!DBI::dbIsValid(con) || missing(con)) {
    stop(cat("log: Conexao inativa ou nao informada, favor reconectar!\n"))
  }

  if(progress){
    utils::timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"), prefix = "log: ", suffix = " inicio.")
  }

  ## Envia para o DW via dbQSendQuery()
  #sqlSendQ <- DBI::dbSendQuery(conn = con, statement = DBI::SQL(query))
  ## (+) Recupera o dado em lotes (JOIA PARA BASES GRANDES). Ex.: Extraindo de 5 em 5 linhas
  sqlGetDataSlice <- c()
  sqlSendQ <- DBI::dbSendQuery(conn = con, statement = DBI::SQL(query))

  while(!DBI::dbHasCompleted(sqlSendQ)){
    sqlGetDataSlice <- rbind(sqlGetDataSlice, DBI::dbFetch(sqlSendQ, n = chunk_size))
    if(progress){
      utils::timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"),
                       prefix = "log: ",
                       suffix = paste0(" ", DBI::dbGetRowCount(sqlSendQ), " linhas extraidas."))
    }
  }
  DBI::dbClearResult(sqlSendQ)
  if(progress){
    utils::timestamp(stamp = format(Sys.time(), "%d %b %Y, %A %H:%M:%S"), prefix = "log: ", suffix = " fim.")
  }
  return(sqlGetDataSlice)
}

#' Funcao para tratar uma coluna contendo NAs
#'
#' @author LOPES, J. E.; PLACIDO, V. B.
#' @description
#' Esta funcao realiza o tratamento de NAs de um vetor onde o tratamento depende do parametro de entrada "fun"
#' @param x Coluna que contem NAs e necessita ser corrigida
#' @param fun Tipo de tratamento a ser realizado, onde a entrada deve ser um dos seguintes: ("mean","median","min","max","tsclean")
#' @importFrom forecast tsclean
#' @seealso \code{\link{tsclean}}
#' @export
rnp_aux_imputation <- function(x, fun = "median") {
  fun <- match.arg(fun, c("mean","median","min","max","tsclean"))
  stopifnot(inherits(x, c("ts","xts","zoo","integer","numeric")))

  x <- as.numeric(x)
  calc <- function(FUN, x, na.rm = TRUE){
    FUN(x, na.rm = TRUE)
  }

  if(fun == "tsclean"){
    x <- x %>%
      forecast::tsclean(x, replace.missing = TRUE)
  } else {
    x[which(is.na(x))] <- calc(get(fun), x)
  }
  return(x)
}


#' Inferencia de performance por Parcelamento
#'
#' @description Utiliza a tecnica do parcelamento para inferir a performance de uma base de
#' Cancelados/Rejeitadoscom base na tabela dos Aprovados contendo a marcacao de maus.
#' A tecnica do parcelamento pode ser compreendida no trabalho de Ash, D e Meester, S. (2002)
#'  - Best Practices in Reject Inferencing. A estrategia e a seguinte:
#' \itemize{
#' \item{gera faixas de percentis dos dados ordenando por score na base de aprovados (tabelao);}
#' \item{calcula taxa de maus em cada faixa de score;}
#' \item{marca a base dos Reprovados nas mesmas faixas de score;}
#' \item{faz amostragem aleatoria simples em cada faixa da base dos Reprovados tomando como
#'  tamanho da amostra o percentual de maus das faixas obtidas da base dos Aprovados;}
#' \item{em cada amostra, fixa-se como mau todas as observacoes obidas;}
#' \item{recompoe a base dos Cancdelados com a identificacao das transacoes que foram marcadas
#'  como mau na amostragem.}
#' }
#' @param dados_apa base de dados dos aprovados, precisa ter score e marcacao de maus para construir o tabelao.
#' @param dados_rep base de dados dos aprovados, precisa ter score e marcacao de maus para construir o tabelao.
#' @param variavel_mau nome da variavel que contem os codigo de status dos pedidos.
#' @param variavel_score nome do campo que possui o score do pedido. Deve ser numerico.
#' @param variavel_valor nome do campo com valor total do pedido. Deve ser numerico.
#' Se nao tiver deixe NULL, padrao.
#' @param nquebras total de quebras da base de dados com base na quantidade de pedidos (volume)
#' @param digits total de digitos para as estatisticas na tabela de saida
#' @param credito Se TRUE, gera o tabelao em ordem inversa, ou seja maiores escores
#' sao melhores que menores scores. O processo inverso do tabelao de scores de fraude.
#' @param exclusoes vetor numerico de codigos de status a excluir da base de dados
#' @param define_mau vetor de codigos de status a considerar como mau.
#' Padrao de modelagem e (10,22,32,33,34,36,43,103,123,160,161,191,203,204)
#' @param return_all se FALSE retorna tabelao dos aprovados, tabelao dos inferidos e
#' base dos reprovados com marcacao dos inferidos. Caso contrario, retorna apenas
#' tabelao dos inferidos.
#' @param seed Padrao 1983. Altamente recomendado alterar essa semente a gosto para
#'  tornar sua amostragem replicavel no futuro.
#' @return Se return_all == TRUE retorna tabelao dos aprovados, tabelao dos inferidos e
#' base dos reprovados com marcacao dos inferidos. Caso contrario, retorna apenas
#' tabelao dos inferidos.
#' @import dplyr purrr
#' @examples
#' \dontrun{
#' require(rnpextras)
#' out <- rnp_inferencia_rep_parcelamento(dados_apa = dados_apa,
#'                                        dados_rep = dados_rep,
#'                                        variavel_mau = "cbk_fraude",
#'                                        variavel_score = "score",
#'                                        nquebras = 20,
#'                                        define_mau = 1,
#'                                        return_all = TRUE,
#'                                        seed = 1983)
#'}
#' @export
rnp_inferencia_rep_parcelamento <- function(dados_apa,
                                            dados_rep,
                                            variavel_mau = "status_id_atual",
                                            variavel_score = "score",
                                            variavel_valor = NULL,
                                            nquebras = 20,
                                            define_mau = c(10, 22, 32, 33, 34, 36, 43, 103, 123, 160, 161, 191, 203,204),
                                            digits = 4,
                                            credito = FALSE,
                                            exclusoes = NULL,
                                            return_all = FALSE,
                                            seed = 1983) {

  # Gera tabelao para a base dos Aprovados/Conhecidos
  tbapa <- rnpextras::rnp_tabelao_score(dados = dados_apa,
                                   variavel_mau = variavel_mau,
                                   variavel_score = variavel_score,
                                   variavel_valor = variavel_valor,
                                   nquebras = nquebras,
                                   define_mau = define_mau,
                                   credito = credito,
                                   digits = 6)

  # Prepara sintaxe para aplicar as mesmas faixas obtidas obtidas via tabe
  s_min  <- tbapa %>% dplyr::pull(min_score)
  s_max  <- tbapa %>% dplyr::pull(min_score) %>% dplyr::lead(default = 1)
  quebra <- tbapa %>% dplyr::pull(quebras)
  rangem <- c(1,length(s_max))
  score  <- dados_rep %>% dplyr::pull(!!rlang::sym(variavel_score))

  inicio <- paste0("score >= ", 0.0, " & score < ", min(s_max), " ~ ", 1, collapse = " , ")
  meio   <- paste0("score >= ", s_min[-rangem], " & score < ", s_max[-rangem], " ~ ", quebra[-rangem], collapse = " , ")
  fim    <- paste0("score >= ", s_min[rangem[2]], " ~ ", quebra[rangem[2]], collapse = " , ")

  string_flag <- paste0("dplyr::case_when(", paste0(c(inicio, meio, fim),  collapse = " , "), ", TRUE ~ NA_real_)")


  dados_rep <- dados_rep %>%
    dplyr::ungroup() %>%
    dplyr::mutate(quebras_rep := !!rlang::parse_quo(string_flag, env = rlang::caller_env()),
                  id = 1:nrow(.)) %>%
    dplyr::select(id, dplyr::everything())

  dados_apa <- dados_apa %>%
    dplyr::ungroup() %>%
    dplyr::mutate(quebras_apa := !!rlang::parse_quo(string_flag, env = rlang::caller_env()),
                  id = 1:nrow(.),
                  flag_amostra = 0,
                  status_inferido = 1
    )

  # Amostragem da base de dos cancelados que devem ser marcados como maus.
  total_amostras <- tbapa %>%
    dplyr::ungroup() %>%
    dplyr::transmute(quebras_apa = quebras, pct_maus = p_maus) %>%
    split(., .[['quebras_apa']]) %>%
    purrr::map(function(i){
      i %>% dplyr::pull(pct_maus)
    })

  #total_amostras <- total_amostras[total_amostras > 0]
  faixas <- as.numeric(names(total_amostras))
  {
    set.seed(seed = seed)
    base_amostras <- purrr::map_df(faixas, function(i){
      dados_rep %>%
        dplyr::ungroup() %>%
        dplyr::filter(quebras_rep == i) %>%
        dplyr::sample_frac(size = total_amostras[[i]]) %>%
        dplyr::mutate(flag_amostra = 1, status_inferido = 1)
    }, .id = NULL)
  }
  # Substitiuir a amostra na base dos REP

  base_rep_inferido <- dplyr::anti_join(x = dados_rep, y = base_amostras, by = "id") %>%
    dplyr::mutate(status_inferido = as.character(!!rlang::sym(variavel_mau)),
                  flag_amostra = 0) %>%
    dplyr::bind_rows(
      base_amostras %>%
        dplyr::mutate(status_inferido = as.character(status_inferido))
    ) %>%
    dplyr::arrange(id)

  tbrep <- rnpextras::rnp_tabelao_score(dados = base_rep_inferido,
                                   variavel_mau = "status_inferido",
                                   variavel_score = variavel_score,
                                   nquebras = nquebras,
                                   define_mau = define_mau,
                                   credito = credito, digits = 6)

  # Erro amostral com base no total de maus.
  tbrep <- tbrep %>%
    dplyr::mutate(erro_amostral = sqrt(((sum(t_maus)*1.96^2*p_maus*(1-p_maus)) - (t_maus*1.96^2*p_maus*(1-p_maus))) / (t_maus*(sum(t_maus)-1))),
                  erro_amostral = dplyr::if_else(is.na(erro_amostral), 0, round(erro_amostral, digits)))

  if(return_all){
    return(invisible(list(tabelao_aprovados = tbapa,
                          tabelao_rep_inferido = tbrep,
                          base_inferencia = base_rep_inferido)))
  } else {
    return(tbrep)
  }
}

#' Split tabela de dados por linhas
#'
#' @description Recebe uma tabela das classes "data.frame","data.table","tbl_df", "tbl"
#' e quebra em uma lista de subtabela de acordo com o total de linhas desejado. Se na
#' ultima parte nao houber linhas suficientes para fecar nrow, retorna o resto das linhas.
#'
#' @param x tabela de dados 2 x 2
#' @param nrows numero de linhas desejado por subtabela
#' @param keep_group deseja que a coluna que identifica os lotes seja retornada?
#' padrao FALSE.
#' @param append se TRUE, mantem campo keep_group e junta os dados gerando uma unica tabela. Padrao = FALSE
#' @return lista de subtabelas
rnp_split_data_table <- function(x, nrows = 5000, keep_group = FALSE, append = FALSE){
  if(!inherits(x, c("data.frame","data.table","tbl_df", "tbl"))){
    stop(message("cat: passe uma tabela 2 x 2 que nao seja da class 'matrix'"))
  }

  if(append){
    x %>%
      dplyr::group_split(group_id = (dplyr::row_number()-1) %/% nrows, .keep = TRUE) %>%
      data.table::rbindlist(idcol = group_id)
  } else {
    x %>%
      dplyr::group_split(group_id = (dplyr::row_number()-1) %/% nrows, .keep = keep_group)
  }
}
