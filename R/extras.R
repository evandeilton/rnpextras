#' Tabela de frequencias dupla entrada.
#' @param data tabela de dados.
#' @param x nome da variavel x. Obrigatorio.
#' @param y nome da variavel y. Pode ser NULL.
#' @param sortclass Se TRUE ordena as categorias de x e/ou y.
#' @param transpor Se TRUE, faz a transposicao das estatisticas gerando um data.frame vertical.
#' @details faz tabela de dupla entrada com base no dados de entrada.
#' @return data.frame
#' @importFrom dplyr group_by count ungroup mutate sym
#' @importFrom tidyr pivot_longer
#' @examples
#' \dontrun{
#' require(rnpextras)
#' data(mtcars)
#' rnp_2freq(mtcars, "am")
#' rnp_2freq(mtcars, "am", transpor = TRUE)
#' rnp_2freq(mtcars, "am", "cyl")
#' rnp_2freq(mtcars, "am", "cyl", transpor = TRUE)
#' }
#' @author LOPES, J. E
#' @export
rnp_2freq <- function(data, x, y = NULL, sortclass = FALSE, transpor = FALSE){

  if(is.null(x) & is.null(y)){
    stop("Informe ao menos uma variavel para calcular suas frequencias.")
  } else if(is.null(x)) {
    stop("Informe a variavel x para calcular suas frequencias.")
  }

  aux_freq <- function(data, x, y){
    x <- if(!is.null(x)){
      dplyr::sym(x)
    }
    y <- if(!is.null(y)){
      dplyr::sym(y)
    }
    if(length(as.character(c(x, y))) < 2){
      data %>%
        dplyr::group_by({{x}}, {{y}}) %>%
        dplyr::count(name = "fa", sort = sortclass) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(fr = fa / sum(fa),
                      faa = cumsum(fr),
                      fra = cumsum(fa))
    } else {
      data %>%
        dplyr::group_by({{x}}, {{y}}) %>%
        dplyr::count(name = "fa", sort = sortclass) %>%
        dplyr::group_by({{x}}) %>%
        dplyr::mutate(fr = fa / sum(fa),
                      faa = cumsum(fr),
                      fra = cumsum(fa))
    }
  }
  if(transpor){
    aux_freq(data, x, y) %>%
      tidyr::pivot_longer(cols = c("fa","fr","faa","fra"),
                          names_to = "est", values_to = "valor")
  } else {
    aux_freq(data, x, y)
  }
}


#' Sumario estatistico
#' @param x vetor numerico de entrada
#' @param digits total de digitos decimais na saida
#' @details Calcula estatisticas descritivas de um vetor numerico informado
#' @return Vetor nomeado
#' @author LOPES, J. E
#' @examples
#' \dontrun{
#' require(rnpextras)
#' data(dm_ies)
#' rnp_summary(dm_ies$VL_RECEITA_PROPRIA)
#' sapply(mtcars, rnp_summary)
#' }
#' @export
rnp_summary <- function(x, digits = 4) {
  o <- c(N        = length(x),
         Soma     = sum(x, na.rm = TRUE),
         Nmis     = sum(is.na(x)),
         Min      = min(x, na.rm = TRUE),
         Q1       = unname(quantile(x, probs = 0.25, na.rm = TRUE)),
         Media    = mean(x, na.rm = TRUE),
         Mediana  = median(x, na.rm = TRUE),
         Q3       = unname(quantile(x, probs = 0.75, na.rm = TRUE)),
         Max      = max(x, na.rm = TRUE),
         DevPad   = sd(x, na.rm = TRUE),
         IQR      = stats::IQR(x, na.rm = TRUE),
         cv       = sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE))
  return(round(o, digits = digits))
}

#' Sumario estatistico geral
#' @param base data.frame de entrada
#' @details Calcula estatisticas descritivas para todos os vetores numpericos de uma base de dados.
#' Se tiver variaveis categoricas, a funcao os ignora.
#' @return data.frame com as estatisticas por variaveis.
#' @author LOPES, J. E
#' @examples
#' \dontrun{
#' require(rnpextras)
#' data(dm_ies)
#' tmp <- rnp_summary_all(dm_ies$VL_RECEITA_PROPRIA)
#' # Variaveis numericas
#' head(tmp$num)
#' # Variaveis categoricas
#' head(tmp$cat)
#' }
#' @export
rnp_summary_all <- function(base){
  cl <- sapply(base, function(x) class(x)[1])
  nu <- cl[which( cl %in% c("numeric","integer","ts","xts","mts"))]
  ca <- cl[which(!cl %in% c("numeric","integer","ts","xts","mts"))]
  p1 <- p2 <- NULL
  if(length(cl) > 0){
    p1 <- do.call("rbind",
                  lapply(names(nu), function(i) {
                    data.frame(variavel = i, t(rnp_summary(base[,i])), stringsAsFactors = FALSE)
                  }))
  }
  if(length(ca) > 0){
    p2 <- do.call("rbind",
                  lapply(names(ca), FUN = function(i) {
                    data.frame(variavel = i, rnp_freq(base[,i]), stringsAsFactors = FALSE)
                  }))
  }
  return(list(num = p1, cat = p2))
}


#' Extrai atributos de um objeto
#' @param obj qualquer objeto em R
#' @param ests Se TRUE, retorna estatisticas do vetor ou das variaveis
#' @return data.frame com lista de atributos
#' @author LOPES, J. E
#' @export
rnp_atributos <- function(obj, ests = FALSE) {
  o <- data.frame(classeBase  = paste0(class(obj)[1], collapse = "|"),
                  comprimento = if (is.null(dim(obj))) length(obj) else paste(dim(obj)[1], "linhas e", dim(obj)[2], "colunas"),
                  variaveis   =   if (is.null(dim(obj))) 0 else paste0(colnames(obj)),
                  classeVars  = if (is.null(dim(obj))) 0 else sapply(obj, class)
  )
  rownames(o) <- NULL
  if(ests) {
    logico <- c("numeric","integer","ts","xts","zoo")
    p1 <- p2 <- c(variaveis = NA, Nmis = NA, Min = NA, Q1 = NA, Media = NA, Mediana = NA, Q3 = NA, Max = NA, DevPad = NA, IQR = NA, cv = NA)
    a <- if(is.null(dim(obj)) & class(obj)[1] %in% logico){
      cbind(o, t(rnp_summary(obj))[,-c(1,2)])
    } else if(is.null(dim(obj)) & !class(obj)[1] %in% logico){
      cbind(o, Nmis = sum(is.na(obj)))
    } else if (!is.null(dim(obj))){
      num <- names(obj)[ sapply(obj, class) %in% logico]
      cha <- names(obj)[!sapply(obj, class) %in% logico]
      if(length(num) > 0){
        p1  <- rnp_summary_all(obj[,num])[[1]]
        p1  <- data.frame(variaveis = names(p1), t(p1)[,-c(1,2)])
      }
      if(length(cha) > 0){
        p2  <- sapply(obj[,cha], function(i) sum(is.na(i) | i %in% c(NULL, NaN, NA, -Inf, +Inf)))
        p2  <- data.frame(variaveis = names(p2), Nmis = p2, Min = NA, Q1 = NA, Media = NA, Mediana = NA, Q3 = NA, Max = NA, DevPad = NA, IQR = NA, cv = NA)
      }
      merge(o, rbind(p1, p2), by = "variaveis")
    } else {
      o
    }
    return(a)
  } else {
    return(o)
  }
}

#' Aplica classes na base do INEP correspondente
#' @description Esta funcao recebe como entrada a base de dados do censo do INEP,
#' podendo ser "DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE" ou "DM_ALUNO" e
#' sua base de classes extraidas com a funcao 'rnp_get_classes_inep' e aplica
#' na base apenas para as variaveis que possuem descricao de classes.
#' @details Se uma variavel nao possui descricao de classe entao e ala nao e processada
#' o data.frame de saida possui novas colunas com prefixo "_DESC" que possui a descricao
#' da categoria e ao lado a variavel original
#' @param base base de dados censo INEP
#' @param classes base de dados com informacoes das classes obtidas do dicionario de dados do INEP.
#' @author LOPES, J. E.
#' @examples
#' \dontrun{
#' nn <- c("DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE")
#' L <- plyr::llply(nn[1], function(base){
#'   classes <- rnp_get_classes_inep(caminho = "Dicionario_de_Dados.xlsx",
#'                                   aba = base, retorna_lista = FALSE)
#'   base <- rnp_read(base = paste0("Dados/CSV/", base, ".CSV"),
#'                    sep = "|",
#'                    dec = ".",
#'                    header = TRUE,
#'                    encoding = "Latin-1",
#'                    verbose = FALSE,
#'                    showProgress = FALSE)
#'   oo <- rnp_aplica_classes(base = base, classes = classes)
#'   return(oo)
#' }, .progress = "text")
#' names(L) <- nn
#' }
#' @export
rnp_aplica_classes <- function(base, classes){
  # Seleciona nomes da base de dados. Vamos tratar apenas os nomes que das
  # variaveis que possuem descricao de classes
  nm_da <- colnames(base)

  # Separando apenas as variaveis que possuem categorias com descricao.
  # na base de classes
  nm_cl <- unname(sapply(split(classes, classes$VAR_NOME), function(i){
    tmp <- if(nrow(i) == 1) {
      NA
    } else {
      unique(i$VAR_NOME)
    }
    tmp
  }))
  nm_cl <- as.character(na.exclude(nm_cl))

  # Criando novas variaveis com as descricoes das categorias

  lda <- lapply(nm_cl, function(i, ...){
    va <- base %>%
      dplyr::select(i)

    cl <- classes %>%
      dplyr::filter(VAR_NOME == i) %>%
      dplyr::transmute(VAR_CATEGORIA, VAR_DESCRICAO) %>%
      magrittr::set_colnames(., c(colnames(va)[1], paste0("DESC_",colnames(va)[1])))

    va <- va %>%
      dplyr::left_join(cl, by = i)
  })

  out <- dplyr::bind_cols(base[,!colnames(base)%in%nm_cl],
                          dplyr::bind_cols(lda))
  return(out)
}

#' Obtem as classes das variaveis censo INEP
#' @description
#' Obtem as classes das variaveis do diconario de dados do censo INEP.
#' @details
#' Exige que passe o caminho para o dicionario de dados em excel e o nome da
#' aba correspondente ou o numero da mesma, ex. 1 = primeira aba e assim por diante.
#' A funcao faz a leitura dos dados utilizando 'read_excel' e retorna
#' um data.frame com 4 aolunas contendo ordem, nome, categoria e descricao
#' para todas as variaveis da base informada.
#' OBS.: Testado apenas nos dados do censo de 2017.
#' @param caminho caminho do arquivo Excel do dicionario de dados do INEP
#' @param aba aba da planilha correspondente aos dados que deseja
#' @param pula_linha quantidade de linhas que deseja pular
#' @param retorna_lista TRUE se quer obter uma lista de data.frames, sendo
#' um para cada variavel ou base ja agregada.
#' @author LOPES, J. E.
#' @examples
#' \dontrun{
#' nn <- c("DM_CURSO","DM_IES","DM_LOCAL_OFERTA","DM_DOCENTE")
#' L <- plyr::llply(nn[1], function(base){
#'   classes <- rnp_get_classes_inep(caminho = "Dicionario_de_Dados.xlsx",
#'                                   aba = base, retorna_lista = FALSE)
#'   base <- rnp_read(base = paste0("Dados/CSV/", base, ".CSV"),
#'                    sep = "|",
#'                    dec = ".",
#'                    header = TRUE,
#'                    encoding = "Latin-1",
#'                    verbose = FALSE,
#'                    showProgress = FALSE)
#'   oo <- rnp_aplica_classes(base = base, classes = classes)
#'   return(oo)
#' }, .progress = "text")
#' names(L) <- nn
#' }
#' @export
rnp_get_classes_inep <- function(caminho, aba = 1, pula_linha = 1, retorna_lista = FALSE){
  # Dicionario de dados
  dic_dm_aluno <- readxl::read_excel(path = caminho,
                                     sheet = aba, col_names = TRUE, skip = pula_linha, trim_ws = TRUE) %>%
    dplyr::select(1:7)
  # Seleciona colunas de interesse e arruma as
  tp <- dic_dm_aluno %>%
    dplyr::select(1:6) %>%
    dplyr::filter(!is.na("ORD")) %>%
    dplyr::filter(!is.na("NOME DA VARIAVEL")) %>%
    magrittr::set_names(., c("ORDEM","VAR_NOME","VAR_DESCRICAO","VAR_TIPO","VAR_TAMANHO","VAR_DESCRICAO_CATEGORIAS")) %>%
    dplyr::mutate(VAR_DESCRICAO_CATEGORIAS_FIX = stringr::str_replace_all(VAR_DESCRICAO_CATEGORIAS, pattern = "\\r|\\n", replacement = "|"))

  li <- lapply(split(tp, tp$VAR_NOME), function (i) {
    temp <- i %>%
      dplyr::select(ORDEM, VAR_NOME, VAR_DESCRICAO_CATEGORIAS_FIX)

    o <- if(nrow(temp) == 0) {
      data.frame(ORDEM = as.numeric(temp$ORDEM), VAR_NOME = temp$VAR_NOME, VAR_CATEGORIA = as.numeric(0), VAR_DESCRICAO = as.character(0))
    } else {
      va <- stringr::str_squish(unlist(stringr::str_split(temp$VAR_DESCRICAO_CATEGORIAS_FIX, "\\|\\|")))
      de <- stringr::str_extract(va, "^[0-9]+")
      data.frame(ORDEM = as.numeric(temp$ORDEM), VAR_NOME = temp$VAR_NOME, VAR_CATEGORIA = as.numeric(de), VAR_DESCRICAO = as.character(va))
    }
    return(o)
  })
  oo <- if(retorna_lista) {
    li
  } else {
    suppressWarnings(dplyr::bind_rows(li) %>%
                       dplyr::arrange(ORDEM))
  }
  return(oo)
}


#' Trata erros
#' @description Trata qualquer erro em chamadas de funcoes. Se der erro, sai um objeto da classe 'try-error'
#' @param code Qualquer codigo R passado
#' @param silent Rodar silenciosamente? TRUE ou FALSE
#' @return Objeto de entrada
#' @author LOPES, J. E
#' @export
rnp_try_error <- function(code, silent = TRUE) {
  W <- NULL
  w.handler <- function(w) {
    W <<- w
    invokeRestart("muffleWarning")
  }
  withCallingHandlers(tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent)
      message(c)
    invisible(structure(msg, class = "try-error"))
  }), warning = w.handler)
}


#' Download dados do INEP
#' @description Recebe uma data ou uma url do portal de microdados do INEP e
#' baixa os dados no local em que a sessao do R foi carregada.
#' @details  Quando passado apenas uma data a funcao baixa os dados do INEP
#' da data correspondente para o censo de educacao superior, desde que esta
#' data seja entre 1995 e 2017. Caso a data esteja fora deste intervalo, a funcao
#' baixara os dados do censo de 2017.
#' Se uma url com final .zip for passada, a funcao ignora a data, caso tenha
#' sido informada e baixa o arquivo da url informada.
#' @param ano ano formato numerico, ex. 2010, 2016
#' @param url url, vetor ou lista de urls completas para baixar os dados.
#' @param salvar caminho onde deseja salvar os dados baixados. Se NULL, a funcao
#' baixa na pasta onde a sessao ativa do R foi iniciada. Execute \code{\link{getwd}} para saber o local.
#' @return arquivo .zip ou da extencao da url passada.
#' @examples
#' \dontrun{
#' require(rnpextras)
#' require(dplyr)
#' url_in = 'http://download.inep.gov.br/microdados/micro_censo_edu_superior1995.zip'
#' rnp_get_inep_censo(url = url_in)
#' rnp_get_inep_censo()
#' }
#' @author LOPES, J. E.
#' @export
rnp_get_inep_censo <- function(ano = 2017, url = NULL, salvar = NULL){
  fn_aux <- function(url, file){
    f = RCurl::CFILE(file, mode="wb")
    a = RCurl::curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
    RCurl::close(f)
    return(a)
  }
  s <- seq(1995, 2017, by = 1)
  if(is.null(ano) & is.null(url)){
    cat("Baixando dados do censo de 2017!\n")
    anos <- max(s)
    nm <- paste0("base_", anos, ".zip")
  } else if(is.null(ano) & !is.null(url)){
    cat("Ignorando a data, pois voce passou urls.\n")
    anos <- NULL
    nm <- sapply(url, function(i){
      o <- unlist(stringr::str_split(i, pattern = "\\/"))
      unname(o[length(o)])
    })
  } else if(!is.null(ano) & is.null(url)) {
    if(!ano %in% s) {
      stop("Passe valores para anos entre 1995 e 2017 ou uma url do arquivo .zip valida.\n")
    } else {
      anos <- ano
      nm <- paste0("base_", anos, ".zip")
    }
  } else if(!is.null(ano) & !is.null(url)){
    cat("Ignorando a data, pois voce passou urls.\n")
    anos <- NULL
    nm <- sapply(url, function(i){
      o <- unlist(stringr::str_split(i, pattern = "\\/"))
      unname(o[length(o)])
    })
  } else {
    anos <- ano
    nm <- paste0("dados_", anos, ".zip")
  }
  url_in <- c('http://download.inep.gov.br/microdados/micro_censo_edu_superior1995.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1996.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1997.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1998.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior1999.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2000.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2001.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2002.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2003.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_educacao_superior_2004.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_educacao_superior_2005.zip',
              'http://download.inep.gov.br/microdados/microdados_educacao_superior_2006.zip',
              'http://download.inep.gov.br/microdados/microdados_educacao_superior_2007.zip',
              'http://download.inep.gov.br/microdados/micro_censo_edu_superior2008.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2009.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2010.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2011.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2012.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2013.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2014.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2015.zip',
              'http://download.inep.gov.br/microdados/microdados_censo_superior_2016.zip',
              'http://download.inep.gov.br/microdados/microdados_educacao_superior_2017.zip')

  inep_url <- data.frame(url_in = as.character(url_in),
                         ano_in = as.numeric(stringr::str_extract(url_in, "[0-9]{4}")), stringsAsFactors = FALSE)

  get_url <- if(!is.null(url)) {
    url
  } else {
    inep_url %>%
      dplyr::filter(ano_in %in% anos) %>%
      dplyr::select(url_in) %>%
      unlist() %>%
      unname()
  }
  lapply(seq_along(get_url), function(i) {
    fn_aux(url = get_url[[i]], file = ifelse(is.null(salvar), nm[[i]], paste0(salvar, nm[[i]])))
  })
}

#' Estatisticas descritivas por grupo
#' @description
#' Calcula estatisticas descritivas por grupo. Ela recebe como entrada um data.frame
#' o nome da variavel numerica e um vetor ou lista de nomes das variaveis que serao utilizadas
#' como grupos. A funcao trabalha com apoio da funcao \code{plyr::\link[plyr]{ddply}} e aceita muitos
#' grupos.
#' @param base data.frame com as variaveis de entrada
#' @param variavel o nome da variavel numerica entre aspas
#' @param grupos lista ou vetor de nomes das estatisticas de agrupamento na ordem em que deseja
#' obter os resultados.
#' @param digits total de digitos para arredondar os decimais.
#' @return
#' As estatisticas de saida desta funcao sao: total (N), soma, numero de missing (Nmis),
#' minimo, primeiro quartil (Q1), mediana (Q2), terceiro quartil (Q3), maximo, desvio pacrao (devpad)
#' e coeficiente de variacao (cv) em um data.frame agrupadas conforme as classes das variaveis de
#' agrupamento.
#' @author LOPES, J. E.
#' @examples
#' rnp_summary_by(base = mtcars, variavel = "wt", grupos = "gear")
#' rnp_summary_by(base = mtcars, variavel = "wt", grupos = c("gear","cyl"))
#' rnp_summary_by(base = mtcars, variavel = "wt",
#'                grupos = list("gear","cyl"), digits = 2)
#' @export
rnp_summary_by <- function(base, variavel, grupos, digits = 3) {
  variavel <- if(length(variavel) > 1){
    stop("Informe apenas uma variavel numerica")
  } else as.character(variavel)
  if(length(grupos) > 10){
    cat("Cuidado, muitos grupos podem gerar dados confusos!")
  }
  grupos <- as.character(unlist(grupos))
  out <- plyr::ddply(.data = base,
                     .variables = grupos,
                     .fun = function(xx){
                       rnp_summary(xx[,variavel], digits = digits)
                     })
  return(out)
}

#' Dados do censo ensino superior INEP 2017 (Cursos)
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior. Nesta parte temos os dados dos cursos
#' mapeados pelo censo naquele ano.
#' Trata-se de uma amostras das variaveis, porem o total de observacoes
#' esta completo.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a funcao \code{\link{rnp_get_inep_censo}}
#' @name dm_curso
#' @docType data
#' @author INEP
#' @references \url{https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior}
#' @keywords dm_curso
#' @examples
#' require(rnpextras)
#' str(dm_curso)
NULL

#' Dados do censo ensino superior INEP 2017 (IES)
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior. Nesta parte temos os dados das IES
#' Instituicoes de Ensino Superior mapeados pelo censo naquele ano.
#' Trata-se de uma amostras das variaveis, porem o total de observacoes
#' esta completo.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a funcao \code{\link{rnp_get_inep_censo}}
#' @name dm_ies
#' @docType data
#' @author INEP
#' @references \url{https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior}
#' @keywords dm_ies
#' @examples
#' require(rnpextras)
#' str(dm_ies)
NULL

#' Dados do censo ensino superior INEP 2017 (Local da oferta)
#' @description
#' Este conjunto de dados faz parte da base de dados do INEP para o censo
#' brasileiro do ensino superior.
#' Nesta parte temos os dados dos locais de oferta do curso das
#' Instituicoes de Ensino Superior mapeados pelo censo naquele ano.
#' Trata-se de uma amostras das variaveis, porem o total de observacoes
#' esta completo.
#' Para obter dados de outros anos consulte o link do site do INEP ou
#' a funcao \code{\link{rnp_get_inep_censo}}
#' @name dm_local
#' @docType data
#' @author INEP
#' @references \url{https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-da-educacao-superior}
#' @keywords dm_local
#' @examples
#' require(rnpextras)
#' str(dm_local)
NULL

#' Estatistica de associacao
#'
#' @description Recebe dois vetores categoricos de tamanhos iguais e calcula as estatisticas
#' Qui-quadrado, V de Cramer e o coeficiante de contingencia para a tabela de contingencia
#' das duas variaveis criada pela funcao \code{\link{table}}.
#'
#' @param x variavel um
#' @param y variavel dois
#' @param ... argumentos passados para a funcao \code{\link{chisq.test}}
#' @author LOPES, J. E.
#' @examples
#' require(rnpextras)
#' rnp_associacao(dm_ies$DESC_TP_ORGANIZACAO_ACADEMICA, dm_ies$DESC_TP_CATEGORIA_ADMINISTRATIVA)
#' @export
rnp_associacao <- function(x, y, ...){
  oo <- table(x, y)
  ch <- chisq.test(oo)
  rr <- sum(ch$expected)*(min(dim(ch$expected))-1)
  vv <- sqrt(ch$statistic / rr)
  cc <- sqrt((min(dim(ch$observed)))/(min(dim(ch$observed))-1))*sqrt(ch$statistic/(ch$statistic+sum(ch$observed)))
  out <- unname(c(ch$statistic, vv, cc))
  names(out) <- c("Qui-quadrado","V-Cramer","C-Contingencia")
  return(out)
}


#' Calculo de correlacao
#'
#' @description Esta funcao recebe como entrada uma base de dados com pelo menos duas
#' variaveis numericas e deternina os coeficientes de correlacao de Pearson, Spearman
#' e Kendal.
#'
#' @details E possivel que existam variaveis nao numerias em sua base de dados. Neste caso,
#' elas serao eliminadas. Para mais detalhes veja \code{\link{cor}}.
#'
#' @param base data.frame, tibla, data.table, etc.
#' @param digits numeros de digitos para arrendondar o valor da correlacao.
#' @author LOPES, J. E.
#' @return Um data.frame com cinco colunas onde as duas primeiras sao as combinacoes
#' aos pares de cada uma das variaveis e as tres ultimas sao as correlacoes de
#' Pearson, Spearman e Kendal respectivamente.
#' @examples
#' rnp_correlacao(mtcars)
#' @export
rnp_correlacao <- function (base, digits = 4) {
  if(missing(base)) {
    stop("log: Informe uma base com pelo menos duas variaveis numericas.\n")
  }

  cl <- sapply(base, class)
  if(any(!cl %in% c("integer", "numeric"))) {
    cat("log: parece que a base tem variaveis nao numericas, removendo e trabalhando apenas com as numericas")
  }
  if(length(names(cl)[cl %in% c("integer", "numeric")]) < 2) {
    stop("log: Informe uma base com pelo menos duas variaveis numericas.\n")
  }
  base <- subset(base, select = names(cl)[cl %in% c("integer", "numeric")])
  metodos <- c("pearson","spearman", "kendall")
  lc <- lapply(metodos, function(i){
    tab_cor <- round(stats::cor(base, use = "complete.obs", method = i), digits)
    tab_cor[lower.tri(tab_cor, diag = TRUE)] <- NA
    tab_cor <- as.data.frame(as.table(tab_cor))
    tab_cor <- stats::na.omit(tab_cor)
    return(tab_cor)
  })
  names(lc) <- metodos
  pe <- lc$pearson
  sp <- lc$spearman
  ke <- lc$kendall
  cors <- merge(merge(pe, sp,
                      by.x = c("Var1","Var2"), by.y = c("Var1","Var2")),
                ke, by.x = c("Var1","Var2"), by.y = c("Var1","Var2"))
  colnames(cors) <- c("x", "y", metodos)
  cors <- cors[order(cors$x, -abs(cors$pearson)),]
  return(cors)
}

#' Media aritmetica simples e ponderada
#' @details Esta funcao e um atalho para as funcoes \code{\link{mean}} e \code{\link{weighted.mean}}
#' que calculam respectivamente as medias simples e ponderada. Caso seja passado um vetor de pesos e
#' ele contenha missing e o argumento remove.na for TRUE constuimos um data.frame e removemos os
#' NA's na mesma posicao que aparecem nos dois vetores.
#' @param x vetor numerico.
#' @param peso vetor de pesos. Se informado, calcula a media ponderada  .
#' @param remove.na Se TRUE remove NA's do vetor x e/ou dos pesos.
#' @return numerico
#' @author LOPES, J. E.
#' @examples
#' require(rnpextras)
#' x <- c(2,2,5)
#' p <- c(3,5,NA)
#' media_aritmetica(x)
#' media_aritmetica(x, p)
#' media_aritmetica(x, p, remove.na = TRUE)
#' @export
media_aritmetica <- function(x, peso = NULL, remove.na = TRUE){
  if(missing(x)) stop("Informe um vetor numerico")
  if(remove.na && !is.null(peso)){
    aux <- na.exclude(data.frame(x = x, peso = peso))
    x <- aux$x
    peso <- aux$peso
  } else if(remove.na) {
    x <- na.exclude(x)
  } else {
    x
  }
  ma <- c()
  ma <- if(!is.null(peso)){
    stats::weighted.mean(x = x, w = peso, na.rm = remove.na)
  } else {
    mean(x = x, na.rm = remove.na)
  }
  return(ma)
}

#' Media geometrica simples e ponderada
#' @details Esta funcao calcula respectivamente as medias geometricas simples e ponderada.
#' Caso seja passado um vetor de pesos e ele contenha missing e o argumento remove.na
#' for TRUE constuimos um data.frame e removemos os NA's na mesma posicao que aparecem
#' nos dois vetores.
#' @param x vetor numerico.
#' @param peso vetor de pesos. Se informado, calcula a media ponderada  .
#' @param remove.na Se TRUE remove NA's do vetor x e/ou dos pesos.
#' @return numerico
#' @author LOPES, J. E.
#' @examples
#' require(rnpextras)
#' x <- c(2,2,5)
#' p <- c(3,5,NA)
#' media_geometrica(x)
#' media_geometrica(x, p)
#' media_geometrica(x, p, remove.na = TRUE)
#' @export
media_geometrica <- function(x, peso = NULL, remove.na = TRUE) {
  if(missing(x)) stop("Informe um vetor numerico positivo")
  if(remove.na && !is.null(peso)){
    aux <- na.exclude(data.frame(x = x, peso = peso))
    x <- aux$x
    peso <- aux$peso
  } else if(remove.na) {
    x <- na.exclude(x)
  } else {
    x
  }

  n <- length(x)
  mg <- c()

  if(!is.null(peso)){
    if(length(peso) != n) stop("O vetor de pesos precisar ser do tamanho do vetor x")
    if(sum(x > 0, na.rm = TRUE) >= 0) {
      mg <- exp((1/(sum(peso, na.rm = remove.na)))*sum(peso*log(x), na.rm = remove.na))
    } else {
      cat("Media geomterica tem sentido apenas para numeros positivos!\n")
    }
  } else {
    if(sum(x > 0, na.rm = TRUE) >= 0) {
      mg <- exp((1/n)*sum(log(x), na.rm = remove.na))
    } else {
      cat("Media geomterica tem sentido apenas para numeros positivos!\n")
    }
  }
  return(mg)
}

#' Media harmonica simples e ponderada
#' @details Esta funcao calcula respectivamente as medias harmonicas simples e ponderada.
#' Caso seja passado um vetor de pesos e ele contenha missing e o argumento remove.na
#' for TRUE constuimos um data.frame e removemos os NA's na mesma posicao que aparecem
#' nos dois vetores.
#' @param x vetor numerico.
#' @param peso vetor de pesos. Se informado, calcula a media ponderada  .
#' @param remove.na Se TRUE remove NA's do vetor x e/ou dos pesos.
#' @return numerico
#' @author LOPES, J. E.
#' @examples
#' require(rnpextras)
#' x <- c(2,2,5)
#' p <- c(3,5,NA)
#' media_harmonica(x)
#' media_harmonica(x, p)
#' media_harmonica(x, p, remove.na = TRUE)
#' @export
media_harmonica <- function(x, peso = NULL, remove.na = TRUE) {
  if(missing(x)) stop("Informe um vetor numerico positivo")

  if(remove.na && !is.null(peso)){
    aux <- na.exclude(data.frame(x = x, peso = peso))
    x <- aux$x
    peso <- aux$peso
  } else if(remove.na) {
    x <- na.exclude(x)
  } else {
    x
  }
  n <- length(x)
  mh <- c()

  if(!is.null(peso)){
    if(length(peso) != n) {
      stop("O vetor de pesos precisar ser do tamanho do vetor x")
    } else {
      mh <- (sum(peso * x^(-1), na.rm = remove.na) / (sum(peso, na.rm = remove.na)))^(-1)
    }
  } else {
    mh <- (sum(x^(-1)) / length(x))^(-1)
  }
  return(mh)
}

#' X e numerico
#' @description testa se x e numerico ou nao e retorn TRUE ou FALSE
#' @param x vetor de teste
#' @export
fn_isnum <- function(x) {
  is.numeric(x) | is.integer(x)
}


#' Trata erro em codigos executados
#' @description forma mais inteligente da try(). Com ela o erro e controlado de uma forma
#' mais efetiva e se aplica a qualquer objeto/operacao.
#' @param code codigo, valor ou operacao
#' @param silent se TRUE roda silencionamente
#' @export
try_error <- function (code, silent = TRUE) {
  W <- NULL
  w.handler <- function(w) {
    W <<- w
    invokeRestart("muffleWarning")
  }
  withCallingHandlers(tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent)
      message(c)
    invisible(structure(msg, class = "try-error"))
  }), warning = w.handler)
}
