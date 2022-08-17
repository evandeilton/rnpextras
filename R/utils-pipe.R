#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @importFrom "stats" "aggregate" "formula" "median" "na.exclude" "pchisq" "qnorm" "quantile" "runif" "sd" "t.test" "chisq.test" "rnorm"
#' @importFrom "utils" "head" "object.size" "timestamp"
#' @usage lhs \%>\% rhs
NULL

#' Variaveis globais
#' @name Variaveis
#' @keywords internal
#@export
utils::globalVariables(c(".","::",":::","classe","codigo_regra","cpf","dados","fa","fa_atual","fa_esperado","fr","fr_atual","fr_conjunta","fr_esperado","ksp","kspa","kspe","metrica","mutate","resultado","score","size","sortclass","sqlQuery","str_pad","top_n","valida_doc","vdi","vdip", '.N','doc_','temp_quebra__','..nm_base','query','ini0', 'csi', 'fl_cbk', 'fl_frd', 'fl_mau', 'fl_sus', 'fl_tan', 'iv', 'ks', 'max_score', 'min_score', 'odds', 'p_bons', 'p_bons_acum', 'p_maus', 'p_maus_acum', 'p_total', 'quebras', 's_decisao', 't_bons', 't_maus', 'timestamp', 'total', 'total_maus', 'woe','variavel','fix','hasName',"faa","cnpj", "ds", "y", "yhat_lower", "yhat_lower.y", "yhat_upper", "yhat_upper.y","resultados","quebras_rep","quebras_apa","pct_maus","quebras_rep","status_inferido","erro_amostral","status_inferido","group_id",'VAR_NOME','VAR_CATEGORIA','VAR_DESCRICAO','ORD','NOME DA VARIAVEL', 'VAR_DESCRICAO_CATEGORIAS','ORDEM','VAR_DESCRICAO_CATEGORIAS_FIX','ano_in'))

#' Tibble support
#'
#' @details See \code{tibble::\link[tibble:tibble]{tibble}} for details.
#' @name tibble
#' @importFrom tibble tibble
#' @export
NULL

