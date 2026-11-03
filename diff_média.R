
# ============================================================
# PROJETO DE ANÁLISE ESTATÍSTICA
# Diferença entre rendimentos médios por sexo
# Paraíba - 2º trimestre de 2026
# Fonte: PNAD Contínua / IBGE
# ============================================================

# 1. Instalar os pacotes (apenas uma vez)
install.packages(c("PNADcIBGE", "survey", "dplyr"))

# 2. Carregar os pacotes
library(PNADcIBGE)
library(survey)
library(dplyr)

# 3. Baixar os microdados de 2026
dados <- get_pnadc(
  year = 2026,
  quarter = 2,
  vars = c(
    "UF",
    "V2007",
    "VD3004",
    "VD4002",
    "VD4019"
  ),
  labels = FALSE,
  deflator = TRUE,
  design = TRUE
)

# 4. Verificar se os dados foram carregados
if (is.null(dados)) {
  stop("Não foi possível carregar os microdados.")
}

# 5. Filtrar a população de interesse
# UF = 25: Paraíba
# V2007 = 1: Homem; V2007 = 2: Mulher
# VD3004 = 7: Ensino superior completo
# VD4002 = 1: Ocupado

dados_pb <- subset(
  dados,
  UF == 25 &
    V2007 %in% c(1, 2) &
    VD3004 == 7 &
    VD4002 == 1 &
    !is.na(VD4019) &
    VD4019 > 0
)

# 6. Criar variável de sexo
dados_pb$variables$sexo <- factor(
  dados_pb$variables$V2007,
  levels = c(1, 2),
  labels = c("Homens", "Mulheres")
)

# 7. Estatísticas descritivas ponderadas
medias <- svyby(
  ~VD4019,
  ~sexo,
  design = dados_pb,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

print(medias)

# 8. Teste de diferença entre médias
# Teste t ajustado ao desenho amostral
resultado <- svyttest(
  VD4019 ~ sexo,
  design = dados_pb
)

print(resultado)

# 9. P-valor
p_valor <- resultado$p.value

cat("P-valor:", p_valor, "\n")

# 10. Conclusão
if (p_valor < 0.05) {
  cat(
    "Rejeitamos H0: há evidências de diferença",
    "estatisticamente significativa entre as médias.\n"
  )
} else {
  cat(
    "Não rejeitamos H0: não há evidências suficientes",
    "de diferença estatisticamente significativa entre as médias.\n"
  )
}
table(dados_pb$variables$sexo)

# Número de observações com rendimento válido
with(
  dados_pb$variables,
  table(sexo, useNA = "ifany")
)
