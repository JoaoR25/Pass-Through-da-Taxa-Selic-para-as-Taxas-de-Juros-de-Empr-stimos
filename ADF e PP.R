# Testes de estacionariedade: ADF e PP
library(readxl)
library(dplyr)
library(urca)
library(purrr)
library(writexl)

# Carregar dados

df <- read_excel("C:/Users/joaog/OneDrive/Documents/Economia/TCC/Dados/Dados trabalho.xlsx")

df <- df %>% 
  mutate(periodo = as.Date(paste0("01-", periodo), format = "%d-%b-%Y")) %>% 
  arrange(periodo)

# Séries de juros

colunas_juros <- c(
  "selic", 
  "juros_total", 
  "juros_livre", 
  "juros_direcionado",
  "juros_outros_bens_pf_livre", 
  "juros_aquisicao_de_veiculos_pf_livre",
  "juros_cheque_especial_pf_livre", 
  "juros_credito_pessoal_total_pf_livre",
  "juros_outros_bens_pj_livre", 
  "juros_duplicatas_e_recebiveis_pj_livre",
  "juros_capital_de_giro_total_pj_livre", 
  "juros_credito_rural_total_pf_direcionado",
  "juros_BNDES_total_pj_direcionado", 
  "juros_credito_rural_total_pj_direcionado"
)

# Séries de inadimplência

col_inad <- c(
  "inadimplencia_total",
  "inadimplencia_livre",
  "inadimplencia_direcionado",
  "inadimplencia_aquisicao_de_outros_bens_pf_livre",
  "inadimplencia_aquisicao_de_veiculos_pf_livre",
  "inadimplencia_cheque_especial_pf_livre",
  "inadimplencia_credito_pessoal_total_pf_livre",
  "inadimplencia_aquisicao_de_outros_bens_pj_livre",
  "inadimplencia_desconto_de_duplicatas_e_recebiveis_pj_livre",
  "inadimplencia_capital_de_giro_total_pj_livre",
  "inadimplencia_credito_rural_total_pf_direcionado",
  "inadimplencia_BNDES_total_pj_direcionado",
  "inadimplencia_credito_rural_total_pj_direcionado"
)

colunas_teste <- c(colunas_juros, col_inad)

# Função dos testes

testar_estacionariedade <- function(x){
  
  x <- na.omit(as.numeric(x))
  
  adf <- ur.df(x, type = "drift", selectlags = "AIC")
  
  adf_estat <- as.numeric(adf@teststat[1])
  adf_crit_1 <- as.numeric(adf@cval[1, "1pct"])
  adf_crit_5 <- as.numeric(adf@cval[1, "5pct"])
  
  resultado_adf <- data.frame(
    Teste = "ADF",
    Estatistica = round(adf_estat, 4),
    Critico_1pct = round(adf_crit_1, 4),
    Critico_5pct = round(adf_crit_5, 4),
    Det = "c",
    Resultado = ifelse(
      adf_estat < adf_crit_5,
      "Estacionária",
      "Não estacionária"
    )
  )
  
  pp <- ur.pp(
    x,
    type = "Z-tau",
    model = "constant",
    lags = "short"
  )
  
  pp_estat <- as.numeric(pp@teststat)
  pp_crit_1 <- as.numeric(pp@cval[1, "1pct"])
  pp_crit_5 <- as.numeric(pp@cval[1, "5pct"])
  
  resultado_pp <- data.frame(
    Teste = "PP",
    Estatistica = round(pp_estat, 4),
    Critico_1pct = round(pp_crit_1, 4),
    Critico_5pct = round(pp_crit_5, 4),
    Det = "c",
    Resultado = ifelse(
      pp_estat < pp_crit_5,
      "Estacionária",
      "Não estacionária"
    )
  )
  
  rbind(resultado_adf, resultado_pp)
}

# Testes em nível

resultados_nivel <- map_dfr(
  colunas_teste,
  ~ testar_estacionariedade(df[[.x]]) %>%
    mutate(
      modalidade = .x,
      etapa = "Nível"
    )
)

# Criar base em primeira diferença

df_diff <- df %>%
  mutate(
    across(
      all_of(colunas_teste),
      ~ .x - lag(.x)
    )
  )

# Testes em primeira diferença

resultados_diff <- map_dfr(
  colunas_teste,
  ~ testar_estacionariedade(df_diff[[.x]]) %>%
    mutate(
      modalidade = .x,
      etapa = "1ª diferença"
    )
)

# Juntar resultados

resultados_estacionariedade <- bind_rows(resultados_nivel, resultados_diff) %>%
  dplyr::select(
    etapa,
    modalidade,
    Teste,
    Estatistica,
    Critico_1pct,
    Critico_5pct,
    Det,
    Resultado
  )

# Separar ADF e PP

resultado_adf <- resultados_estacionariedade %>%
  filter(Teste == "ADF")

resultado_pp <- resultados_estacionariedade %>%
  filter(Teste == "PP")

# Separar nível e primeira diferença

resultado_nivel <- resultados_estacionariedade %>%
  filter(etapa == "Nível")

resultado_primeira_diferenca <- resultados_estacionariedade %>%
  filter(etapa == "1ª diferença")

# Visualizar

View(resultados_estacionariedade)
View(resultado_adf)
View(resultado_pp)

# Salvar em Excel

write_xlsx(
  list(
    "Resultados_completos" = resultados_estacionariedade,
    "ADF" = resultado_adf,
    "PP" = resultado_pp,
    "Nivel" = resultado_nivel,
    "Primeira_diferenca" = resultado_primeira_diferenca
  ),
  path = "testes_estacionariedade_adf_pp.xlsx"
)

cat("\nTestes ADF e PP concluídos.\n")
cat("Arquivo salvo como: testes_estacionariedade_adf_pp.xlsx\n")
