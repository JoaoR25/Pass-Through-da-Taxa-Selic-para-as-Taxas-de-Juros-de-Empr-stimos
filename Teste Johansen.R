library(readxl)
library(dplyr)
library(urca)
library(vars)
library(purrr)
library(writexl)

df <- read_excel("C:/Users/joaog/OneDrive/Documents/Economia/TCC/Dados/Dados trabalho.xlsx")

df <- df %>% 
  mutate(periodo = as.Date(paste0("01-", periodo), format = "%d-%b-%Y")) %>% 
  arrange(periodo)

col_juros <- c(
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

teste_johansen <- function(juros_col, df){
  
  base <- df %>%
    dplyr::select(selic, dplyr::all_of(juros_col)) %>%
    na.omit()
  
  colnames(base) <- c("selic", "juros")
  
  base_ts <- ts(as.matrix(base), start = c(2011, 3), frequency = 12)
  
  lag_aic <- vars::VARselect(base_ts, lag.max = 12, type = "const")$selection["AIC(n)"]
  K_escolhido <- max(2, as.numeric(lag_aic))
  
  johansen <- urca::ca.jo(
    base_ts,
    type = "trace",
    ecdet = "const",
    K = K_escolhido
  )
  
  tabela <- data.frame(
    modalidade = juros_col,
    hipotese = rownames(johansen@cval),
    estatistica = round(johansen@teststat, 4),
    critico_5pct = johansen@cval[, "5pct"],
    rejeita_5pct = johansen@teststat > johansen@cval[, "5pct"],
    K = K_escolhido
  )
  
  return(tabela)
}

resultado_johansen <- map_dfr(col_juros, ~ teste_johansen(.x, df))

print(resultado_johansen)

write_xlsx(resultado_johansen, "resultado_johansen.xlsx")

