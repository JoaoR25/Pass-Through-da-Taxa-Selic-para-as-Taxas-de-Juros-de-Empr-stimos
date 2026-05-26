# --- 0. Pacotes ---
library(readxl)
library(dplyr)
library(dynlm)
library(lmtest)
library(tseries)
library(car)
library(ggplot2)
library(tidyr)
library(writexl)

# caminho da base
caminho_arquivo <- "Dados trabalho.xlsx"

# pasta onde os resultados serão salvos
pasta_saida <- "Resultados"

# usar todo o período do gráfico?
usar_periodo_completo <- TRUE
# usar_periodo_completo <- FALSE

# se usar_periodo_completo = FALSE, defina as datas abaixo
data_inicio_grafico <- as.Date("2021-01-01")
data_fim_grafico    <- as.Date("2022-12-31")

# modalidades dos gráficos
modalidades_graficos <- c("juros_total", "juros_direcionado")

# --- 1. Importar planilha ---
df <- readxl::read_excel(caminho_arquivo)

# --- 2. Converter coluna de período para Date ---
df <- df %>%
  dplyr::mutate(periodo = as.Date(paste0("01-", periodo), format = "%d-%b-%Y")) %>%
  dplyr::arrange(periodo)

# --- 3. Listas de colunas ---
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

# --- 4. Diferença da Selic ---
df <- df %>%
  dplyr::mutate(d_selic = c(NA, diff(selic)))

# --- 5. Função principal ---
estima_pass_through <- function(juros_col, inad_col, df, start_year = 2011, start_month = 3){
  
  base <- df %>%
    dplyr::arrange(periodo) %>%
    dplyr::mutate(
      d_juros = c(NA, diff(.data[[juros_col]])),
      d_inad  = c(NA, diff(.data[[inad_col]]))
    ) %>%
    dplyr::select(periodo, d_juros, d_selic, d_inad) %>%
    dplyr::filter(
      !is.na(d_juros),
      !is.na(d_selic),
      !is.na(d_inad)
    )
  
  base_ts <- ts(
    base[, c("d_juros", "d_selic", "d_inad")],
    start = c(start_year, start_month),
    frequency = 12
  )
  
  modelo <- dynlm::dynlm(
    d_juros ~ L(d_selic, 0:11) + L(d_inad, 0:3),
    data = base_ts
  )
  
  # Betas da Selic
  coef_modelo <- coef(modelo)
  betas <- coef_modelo[grep("d_selic", names(coef_modelo))]
  
  betas_full <- rep(0, 12)
  betas_full[1:length(betas)] <- betas
  
  repasse_3m  <- sum(betas_full[1:3], na.rm = TRUE)
  repasse_6m  <- sum(betas_full[1:6], na.rm = TRUE)
  repasse_9m  <- sum(betas_full[1:9], na.rm = TRUE)
  repasse_12m <- sum(betas_full[1:12], na.rm = TRUE)
  
  # Teste de Wald do repasse total
  nomes_selic <- names(coef_modelo)[grep("d_selic", names(coef_modelo))]
  restricao <- paste(paste(nomes_selic, collapse = " + "), "= 0")
  teste_wald <- car::linearHypothesis(modelo, restricao, test = "F")
  
  estat_wald <- teste_wald$F[2]
  p_repasse <- teste_wald$`Pr(>F)`[2]
  
  # Diagnósticos
  residuos <- residuals(modelo)
  bg <- lmtest::bgtest(modelo)
  bp <- lmtest::bptest(modelo)
  jb <- tseries::jarque.bera.test(residuos)
  
  # Tabela principal
  tabela_principal <- data.frame(
    modalidade = juros_col,
    inad_col = inad_col,
    repasse_12m = round(repasse_12m, 4),
    estatistica_repasse = round(as.numeric(estat_wald), 4),
    p_valor_repasse = round(p_repasse, 4),
    R2_aj = round(summary(modelo)$adj.r.squared, 4),
    RMSE = round(sqrt(mean(residuos^2, na.rm = TRUE)), 4),
    BG_pvalor = round(bg$p.value, 4),
    BP_pvalor = round(bp$p.value, 4),
    JB_pvalor = round(jb$p.value, 4)
  )
  
  # Tabela trimestral
  tabela_trimestral <- data.frame(
    modalidade = juros_col,
    repasse_3m = round(repasse_3m, 4),
    repasse_6m = round(repasse_6m, 4),
    repasse_9m = round(repasse_9m, 4),
    repasse_12m = round(repasse_12m, 4)
  )
  
  # Observado vs Estimado
  X <- model.matrix(modelo)
  cols_selic <- intersect(colnames(X), nomes_selic)
  
  X_selic <- X[, cols_selic, drop = FALSE]
  betas_graf <- coef_modelo[cols_selic]
  
  efeito_selic_diff <- as.numeric(X_selic %*% betas_graf)
  y_diff <- as.numeric(model.response(model.frame(modelo)))
  
  observado_acum <- cumsum(y_diff)
  estimado_acum  <- cumsum(efeito_selic_diff)
  
  tempo <- base$periodo[(nrow(base) - length(y_diff) + 1):nrow(base)]
  
  grafico_df <- data.frame(
    periodo = tempo,
    observado = observado_acum,
    estimado = estimado_acum,
    modalidade = juros_col
  )
  
  return(list(
    modelo = modelo,
    tabela_principal = tabela_principal,
    tabela_trimestral = tabela_trimestral,
    grafico_df = grafico_df
  ))
}

# --- 6. Rodar para todas as modalidades ---
resultados <- lapply(seq_along(col_juros), function(i){
  estima_pass_through(col_juros[i], col_inad[i], df)
})

# --- 7. Montar tabelas finais ---
tabela_principal_final <- do.call(
  rbind,
  lapply(resultados, function(x) x$tabela_principal)
)

tabela_trimestral_final <- do.call(
  rbind,
  lapply(resultados, function(x) x$tabela_trimestral)
)

grafico_final <- do.call(
  rbind,
  lapply(resultados, function(x) x$grafico_df)
)

cat("\n=== TABELA PRINCIPAL ===\n")
print(tabela_principal_final)

cat("\n=== TABELA TRIMESTRAL ===\n")
print(tabela_trimestral_final)

# --- 8. Exportar tabelas para Excel ---
writexl::write_xlsx(
  list(
    "Tabela_principal" = tabela_principal_final,
    "Tabela_trimestral" = tabela_trimestral_final,
    "Dados_graficos" = grafico_final
  ),
  path = file.path(pasta_saida, "resultados_pass_through.xlsx")
)

# --- 9. Gráficos: juros_total e juros_direcionado ---
for(mod in modalidades_graficos){
  
  df_plot <- grafico_final %>%
    dplyr::filter(modalidade == mod)
  
  # opção para usar período completo ou período definido manualmente
  if(!usar_periodo_completo){
    df_plot <- df_plot %>%
      dplyr::filter(
        periodo >= data_inicio_grafico,
        periodo <= data_fim_grafico
      )
  }
  
  df_plot <- df_plot %>%
    tidyr::pivot_longer(
      cols = c(observado, estimado),
      names_to = "serie",
      values_to = "valor"
    ) %>%
    dplyr::mutate(
      serie = dplyr::recode(
        serie,
        "observado" = "Observado",
        "estimado" = "Estimado (Selic)"
      )
    )
  
  g <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = periodo, y = valor, color = serie, linetype = serie)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(
      title = paste("Observado vs Estimado -", mod),
      x = "",
      y = "Variação acumulada (p.p.)",
      color = "",
      linetype = ""
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  print(g)
  
  ggplot2::ggsave(
    filename = file.path(pasta_saida, paste0("grafico_observado_estimado_", mod, ".png")),
    plot = g,
    width = 9,
    height = 5,
    dpi = 300
  )
}
