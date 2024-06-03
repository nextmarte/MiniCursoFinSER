## pacote utilizados
pacotes <- c("tidyverse", "quantmod", "timetk", "plotly", "PerformanceAnalytics","DT")
purrr::walk(pacotes, library, character.only = TRUE)

# Ativos escolhidos
tickers <- c("VALE3.SA","PETR4.SA","CMIG4.SA","RADL3.SA","ITUB4.SA","^BVSP")

# w <- c(0.15, 0.25, 0.1, 0.2, 0.3)

prices_raw <- 
    getSymbols(tickers, 
                         src = 'yahoo', 
                         from = "2019-12-31",
                         to = "2024-05-31",
                         auto.assign = TRUE, #obs auto asign carrega os resultados para o ambiente
                         warnings = FALSE,)

prices<-prices_raw %>%             
    map(~Ad(get(.))) %>% 
    reduce(merge) %>%
    `colnames<-`(tickers)

prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)

asset_returns_xts <-
        PerformanceAnalytics::Return.calculate(prices_monthly,
                method = "discrete"
        ) %>%
        na.omit()


asset_returns_tbl <- asset_returns_xts %>% 
    tk_tbl(rename_index = "Data") %>% #converte em um data frame para plotar com ggplot
    pivot_longer(!"Data" ,names_to = "assets")#seleciona todas as colunas menos data

# plotando com o ggplot e com o plotly
g1 <- plotly::ggplotly(ggplot(asset_returns_tbl) +
        aes(x = Data, y = value, colour = assets) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(title = "Retornos de 2019 a 2024") + # 
        theme_minimal())

pbp <- ggplot(asset_returns_tbl) +
    aes(x = "", y = value, fill = assets) +
    geom_boxplot() +
    scale_fill_brewer(palette = "OrRd", direction = 1) +
    labs(
        x = "Ativos",
        y = "Retornos",
        title = "Box Plot - Retorno dos ativos",
        fill = "Ativos"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16L,
        face = "bold",
        hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    )

set.seed(1234) 
w <- runif(5) # Gerando 4 pesos aleatórios
w <- w / sum(w) # Normalizando os pesos para que a soma seja 1

# removendo o índice do IBOV da lista de ativos para calcular o retorno da carteira
retorno_ativos <- asset_returns_xts[, tickers[1:5]]

portfolio_returns_xts <- PerformanceAnalytics::Return.portfolio(retorno_ativos,
        weights = w,
        rebalance_on = "months",
        type = "discrete",
        verbose = FALSE
) %>%
        `colnames<-`("CARTEIRA")


comparacao <- cbind(asset_returns_xts, portfolio_returns_xts) %>% 
    tk_tbl(rename_index = "Data") %>% 
    pivot_longer(-Data, names_to = "assets", values_to = "value")

comp <- ggplot(comparacao, aes(x = Data, y = value, colour = assets)) +
    geom_line() +
    scale_color_hue(direction = 1) +
    labs(
        x = "Data",
        y = "Retornos",
        title = "Retorno dos ativos vs carteira",
        colour = "Ativos"  
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    )

comp_mkt <- comparacao %>%
 filter(assets %in% c("X.BVSP", "CARTEIRA")) %>%
 ggplot() +
    aes(x = Data, y = value, colour = assets) +
    geom_line() +
    scale_color_hue(direction = 1) +
    labs(title = "Carteira VS Índice do mercado - 2020 - 2023",
                x = "Data",
                y = "Retornos",
                colour = "Ativos"  
    )+
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold")
    )

# desvio padrao dos precos dos ativos

mean_value <- mean(asset_returns_tbl$value, na.rm = TRUE)
sd_value <- sd(asset_returns_tbl$value, na.rm = TRUE)

distribuicaodp <- plotly::ggplotly(asset_returns_tbl %>%
        filter(assets %in% "PETR4.SA") %>%
        mutate(
                faixa_inferior = if_else(value < (mean_value - sd_value), value, as.numeric(NA)),
                faixa_superior = if_else(value > (mean_value + sd_value), value, as.numeric(NA)),
                faixa_central = if_else(value > (mean_value - sd_value) & value < (mean_value + sd_value), value, as.numeric(NA))
        ) %>%
        ggplot() +
        geom_point(aes(x = Data, y = faixa_inferior), color = "red") +
        geom_point(aes(x = Data, y = faixa_superior), color = "green") +
        geom_point(aes(x = Data, y = faixa_central), color = "blue") +
        geom_hline(yintercept = (mean_value + sd_value), color = "purple", linetype = "dotted") +
        geom_hline(yintercept = (mean_value - sd_value), color = "purple", linetype = "dotted") +
        labs(
                x = "Data",
                y = "Retornos",
                title = "Distribuição padronizada - PETR4.SA",
                color = "Ativo"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 16L, face = "bold", hjust = 0.5),
                axis.title.y = element_text(face = "bold"),
                axis.title.x = element_text(face = "bold")
        ))


# Cria o data frame risco_retorno
risco_retorno <- cbind(asset_returns_xts, portfolio_returns_xts) %>% 
    tk_tbl(preserve_index = FALSE) %>%
    summarise(across(everything(), list(Desvio_padrao = sd, Retorno_Medio = mean, Risco = ~sd(.)/mean(.)))) %>% 
    pivot_longer(cols = everything(), names_to = "Ativo", values_to = "Valor") %>%
    separate(Ativo, into = c("Ativo", "Medida"), sep = "_", extra = "drop") %>%
    pivot_wider(names_from = "Medida", values_from = "Valor")

pesos_ativos <- tibble(Ativo = tickers[1:5], Peso = w)
pesos_ativos <- pesos_ativos %>% add_row(Ativo = "CARTEIRA", Peso = 1)

# Junta o novo data frame ao data frame risco_retorno
risco_retorno <- risco_retorno %>%
    left_join(pesos_ativos, by = "Ativo")

# Cria o gráfico
rr <- plotly::ggplotly(risco_retorno %>%
        ggplot(aes(x = Risco, y = Retorno, colour = Ativo, size = Peso)) +
        geom_point() +
        scale_color_hue(direction = 1) +
        labs(title = "Risco vs Retorno dos Ativos", size = "Pesos e", colour = "ativos") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16L, face = "bold", hjust = 0.5)))

beta_long <- PerformanceAnalytics::CAPM.beta(cbind(asset_returns_xts, portfolio_returns_xts), asset_returns_xts$"^BVSP", Rf = 0) %>%
        as.data.frame() %>%
        rownames_to_column(var = "indice") %>%
        pivot_longer(cols = -indice, names_to = "Ativo", values_to = "beta") %>%
        select(-indice) %>%
        inner_join(risco_retorno, by = "Ativo") %>%
        select(Ativo, Desvio, beta, Risco, Retorno)

beta <- plotly::ggplotly(ggplot(beta_long) +
        aes(y = Retorno, x = beta, colour = Ativo) +
        geom_point() +
        scale_color_hue(direction = 1) +
        labs(y = "Retorno", x = "Beta", title = "Beta vs Retorno") +
        theme_minimal() +
        theme(
                plot.title = element_text(
                        size = 16L,
                        face = "bold",
                        hjust = 0.5
                )
        ))
