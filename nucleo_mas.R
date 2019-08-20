### Programa para calcular medidas de núcleo para o IPCA Brasília ###
### Equivalentes às calculadas a nível nacional pelo BCB
### Metodologia baseada em: http://www.bcb.gov.br/htms/relinf/port/2009/12/ri200912b7p.pdf

### Média aparada com suavização ###

##### Configurações preliminares ####

## Definir diretório de trabalho

ipca_mas <- function(){

## Pacotes utilizados

library(tidyverse)
library(sidrar)
library(reshape2)
library(zoo)
library(scales)

## Baixar séries do sidra - IPCA Brasília por itens.

itens <- get_sidra(api = "/t/1419/n6/5300108/v/63,66/p/all/c315/7172,7184,7200,7219,7241,7254,7283,7303,7335,7349,7356,7372,7384,7389,7401,7415,7433,7447,7454,7461,7480,7484,7488,7495,7517,7522,7541,7549,7560,7572,7587,7605,7616,7621,7627,7640,7656,7662,7684,7690,7695,7698,7714,7730,7758,7760,7777,7782,7788,12427,107678,109464/d/v63%202,v66%204")

geral <- get_sidra(api = "/t/1419/n6/5300108/v/63/p/all/c315/7169/d/v63%202")

## Definindo coluna de Mês como datas em formato apropriado

geral$`Mês (Código)` <- as.Date(paste(geral$`Mês (Código)`,"01",sep=""), format = "%Y%m%d")

itens$`Mês (Código)` <- as.Date(paste(itens$`Mês (Código)`,"01",sep=""), format = "%Y%m%d")

## Selecionar apenas os dados que interessam

ipca_itens <- dplyr::select(itens, c(`Mês (Código)`, `Geral, grupo, subgrupo, item e subitem`, Variável, Valor))

ipca_geral <- dplyr::select(geral, c(`Mês (Código)`, Valor)) %>% filter(`Mês (Código)` >= "2012-12-01")

geral <- dplyr::filter(geral, geral$`Mês (Código)` >= "2012-12-01")

## Calcular a média móvel centrada de 13 meses do índice geral para comparação com o núcleo
## E introduzir no frame do ipca geral

ipca_geral$cma <- c(rep(NA, 6),
                    rollmean(ipca_geral$Valor, k = 13, align = "center"),
                    rep(NA,6))

ipca_geral <- dplyr::select(ipca_geral, c(`Mês (Código)`, cma))

ipca_geral <- ipca_geral[complete.cases(ipca_geral), ]

## Colocar em formato wide

ipca_itens <- spread(ipca_itens, key = Variável, value = Valor)

## Suavizar os itens cuja elevação são elevadas e/ou infrequentes

## Itens

sm <- c("5104.Combustíveis (veículos)","2201.Combustíveis (domésticos)", "2202.Energia elétrica residencial" , "5101.Transporte público",
        "7202.Fumo", "8101.Cursos regulares", "8104.Cursos diversos",
        "9101.Comunicação")

## Selecionar apenas a variação, a data e os itens e colocar em formato wide

ipca_itens_sm <- dplyr::filter(ipca_itens, ipca_itens$`Geral, grupo, subgrupo, item e subitem` %in% sm) %>%

  select(c(`Mês (Código)`, `Geral, grupo, subgrupo, item e subitem`, `IPCA - Variação mensal`)) %>%

  spread(key = `Geral, grupo, subgrupo, item e subitem`, value = `IPCA - Variação mensal`)

## Dividir por 12 e aplicar em cada coluna (item) a soma móvel de 12 meses à direita

smooth <- function(x){rollsum(x, k = 12, align = "right")}

ipca_itens_sm_aux <- plyr::colwise(smooth)(ipca_itens_sm[, -1]/12)

## Introduzir a coluna de datas novamente

ipca_itens_sm_aux$`Mês (Código)` <- ipca_itens_sm$`Mês (Código)`[12:length(ipca_itens_sm$`Mês (Código)`)]

## Colocar no formato long e na ordem cronológica

ipca_itens_sm_aux <- ipca_itens_sm_aux %>%

  gather(key = "Geral, grupo, subgrupo, item e subitem",
                                                  value = "IPCA - Variação mensal",
                                                  -`Mês (Código)`) %>%


  arrange(`Mês (Código)`)


## Substituir os valores originais pelos valores suavizados no data frame de itens

ipca_itens[which(ipca_itens$`Geral, grupo, subgrupo, item e subitem` %in% ipca_itens_sm_aux$`Geral, grupo, subgrupo, item e subitem` & ipca_itens$`Mês (Código)` %in% ipca_itens_sm_aux$`Mês (Código)`), ]$`IPCA - Variação mensal` <- ipca_itens_sm_aux$`IPCA - Variação mensal`

## Considerar apenas os valores de 2012-12-01 em diante, os quais contém as suavizações

ipca_itens <- dplyr::filter(ipca_itens, ipca_itens$`Mês (Código)` >= "2012-12-01")

## Agrupar o frame por data e ordenar pela magnitude da variação
## Isso vai me permitir, adiante, remover as maiores e menores variações a partir da posição da linha
## em cada grupo

ipca_itens <- ipca_itens %>% 
  
  group_by(`Mês (Código)`) %>% 
  
  dplyr::arrange(`IPCA - Variação mensal`, .by_group = TRUE)

## Remover a menor e a maior variação (corte simétrico), calcular o IPCA e o RMSE para cada corte.
## O RMSE é calculado a partir da diferença entre o IPCA aparado e a média móvel centrada de 13 meses.
## São 52 itens no total, vou utilizar no máximo 25% de remoção, o que dá 13 itens, em cada cauda.

ipca_tm <- list()

ipca_tm_aux <- list()

ipca_rmse <- c()

for(i in 1:13){

  ipca_tm_aux[[i]] <- ipca_itens %>%

    group_by(`Mês (Código)`) %>%

    arrange(`IPCA - Variação mensal`, .by_group = TRUE) %>%

    slice(-c(1:i, 52:(52-i+1))) %>%

    summarise(ipca_tm = (`IPCA - Variação mensal` %*% `IPCA - Peso mensal`)/100)

  ipca_tm[[i]] <- left_join(ipca_geral, ipca_tm_aux[[i]], by = "Mês (Código)")

}

ipca_erro <- lapply(ipca_tm, FUN = function(x){

  mutate(x, erro = ((x$ipca_tm - x$cma)^2))

})

ipca_rmse <- sapply(ipca_erro, FUN = function(x){

  x$erro %>% mean() %>% sqrt()

})

k <- which.min(ipca_rmse)

## Tendo o número de itens em cada cauda a serem removidos, calcular o núcleo para toda a amostra

core <- ipca_itens %>%

  group_by(`Mês (Código)`) %>%

  arrange(`IPCA - Variação mensal`, .by_group = TRUE) %>%

  slice(-c(1:k, 52:(52-k+1))) %>%

  summarise(ipca_tm = (`IPCA - Variação mensal` %*% `IPCA - Peso mensal`)/100)

## Criar o frame para gerar o gráfico

core_aux <- left_join(dplyr::select(geral, c(`Mês (Código)`, Valor)), core, by = "Mês (Código)")

colnames(core_aux)[2:3] <- c("IPCA", "IPCA_MAS")

## Criar gráfico mensal

core_aux %>% gather(key = "var", value = "valor", -`Mês (Código)`) %>%

  ggplot(aes(x = `Mês (Código)`, y = valor, color = var)) +

  geom_line(lwd = 1.0) +

  scale_colour_brewer(palette = "Set2") +

  scale_x_date(breaks = date_breaks(paste("4","months",sep=" ")),
               labels = date_format("%b/%Y")) +

  xlab('')+ ylab('') +

  labs(title = "IPCA e IPCA com média aparada suavizada (stm)",
       subtitle = "% mensal",
       caption = "Elaboração: Codeplan/DF com dados do IBGE",
       color = "") +

  theme_light()

## Criar gráfico acumulado em 12 meses

source("W:\\Núcleo de análises econômicas\\Renato\\ts_tools\\acumular.R")

dados_graf_7 <<- data.frame(Data = core_aux$`Mês (Código)`[12:length(core_aux$`Mês (Código)`)],
                      IPCA = acum(core_aux$IPCA, 12),
                      "IPCA_MAS" = acum(core_aux$IPCA_MAS, 12)) %>% as.tibble() %>%

  plyr::rename(c("IPCA_MAS" = "Média aparada suavizada"))

plot_graf_7 <<- dados_graf_7 %>% 
  
  tidyr::gather(key = "var", value = "valor", -Data) %>%

  dplyr::filter(Data >= last(Data) %m-% months(60)) %>%
  
  dplyr::group_by(var) %>%
  
  dplyr::mutate(valor = valor+0.02) %>%
  
  dplyr::mutate(label = ifelse(valor %in% tail(valor, 2), round(valor, 2), NA)) %>% 

  ggplot(aes(x = Data, y = valor, color = var, label = label)) +

  geom_line(lwd = 1.0) +

  scale_x_date(breaks = date_breaks("8 months"),
               labels = date_format("%b-%Y")) +

  xlab('')+ ylab('') +

  labs(color = "") +

  theme_light() +

  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +

  scale_color_manual(values = c("red", "darkgreen")) +
  
  geom_label_repel(show.legend = F)

}
