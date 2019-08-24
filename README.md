
```{r setup, include=FALSE}
knitr::opts_chunk$set(warning=F)
```

## O que vamos aprender hoje?

1. Dessazonalização, interpolação e algumas visualizações úteis.

**2. rafa: algoritmo para previsão de séries temporais.**

<font size="6">**Objetivos gerais**</font>

- O objetivo **não é** aprender a programar em R.

- O objetivo *é* ser capaz de realizar as atividades 1, 2 e 3.

<font size="6">**Nossos insumos**</font> 

- Série temporal: **Taxa de desocupação (PED/DF)**
- R: pacote ***forecast*** (Hyndman et al, 2019).
- R: pacote ***rafa*** (Leripio, 2019)

## Configurações preliminares

- Instalar os pacotes necessários

```{r, out.width="70%", fig.align='center', echo = F}

knitr::include_graphics("pack.png")

```

- Carregar os pacotes instalados

```{r, warning = F, message = F}

library(forecast)
library(seasonal)
library(tidyverse)

```

## Importando nossos dados

- **Repositório**: <https://github.com/leripio/mini-curso-codeplan>

- Fazer o download do arquivo: *ped.xlsx*.

- Abra o R e importe o arquivo através do botão *Import Dataset*.

```{r, out.width="70%", fig.align='center', echo = F}

knitr::include_graphics("import.png")

```

## Adequando os dados

- O pacote *forecast* (versão <= 8.5) utiliza objetos de classe 'ts'.

```{r, echo = F, warning = F}

library(readxl)

ped <- read_excel("W:/Núcleo de análises econômicas/Curso FC/ped.xlsx")

```

```{r}

ped_ts <- ts(ped$TD, start = c(2012,3), frequency = 12)

ped_ts

```

<span style="color:red">Problema: observações ausentes (NA)!</span>

## Antes de prosseguir...

- Convém visualizar graficamente os dados:

```{r, fig.width=5, fig.height=3, fig.align='center'}

library(forecast)

autoplot(ped_ts)

```

## Preenchendo os valores ausentes

- Alguns métodos de previsão aceitam valores ausentes (NA's), outros não.

- Basicamente, precisamos adicionar os valores ausentes para ligar os pontos:

```{r, echo = F, warning = F, message = F, fig.align='center', fig.height=3, fig.width=4}

library(tidyverse)

ped_sem_na <- tibble::tibble(date = seq.Date(from = as.Date("2012-03-01"), 
                                         to = as.Date("2019-06-01"), 
                                         by = "month"),
                         
                         ped = c(ped_ts),
                         
                         ped_sem_na = c(zoo::na.approx(ped)),
                         
                         na = ifelse(is.na(ped), 1, 0))

ped_sem_na %>%

ggplot(aes(x = date, y = ped_sem_na, color = factor(na))) + 
  
  geom_line(lwd = 1) +
  
  scale_color_manual(values = c("black", "red")) +
  
  labs(color = "", x = "", y = "") +
  
  theme(legend.position = "none")

```
- **Porém, é necessário levar a conta o padrão sazonal da série.**

## Começando com o pacote *forecast*

- Função **na.interp()** considera a sazonalidade.

```{r, eval=F}

library(forecast)

ped_completa <- na.interp(ped_ts)

autoplot(ped_completa)

```

```{r, echo = F, warning = F, message = F, fig.width=5, fig.height=3, fig.align='center'}

ped_completa <- na.interp(ped_ts)

ped_completa_tib <- tibble::tibble(date = seq.Date(from = as.Date("2012-03-01"), 
                                         to = as.Date("2019-06-01"), 
                                         by = "month"),
                         
                         ped = c(ped_ts),
                         
                         ped_completa = c(forecast::na.interp(ped_ts)),
                         
                         na = ifelse(is.na(ped), 1, 0))

ped_completa_tib %>%

ggplot(aes(x = date, y = ped_completa, color = na)) + 
  
  geom_line(lwd = 1) +
  
  labs(color = "", x = "", y = "") +
  
  theme(legend.position = "none")

```

## Por falar em sazonalidade...

- Funções úteis para visualizar sazonalidade: **ggseasonplot()** e **ggmonthplot()**

```{r, fig.align='center', fig.height=4}

ggseasonplot(ped_completa)

```

## Por falar em sazonalidade...

```{r, fig.align='center', fig.height=4.5}

ggmonthplot(ped_completa)

```

## Você deve estar se perguntando...

- Ok, mas agora eu quero retirar a sazonalidade!

```{r, warning=F, message=F, fig.align="center", fig.height=3}

library(seasonal)

ped_sa <- ped_completa %>% seas() %>% final()

autoplot(ped_sa)

```

## Será que removeu a sazonalidade?

```{r, fig.height=3, fig.align = "center"}

ggmonthplot(ped_sa)

```

- Ajustes sazonais nunca são perfeitos.
- Séries longas ajudam o processo.
- <span style="color:blue">Mas o resultado é bom!</span>

## Previsão: visão geral

- Pacote *forecast* conta com diversos métodos implementados
    - **auto.arima()**
    - **ets()**
    - **tbats()**
    - **meanf()**
    - ...

## Exemplo: auto.arima()

```{r, fig.align='center'}

ped_prev_arima <- ped_completa %>% auto.arima() %>% forecast(h = 6)

ped_prev_arima

```

## Exemplo: auto.arima()

```{r, fig.align='center', fig.height=4}

autoplot(ped_prev_arima)

```

## Mas poderia usar outros métodos...

```{r}

ped_completa %>% ets() %>% forecast(h = 4)

```

```{r}

ped_completa %>% tbats() %>% forecast(h = 4)

```

## Como saber qual o melhor?

- A amostra da PED contém 88 observações:

    1. Vamos separar as primeiras 64 observações para estimar o modelo;
    
    2. Em seguida, gerar previsão para 24 períodos à frente;
    
    3. Por fim, vamos <u>**comparar**</u> as 24 previsões com as 24 realizações de fato;

## Como comparar?

 - Através de estatísticas de acurácia.
 - Basicamente, calculam alguma medida que <u>**sumariza**</u> o desvio dos valores previstos em relação aos valores observados.
 - Ex: RMSE e MAE

  <script language="javascript"> 
    function toggle(num) {
      var ele = document.getElementById("toggleText" + num);
      var text = document.getElementById("displayText" + num);
      if(ele.style.display == "block") {
        ele.style.display = "none";
        text.innerHTML = "show";
      }
      else {
        ele.style.display = "block";
        text.innerHTML = "hide";
      }
   } 
  </script>

  <a id="displayText" href="javascript:toggle(1);">Mostrar fórmulas</a>
  <div id="toggleText1" style="display: none">

$$ MAE = \frac{1}{T} \sum_{t=1}^{T}  \left \| \hat{Y_t} - Y_t \right \| $$
$$ RMSE = \sqrt{\frac{\sum_{t=1}^{T} (\hat{Y_t} - Y_t)^2}{T}} $$

  </div>
<br/>
 
 - Mas você não vai precisar calcular à mão!
 
 - O pacote *forecast* dispõe da função **accuracy()** para isso.
 
## Vamos comparar os modelos?

Passo 1: definir as amostras de treino e teste

```{r}

ped_treino <- window(ped_completa, end = c(2017,6))

ped_teste <- window(ped_completa, start = c(2017,7))

```

Passo 2: fazer as previsões p/ 24 períodos

```{r}

ped_arima <- ped_treino %>% auto.arima() %>% forecast(h = 24)

ped_ets <- ped_treino %>% ets() %>% forecast(h = 24)

```

## Vamos comparar os modelos

Passo 3: calcular medidas de acurácia

```{r, eval = F}

accuracy(ped_arima, ped_teste)

```

```{r, echo=F}

accuracy(ped_arima, ped_teste)[, c("MAE", "RMSE")]

```

```{r, eval=F}

accuracy(ped_ets, ped_teste)

```

```{r, echo=F}

accuracy(ped_ets, ped_teste)[, c("MAE", "RMSE")]

```

<span style="color:red">Diferença elevada entre a acurácia dentro e fora da amostra no modelo arima reflete pouca capacidade de generalização!</span>

## De fato...

```{r, echo = F, fig.align="center"}

autoplot(ped_completa, lwd = 1) + 
  
  autolayer(ped_arima$mean, series = "arima", lwd = 1) + 
  
  autolayer(ped_ets$mean, series = "ets", lwd = 1) +
  
  labs(x = "", y = "", title = "Previsões para a PED", color = "Modelo")

```

## E o algoritmo, onde entra nisso?

- Generalizar o processo de busca pelo melhor modelo.

- Além disso, introduz algumas ferramentas de refinamento:
    
    1. Previsão central e intervalos mais robustos (*bagging*)
    
    2. Opção de cálculo de acurácia mais robusto (CV).
    
    3. Opção de medir acurácia direcional.
    
## Me convenceu, quero usar!

Passo 1: instalar o pacote **rafa**

```{r, eval = F}

library(devtools)

install_github("leripio/rafa")

```

Passo 2: utilizar a função *auto_forecast()*

```{r, cache = T}

library(rafa)

ped_rafa <- auto_forecast(ped_completa, test = c(2017,6), h = 6)

```

O objeto criado *ped_rafa* contém alguns elementos:

1. **fc**: as previsões centrais e intervalos.

2. **ac**: acurácia dos modelos.

## Acurácia dos modelos

```{r}

ped_rafa$acc

```

## Previsões do melhor modelo

```{r}

ped_rafa$fc

```

## Obrigado!

- Repositório: http://github.com/leripio

- Blog pessoal: http://www.rleripio.com.br/blog

- Contato: leripiorenato@gmail.com
