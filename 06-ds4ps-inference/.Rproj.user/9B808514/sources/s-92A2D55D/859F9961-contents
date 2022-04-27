# ---
#    title: "DS4PS"
# subtitle: "Inferência, amostragem e estimação"
# author: 
#    - Professor Davi Moreira
# date: "`r format(Sys.time(), '%d-%m-%Y')`"
# output: 
#    revealjs::revealjs_presentation:
#    theme: simple
# highlight: haddock
# transition: slide
# center: true
# css: stylesheet.css
# reveal_options:
#    controls: false  # Desativar botões de navegação no slide
# mouseWheel: true # Passar slides com o mouse
# ---
   
# ```{r message=FALSE, warning=FALSE, include=FALSE, results='hide'}
library(tidyverse)
library(skimr) # pacote que ajuda no sumário de dados
library(jtools) # pacote com ferramentas de visualização de modelos
library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)
library(moderndive)
library(infer)
library(here)

# ```
# 
# ## Programa
# 
# - Inferência
# - Amostragem
# - Estimação: pontual e intervalar
# <!---- Associação e Causalidade
# - Condições para causalidade
# - Modelagem
# - Regressão 
# 
# ## Motivação
# 
# Nas eleições de Julho de 1932, o Partido Nazista se transformou na maior bancada do 
# Parlamento alemão (ainda que não tenha ganho maioria). Quem os elegeu? Como podemos 
# entender os valores desses eleitores por meio de pesquisas amostrais? E como podemos 
# fazer comparações precisas sobre os grupos que demonstraram maior apoio ao partido?
# 
# --->
# 
# ## Motivação
# 
# <!---## Amostragem
# 
# ## Amostragem--->
# Em um dos [relatórios de pesquisa](http://media.folha.uol.com.br/datafolha/2018/10/26/3416374d208f7def05d1476d05ede73e.pdf) 
# do Datafolha para as eleições de 2018, lemos a seguinte frase:
# 
# <center>
# ![datafolha](images/datafolha_2018.PNG)
# </center>
# 
# Para entender o que quer dizer isso (e por que essa descrição está errada), precisamos 
# falar de amostras, estimação e incerteza.
# 
# <!---
# ## Como selecionar a amostra?
# Toda inferência depende dos dados coletados e de um conjunto de suposições. Uma das 
# suposições mais importantes é a de que a **amostra é aleatória**, ou seja, todos os 
# indivíduos/observações têm a mesma probabilidade de serem selecionados.
# 
# Parece uma coisa simples, mas conseguir uma amostra aleatória de verdade é mais difícil 
# do que parece. Na prática, é raríssimo fazer uma amostra aleatória simples, e algumas 
# técnicas são adotadas para selecionar amostras *como se fossem* aleatórias. Não vamos 
# entrar em detalhes aqui, mas podemos destacar duas famílias de técnicas bastante utilizadas.
# 
# ## Amostras na prática
# 
# - Pré-estratificação: as observações são selecionadas de modo a cumprir proporções 
# equivalentes àquelas encontradas na população
# 
# - Pós-estratificação: As observações são ponderadas de modo a aumentar a influência 
# de estratos subrepresentados na amostra e diminuir a influência de estratos sobrerepresentados
# 
# Qual é a desvantagem dessas técnicas?
# 
# ## Tamanho da amostra não depende da população!
# Note que as fórmulas para definir o tamanho da amostra são
# 
# $$n = \sigma^{2} \left(\frac{z}{M} \right)^{2}$$
# 
# $$n = \pi(1 - \pi) \left(\frac{z}{M} \right)^{2}$$
# 
# O tamanho da população não influencia o tamanho da amostra. O que a define são o 
# nível de confiança, a estimativa intervalar e a dispersão da variável 
# 
# ## Uma Senhora Toma Chá...
# 
# Uma senhora inglesa, grande apreciadora de chá com leite (sim, eu sei), dizia ser 
# capaz de identificar se o chá foi colocado antes ou depois do leite na mistura. Segundo ela, 
# a mistura só ficava boa se o leite era posto antes do chá. Mas será que ela sabe mesmo identificar?
# 
# - Se a gente der apenas uma xícara para testar o conhecimento dela, ela pode acertar 
# por sorte (50% de chance)
# - Se dermos mais xícaras, como identificar se a proporção de acertos dela é 
# indestinguível da sorte?
# 
# Fisher preparou 8 xícaras, alternando entre chá antes do leite e leite antes do chá. Ela 
# deveria pegar 4 dessas xícaras e dizer o que foi posto antes.
# 
# ## Uma Senhora Toma Chá...
# Assumindo que ela estivesse chutando:
# 
# - A chance de acertar os 4 resultados é de 1,4% (1/70)
# 
# - A chance de acertar 3 resultados é 22,8% (16/70)
# 
# <center>
# ![Chá](images/cha_permut.PNG)
# </center>
# 
# Como distinguir se ela está chutando ou se realmente consegue identificar corretamente as bebidas?
# 
# ## Uma Senhora Toma Chá...
# 
# <center>
# ![Livro](images/capa_salzburg.jpg){width=200px}
# </center>
# 
# 
# ## Inferência
# --->
# 
# ## Inferência
# A motivação mais comum para o uso de estatística nas ciências sociais é fazer inferências 
# com base em amostras. A ideia é que, para saber uma característica qualquer da população, 
# podemos observar alguns casos e seguir algumas regras de estimação.
# 
# A diferença mais importante entre a estatística inferencial e a estatística descritiva 
# é que, na primeira, nosso objetivo é **calcular e comunicar incerteza**. Não fazemos 
# inferência estatística para *descobrir* qual é o valor de um parâmetro; fazemos inferência 
# para dar um palpite razoável sobre esse valor, com cálculo preciso da incerteza.
# 
# Vamos ver alguns conceitos.
# 
# ## Teoremas fundamentais
# Você pode ser perguntar: se estamos fazendo apenas uma pesquisa, como sabemos que o 
# valor dela representa um palpite razoável sobre o parâmetro?
# 
# - [Teorema Central do Limite](https://seeing-theory.brown.edu/probability-distributions/index.html#section3): A distribuição 
# amostral da média se aproxima de uma normal, independentemente da distribuição original da variável
# 
# 
# <center>
# ![Central Limit Theorem](images/central_limit_theorem.png){width=400px}
# 
# [Bunnies, Dragons and the 'Normal' World: Central Limit Theorem](https://www.youtube.com/watch?v=jvoxEYmQHNM)
# </center>
# 
# ## Probabilidades na curva normal (regra empírica)
# 
# <center>
# ![Curva Normal](images/normal_dist.png){width=500px}
# </center>
# 
# - Distribuição normal é simétrica
# - Pode ser inteiramente descrita com dois parâmetros: média e desvio-padrão
# 
# 1. 68% dos valores estão entre $\pm$ 1 desvio padrão da média.
# 2. 95% dos valores estão entre $\pm$ 1,96 $\approx$ 2 desvios padrões da média.  
# 3. 99.7% of values will lie within $\pm$ 3 desvios padrões da média.
# 
# ## Distribuição de amostras
# - Em ciências sociais, quase nunca as distribuições são normais
# - Muitas vezes, não sabemos como se comporta a distribuição real
# - Podemos usar amostras para fazer inferências sobre os parâmetros da distribuição
# - As estatísticas dessas amostras, quando obtidas repetidamente, seguem uma distribuição repetida
# - Não estamos mais falando de distribuição de valores de uma variável, mas de estatísticas 
# que emergem de diversas amostras
# - Exemplo: como se comporta a distribuição da intenção de votos em uma eleição?
# 
# ## Distribuição de amostras
# - Se fizermos diversas amostras de uma mesma população, podemos esperar variabilidade 
# nos resultados
# - **Esse erro é inerente ao fato de que estamos utilizando apenas uma parte da população**: a amostra
# - Por isso institutos de pesquisas têm números diferentes para uma eleição. Mas essa 
# variação segue um padrão conhecido
# - Na prática, não precisamos tirar diversas amostras: podemos calcular o **erro padrão**
# 
# <center>
# <span class="red">
# Erro padrão é o erro que esperamos obter se fizermos diversas amostras de uma mesma população
# </span>
# </center>
# 
# ## Teoremas fundamentais
# Você pode ser perguntar: se estamos fazendo apenas uma pesquisa, como sabemos que o 
# valor dela representa um palpite razoável sobre o parâmetro?
# 
# - [Lei dos Grandes Números](https://seeing-theory.brown.edu/basic-probability/index.html#section2): A média 
# amostral converge para o valor esperado da distribuição à medida em que o n cresce. Exemplo: jogar dados repetidas vezes
# 
# ## Simulação - Lei dos Grandes Números
# ```{r echo=FALSE, cache=TRUE, fig.align="center", fig.height=5, message=FALSE, warning=FALSE}
library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)

# Para replicar a simuação
set.seed(1234)

n <- 500 # número de jogadas
p_cara <- .5 # probabilidade teórica  de sair cara

# Simulação das jogadas
simul <- sample(x = c(0,1),
            prob = c(1 - p_cara, p_cara),
            size = n,
            replace = TRUE)

r <- cumsum(simul) # soma acumulada
n_jogada <-  1:n
v_esperado <- r/n_jogada # probabilidade de cara em cada jogada

# Colocando a simulação em um banco de dados
bd <- tibble(jogada = 1:n, prop = v_esperado)

# Animação
bd %>% ggplot(aes(x = jogada, y = prop, frame = jogada)) +
geom_path(aes(cumulative = TRUE)#, size = 1
) + xlim(1, n) + ylim(0.0, 1.0) +
geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
ggtitle("Proporção de caras quando jogamos uma moeda") +
ylab(NULL) +
xlab("Jogadas") +
theme_bw() +
transition_reveal(jogada)

# ```
# 
# ## Estimação
# 
# ## Estimação pontual e intervalar
# 
# Como fazer a ponte entre os dados que coletamos em nossa amostra e o valor verdadeiro 
# do parâmetro populacional? Como comunicar a incerteza?
# 
# - **Estimativa pontual**: o melhor palpite que podemos dar sobre o valor do parâmetro
# - **Estimativa intervalar**: um intervalo de valores dentro do qual acreditamos que o parâmetro se encontra
# 
# ## Amostragem
# 
# **Vamos testar o que vimos até agora!** Qual a porcentagem de bolas vermelhas 
# dentro da bacia? 
# 
# <center>
# ![Bacia](images/bowl_red_white_balls.png){width=600px}
# </center>
# 
# ## Amostragem
# 
# Ao sacudir a bacia garantimos aleatoriedade na distribuição de bolinhas. Com uma pá adequada, podemos retirar uma amostra aleatória de 50 bolinhas. 
# 
# Na Figura, vemos a presença de 17 bolas vermelhas, o equivalente a 34% da amostra. Podemos usar o resultado obtido na amostra aleatória como um palpite adequado (estimativa pontual) sobre a proporção de bolas vermelhas na bacia? Vamos usar simulação e aplicar o Teorema Central do Limite para nos ajudar com a resposta!
# 
# <center>
# ![Pá](images/shovel.png){width=400px}
# </center>
# 
# ## Amostragem
# 
# Vamos simular nossa bacia com bolinhas!
# 
# ```{r echo=T, results='hide', message=FALSE, warning=FALSE}

bowl

# ```
# 
# ## Amostragem
# 
# 
# ```{r echo=T, results='hide', message=FALSE, warning=FALSE}

red_prop <- bowl %>% mutate(is_red = (color == "red")) %>%
summarize(num_red = sum(is_red),
        perc = num_red/n()) %>% select(perc)

# ```
# 
# Como fomos nós que criamos a bacia com bolinhas (a nossa população em análise), podemos computar o verdadeiro valor do parâmetro. Ou seja, nós sabemos exatamente qual a proporção de bolinhas vermelhas na bacia, `r red_prop[[1]]*100`%. 
# 
# Esse é o valor que desejamos estimar com a máxima precisão e acurácia possíveis!
# 
# ## Amostragem
# 
# Vamos agora simular a nossa seleção aleatória de casos.
# 
# ```{r echo=T, results='hide', message=FALSE, warning=FALSE}

virtual_shovel <- bowl %>%
rep_sample_n(size = 50)

red_prop_sample <- virtual_shovel %>%
mutate(is_red = (color == "red")) %>%
summarize(num_red = sum(is_red),
        perc = num_red/n()) %>% select(perc)

# ```
# 
# Em nossa amostra aleatória, nós podemos identificar que a proporção de bolinhas vermelhas é igual a `r red_prop_sample[[1]]*100`%. É igual o verdadeiro valor do parâmetro?
# 
# ## Amostragem
# 
# ```{r echo=T, results='hide', message=FALSE, warning=FALSE}
n_rounds <- 30
sample_size <- 50

virtual_samples <- bowl %>%
rep_sample_n(size = sample_size, reps = n_rounds)

virtual_samples

# ```
# 
# Seguindo os pressupostos do Teorema Central do Limite, e se utilizarmos nossa pá virtual para coletar diferentes amostras (`r n_rounds` amostras) com `r sample_size` bolinhas cada? Qual será a distriuição da proporção de bolinhas vermelhas?
# 
# ## Amostragem
# 
# Seguindo os pressupostos do Teorema Central do Limite, e se utilizarmos nossa pá virtual para coletar diferentes amostras (`r n_rounds`) com `r sample_size` bolinhas cada? Qual será a distriuição da proporção de bolinhas vermelhas?
# 
# ```{r echo=F, results='hide', fig.align="center", fig.height=4, message=FALSE, warning=FALSE}

virtual_prop_red <- virtual_samples %>%
group_by(replicate) %>%
summarize(red = sum(color == "red")) %>%
mutate(prop_red = red / sample_size)

virtual_prop_red %>% ggplot(aes(x = prop_red)) +
geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
labs(x = "Proporção de bolinhas vermelhas",
   title = "Distribuição de proporções de bolinhas vermelhas")

# ```
# 
# 
# ## Amostragem
# 
# O que acontece se aumentarmos o número de amostras para 1000?
# 
# ```{r echo=F, results='hide', fig.align="center", fig.height=4, message=FALSE, warning=FALSE}

n_rounds <- 1000
sample_size <- 50

virtual_samples <- bowl %>%
rep_sample_n(size = sample_size, reps = n_rounds)

virtual_prop_red <- virtual_samples %>%
group_by(replicate) %>%
summarize(red = sum(color == "red")) %>%
mutate(prop_red = red / sample_size)

virtual_prop_red %>% ggplot(aes(x = prop_red)) +
geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
labs(x = "Proporção de bolinhas vermelhas",
   title = "Distribuição de proporções de bolinhas vermelhas")

# ```
# 
# Obtemos uma distribuição em forma de sino mais simétrica e suave. Uma distribuição bem aproximada de uma distribuição normal.
# 
# ## Amostragem
# 
# O que acontece se mantivermos o número de amostras em 1000, mas variarmos o tamamnho das amostras em 25, 50 e 100 bolinhas por experimento?
# 
# ```{r echo = F, fig.align="center", fig.height=3, message=FALSE, warning=FALSE}

n_rounds <- 1000

# Segment 1: sample size = 25 ------------------------------
sample_size <- 25

# 1.a) Virtually use shovel 1000 times
virtual_samples_25 <- bowl %>%
rep_sample_n(size = sample_size, reps = n_rounds)

# 1.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_25 <- virtual_samples_25 %>%
group_by(replicate) %>%
summarize(red = sum(color == "red")) %>%
mutate(prop_red = red / sample_size,
     experiment = paste0("n = ", sample_size))

# Segment 2: sample size = 50 ------------------------------
sample_size <- 50

# 2.a) Virtually use shovel 1000 times
virtual_samples_50 <- bowl %>%
rep_sample_n(size = sample_size, reps = n_rounds)

# 2.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_50 <- virtual_samples_50 %>%
group_by(replicate) %>%
summarize(red = sum(color == "red")) %>%
mutate(prop_red = red / sample_size,
     experiment = paste0("n = ", sample_size))

# Segment 3: sample size = 100 ------------------------------
sample_size <- 100

# 3.a) Virtually using shovel with 100 slots 1000 times
virtual_samples_100 <- bowl %>%
rep_sample_n(size = sample_size, reps = n_rounds)

# 3.b) Compute resulting 1000 replicates of proportion red
virtual_prop_red_100 <- virtual_samples_100 %>%
group_by(replicate) %>%
summarize(red = sum(color == "red")) %>%
mutate(prop_red = red / sample_size,
     experiment = paste0("n = ", sample_size))

# Plot distribution via a histogram

virtual_prop_red <- bind_rows(virtual_prop_red_25,
                          virtual_prop_red_50,
                          virtual_prop_red_100)

virtual_prop_red %>%
mutate(experiment = factor(experiment, levels = c("n = 25",
                                                "n = 50", "n = 100"))) %>%
ggplot(aes(x = prop_red)) +
geom_histogram(binwidth = 0.05, boundary = 0.4, color = "white") +
facet_wrap(~experiment) +
labs(x = "Proporção de bolinhas vermelhas",
   title = "Distribuição de proporções de bolinhas vermelhas")

# ```
# À medida que o tamanho da amostra aumenta, a variação das 1000 rodadas da proporção de vermelho diminui. Em outras palavras, conforme o tamanho da amostra aumenta, há menos diferenças devido à variação da amostragem e os centros das distribuições ficam mais próximos do mesmo valor.
# 
# ## Amostragem
# 
# Conceitos vistos até agora:
# 
# - **População** é uma coleção de indivíduos/observações em que estamos interessados.
# - **Parâmetro da população** é uma quantidade/valor numérico desconhecido, mas que desejamos saber.
# - **Censo** é a contagem de todos os N indivíduos ou observações na população, a fim de calcular o valor exato do parâmetro da população.
# - **Amostragem** é o ato de coletar uma parcela da população quando não temos os meios para realizar um censo.
# - **Estimativa pontual** (estatística da amostra) é uma estatística calculada a partir de uma amostra que estima um parâmetro desconhecido da população.
# 
# ## Amostragem
# 
# Conceitos vistos até agora (cont.):
# 
# - **Amostra representativa**: uma amostra é considerada representativa se ela for semelhante à população.
# - **Generalização**: uma amostra é generalizável se quaisquer resultados baseados na amostra puderem ser generalizados para a população.
# - **Amostragem enviesada** ocorre se certos indivíduos ou observações em uma população têm uma chance maior de serem incluídos em uma amostra do que outros.
# - **Amostragem aleatória**: um procedimento de amostragem é aleatório se a seleção de representantes da população apra compor a amostra for imparcial.
# 
# ## Amostragem
# 
# Até aqui:
# 
# - Se a seleção de uma amostra de tamanho $n$ for feita aleatoriamente, então
# - a amostra é imparcial e representativa da população de tamanho $N$. Logo,
# - qualquer resultado com base na amostra pode ser generalizado para a população e
# - a estimativa pontual é um "bom palpite" do parâmetro desconhecido da população. Isto significa que
# - em vez de realizar um censo, podemos inferir sobre a população por meio de amostragem.
# 
# ## Amostragem
# 
# Em suma, a amostragem aleatória garante que nossas estimativas pontuais sejam acuradas. E, ao mesmo tempo, ter um tamanho de amostra grande garante que nossas estimativas pontuais sejam precisas.
# 
# <center>
# ![Precisao](images/precision_accuracy.png){width=400px}
# </center>
# 
# ## Bootstraping e Intervalos de Confiança
# 
# ## Bootstraping e Intervalos de Confiança
# 
# Vimos como a teoria opera e podemos confiar na possibilidade de fazer estimativas pontuais a partir de uma amostra aleatória. 
# 
# No entanto, é viável realizar diferentes amostras de uma população do ponto de vista prático? **Não!** Na prática, temos apenas uma amostra para produzir inferências sobre a população.
# 
# Diante desse fato, como produzir inferências que não dependam da sorte de obter a amostra adequada?
# 
# Faremos isso usando uma técnica conhecida como reamostragem de *bootstrap* (*Bootstraping*) com reposição e a obtenção de intervalos de confiança.
# 
# ## Bootstraping 
# 
# <center>
# ![Penny](images/penny.png){width=600px}
# </center>
# 
# Tente imaginar todas as moedas de um centavo sendo usados nos Estados Unidos. Agora, digamos que estamos interessados no ano médio de produção das moedas de um centavo em circulação. Como é impossível adquirir todas as moedas para calcular o ano médio de produção, podemos acessar uma amostra aleatória de 50 delas.
# 
# ```{r echo = F, results='hide', message=FALSE, warning=FALSE}

pennies_sample

# ```
# 
# ## Bootstraping 
# 
# Com base nesses 50 centavos de amostra, o que podemos dizer sobre todos os centavos em circulação nos EUA? Vamos estudar algumas propriedades de nossa amostra realizando uma análise exploratória de dados. Vamos primeiro visualizar a distribuição do ano desses 50 centavos. 
# 
# ```{r echo=F, fig.align="center", fig.height=4, message=FALSE, warning=FALSE}

pennies_sample %>% ggplot(aes(x = year)) +
geom_histogram(binwidth = 10, color = "white")

# calculando a media amostral
x_bar <- pennies_sample %>%
summarize(mean_year = mean(year))

# ```
# 
# ## Bootstraping 
# 
# Se assumirmos que temos uma amostra representativa de todos os centavos dos EUA, uma boa estimativa pontual do ano médio de cunhagem de todos os centavos dos EUA seria `r x_bar[[1]]`. Em outras palavras, por volta de 1995. 
# 
# Contudo, vimos que nossa estimativa pontual está sujeita a variações de amostragem. Por exemplo, nesta amostra específica, observamos três centavos com o ano de 1999. Se amostrássemos outros 50 centavos, observaríamos exatamente três centavos com o ano de 1999 novamente? Provavelmente não. Logo, não sabemos se estamos diante de uma boa amostra e, para avançar, precisamos estudar nossa variação amostral através da única amostra que temos.
# 
# Faremos isso usando uma técnica conhecida como reamostragem de bootstrap com reposição!
# 
# ## Bootstraping 
# 
# Exemplo de uma reamostragem:
# 
# 1. Coloque todas as 50 moedas da amostra numa urna;
# 2. Agite a urna para embaralhar;
# 3. Sorteie uma moeda e anote seu ano;
# 4. Devolva a moeda sorteada para a urna;
# 5. Agite a urna para embaralhar as moedas novamente;
# 6. Faça um novo sorteio/registro e repita os passos 1 a 5 até obter 50 registros.
# 
# ## Bootstraping 
# 
# ```{r echo=F, fig.align="center", fig.height=5, message=FALSE, warning=FALSE}

pennies_resample <- pennies_sample %>%
rep_sample_n(size = 50, replace = TRUE)


# pennies_resample %>%
#  summarize(mean_year = mean(year))

pennies_resample <- pennies_resample %>%
mutate(type = "Reamostra")

pennies_sample <- pennies_sample %>%
mutate(type = "Amostra")

cents <- bind_rows(pennies_resample, pennies_sample)

cents %>% ggplot(aes(x = year)) +
geom_histogram(binwidth = 10, color = "white") +
facet_wrap(~type) +
labs(title = "Reamostra vs Amostra Original")

# ```
# 
# ## Bootstraping 
# 
# O que acabamos de realizar foi uma reamostragem da amostra original de 50 centavos. Não estamos amostrando 50 centavos da população de todos os centavos dos EUA. Em vez disso, estamos imitando essa tarefa reamostrando 50 centavos de nossa amostra original de 50 centavos. 
# 
# Utilizando simulação computacional, podemos, portanto, realizar este procedimento quantas vezes desejarmos! Ao final, temos a distribuição das médias amostrais!
# 
# ```{r echo=F, fig.align="center", fig.height=3, message=FALSE, warning=FALSE}

n_rounds <- 1000

virtual_resampled_means <- pennies_sample %>%
rep_sample_n(size = 50, replace = TRUE, reps = n_rounds) %>%
group_by(replicate) %>%
summarize(mean_year = mean(year))

# virtual_resampled_means

cents_mean_of_means <- virtual_resampled_means %>% summarize(mean_of_means = mean(mean_year))

virtual_resampled_means %>% ggplot(aes(x = mean_year)) +
geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
labs(x = "Reamostra do ano médio")

# ```
# 
# ## Bootstraping 
# 
# Acabamos de construir nossa primeira distribuição bootstrap! A média dessas 1.000 médias é `r cents_mean_of_means[[1]]`, que é bastante próxima da média de nossa amostra original de 50 centavos de `r x_bar`. Usando essa distribuição bootstrap, podemos estudar o efeito da variação da amostragem em nossas estimativas. Em particular,o "erro" típico de nossas estimativas, conhecido como erro padrão.
# 
# Para refinar nossa análise, ao invés de adotar uma simples estimativa pontual sobre o parâmetro da população (ano médio das moedas de um centavo circulando nos EUA), vamos aprender a construir intervalos de confiança.
# 
# ## Intervalos de Confiança
# 
# <center>
# ![CI](images/point_estimate_confidence_interval.png){width=700px}
# </center>
# 
# Ao contrário de uma estimativa pontual que estima o parâmetro desconhecido da população com um único valor, um intervalo de confiança (IC) permite inferir uma gama de valores plausíveis sobre o parâmetro populacional. Veremos dois métodos para construir tais intervalos, ambos construídos a partir da distribuição bootstrap: o método do percentil e o método do erro padrão.
# 
# ## Intervalos de Confiança
# 
# Ambos métodos exigem que se especifique um **nível de confiança**. Em outras palavras, a confiança com a qual assume-se que o intervalo construído possuirá o valor verdadeiro do parâmetro populacional. 
# 
# Tudo mantido constante, níveis de confiança mais altos correspondem a intervalos de confiança mais amplos e níveis de confiança mais baixos correspondem a intervalos de confiança mais estreitos. Por padrão, vamos adotar o nível de confiança de 95%.
# 
# ## Intervalos de Confiança: método do percentil
# 
# Um método para construir um intervalo de confiança é usar os 95% intermediários dos valores da distribuição de bootstrap. Podemos fazer isso calculando os percentis 2,5 e 97,5. Continuando com nosso exemplo sobre as moedas de um centavo, temos como limite inferior do intervalo `r quantile(virtual_resampled_means$mean_year, .025)` e como limite superior do intervalo `r quantile(virtual_resampled_means$mean_year, .975)`.
# 
# ```{r echo=F, fig.align="center", fig.height=3, message=FALSE, warning=FALSE}

virtual_resampled_means %>% ggplot(aes(x = mean_year)) +
geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
geom_vline(xintercept = quantile(virtual_resampled_means$mean_year, .025), linetype="dashed") +
geom_vline(xintercept = quantile(virtual_resampled_means$mean_year, .975), linetype="dashed") +
labs(x = "Reamostra do ano médio")

# ```
# 
# ## Intervalos de Confiança: método do erro padrão
# 
# Usando nossa regra de ouro de 95% sobre distribuições normais, 
# 
# $$IC = \overline{x} \pm 1.96 \cdot SE$$
# ```{r echo=F, message=FALSE, warning=FALSE}

upper_ci <- mean(virtual_resampled_means$mean_year) + 1.96*sd(virtual_resampled_means$mean_year)
lower_ci <- mean(virtual_resampled_means$mean_year) - 1.96*sd(virtual_resampled_means$mean_year)

# ```
# 
# temos como limite inferior o valor de `r lower_ci` e como limite superior o valor de `r upper_ci`.
# 
# ```{r echo=F, fig.align="center", fig.height=3, message=FALSE, warning=FALSE}

virtual_resampled_means %>% ggplot(aes(x = mean_year)) +
geom_histogram(binwidth = 1, color = "white", boundary = 1990) +
geom_vline(xintercept = quantile(virtual_resampled_means$mean_year, .025), linetype="dashed") +
geom_vline(xintercept = quantile(virtual_resampled_means$mean_year, .975), linetype="dashed") +
geom_vline(xintercept = lower_ci, linetype="solid") +
geom_vline(xintercept = upper_ci, linetype="solid") +
labs(x = "Percentil (linha tracejada) vs Erro padrão (linha sólida)")

# ```
# 
# ## Intervalos de Confiança: pacote `infer`
# 
# <center>
# ![ci_infer](images/ci_infer.png){width=900px}
# </center>
# 
# ## Intervalos de Confiança: pacote `infer`
# 
# <center>
# ![viz_infer](images/infer_visualize.png){width=900px}
# </center>
# 
# ## Intervalos de Confiança: pacote `infer`
# 
# ```{r echo=F, message=FALSE, warning=FALSE}

bootstrap_distribution <- pennies_sample %>%
specify(response = year) %>%
generate(reps = 1000) %>%
calculate(stat = "mean")

bootstrap_distribution %>% visualise()

# ```
# 
# ## Intervalos de Confiança: método do percentil com `infer`
# 
# 
# ```{r echo=F, message=FALSE, warning=FALSE}

percentile_ci <- bootstrap_distribution %>%
get_confidence_interval(level = 0.95, type = "percentile")

# percentile_ci

bootstrap_distribution %>% visualise() +
shade_ci(endpoints = percentile_ci)

# ```
# 
# ## Intervalos de Confiança: método do erro padrão com `infer`
# 
# 
# ```{r echo=F, message=FALSE, warning=FALSE}

standard_error_ci <- bootstrap_distribution %>%
get_confidence_interval(type = "se", point_estimate = mean(bootstrap_distribution$stat))

# standard_error_ci

bootstrap_distribution %>% visualise() +
shade_ci(endpoints = standard_error_ci, fill = "khaki")

# ```
# 
# ## Intervalos de Confiança: interpretação
# 
# A eficácia de um intervalo de confiança é avaliada pelo fato de conter ou não o valor verdadeiro do parâmetro da população. Logo, o intervalo de confiança construído a partir de 95% de nível de confiança **sempre** captura o valor do parâmetro populacional? A resposta é **não!**
# 
# - **Interpretação precisa**: Se repetirmos nosso procedimento de amostragem um grande número de vezes, esperamos que cerca de 95% dos intervalos de confiança resultantes capturem o valor do parâmetro da população. 
# 
# Nosso procedimento de construção do intervalo de confiança é 95% confiável. Ou seja, podemos esperar que nossos intervalos de confiança incluam o parâmetro real da população em cerca de 95% do tempo.
# 
# ## Intervalos de Confiança: interpretação
# 
# - **Interpretação incorreta**: Há 95% de probabilidade de que o intervalo de confiança contenha o parâmetro. 
# 
# Perceba que o intervalo de confiança contém ou não contém o parâmetro. Em outras palavras, a probabilidade é 1 ou 0.
# 
# Portanto, se o nível de confiança de 95% se relaciona apenas à confiabilidade do procedimento de construção do intervalo de confiança e não a um determinado intervalo de confiança em si, que percepção pode ser derivada de um determinado intervalo de confiança? 
# 
# ## Intervalos de Confiança: interpretação
# 
# Em termos gerais, podemos pensar nos intervalos calculados (método do percentil ou do erro padrão) como nossa “melhor estimativa”, "melhor palpite", de uma faixa plausível de valores para o parâmetro populacional. 
# 
# - **Interpretação resumida**: estamos 95% “confiantes” de que um intervalo de confiança de 95% captura o valor do parâmetro da população.
# 
# ## Trade-off entre nível de confiança, tamanho da amostra e intervalo de confiança
# 
# <center>
# ![Trade Off](images/garfield.PNG){width=800px}
# 
# [Uma simulação ilustrativa](http://rpsychologist.com/d3/CI/)
# </center>
# 
# 1. Níveis de confiança mais altos tendem a produzir intervalos de confiança mais amplos.
# 2. Tamanhos de amostra maiores tendem a produzir intervalos de confiança mais estreitos.
# 
# ## Aplicação: World Values Survey
# 
# O [World Values Survey](https://www.worldvaluessurvey.org/) é um projeto que faz 
# pesquisas de opinião representativas em diversos países, entre eles o Brasil.
# 
# As perguntas são padronizadas, de modo a facilitar a comparação. São avaliadas as 
# percepções das pessoas sobre assuntos como instituições políticas, capital social, 
# religião e valores pós-materialistas.
# 
# Vamos criar um objeto `wvs` com os resultados.
# 
# ```{r echo=F, results='hide'}

wvs <- tibble(readRDS(here("./data/EVS_WVS_Cross-National_Wave_7_joint_core_R_v1_1.rds")))

# vamos conhecer nossos dados
glimpse(wvs)

table(wvs$C_COW_ALPHA) # n de entrevistas em cada país

wvs %>% select(C_COW_ALPHA) %>% mutate( C_COW_ALPHA = factor(C_COW_ALPHA)) %>%
skim()

# ```
# 
# ## Confiança nas instituições
# Usando os dados do WVS, podemos estimar a confiança dos brasileiros em suas instituiições políticas. Vamos utilizar o seguinte bloco de perguntas:
# 
# <center>
# ![wvs](images/wvs.png){width=600}
# </center>
# 
# ## Confiança nas instituições
# 
# Qual é o percentual de pessoas que confia em partidos políticos no Brasil?
# 
# ```{r echo=F, results='hide', fig.align="center", fig.height=4, message=FALSE, warning=FALSE}

partidos_bootstrap <- wvs %>%
select(C_COW_ALPHA, A_YEAR, Q72) %>%
filter(C_COW_ALPHA == "BRA") %>%
mutate(confia_em_partidos = case_when(Q72 %in% c(1, 2) ~ 1,
                                    TRUE ~ 0)) %>%
specify(response = confia_em_partidos) %>%
generate(reps = 1000, type = "bootstrap") %>%
calculate(stat = "mean")

ci_se <- partidos_bootstrap %>%
get_confidence_interval(type = "se", point_estimate = mean(partidos_bootstrap$stat))

partidos_bootstrap %>%  visualize() +
shade_confidence_interval(endpoints = ci_se)

# ```
# 
# 
# 
# *Resposta*: Pela pesquisa, podemos estimar que `r round(mean(partidos_bootstrap$stat)*100, 1)`% dos brasileiros confiavam nos partidos políticos em 2018. Se repetíssemos essa pesquisa infinitas vezes, 95% das médias estariam entre `r round(ci_se$lower_ci*100, 1)`%  e `r round(ci_se$upper_ci*100, 1)`%. 
# 
# 
# ## Exercício
# 
# Identifique o país com maior confiança na ONU, e construa o intervalo de 95% de confiança em torno da média. 
# 
# ## Exercício: resposta



# 
# ##  Comunicando incerteza
# Lembre-se de que não estamos olhando para toda a população. Tão importante quanto 
# identificar qual é o “melhor palpite” sobre o valor do parâmetro é comunicar seu grau de 
# incerteza em relação à estimativa.
# 
# - **Intervalo de confiança**: estimativa intervalar, calculada pelo pesquisador durante a análise
# - **Nível de confiança**: <span class="red">definido</span> pelo pesquisador antes da análise dos dados
# - **Margem de erro**: erro-padrão multiplicado pelo número de desvios-padrões que produzem o nível de confiança definido previamente pelo pesquisador
# 
# <center>
# **intervalo de confiança = estimativa pontual $\pm$ m. de erro**
# </center>
# 
# ## Em resumo
# 
# ## Amostragem e inferência
# 
# - O primeiro passo é deixar de pensar em distribuição dos dados e passar a pensar 
# em **distribuiçao amostral**
# - Distribuição amostral é uma distribuição de *estatísticas*, em vez das distribuições 
# de observações com as quais estamos habituados
# - O [Teorema Central do Limite](https://seeing-theory.brown.edu/probability-distributions/index.html#section3) mostra 
#                             **convergência em distribuição**: qualquer que seja a distribuição original das observações, suas 
#                             médias convergem para a distribuição Normal
#                             - A [Lei dos Grandes Números](https://seeing-theory.brown.edu/basic-probability/index.html#section2) mostra **convergência pontual**: com n grande o suficiente, as estimativas convergem para o valor 
#                                                           verdadeiro na população
#                                                           
#                                                           ## Em inferência, é muito importante comunicar incerteza!
#                                                           
#                                                           Inferência é um **palpite bem informado** sobre o valor do parâmetro, condicionado 
#                                                           a algumas suposições. Não basta falar qual é o seu palpite, é fundamental comunicar 
#                                                           com clareza quão certo você está dele!
#                                                             
#                                                             ## Em inferência, é muito importante comunicar incerteza!
#                                                             
#                                                             - <span class="red">Estimativa pontual</span> é o melhor palpite que podemos dar 
#                                                           sobre o valor do parâmetro. Sua representação mais comum é a **média**
#                                                             - <span class="red">Estimativa intervalar</span> é um conjunto de valores que consideramos 
#                                                           palpites razoáveis. Quanto mais largo o intervalo, maior nossa incerteza. Geralmente é 
#                                                           representado pelo **intervalo de confiança** ou pela **margem de erro**
#                                                             - <span class="red">Nível de confiança</span> é um valor definido *a priori*, que traduz 
#                                                           qual é o grau de incerteza que estamos dispostos a aceitar em nosso palpite
#                                                           
#                                                           ## Não confunda!
#                                                           
#                                                           - Distribuição da população **vs** distribuição dos dados amostrais **vs** distribuição amostral **vs** distribuição bootstrapping
#                                                           - Desvio-padrão **vs** erro padrão
#                                                           - Número de amostras **vs** número de observações
#                                                           
#                                                           ## A leitura no nosso primeiro exemplo faz sentido?
#                                                           
#                                                           Datafolha em 2018: 
#                                                             
#                                                             <center>
#                                                             ![Datafolha 2018](images/datafolha_2018.PNG){width=900px}
#                                                           </center>
#                                                             
#                                                             Datafolha em 2014: 
#                                                             
#                                                             <center>
#                                                             ![Datafolha 2014](images/datafolha_2014.PNG){width=900px}
#                                                           </center>
#                                                             
#                                                             ## Material adicional
#                                                             
#                                                             - [Modern Dive](https://moderndive.com/): inferência estatística no formato tidy
#                                                           - [gginference](https://github.com/okgreece/gginference)
#                                                           - [Seeing Theory](https://seeing-theory.brown.edu/)
#                                                           <!--- - [Causal Inference: The Mixtape](https://mixtape.scunning.com/) --->
#                                                             
#                                                             ## Tarefa da aula
#                                                             
# As instruções da tarefa estão no arquivo `NN-ds4ps-inference-assignment.rmd` da pasta 
# `assignment` deste projeto.
#                                                           
#                                                           