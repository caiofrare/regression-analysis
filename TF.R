# ============================================================================
# IMC, Tabagismo e Custos de Saude: uma Analise de Regressao do
# Medical Cost Personal Dataset
#
# Script R com todo o codigo e as analises do trabalho, sem a formatacao
# de relatorio (LaTeX/bookdown).
# ============================================================================

# ---- Pacotes ----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(GGally)
library(lmtest)
library(broom)
set.seed(1)

options(scipen = 9999)

# ==============================================================================
# 1. BANCO DE DADOS
# ==============================================================================

# O dataset Medical Cost Personal Dataset (Kaggle, mirichoi0218/insurance) e
# pequeno (1338 linhas). Para garantir reprodutibilidade sem depender de
# upload manual, ele e baixado de um espelho publico no GitHub.
url <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv"
dados <- read.csv(url, stringsAsFactors = FALSE)

# Nao ha filtros de observacoes: o banco nao possui valores faltantes
# (sum(is.na(dados)) == 0) e todas as 1338 observacoes sao utilizadas.
n_obs <- nrow(dados)
n_na  <- sum(is.na(dados))
n_obs
n_na

# Codebook das variaveis:
#   age      - Numerica (discreta)  - Idade do beneficiario, em anos
#   sex      - Categorica           - Sexo (female, male)
#   bmi      - Numerica (continua)  - Indice de Massa Corporal (kg/m^2)
#   children - Numerica (discreta)  - Numero de filhos/dependentes cobertos pelo plano
#   smoker   - Categorica           - Indicador de tabagismo (yes, no)
#   region   - Categorica           - Regiao dos EUA (northeast, northwest, southeast, southwest)
#   charges  - Numerica (continua)  - Custo medico anual cobrado pelo plano (US$) -- resposta

codebook <- data.frame(
  Variavel = c("age", "sex", "bmi", "children", "smoker", "region", "charges"),
  Tipo = c("Numerica (discreta)", "Categorica", "Numerica (continua)",
           "Numerica (discreta)", "Categorica", "Categorica", "Numerica (continua)"),
  Descricao = c(
    "Idade do beneficiario, em anos",
    "Sexo (female, male)",
    "Indice de Massa Corporal (kg/m2)",
    "Numero de filhos/dependentes cobertos pelo plano",
    "Indicador de tabagismo (yes, no)",
    "Regiao dos EUA (northeast, northwest, southeast, southwest)",
    "Custo medico anual cobrado pelo plano (US$) - variavel resposta"
  )
)
print(codebook)

# ==============================================================================
# 2. ANALISE EXPLORATORIA
# ==============================================================================

# ---- Estatisticas descritivas das variaveis numericas ----
desc_num <- dados %>%
  summarise(across(c(age, bmi, children, charges),
                    list(Minimo = min, Media = mean, Mediana = median,
                         Maximo = max, `Desvio.padrao` = sd))) %>%
  pivot_longer(everything(),
               names_to = c("Variavel", ".value"),
               names_sep = "_")
print(desc_num)

charges_media <- mean(dados$charges)
charges_med   <- median(dados$charges)
prop_fumante  <- mean(dados$smoker == "yes") * 100
charges_media
charges_med
prop_fumante

# Analise: o custo medio anual (charges_media) e bem superior a mediana
# (charges_med), o que ja antecipa uma distribuicao fortemente assimetrica a
# direita. Os fumantes representam apenas ~prop_fumante% da amostra, e as
# quatro regioes e os dois sexos estao aproximadamente balanceados.

# ---- Pergunta 1: como se distribui o custo medico? ----
# `charges` e fortemente assimetrico a direita: a maioria dos beneficiarios
# concentra-se em custos baixos, com uma cauda longa de casos muito caros.
# Apos a transformacao logaritmica, a distribuicao torna-se aproximadamente
# simetrica. Essa e a principal decisao de pre-processamento do trabalho:
# adotamos log(charges) como variavel resposta, o que estabiliza a variancia
# e aproxima os residuos da normalidade (confirmado mais adiante).

p1 <- ggplot(dados, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "#3182bd", color = "white") +
  labs(x = "Custo (US$)", y = "Frequencia") +
  theme_minimal()
p2 <- ggplot(dados, aes(x = log(charges))) +
  geom_histogram(bins = 30, fill = "#3182bd", color = "white") +
  labs(x = "log(Custo)", y = "Frequencia") +
  theme_minimal()
gridExtra::grid.arrange(p1, p2, ncol = 2)
# Distribuicao do custo medico anual na escala original (esquerda) e
# logaritmica (direita). A forte assimetria a direita na escala original
# motiva o uso de log(charges) como resposta.

# ---- Pergunta 2: quais variaveis mais separam o custo? ----
# O contraste entre fumantes e nao fumantes e, de longe, o mais expressivo:
# o custo mediano dos fumantes e varias vezes maior. Sexo e regiao tem efeito
# modesto, e o numero de filhos mostra leve tendencia crescente.

g_smoker <- ggplot(dados, aes(factor(smoker), charges, fill = factor(smoker))) +
  geom_boxplot(alpha = .9) + scale_fill_brewer(palette = "Blues") +
  labs(x = "Fumante", y = "Custo (US$)", title = "Por tabagismo") +
  theme_minimal() + theme(legend.position = "none")
g_sex <- ggplot(dados, aes(factor(sex), charges, fill = factor(sex))) +
  geom_boxplot(alpha = .9) + scale_fill_brewer(palette = "Blues") +
  labs(x = "Sexo", y = "Custo (US$)", title = "Por sexo") +
  theme_minimal() + theme(legend.position = "none")
g_region <- ggplot(dados, aes(factor(region), charges, fill = factor(region))) +
  geom_boxplot(alpha = .9) + scale_fill_brewer(palette = "Blues") +
  labs(x = "Regiao", y = "Custo (US$)", title = "Por regiao") +
  theme_minimal() + theme(legend.position = "none",
                          axis.text.x = element_text(angle = 20, hjust = 1))
g_child <- ggplot(dados, aes(factor(children), charges, fill = factor(children))) +
  geom_boxplot(alpha = .9) + scale_fill_brewer(palette = "Blues") +
  labs(x = "N de filhos", y = "Custo (US$)", title = "Por n de filhos") +
  theme_minimal() + theme(legend.position = "none")
gridExtra::grid.arrange(g_smoker, g_sex, g_region, g_child, ncol = 2)
# Distribuicao do custo medico por condicao de tabagismo, sexo, regiao e
# numero de filhos. O tabagismo e a variavel categorica que mais separa
# o custo.

# ---- Pergunta 3: como idade e IMC se associam ao custo, e isso depende de fumar? ----
# Este e o achado central da analise exploratoria. A idade tem efeito linear
# positivo e semelhante em ambos os grupos, mas o patamar de custo e muito
# mais alto entre fumantes -- sugerindo o tabagismo como fator dominante.
# Entre nao fumantes o custo e praticamente insensivel ao IMC (curva quase
# plana), enquanto entre fumantes ele dispara a partir de IMC ~ 30 (faixa de
# obesidade). Ou seja: o efeito do IMC sobre o custo depende de fumar --
# evidencia visual de uma interacao smoker:bmi, que justifica sua inclusao
# no modelo.

cores_smoker <- c("no" = "grey40", "yes" = "#08519c")
s1 <- ggplot(dados, aes(age, charges, color = smoker)) +
  geom_point(alpha = .4) + geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = cores_smoker) +
  labs(x = "Idade (anos)", y = "Custo (US$)", color = "Fumante") +
  theme_minimal()
s2 <- ggplot(dados, aes(bmi, charges, color = smoker)) +
  geom_point(alpha = .4) + geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(values = cores_smoker) +
  labs(x = "IMC (kg/m2)", y = "Custo (US$)", color = "Fumante") +
  theme_minimal()
gridExtra::grid.arrange(s1, s2, ncol = 2)
# Custo por idade (esquerda) e por IMC (direita), segundo a condicao de
# tabagismo. Entre nao fumantes o IMC quase nao altera o custo; entre
# fumantes o custo dispara apos IMC aproximado de 30, evidenciando
# interacao smoker:bmi.

# ==============================================================================
# 3. MODELO PROPOSTO
# ==============================================================================

# Com base na analise exploratoria, adotamos como modelo final uma regressao
# linear sobre o LOGARITMO do custo, incluindo todas as covariaveis e a
# interacao smoker:bmi:
#
#   log(charges_hat) = b0 + b1*age + b2*bmi + b3*children + b4*sex +
#                       b5*smoker + b6*region + b7*(smoker x bmi) + erro

fit_final <- lm(log(charges) ~ age + bmi + children + sex + smoker + region +
                  smoker:bmi, data = dados)
summary(fit_final)

r2_aj <- summary(fit_final)$adj.r.squared
cf <- coef(fit_final)
ic <- confint(fit_final)
r2_aj
cf
ic

# Converte coeficientes em variacao percentual aproximada: (exp(b)-1)*100
pct <- function(b) (exp(b) - 1) * 100

# O modelo explica round(r2_aj * 100, 1)% da variabilidade do log-custo
# (R2 ajustado = round(r2_aj, 3)).

tab_coef <- tidy(fit_final, conf.int = TRUE) %>%
  transmute(
    Termo = c("Intercepto", "Idade", "IMC", "N de filhos", "Sexo (masculino)",
              "Fumante (sim)", "Regiao: noroeste", "Regiao: sudeste",
              "Regiao: sudoeste", "IMC : Fumante"),
    Estimativa = round(estimate, 4),
    `IC 95% inf` = round(conf.low, 4),
    `IC 95% sup` = round(conf.high, 4),
    `p-valor` = ifelse(p.value < 0.001, "< 0,001", formatC(p.value, format = "f", digits = 3))
  )
print(tab_coef)

# ---- Interpretacao dos parametros ----
# Como a resposta esta em escala logaritmica, cada coeficiente beta traduz-se
# em uma variacao percentual aproximada do custo dada por (e^beta - 1) * 100%.

# Idade: cada ano adicional de idade esta associado a um aumento de
# aproximadamente pct(cf["age"])% no custo esperado, mantidas as demais
# variaveis constantes.
round(cf["age"], 4)
round(pct(cf["age"]), 1)

# Numero de filhos: cada dependente adicional eleva o custo em cerca de
# pct(cf["children"])%.
round(cf["children"], 4)
round(pct(cf["children"]), 1)

# Sexo masculino: homens tem custo esperado cerca de |pct(cf["sexmale"])|%
# menor que mulheres, tudo o mais constante.
round(cf["sexmale"], 4)
round(abs(pct(cf["sexmale"])), 1)

# Regiao: tomando o nordeste como referencia, as tres demais regioes
# apresentam custos esperados ligeiramente menores.
round(abs(pct(cf["regionnorthwest"])), 1)
round(abs(pct(cf["regionsoutheast"])), 1)
round(abs(pct(cf["regionsouthwest"])), 1)

# IMC e tabagismo (efeito conjunto) -- resultado central. Por causa da
# interacao, o efeito do IMC sobre o custo DIFERE entre fumantes e nao
# fumantes:
#
# - Entre NAO FUMANTES, o coeficiente do IMC e cf["bmi"], ou seja, um
#   aumento de 1 ponto no IMC eleva o custo em apenas cerca de
#   pct(cf["bmi"])% -- efeito pequeno e nao significativo.
round(cf["bmi"], 4)
summary(fit_final)$coefficients["bmi", "Pr(>|t|)"]
round(pct(cf["bmi"]), 1)

# - Entre FUMANTES, o efeito do IMC e a soma dos coeficientes bmi e
#   smoker:bmi, de modo que cada ponto adicional de IMC aumenta o custo em
#   aproximadamente pct(bmi + bmi:smokeryes)%. A interacao e altamente
#   significativa (p < 0,001), confirmando estatisticamente o padrao visto
#   no grafico de dispersao por IMC: a obesidade e financeiramente onerosa
#   sobretudo quando combinada ao tabagismo.
round(cf["bmi"] + cf["bmi:smokeryes"], 4)
round(pct(cf["bmi"] + cf["bmi:smokeryes"]), 1)
round(cf["bmi:smokeryes"], 4)

# ---- Predito vs. observado ----
# Compara os valores preditos pelo modelo com os observados (na escala
# original de dolares, retornando exp(log(charges)_hat)). Os pontos
# acompanham razoavelmente a reta identidade (coeficiente angular 1),
# indicando boa capacidade preditiva, com alguma subestimacao nos custos
# mais altos -- esperada por se tratar de um modelo na escala logaritmica.

dados_pred <- dados %>%
  mutate(predito = exp(predict(fit_final)))
ggplot(dados_pred, aes(charges, predito)) +
  geom_point(alpha = .4, color = "#3182bd") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
  labs(x = "Custo observado (US$)", y = "Custo predito (US$)") +
  theme_minimal()
# Custo predito pelo modelo final versus custo observado (escala original,
# US$). A linha tracejada e a reta identidade (predito = observado).

# ==============================================================================
# 4. DISCUSSAO
# ==============================================================================

# Objetivo (i) - efeito isolado e conjunto de IMC e tabagismo: os resultados
# mostram que esses dois fatores nao atuam de forma independente: o IMC,
# isoladamente, tem efeito praticamente nulo sobre o custo entre nao
# fumantes, mas torna-se um forte determinante entre fumantes. O tabagismo,
# por sua vez, e a variavel de maior impacto individual, e seu efeito e
# amplificado pela obesidade.
#
# Objetivo (ii) - demais covariaveis: comportam-se conforme o esperado: a
# idade tem efeito positivo e significativo, o numero de dependentes eleva
# moderadamente o custo, e sexo e regiao tem contribuicoes pequenas, porem
# estatisticamente detectaveis. O modelo explica cerca de round(r2_aj*100,1)%
# da variabilidade do log-custo, um ajuste elevado para dados individuais de
# saude.
#
# Objetivo (iii) - comunicabilidade: a interpretacao em termos de variacao
# percentual ((e^beta - 1) * 100%) permite que os resultados sejam
# comunicados a um publico nao tecnico -- por exemplo, "cada ano de idade
# encarece o plano em torno de pct(cf['age'])%".
#
# Aplicacao pratica: o modelo e diretamente util para a precificacao
# atuarial e a gestao de risco de operadoras de saude -- identifica o
# subgrupo de fumantes obesos como o de maior custo esperado, justificando
# premios diferenciados e programas de prevencao (cessacao do tabagismo e
# controle de peso) com potencial retorno financeiro elevado.
#
# Limitacoes:
# (a) dados observacionais -- as associacoes estimadas nao devem ser
#     interpretadas como relacoes causais;
# (b) mesmo apos a transformacao logaritmica, os residuos ainda exibem
#     heterocedasticidade e caudas mais pesadas que a normal, o que
#     recomenda cautela sobretudo na predicao de custos extremos -- embora
#     o grande tamanho amostral assegure inferencia aproximadamente valida
#     sobre os coeficientes;
# (c) o banco nao inclui variaveis clinicas relevantes (comorbidades,
#     historico medico), o que limita o poder explicativo.
# Trabalhos futuros poderiam explorar erros-padrao robustos a
# heterocedasticidade, modelos com cauda pesada ou modelos de regressao
# gama, mais adequados a custos estritamente positivos e assimetricos.

# ==============================================================================
# 5. APENDICE
# ==============================================================================

# ---- 5.1 Selecao do modelo: escala da resposta e interacao ----
# Foram comparados tres modelos de complexidade crescente:
#   (1) fit_orig  - regressao de charges na escala original, todas covariaveis
#   (2) fit_log   - igual ao anterior, mas com resposta log(charges)
#   (3) fit_final - o modelo log acrescido da interacao smoker:bmi

fit_orig  <- lm(charges ~ ., data = dados)
fit_log   <- lm(log(charges) ~ ., data = dados)
# fit_final ja ajustado na secao 3

bp_orig  <- bptest(fit_orig)$p.value
bp_log   <- bptest(fit_log)$p.value
bp_orig
bp_log

# O teste de Breusch-Pagan detecta heterocedasticidade em AMBAS as escalas.
# Ainda assim, a transformacao logaritmica e adotada porque reduz
# drasticamente a forte assimetria a direita da resposta e a severidade do
# padrao de funil nos residuos, alem de aproximar os residuos da
# normalidade e permitir a interpretacao dos efeitos em termos percentuais;
# a heterocedasticidade remanescente e tratada como limitacao.

comp <- data.frame(
  Modelo = c("log(charges) ~ . (sem interacao)", "log(charges) ~ . + smoker:bmi"),
  `R2 ajustado` = round(c(summary(fit_log)$adj.r.squared,
                          summary(fit_final)$adj.r.squared), 4),
  AIC = round(c(AIC(fit_log), AIC(fit_final)), 1),
  BIC = round(c(BIC(fit_log), BIC(fit_final)), 1),
  check.names = FALSE
)
print(comp)
# A inclusao da interacao aumenta o R2 ajustado e reduz substancialmente
# tanto o AIC quanto o BIC (mesmo este ultimo, que penaliza fortemente
# parametros extras).

anova_int <- anova(fit_log, fit_final)
anova_int
F_int <- anova_int$F[2]
F_int

# O teste F para modelos aninhados confirma a significancia da interacao
# (p < 0,001): a reducao na soma de quadrados dos residuos e grande demais
# para ser atribuida ao acaso. A evidencia e, portanto, unanime a favor do
# modelo com interacao, adotado como modelo final.

# ---- 5.2 Diagnostico do modelo final ----

par(mfrow = c(2, 2))
plot(fit_final)
par(mfrow = c(1, 1))
# Graficos de diagnostico dos residuos do modelo final: residuos vs.
# ajustados, Q-Q normal, escala-locacao e residuos vs. alavancagem.

res <- residuals(fit_final)
ggplot(data.frame(res), aes(res)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#3182bd", color = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(res), sd = sd(res)),
                color = "darkred", linewidth = 1) +
  labs(x = "Residuos", y = "Densidade") +
  theme_minimal()
# Histograma dos residuos do modelo final com a densidade normal padrao
# sobreposta (linha vermelha).

sh <- shapiro.test(res)
bp <- bptest(fit_final)
dw <- dwtest(fit_final)
sh
bp
dw

# Os residuos nao exibem padrao sistematico contra os valores ajustados
# (sustentando a forma funcional adotada), embora o Q-Q plot e o histograma
# revelem caudas mais pesadas que a normal. Os testes formais corroboram
# esse quadro:
#
# - Normalidade (Shapiro-Wilk): rejeita-se a normalidade dos residuos,
#   devido as caudas pesadas. Dado o tamanho da amostra (n_obs
#   observacoes), o Teorema Central do Limite garante validade aproximada
#   da inferencia sobre os coeficientes.
# - Homocedasticidade (Breusch-Pagan): rejeita-se a hipotese de variancia
#   constante: ha heterocedasticidade remanescente, embora atenuada em
#   relacao a escala original. Como a amostra e grande, as estimativas dos
#   coeficientes permanecem nao-viesadas e a inferencia e aproximadamente
#   valida.
# - Independencia (Durbin-Watson): nao ha evidencia de autocorrelacao dos
#   residuos.
#
# Em sintese, o modelo final satisfaz a suposicao de independencia dos
# residuos, mas apresenta dois desvios: heterocedasticidade remanescente
# (atenuada pela transformacao logaritmica) e caudas mais pesadas que a
# normal. Ambos sao mitigados pelo grande tamanho amostral.

# ---- 5.3 Correlacao entre as variaveis continuas ----
# O triangulo inferior traz os diagramas de dispersao, a diagonal as
# densidades univariadas e o triangulo superior os coeficientes de
# correlacao de Pearson entre custo, idade e IMC. As correlacoes entre os
# preditores (idade e IMC) sao baixas, o que afasta preocupacoes com
# multicolinearidade; o custo, por sua vez, associa-se de forma apenas
# moderada as variaveis continuas isoladamente -- coerente com o achado de
# que o principal determinante (tabagismo e sua interacao com o IMC) e
# categorico.

dados %>%
  select(charges, age, bmi) %>%
  ggpairs(
    columnLabels = c("Custo (US$)", "Idade", "IMC (kg/m2)"),
    lower = list(continuous = wrap("points", colour = "#3182bd", alpha = 0.4)),
    diag  = list(continuous = wrap("densityDiag", fill = "#3182bd", alpha = 0.6)),
    upper = list(continuous = wrap("cor", size = 4))
  ) +
  theme_minimal()

# ---- 5.4 Ambiente computacional ----
# Analises conduzidas em R, com os pacotes tidyverse, ggplot2, GGally,
# lmtest, broom e knitr.
sessionInfo()
