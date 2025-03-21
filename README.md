
# Artigo

**Título:** Fatores associados à participação dos idosos por cor ou raça no mercado de trabalho brasileiro

Artigo publicado no âmbito do Desafio Longeviver 2023: (Clique aqui para acessar)[https://cebrap.org.br/wp-content/uploads/2023/11/Desafio-Longeviver-2023.pdf].

*Paper published in Portuguese without translation. This Readme file is also in portuguese*

## Dados

Foi utilizado dados da Pesquisa Nacional por Amostra de Domicílios Contínua (PNADC).

- **Período:** 2012-2019 (para o artigo) e 2012-2023 (para o presente Readme file).
  
- **Recorte espacial**: Brasil.

## Objetivo deste readme

Desde a publicação do artigo em 2023, novos dados da PNADC foram divulgados, o que possibilitou a extensão do período de análise até 2023. Com isso, mudanças das tendências pós-pandemia.

Além disso, atualizações dos modelos de regressão utilizados no artigo também foram feitas com o intuito de aprofundar os diferenciais entre grupos raciais. Mais detalhes serão apresentados a seguir.


## Mudanças metodológicas - propensão de participar no mercado de trabalho

Modelo de regressão logística foi utilizado para estimar a propensão de participar no mercado de trabalho para a população idosa.

Esse readme conta com duas específicações de modelos:

**Modelo 1**: modelo *sem* interação entre covariáveis e raça.

$$Modelo \space 1 : y = \log(\frac{p}{1-p}) = \alpha + \gamma x_{raça} + \beta_i x_i + \theta_j x_j$$

**Modelo 2**: modelo *com* interação entre covariáveis e raça.

$$Modelo \space 1 : y = \log(\frac{p}{1-p}) = \alpha + \gamma x_{raça} + \beta_i x_i + \theta_j x_j + \beta_i x_i \cdot x_{raça} + \theta_j x_j \cdot x_{raça}$$

Em que:

- $$

