
# Artigo

**Título:** Fatores associados à participação dos idosos por cor ou raça no mercado de trabalho brasileiro

Artigo publicado no âmbito do Desafio Longeviver 2023: [Clique aqui para acessar](https://cebrap.org.br/wp-content/uploads/2023/11/Desafio-Longeviver-2023.pdf).

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

- $i$ diz respeito aos diferentes fatores incluídos no modelo. Sendo eles: sexo, grupo etário quinquenal, quintil de renda domiciliar, nível de escolaridade, parcela da renda do idoso na renda domiciliar, parcela da renda de aposentadoria na renda domiciliar, tipo de domicílio, localização da residência e região geográfica.

- $j$ diz resito às dummies para período, variando entre 2012-2023.

Vale ressaltar que o modelo interativo não incluiu a variável sexo na interação, uma vez que não houve indícios de significância estatística nos testes de especificidade do modelo.

## Comparação entre modelos

A tabela a seguir apresenta as estimativas dos coeficientes para os modelos 1 e 2.

<table style="text-align:center"><caption><strong>Coeficientes dos modelos sem interação (1) e com interação (2)</strong></caption>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2">Participa ou não no mercado de trabalho</td></tr>
<tr><td style="text-align:left"></td><td>Model 1</td><td>Model 2</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">cor_racaNegro</td><td>0.110<sup>***</sup></td><td>0.662<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.086, 0.133)</td><td>(0.387, 0.937)</td></tr>
<tr><td style="text-align:left">sexoFeminino</td><td>-1.154<sup>***</sup></td><td>-1.154<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-1.176, -1.132)</td><td>(-1.176, -1.132)</td></tr>
<tr><td style="text-align:left">grupo_etario65</td><td>-0.682<sup>***</sup></td><td>-0.673<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-0.707, -0.656)</td><td>(-0.710, -0.635)</td></tr>
<tr><td style="text-align:left">grupo_etario70</td><td>-1.302<sup>***</sup></td><td>-1.293<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-1.334, -1.270)</td><td>(-1.339, -1.246)</td></tr>
<tr><td style="text-align:left">grupo_etario75</td><td>-1.880<sup>***</sup></td><td>-1.864<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-1.924, -1.836)</td><td>(-1.926, -1.802)</td></tr>
<tr><td style="text-align:left">grupo_etario80</td><td>-2.594<sup>***</sup></td><td>-2.703<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-2.657, -2.530)</td><td>(-2.792, -2.614)</td></tr>
<tr><td style="text-align:left">grupo_etario85</td><td>-3.200<sup>***</sup></td><td>-3.213<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-3.308, -3.093)</td><td>(-3.358, -3.068)</td></tr>
<tr><td style="text-align:left">grupo_etario90</td><td>-3.758<sup>***</sup></td><td>-3.832<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-4.130, -3.385)</td><td>(-4.351, -3.313)</td></tr>
<tr><td style="text-align:left">quintil_incP40</td><td>0.063</td><td>0.022</td></tr>
<tr><td style="text-align:left"></td><td>(-0.008, 0.134)</td><td>(-0.135, 0.180)</td></tr>
<tr><td style="text-align:left">quintil_incP60</td><td>0.573<sup>***</sup></td><td>0.554<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.506, 0.639)</td><td>(0.409, 0.699)</td></tr>
<tr><td style="text-align:left">quintil_incP80</td><td>0.681<sup>***</sup></td><td>0.634<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.615, 0.748)</td><td>(0.491, 0.777)</td></tr>
<tr><td style="text-align:left">quintil_incP100</td><td>1.302<sup>***</sup></td><td>1.274<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.234, 1.370)</td><td>(1.130, 1.418)</td></tr>
<tr><td style="text-align:left">educ_atingidaEnsino Fundamental</td><td>0.377<sup>***</sup></td><td>0.450<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.343, 0.411)</td><td>(0.385, 0.516)</td></tr>
<tr><td style="text-align:left">educ_atingidaEnsino Médio</td><td>0.532<sup>***</sup></td><td>0.602<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.489, 0.575)</td><td>(0.528, 0.677)</td></tr>
<tr><td style="text-align:left">educ_atingidaEnsino Superior</td><td>0.737<sup>***</sup></td><td>0.848<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.689, 0.785)</td><td>(0.770, 0.926)</td></tr>
<tr><td style="text-align:left">inc_prop_individuoAlguma</td><td>2.541<sup>***</sup></td><td>2.810<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(2.488, 2.594)</td><td>(2.725, 2.896)</td></tr>
<tr><td style="text-align:left">inc_prop_individuoMaior</td><td>4.291<sup>***</sup></td><td>4.627<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(4.235, 4.347)</td><td>(4.536, 4.718)</td></tr>
<tr><td style="text-align:left">inc_prop_aposentado_domAlguma</td><td>-1.492<sup>***</sup></td><td>-1.556<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-1.519, -1.465)</td><td>(-1.596, -1.516)</td></tr>
<tr><td style="text-align:left">inc_prop_aposentado_domMaior</td><td>-3.926<sup>***</sup></td><td>-4.207<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-3.965, -3.888)</td><td>(-4.266, -4.148)</td></tr>
<tr><td style="text-align:left">tipo_domEstendida</td><td>-0.066</td><td>-0.056</td></tr>
<tr><td style="text-align:left"></td><td>(-0.155, 0.023)</td><td>(-0.189, 0.078)</td></tr>
<tr><td style="text-align:left">tipo_domNuclear</td><td>-0.075</td><td>-0.058</td></tr>
<tr><td style="text-align:left"></td><td>(-0.163, 0.013)</td><td>(-0.189, 0.074)</td></tr>
<tr><td style="text-align:left">tipo_domUnipessoal</td><td>-0.666<sup>***</sup></td><td>-0.586<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-0.761, -0.570)</td><td>(-0.727, -0.445)</td></tr>
<tr><td style="text-align:left">status_rmUrbano metropolitano</td><td>-0.685<sup>***</sup></td><td>-0.773<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-0.714, -0.655)</td><td>(-0.816, -0.730)</td></tr>
<tr><td style="text-align:left">status_rmUrbano não-metropolitano</td><td>-0.595<sup>***</sup></td><td>-0.641<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-0.622, -0.569)</td><td>(-0.680, -0.602)</td></tr>
<tr><td style="text-align:left">regiaoNordeste</td><td>0.061<sup>***</sup></td><td>0.065<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.025, 0.098)</td><td>(0.006, 0.124)</td></tr>
<tr><td style="text-align:left">regiaoNorte</td><td>0.176<sup>***</sup></td><td>0.175<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.131, 0.221)</td><td>(0.089, 0.261)</td></tr>
<tr><td style="text-align:left">regiaoSudeste</td><td>0.046<sup>**</sup></td><td>0.008</td></tr>
<tr><td style="text-align:left"></td><td>(0.011, 0.080)</td><td>(-0.043, 0.059)</td></tr>
<tr><td style="text-align:left">regiaoSul</td><td>-0.044<sup>*</sup></td><td>-0.072<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-0.081, -0.007)</td><td>(-0.123, -0.021)</td></tr>
<tr><td style="text-align:left">anofct2013</td><td>-0.024</td><td>0.008</td></tr>
<tr><td style="text-align:left"></td><td>(-0.075, 0.027)</td><td>(-0.067, 0.083)</td></tr>
<tr><td style="text-align:left">anofct2014</td><td>-0.016</td><td>0.010</td></tr>
<tr><td style="text-align:left"></td><td>(-0.066, 0.035)</td><td>(-0.064, 0.084)</td></tr>
<tr><td style="text-align:left">anofct2015</td><td>-0.471<sup>***</sup></td><td>-0.477<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-0.523, -0.418)</td><td>(-0.554, -0.400)</td></tr>
<tr><td style="text-align:left">anofct2016</td><td>0.304<sup>***</sup></td><td>0.436<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.253, 0.355)</td><td>(0.361, 0.510)</td></tr>
<tr><td style="text-align:left">anofct2017</td><td>0.365<sup>***</sup></td><td>0.480<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.315, 0.415)</td><td>(0.406, 0.554)</td></tr>
<tr><td style="text-align:left">anofct2018</td><td>0.389<sup>***</sup></td><td>0.504<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.339, 0.439)</td><td>(0.431, 0.578)</td></tr>
<tr><td style="text-align:left">anofct2019</td><td>0.382<sup>***</sup></td><td>0.472<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.332, 0.432)</td><td>(0.397, 0.546)</td></tr>
<tr><td style="text-align:left">anofct2020</td><td>0.149<sup>***</sup></td><td>0.287<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.096, 0.201)</td><td>(0.211, 0.364)</td></tr>
<tr><td style="text-align:left">anofct2021</td><td>0.136<sup>***</sup></td><td>0.278<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.081, 0.191)</td><td>(0.197, 0.359)</td></tr>
<tr><td style="text-align:left">anofct2022</td><td>0.318<sup>***</sup></td><td>0.463<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.268, 0.369)</td><td>(0.390, 0.535)</td></tr>
<tr><td style="text-align:left">anofct2023</td><td>0.305<sup>***</sup></td><td>0.414<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.255, 0.356)</td><td>(0.340, 0.487)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:grupo_etario65</td><td></td><td>-0.017</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.067, 0.034)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:grupo_etario70</td><td></td><td>-0.022</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.086, 0.042)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:grupo_etario75</td><td></td><td>-0.038</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.126, 0.050)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:grupo_etario80</td><td></td><td>0.255<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.128, 0.382)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:grupo_etario85</td><td></td><td>0.015</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.197, 0.227)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:grupo_etario90</td><td></td><td>0.154</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.553, 0.861)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:quintil_incP40</td><td></td><td>0.052</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.122, 0.226)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:quintil_incP60</td><td></td><td>0.025</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.137, 0.188)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:quintil_incP80</td><td></td><td>0.084</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.077, 0.244)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:quintil_incP100</td><td></td><td>0.039</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.124, 0.202)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:educ_atingidaEnsino Fundamental</td><td></td><td>-0.115<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.191, -0.038)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:educ_atingidaEnsino Médio</td><td></td><td>-0.105<sup>*</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.197, -0.013)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:educ_atingidaEnsino Superior</td><td></td><td>-0.235<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.337, -0.132)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:inc_prop_individuoAlguma</td><td></td><td>-0.491<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.599, -0.382)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:inc_prop_individuoMaior</td><td></td><td>-0.630<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.745, -0.515)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:inc_prop_aposentado_domAlguma</td><td></td><td>0.092<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.039, 0.146)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:inc_prop_aposentado_domMaior</td><td></td><td>0.555<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.479, 0.631)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:status_rmUrbano metropolitano</td><td></td><td>0.167<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.109, 0.226)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:status_rmUrbano não-metropolitano</td><td></td><td>0.085<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(0.031, 0.138)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:regiaoNordeste</td><td></td><td>0.004</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.071, 0.078)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:regiaoNorte</td><td></td><td>0.019</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.082, 0.120)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:regiaoSudeste</td><td></td><td>0.066</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.002, 0.135)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:regiaoSul</td><td></td><td>0.031</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.049, 0.110)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:tipo_domEstendida</td><td></td><td>-0.020</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.197, 0.158)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:tipo_domNuclear</td><td></td><td>-0.034</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.210, 0.142)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:tipo_domUnipessoal</td><td></td><td>-0.173</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.362, 0.017)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2013</td><td></td><td>-0.069</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.171, 0.034)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2014</td><td></td><td>-0.058</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.158, 0.043)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2015</td><td></td><td>-0.002</td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.108, 0.103)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2016</td><td></td><td>-0.277<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.378, -0.176)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2017</td><td></td><td>-0.242<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.342, -0.142)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2018</td><td></td><td>-0.247<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.346, -0.147)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2019</td><td></td><td>-0.193<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.293, -0.093)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2020</td><td></td><td>-0.288<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.393, -0.183)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2021</td><td></td><td>-0.302<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.412, -0.192)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2022</td><td></td><td>-0.305<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.406, -0.204)</td></tr>
<tr><td style="text-align:left">cor_racaNegro:anofct2023</td><td></td><td>-0.229<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(-0.330, -0.128)</td></tr>
<tr><td style="text-align:left">Constant</td><td>-2.282<sup>***</sup></td><td>-2.566<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(-2.413, -2.151)</td><td>(-2.791, -2.341)</td></tr>
<tr><td style="text-align:left"><em>N</em></td><td>628,388</td><td>628,388</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-215,835.300</td><td>-215,392.900</td></tr>
<tr><td style="text-align:left">AIC</td><td>431,750.500</td><td>430,939.800</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td colspan="3" style="text-align:left"><sup>*</sup>p < .05; <sup>**</sup>p < .01; <sup>***</sup>p < .001</td></tr>
</table>

## Probabilidades preditas para modelo interativo

A seguir, as estimativas de *Average Marginal Effect* da variável independente raça e o efeito interativo com cada uma das outras variáveis serão apresentados.

- **Raça**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca.png">
</p>

- **Raça e idade**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_idade.png">
</p>

- **Raça e quintil de renda domiciliar per capita**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_rendaquintis.png">
</p>

- **Raça e escolaridade**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_escolaridade.png">
</p>

- **Raça e parcela da renda do idoso na renda domiciliar total**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_prop_inc_ind.png">
</p>

- **Raça e parcela da renda do idoso oriunda de aposentadoria na renda domiciliar total**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_prop_inc_aposent.png">
</p>

- **Raça e tipo de domicílios**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_tipodomicilio.png">
</p>

- **Raça e localização da residência**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_rm.png">
</p>

- **Raça e região geográfica**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_regiao.png">
</p>

- **Raça e período**

<p align="center" width="80%">
    <img width="80%" src="https://github.com/thiagocalm/Idoso_raca_mt/blob/6836b0b284621d65847a72ea50d695b0fa65ce23/Analises/outputs/pt4/ame_raca_periodo.png">
</p>

## Avaliação da comparação dos modelos

Diferentemente da abordagem metodológica utilizada no artigo de 2023, o modelo interativo permite que se faça comparações entre e intra grupos raciais, ao invés de somente comparações intra grupos. Desse modo, fatores que afetam diferencialmente os grupos de forma mediadora/moderadora podem ser contabilizados.

## Materiais de referência

- Análises do artigo para os modelos foram realizadas no script [04_exploratoria_reg.R](https://github.com/thiagocalm/Idoso_raca_mt/raw/refs/heads/master/Analises/scripts/04_exploratoria_reg.R).

- Análises do novo modelo e seus testes de validação e avaliação de consistência podem ser encontrados no script [06_analise_regressao_atualizada.R](https://github.com/thiagocalm/Idoso_raca_mt/raw/refs/heads/master/Analises/scripts/06_analise_regressao_atualizada.R).
