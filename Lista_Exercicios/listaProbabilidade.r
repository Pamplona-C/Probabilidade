DISCENTE <- "Gustavo Pamplona Melo Coelho"
MATRICULA <- 202100887


##Exercício 1.1
#O Dr. Doolittle finalmente desistiu da ideia de conversar com animais e decidiu tornar-se
# um psicólogo experimental de animais. Ele está particularmente interessado em descobrir se os gatos são
# ou não mais inteligentes que os cachorros. Para isso, ele desenvolveu um teste de inteligência específico
# para esse estudo ele testa amostras de gatos e cachorros. Ele foi cuidadoso para não introduzir qualquer tipo de vício no teste e acredita que criou um teste que não está associado às espécies, ou seja, pode ser usado em qualquer espécie. Dr. Doolittle espera que exista uma diferença entre os escores de inteligência entre gatos e cachorros. No experimento ele trabalhou com duas amostras aleatórias de 10 gatos e 10 cachorros e, os resultados obtidos, estão na tabela a seguir. Apresente uma análise via teste de hipóteses e conclua se é possível comprovar ou não, as hipóteses do Dr. Doolittle.

#gatos  <-    95  100 104 78  130 111 89  114 102 97
#cachorros <- 116 112 102 96  89  124 131 117 107 110

####################################Obejtivo
##Doolittle espera que exista uma diferença entre os escores de inteligência entre gatos e cachorros.
## hipóteses ==> 
##H_0:Não há diferença significativa nos escores 
##H_1:Há uma diferença significativa entre os escores

################################1º Verificar a normalidade
#Hipóteses : H_0:os dados seguem distribuição normal
#            H_1:os dados não seguem distribuição normal   
shapiro.test(gatos)
shapiro.test(cachorros)
###############################Saida
# data:  gatos
# W = 0.98041, p-value = 0.9673
###############################Conclusão
#Conclusão: Como o p-valor de ambos os vetores foi maior que 0.05, não rejeitamos H_0.Portanto os dados seguem distribuição normal.

################################2º as amostras são independentes
################################3º Verificar a variância
#Hipóteses : H_0: variâncias iguais
#            H_1: variâncias diferentes   
var.test(gatos,cachorros)
###############################Saida
# data:  gatos and cachorros
# F = 1.28, num df = 9, denom df = 9, p-value = 0.7191
# alternative hypothesis: true ratio of variances is not equal to 1
###############################Conclusão
#Conclusão: Como o p-valor = 0.7191 > 0.05 não rejeitamos H_0. Portanto as variâncias são consideradas iguais.

################################3º Verificar as médias
#Hipóteses : H_0: Médias são iguais
#            H_1: Médias são diferentes   
t.test(gatos,cachorros)
###############################Saida
# data:  gatos and cachorros
# t = -1.3935, df = 17.733, p-value = 0.1807
###############################CONCLUSÃO
#Conclusão: Como o p-valor = 0.1807 > 0.05 não rejeitamos H_0. Portanto as médias são consideradas iguais.
################################Conclusão do exercício, como o resultado do teste de médias foi p-valor=1807>0.05. portanto não rejeitamos H_0. Logo não há diferença significativa entre os escores dos gatos e cachorros.

#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################


# Exercício 1.2. 
#Suponha que o tempo m ́edio de terapia tradicional em pacientes com depress ̃ao seja de
# 2 anos. Admita ainda que se pretende testar um tipo de terapia alternativa cujo tempo de recupera ̧c ̃ao
# esperado seja menor que 2 anos. Realizou-se um experimento com 20 pacientes submetidos `a nova terapia,
# com dados apresentados na tabela abaixo. Com base nestas informa ̧c ̃oes teste a hip ́otese que a nova terapia
# apresenta tempo de recupera ̧c ̃ao diferente de 2 anos. Use significˆancia de 5%.
#Hipóteses : H_0: Tempo de recuperação != 2 anos
#            H_1: Tempo de recuperação = 2 anos   

dados <- c(1.5, 1.2, 1.8, 1.3, 1.7, 1.8, 1.9, 2.1, 1.6, 1.8, 1.8, 1.9, 1.5, 1.7, 1.6, 1.9, 2.1, 2.2, 1.7, 1.7)

###############################Teste de normalidade
shapiro.test(dados)
###############################Saida
# data:  dados
# W = 0.96746, p-value = 0.7006
###############################Conclusão
#Conclusão: Como o p-valor = 0.7006 > 0.05 não rejeitamos H_0. Portanto os dados seguem distribuição normal.

#####Teste de igualdade de médias considerando 1 população
#Hipóteses : H_0: Tempo de recuperação != 2 anos
#            H_1: Tempo de recuperação = 2 anos  
t.test(dados, mu=2, alternative="less")

###############################Saida
# data:  dados
# t = -4.6114, df = 19, p-value = 9.525e-05

###############################Conclusão
#Conclusão: Como o p-valor = 9.525e-05 < 0.05 rejeitamos H_0. Portanto o tempo de recuperação do novo teste é igual a 2 anos.




#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.3.
# A média em dias de internação de crianças que sofreram acidente de trânsito e que não
# estavam usando o cinto de segurança é de 1,39 dias. Um levantamento feito com 20 crianças acidentadas
# e que estavam usando o cinto, apresentam dados na tabela abaixo. Diante disso, podemos concluir que o
# uso do cinto apresenta um tempo médio de internação diferente daqueles que não usam cinto? Adote = 5
# %. 
#Hipóteses : H_0: McomCinto = MsemCinto
#            H_1: McomCinto != MsemCinto 

tempo_internação <- c(1.1, 1.2, 1.1, 1.3, 1.0, 1.1, 1.2, 1.1, 1.3, 1.4, 1.0, 1.1, 1.5, 1.2, 1.1, 1.0, 0.5, 1.2, 1.7, 1.2)

t.test(tempo_internação)
###############################Saida
# data:  tempo_internacao
# t = 22.21, df = 19, p-value = 4.698e-15

###############################Conclusão
#Conclusão: Como o p-valor = 9.525e-05 < 0.05 rejeitamos H_0. Portanto o tempo de recuperação do novo teste é igual a 2 anos.




#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.4.
# A fim de acelerar o tempo que um analgésico leva para surtir efeito, um quı́mico analista
# acrescentou certo ingrediente à fórmula original, que acusava um tempo médio de 43 minutos para fazer
# efeito. A tabela abaixo apresenta 20 observações com a nova fórmula. Podemos concluir, com base em
# uma análise via teste de hipóteses que a nova fórmula é melhor, pior ou igual a anterior? Adote = 5 %.

tempo <- c(40.1, 42.2, 38.1, 39.3, 44.0, 42.1, 39.2, 40.1, 40.3, 38.4, 39.0, 41.1, 38.5, 39.2, 41.1, 38.0, 40.5, 41.2, 42.7, 45.2)

#Hipóteses : H_0: M = 43
#            H_1: M != 43 

t_test_result <- t.test(tempo, mu = 43, alternative = "two.sided")

# Exibindo o resultado
t_test_result

###############################Conclusão
#Conclusão: Como o valor-p = 1.93e-05 é muito menor que o nível de significância comum de 0.05, rejeitamos a hipótese nula. Isso sugere que há evidências estatísticas significativas para concluir que a média do tempo para fazer efeito com a nova fórmula é diferente de 43 minutos.



#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.5.
# Deseja-se avaliar a efetividade de uma dieta combinada com um programa de exercı́cios
# fı́sicos na redução do nı́vel sérico de colesterol. A tabela abaixo mostra os nı́veis de colesterol de 12
# participantes no inı́cio e no final do programa.

inicio <- c(201, 231, 221, 260, 228, 237, 326, 235, 240, 267, 284, 201)
final <- c(200, 236, 216, 233, 224, 216, 296, 195, 207, 247, 210, 209)

#Hipóteses : H_0: NivelColesterolAntes = NivelColesterolDepois
#            H_1: NivelColesterolAntes !=  NivelColesterolDepois

# Comparação dos níveis de colesterol antes e depois do programa
t_test_result <- t.test(inicio, final, paired = TRUE)


t_test_result
###############################Saida
# data:  inicio and final
# t = 3.0201, df = 11, p-value = 0.01165

###############################Conclusão
#Conclusão: O valor-p (0.01165) é menor que o nível de significância comum de 0.05. Portanto, rejeitamos a hipótese nula (H0H0​), indicando que há uma diferença significativa nos níveis médios de colesterol antes e depois do programa. Em outras palavras, o programa de dieta e exercícios parece ter um impacto significativo na redução dos níveis séricos de colesterol.



#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.6.
# Dez cobaias foram submetidas ao tratamento de engorda com certa ra ̧c ̃ao. Os pesos em
# gramas, antes e ap ́os o teste s ̃ao dados a seguir. A 1% de significˆancia, podemos concluir que o uso da
# ração contribuiu para o aumento do peso m ́edio dos animais?

antes <- c(635, 704, 662, 560, 603, 745, 698, 575, 633, 669)
depois <- c(640, 712, 681, 558, 610, 740, 707, 585, 635, 682)

#Hipóteses : H_0: Não há diferença significativa nos pesos médios antes e depois do tratamento.
#            H_1: Há uma diferença significativa nos pesos médios antes e depois do tratamento.
# Comparação dos pesos antes e depois do tratamento
t_test_result <- t.test(antes, depois, paired = TRUE)

# Exibindo o resultado
t_test_result
###############################Saida
# data:  antes and depois
# t = -2.9635, df = 9, p-value = 0.01587

#Conclusão: O valor-p (0.01587) é menor que o nível de significância comum de 0.05. Portanto, rejeitamos a hipótese nula (H0H0​), indicando que há uma diferença significativa nos pesos médios antes e depois do tratamento. Em outras palavras, o tratamento com a ração parece ter um impacto significativo no aumento dos pesos das cobaias.


#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.7.
# um estudo relata os resultados de um ensaio cl ́ınico aleatorizado, duplo-cego, realizado com
# o objetivo de comparar a tianeptina com o placebo. Participaram desse estudo pacientes de Belo Horizonte,
# Campinas e Rio de Janeiro. Sucintamente, o ensaio consistiu em administrar a droga a dois grupos de
# pacientes, compostos de forma aleat ́oria, e quantificar a depress ̃ao atrav ́es da escala de MADRS, em que os
# valores maiores indicam maior gravidade da doen ̧ca. O escore foi obtido para cada paciente 7, 14, 21, 28
# e 42 dias ap ́os o in ́ıcio do ensaio. Pelo planejamento adotado, os dois grupos n ̃ao diferiam em termos de
# depress ̃ao no in ́ıcio do ensaio. Assim, uma evidˆencia sobre o efeito da tianeptina  ́e obtida comparando-se
# os dois grupos ao fim de 42 dias. A Tabela abaixo apresenta os escores finais dos pacientes dos dois grupos
# admitidos em Belo Horizonte

Placebo <- c(6, 33, 21, 26, 10, 29, 33, 29, 37, 15, 2, 21, 7, 26, 13)
Tianeptina <- c(10, 8, 17, 4, 17, 14, 9, 4, 21, 3, 7, 10, 29, 13, 14, 2)
#Hipóteses : H_0: Não há diferença significativa nos escores finais entre os grupos tratados com Placebo e Tianeptina.
#            H_1: Há uma diferença significativa nos escores finais entre os grupos tratados com Placebo e Tianeptina.
# Teste t para duas amostras independentes
t_test_result <- t.test(Placebo, Tianeptina)

# Exibindo o resultado
t_test_result
###############################Saida
# t = 2.7019, df = 23.9, p-value = 0.01248

#Conclusão: O valor-p (0.01248) é menor que o nível de significância comum de 0.05. Portanto, rejeitamos a hipótese nula, indicando que há uma diferença significativa nos escores finais entre os grupos Placebo e Tianeptina após 42 dias. Em outras palavras, há evidências para sugerir que o tratamento com Tianeptina teve um efeito diferente do Placebo.


#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.8.
# Estuda-se o conte ́udo de nicotina em duas marcas de cigarros (A e B), obtendo-se os
# seguintes resultados. A: 17; 20; 23; 20; 18; 19; 20; 18; 19 B: 18; 20; 21; 22; 24; 23; 24; 25; 22; 26; 24
# Com α = 0,05, pode-se afirmar que existe alguma diferen ̧ca significativa no conte ́udo m ́edio de nicotina
# entre as duas marcas?

# Hipótese Nula (H_0​): Não há diferença significativa no conteúdo médio de nicotina entre as marcas de cigarros A e B.

#     H0:μA=μB

# Hipótese Alternativa (H_1​): Existe uma diferença significativa no conteúdo médio de nicotina entre as marcas de cigarros A e B.

#     H1:μA≠μB

# Dados
A <- c(17, 20, 23, 20, 18, 19, 20, 18, 19)
B <- c(18, 20, 21, 22, 24, 23, 24, 25, 22, 26, 24)

# Teste t para duas amostras independentes
t_test_result <- t.test(A, B)

# Exibindo o resultado
t_test_result
###############################Saida
# t = -3.6273, df = 17.87, p-value = 0.001945

#Conclusão: O valor-p (0.001945) é menor que o nível de significância comum de 0.05. Portanto, rejeitamos a hipótese nula (H0H0​), indicando que há uma diferença significativa no conteúdo médio de nicotina entre as marcas de cigarros A e B. Em outras palavras, existem evidências para sugerir que há uma diferença significativa no conteúdo médio de nicotina entre as duas marcas.




#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.9.
# O processo de cura de presunto inclui a imers ̃ao da pe ̧ca numa solu ̧c ̃ao de  ́acido s ́orbico.
# Numa f ́abrica de presunto registaram-se os res ́ıduos de  ́acido s ́orbico, em partes por milh ̃ao, em 8 pe ̧cas de
# presunto imediatamente depois de estas serem imersas na solu ̧c ̃ao, e depois de 60 dias de cura:

# Hipótese Nula (H_0): Não há diferença significativa nos resíduos de ácido sórbico antes e depois do processo de cura.

#     H_0:μantes=μdepois

# Hipótese Alternativa (H_1​): Há uma diferença significativa nos resíduos de ácido sórbico antes e depois do processo de cura.

#     H_1:μantes≠μdepois
Antes_da_cura <- c(224, 270, 400, 444, 590, 660, 1400, 680)
depois <- c(116, 96, 239, 329, 437, 597, 689, 576)

# Teste t pareado
t_test_result <- t.test(Antes_da_cura, depois, paired = TRUE)

# Exibindo o resultado
t_test_result
###############################Saida
# t = 2.6731, df = 7, p-value = 0.03186

#Conclusão: O valor-p (0.03186) é menor que o nível de significância comum de 0.05. Portanto, rejeitamos a hipótese nula (H0H0​), indicando que há uma diferença significativa nos resíduos de ácido sórbico antes e depois do processo de cura. Em outras palavras, há evidências para sugerir que o processo de cura afeta os resíduos de ácido sórbico.




#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.10.
# A tabela abaixo apresenta informa ̧c ̃oes sobre pedidos de produtos por canal de distribui ̧c ̃ao,
# durante o ano de 2020. Realize um teste de hip ́oteses para verificar quais canais apresentam m ́edias de
# pedidos iguais.
# Dados
c_i <- c(46307, 55013, 44683, 54528, 48492, 42230, 46709, 50983, 46792, 65775, 57932, 47152)
cliente <- c(24709, 28023, 21511, 23487, 29644, 21204, 24089, 25958, 26182, 37272, 33650, 25482)
oem <- c(32007, 33675, 35761, 33987, 31626, 32564, 33078, 34021, 34123, 32347, 33690, 32896)

# Criando um data frame
dados_exercicio <- data.frame(CI = c_i, Cliente = cliente, OEM = oem)

# Boxplot para visualizar as possíveis diferenças
boxplot(CI ~ Cliente, data = dados_exercicio, main = "Boxplot para comparar médias de pedidos por canal de distribuição")

# Teste de normalidade para cada canal
shapiro.test(dados_exercicio$CI[dados_exercicio$Cliente == "c"])
shapiro.test(dados_exercicio$CI[dados_exercicio$Cliente == "cliente"])
shapiro.test(dados_exercicio$CI[dados_exercicio$Cliente == "oem"])

# Conclusão: Como os p-valores são maiores que 0.05, não rejeitamos H_0, indicando que os conjuntos seguem a distribuição normal.

# Teste de ANOVA
ajuste <- lm(CI ~ Cliente, data = dados_exercicio)
anova_resultado <- anova(ajuste)
anova_resultado

# Conclusão: Como o p-valor de 0.0216 é menor que 0.05, rejeitamos H_0, indicando que pelo menos uma média é diferente.


#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.11.
# A tabela abaixo apresenta informa ̧c ̃oes sobre a demanda mensal de três varejistas. Realize
# um teste de hip ́oteses para verificar quais varejistas apresentam m ́edias de demandas iguais.
# Dados
# Dados
varejista1 <- c(218, 188, 225, 217, 176, 187, 221, 212, 210, 203, 188, 185)
varejista2 <- c(101, 87, 123, 101, 95, 97, 93, 131, 76, 101, 87, 114)
varejista3 <- c(268, 296, 321, 312, 301, 294, 285, 305, 289, 303, 324, 332)

# Criar um data frame para organizar os dados
dados_demandas <- data.frame(
  Varejista = rep(c("Varejista1", "Varejista2", "Varejista3"), each = 12),
  Demanda = c(varejista1, varejista2, varejista3)
)

# Teste de ANOVA
ajuste_demandas <- lm(Demanda ~ Varejista, data = dados_demandas)
anova_resultado_demandas <- anova(ajuste_demandas)
anova_resultado_demandas

# Conclusão: Com base nos resultados do teste de ANOVA, podemos concluir que existe uma diferença estatisticamente significativa nas médias de demandas entre os varejistas. O p-valor extremamente baixo (menor que 2.2e-16) sugere fortemente a rejeição da hipótese nula, indicando que pelo menos uma média de demanda entre os varejistas é diferente.


#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.12.
# O tempo para transmitir 10 MB na rede de computadores da empresa do sr chico  ́e,
# em m ́edia, 7,4 seg. Ap ́os realizada algumas mudan ̧cas na rede acredita-se numa redu ̧c ̃ao no tempo de
# transmiss ̃ao de dados. Foram realizados 10 ensaios independentes com um arquivo de 10 MB e foram
# anotados os tempos de transmiss ̃ao, em segundos: 6,8; 7,1; 5,9; 7,5; 6,3; 6,9; 7,2; 7,6; 6,6; 6,3. Existe
# evidˆencia suficiente de que o tempo m ́edio de transmiss ̃ao foi alterado? Use n ́ıvel de significˆancia de 5
# Dados
tempos <- c(6.8, 7.1, 5.9, 7.5, 6.3, 6.9, 7.2, 7.6, 6.6, 6.3)

# Teste t de uma amostra
t_test_resultado <- t.test(tempos, mu = 7.4)

# Exibir o resultado
t_test_resultado
###############################Saida
# t = -3.3265, df = 9, p-value = 0.008848

#Conclusão: Com um valor de p (p-value) de 0.008848, que é menor que 0.05 (nível de significância de 5%), rejeitamos a hipótese nula H0H0​. Portanto, há evidência estatística suficiente para concluir que o tempo médio de transmissão foi alterado após as mudanças na rede. O intervalo de confiança para a média não inclui o valor 7.4, reforçando a ideia de que houve uma redução no tempo médio de transmissão.



#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.13.
# Desejamos verificar se os catalisadores A e B tˆem efeitos diferentes no rendimento de
# certa rea ̧c ̃ao qu ́ımica. Foram realizados dez ensaios com cada catalisador. Os resultados s ̃ao mostrados na
# tabela a seguir. Teste a hip ́otese de as m ́edias diferirem entre si, a um n ́ıvel de confian ̧ca de 5

# H0​:μA​=μB​ (As médias dos catalisadores A e B são iguais)
# H1:μA≠μB (As médias dos catalisadores A e B são diferentes)
# Dados
catalisador_A <- c(45, 51, 50, 62, 43, 42, 53, 50, 48, 55)
catalisador_B <- c(45, 35, 43, 59, 48, 45, 41, 43, 49, 39)

# Teste t de duas amostras
t_test_resultado <- t.test(catalisador_A, catalisador_B)

# Exibir o resultado
t_test_resultado
###############################Saida
# t = 1.8632, df = 17.873, p-value = 0.07895

#Conclusão: Com um valor de p (p-value) de 0.07895, que é maior que 0.05 (nível de significância de 5%), não rejeitamos a hipótese nula H0​. Portanto, não há evidência estatística suficiente para concluir que as médias dos rendimentos com os catalisadores A e B são diferentes a um nível de confiança de 5%. O intervalo de confiança para a diferença nas médias também inclui o valor zero, reforçando a falta de diferença significativa entre os catalisadores.



#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.14.
# Uma companhia deseja testar 4 diferentes tipos de pneus, A, B, C e D. As vidas m ́edias
# dos pneus (em milhares de milhas) constam na tabela abaixo, onde cada tipo foi testado aleatoriamente
# em 6 autom ́oveis idˆenticos. Teste a hip ́otese de as m ́edias diferirem entre si, a um n ́ıvel de confian ̧ca de 5
# Marca A 33 38 36 40 31 35
# Marca B 32 40 42 38 30 34
# Marca C 31 37 35 33 34 30
# Marca B 28 34 32 30 33 31

# Hipóteses:

#     H0:μA = μB = μC = μD​ (As médias das vidas médias dos pneus A, B, C e D são iguais)
#     H1: Pelo menos uma média é diferente das outras

# Dados
marca_A <- c(33, 38, 36, 40, 31, 35)
marca_B <- c(32, 40, 42, 38, 30, 34)
marca_C <- c(31, 37, 35, 33, 34, 30)
marca_D <- c(28, 34, 32, 30, 33, 31)

# Teste ANOVA
anova_resultado <- anova(lm(c(marca_A, marca_B, marca_C, marca_D) ~ rep(c("A", "B", "C", "D"), each = 6)))

# Exibir o resultado
anova_resultado
#Conclusão: Com um valor de p (p-value) de 0.09022, que é maior que 0.05 (nível de significância de 5%), não temos evidência estatística suficiente para rejeitar a hipótese nula H0H0​. Portanto, não há diferença significativa nas médias das vidas médias dos pneus A, B, C e D a um nível de confiança de 5%.


#########################################################################################################
#########################################################################################################
#########################################################################################################
########################################################################################################
# Exercício 1.15.
# A tabela abaixo apresenta o tempo (em anos) que indiv ́ıduos levaram para deixarem
# totalmente o vicio do fumo. Considerando um n ́ıvel de significancia de 5%, podemos dizer que esses dados
# apresentam evidˆencias para acreditar que o tempo gasto para deixar o v ́ıcio  ́e de 15 anos?
# Tempo 15.7 13.2 22.6 13 10.7 18.1 14.7 7.0 17.3 7.5 21.8 12,3 19.8 13.8 16 15.5 13.1 20.7 15.5 9.8

# Dados
tempos <- c(15.7, 13.2, 22.6, 13, 10.7, 18.1, 14.7, 7.0, 17.3, 7.5, 21.8, 12.3, 19.8, 13.8, 16, 15.5, 13.1, 20.7, 15.5, 9.8)

# Hipóteses:

#     H_0​: A média do tempo é igual a 15 anos.
#     H_1​: A média do tempo é menor que 15 anos.
# Teste t unicaudal
resultado_teste <- t.test(tempos, mu = 15, alternative = "less")

# Exibindo o resultado
print(resultado_teste)
###############################Saida
# data:  tempos
# t = -0.097375, df = 19, p-value = 0.4617

#Conclusão: Com um valor de p (p-value) de 0.4617, que é muito maior que 0.05 (nível de significância de 5%), não temos evidência estatística suficiente para rejeitar a hipótese nula H0H0​. Portanto, com base nos dados fornecidos, não podemos afirmar que o tempo médio para deixar o vício é menor que 15 anos.
