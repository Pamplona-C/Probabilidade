
############# Objetivo
#Verificar se a dieta acompanhada de um medicamento tem 
#efeito na diminuição do peso corporal de pacientes
# hipóteses==> H_0: PesoA =< PesoD; H_1: PesoA > PesoD


PesoA <- c(85,75,65,69,89,74.5,68.2,74.6,59.8,60.9,72.5)
PesoD <- c(80.5,70,63,61,82,71,65,73,55,56,70.8)

#Primeiramente: Fazer um boxplot para visualizar as possíveis diferenças
boxplot(PesoA,PesoD, main="Boxplot para comparar Peso")


##### Teste para igualdade de médias considerando duas populações 
#dependentes, ou seja, vamos realizar um teste Pareado

t.test(PesoA,PesoD,paired = TRUE, alternative="greater")
##Conclusão:
# Como o p-valor = 0.000026 é menor que 0.05, rejeitamos H_0, 
#ou seja, o peso antes é maior que o peso depois, o que implica 
#em dizer que a dieta seguida da medicação diminui o peso corporal.


######################################################
#####################################################
## Teste ANOVA
# Objetivo: Testar a igualdade média de 3 ou mais populações com 
#distribuição Normal

# Dados da idade de alunos da disciplina de estatística
# X refere-se aos alunos do ano de 2009, y dos alunos de 2015 
#e z dos alunos de 2023.
# Objetivo é comparar se a média de idade é a mesma em 
#relação a oferta do curso

# hipóteses: H_0: média de 2009 = média de 2015 = média de 2023 ; 
#H_1: pelo menos uma média é diferente

x <- c(25, 26, 25.5,27.2,21.1,19.8,17.9,18.3,17.8,19,21,22,22.5,23)
y <- c(22,25,22.8,24,25,23,23.8,24,22.5,23.1,25,22.8,22.3,21,22.8,22.9,23.4)
z <- c(17,18,17.9,18.2,18.5,19,18.8,19.2,19,19.1,20,18.7)

#Primeiramente: Fazer um boxplot para visualizar as possíveis diferenças
boxplot(x,y,z, main="Boxplot para comparar idade dos alunos de 2009, 2015 e 2023")

#segundo: Testar a normalidade para x, y e z
shapiro.test(x) # testa a normalidade dos dados de x
shapiro.test(y) # testa a normalidade dos dados de y
shapiro.test(z) # testa a normalidade dos dados de z

#Conclusão
# Como o p-valor de x (0.3924), de y (0.3813) e de z (0.8645) são maiores que 0.05, 
#não rejeitamos H_0, ou seja, os conjuntos seguem a 
#distribuição Normal

#segundo: Realizar o teste de ANOVA
# hipóteses: H_0: média de 2009 = média de 2015 = média de 2019 ; 
#H_1: pelo menos uma média é diferente

dados <- c(x,y,z)
cat <- c(rep("x", length(x)), rep("y", length(y)),rep("z",length(z)))

Ajuste <- lm(dados~cat)
anovad <- anova(Ajuste)
anovad
mean(z)
#Conclusão
# Como o p-valor de 0.00000101 é menor que 0.05, 
#rejeitamos H_0, ou seja, pelo menos uma média é diferente. Neste caso, pelo boxplot
#observamos que a idade média (18.61667) dos alunos de 2023 é menor que dos outros anos.


  