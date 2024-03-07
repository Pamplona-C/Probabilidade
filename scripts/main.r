########################################
##Teste ANOVA
#Objetivo: Testar a igualdade média de 3 ou mais populações com distribuição Normal

#dados da idade de alunos da disciplina de estatística
#X refere-se aos alunos do ano de 2009, y dos alunos de 2015
# e z dos alunos de 2023.
# Obejtivo é comparar se a média de idade é a mesma em relação a oferta do curso

#Hipóteses : H_0: média de 2009 = média de 2015 = média de 2019;
#H_1: Pelo menos uma média é diferente

x <- c(25,26,25.5,27.2,21.1,19.8,17.9,18.3,17.8,19,21,22,22.5,23)
y <- c(22,25,22.8,24,25,23,23.8,24,22.5,23.1,25,22.8,22.3,21,22.8,22.9,23.4)
z <- c(17,18,17.9,18.2,18.5,19,18.8,19.2,19,19.1,20,18.7)

#Primeiramente: Fazer um boxplot para visualizar as possíveis diferenças
boxplot(x,y,z, main="Boxplot para comparar médias")

#Segundo : Testar a normalidade para x,y,z
shapiro.test(x)
shapiro.test(y)
shapiro.test(z)

##Conclusão 
#COmo o p-valor das variáveis são maiores que 0.05, não rejeitamos H_0, ou seja 
# os conjuntos seguem a distribuição normalizePath

##Segundo: Realizar o teste ANOVA
#hipóteses : H_0: média de 2009 = média de 2015 = média de 2019;
#H_1 : pelo menos uma média é diferente

dados <- c(x,y,z)
cat <- c(rep("x", length(x)), rep("y",length(y)), rep("z",length(z)))

Ajuste <- lm(dados~cat)
Ajuste
anovad <- anova(Ajuste)
anovad
mean(z)

#Conclusão 
# Como p-valor de 0.000000101 é menor que 0.05,
#rejeitamos  H_0, ou seja, pelo menos uma média é diferente. Neste caso,
#observamos que a idade média (18.61667) dos alunos de 2023 é menor que dos outros anos
