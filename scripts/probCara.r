monte_carlo <- function(num_simulacoes) {
  # Gerando N números aleatórios entre 0 e 1 (representando lançamentos de moeda)
  n <- runif(num_simulacoes)
  
  # Contador de quantos são maiores que 0,5 (representando caras)
  contador <- sum(n > 0.5)
  
  # Calculando a probabilidade estimada
  probabilidade <- contador / num_simulacoes
  
  return(probabilidade)
}

# Número de simulações
num_simulacoes <- 1000000

# Executa o método de Monte Carlo
probabilidade_estimada <- monte_carlo(num_simulacoes)

probabilidade_estimada
