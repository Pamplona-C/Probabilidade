monte_carlo <- function(num_simulacoes, valor) {
  # Gere N números aleatórios entre 1 e 6 (representando lançamentos de um dado)
  rolls <- sample(1:6, num_simulacoes, replace=TRUE)
  
  # Conte quantos são iguais ao número alvo
  target_count <- sum(rolls == valor)
  
  # Calcule a probabilidade estimada
  probability_target <- target_count / num_simulacoes
  
  return(probability_target)
}

# Número de simulações
num_simulacoes <- 100000000

# Número alvo
valor <- 2

# Executa o método de Monte Carlo
probabilidade_estimada <- monte_carlo(num_simulacoes, valor)

probabilidade_estimada
