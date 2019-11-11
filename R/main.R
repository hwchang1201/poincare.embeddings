# main function defined inputs and outputs
source("embeddingTrainer.R")
source("illustration.R")
source("performanceScores.R")
source("ToyDataset_yaml.R")
source("dataLoader.R")

# main function.
poincareEmbeddings <- function(yamlDataset, theta_dim=2, N_epoch=50, lr=0.01, n_neg=5){

  return(list(rank = performance$ranking, map = performance$MAP))
}
