# main function defined inputs and outputs
source("embeddingTrainer.R")
source("illustration.R")
source("performanceScores.R")
source("ToyDataset_yaml.R")
source("dataLoader.R")

poincareEmbeddings <- function(yamlDataset, theta_dim=2, N_epoch=50, lr=0.01, n_neg=5){
  #retrieve dataset.
  dataset <- dataLoader_yaml(yamlDataset)
  #make embedding.
  theta <- embeddingTrainer(dataset$POS, dataset$NEG, dataset$entity, theta_dim, N_epoch, lr, n_neg)
  # if dimension of theta is 2, we can plot the embedding.
  if (theta_dim == 2){
    # from "illustration.R"
    make2dPlot(theta, dataset$entity, dataset$POS, N_epoch, lr, n_neg)
  }
  # from "performanceScores.R"
  performance <- performanceScores(getPoincareDistance(theta), dataset$POS)
  return(list(rank = performance$ranking, map = performance$MAP))
}
