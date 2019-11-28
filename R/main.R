#' Poincare-embeddings
#'
#' @param dataset_tree - A tree-shape dataset from "data.tree"
#' @param theta_dim - The dimension of the embedding space.
#' @param N_epoch - The number of epochs.
#' @param lr - The learning rate.
#' @param n_neg - The number of negative samples for each iteration.
#'
#' @return Ranking : The ranking of the true related entity.
#' @return MAP : Mean average precision.
#' @return A 2-dimension plot only if theta_dim = 2.
#' @export
#'
#' @examples statistics_adv_yaml <- yaml::yaml.load(statistics_adv)
#' @examples statistics_adv_tree <- data.tree::as.Node(statistics_adv_yaml)
#' @examples poincareEmbeddings(statistics_adv_tree, theta_dim = 2, N_epoch = 200, lr = 0.005, n_neg = 10)
#'
# main function defined inputs and outputs
poincareEmbeddings <- function(dataset_tree, theta_dim=2, N_epoch=50, lr=0.01, n_neg=5){
  #retrieve dataset.
  dataset <- dataLoader(dataset_tree)
  #make embedding.
  theta <- embeddingTrainer(dataset$POS, dataset$NEG, dataset$entity, theta_dim, N_epoch, lr, n_neg)
  # if dimension of theta is 2, we can plot the embedding.
  if (theta_dim == 2){
    # from "illustration.R"
    make2dPlot(theta, dataset$entity, dataset$POS, N_epoch, lr, n_neg)
  }
  # from "performanceScores.R"
  performance <- performanceScores(getPoincareDistance(theta), dataset$POS)
  return(list(theta = theta, rank = performance$ranking, map = performance$MAP))
}
