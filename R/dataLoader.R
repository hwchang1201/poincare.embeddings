library("yaml")
library("data.tree")

dataLoader_yaml <- function(yamlDataset){
  # loading yaml dataset.
  dataset <- yaml.load(yamlDataset)
  # convert it with tree structure.
  dataset_tree <- as.Node(dataset)
  # name of all nodes in dataset.
  entity <- unique(c(as.vector(as.matrix(ToDataFrameNetwork(dataset_tree)["from"])), as.vector(as.matrix(ToDataFrameNetwork(dataset_tree)["to"]))))
  # proxy : list that numerate on the name of nodes.
  proxy <- vector(mode = "list", length = length(entity))
  names(proxy) <- entity
  for (i in 1:length(entity)){
    proxy[i] <- i
  }
  # positive relation matrix
  POS <- matrix(rep(0, 2 * length(ToDataFrameNetwork(dataset_tree)[,1])), nrow = 2)
  for (i in 1:2){
    k = 1
    for (j in as.vector(as.matrix(ToDataFrameNetwork(dataset_tree)[,i]))){
      POS[i, k] <- proxy[[j]]
      k = k + 1
    }
  }
  NEG = matrix(, nrow = 2, ncol = 0)
  # negative relation matrix
  for (i in as.matrix(unique(ToDataFrameNetwork(dataset_tree)['from']))){
    neg_vec <- 1:length(entity)
    for (j in as.matrix(ToDataFrameNetwork(dataset_tree)['to'][ToDataFrameNetwork(dataset_tree)['from'] == i])){
      neg_vec <- neg_vec[!neg_vec == proxy[[j]]]
    }
    parent <- i
    while (length(parent) != 0){
      if (sum(ToDataFrameNetwork(dataset_tree)['to'] == parent) > 0){
        parent = ToDataFrameNetwork(dataset_tree)['from'][ToDataFrameNetwork(dataset_tree)['to'] == parent]
        neg_vec <- neg_vec[!neg_vec == proxy[[parent]]]
      }else{
        break
      }
    }
    matrix(c(rep(proxy[[i]], length(neg_vec)), neg_vec), nrow = 2, byrow = TRUE)
    NEG <- cbind(NEG, matrix(c(rep(proxy[[i]], length(neg_vec)), neg_vec), nrow = 2, byrow = TRUE))



  }

  return(list(entity = entity, POS = POS, NEG = NEG))
}
