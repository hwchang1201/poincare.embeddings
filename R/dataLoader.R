#' Converting a tree-shape dataset to the components for poincare-embeddings.
#'
#' Please refer to 'vignette' to find detailed concepts of what this function does with detailed example.
#'
#' @param dataset_tree - A tree-shape dataset with yaml format.
#'
#' @return entity - A vector of all the entities in the tree-shape dataset.
#' @return POS - A 2-row positive relation matrix that contains positive relation entities columnwisely.
#' @return NEG - A 2-row negative relation matrix that contains negative relation entities columnwisely.
#' @export
#'
#' @examples statistics_yaml <- yaml::yaml.load(statistics)
#' @examples statistics_tree <- data.tree::as.Node(statistics_yaml)
#' @examples dataLoader(statistics_tree)


dataLoader <- function(dataset_tree){
  # check the compatibility.
  if (typeof(dataset_tree) != "environment"){
    stop("Please make sure your input is tree structure dataset from data.tree")
  }
  # name of all nodes in dataset.
  entity <- unique(c(as.vector(as.matrix(data.tree::ToDataFrameNetwork(dataset_tree)["from"])), as.vector(as.matrix(data.tree::ToDataFrameNetwork(dataset_tree)["to"]))))
  # proxy : list that numerate on the name of nodes.
  proxy <- vector(mode = "list", length = length(entity))
  names(proxy) <- entity # name the proxy(1,..., N)
  for (i in 1:length(entity)){
    proxy[i] <- i
  }
  # make positive relation matrix, refer vignette.
  # data.tree::ToDataFrameNetwork is making 2 columns that each row specifies the name of parent and child node
  POS <- matrix(rep(0, 2 * length(data.tree::ToDataFrameNetwork(dataset_tree)[,1])), nrow = 2)
  for (i in 1:2){
    k = 1
    for (j in as.vector(as.matrix(data.tree::ToDataFrameNetwork(dataset_tree)[,i]))){
      POS[i, k] <- proxy[[j]]
      k = k + 1
    }
  }
  NEG = matrix(, nrow = 2, ncol = 0)

  # negative relation matrix , refer vignette.
  for (i in as.matrix(unique(data.tree::ToDataFrameNetwork(dataset_tree)['from']))){ # iteration on the number of parent nodes.
    neg_vec <- 1:length(entity) # entity 1:N vector.
    # j = child node when i is parent node.
    for (j in as.matrix(data.tree::ToDataFrameNetwork(dataset_tree)['to'][data.tree::ToDataFrameNetwork(dataset_tree)['from'] == i])){
      neg_vec <- neg_vec[!neg_vec == proxy[[j]]] # we erase positive node from 1:N vector one by one if node is related to the parent.
    }
    parent <- i # naming parent
    while (length(parent) != 0){
      if (sum(data.tree::ToDataFrameNetwork(dataset_tree)['to'] == parent) > 0){ # before we find all the child nodes of parent node.
        parent = data.tree::ToDataFrameNetwork(dataset_tree)['from'][data.tree::ToDataFrameNetwork(dataset_tree)['to'] == parent]
        #'parent' node of parent node is also related, so we should remove it from neg_vec.
        neg_vec <- neg_vec[!neg_vec == proxy[[parent]]] # erase.
      }else{
        break
      }
    }
    matrix(c(rep(proxy[[i]], length(neg_vec)), neg_vec), nrow = 2, byrow = TRUE) # for every parent node, column bind.
    NEG <- cbind(NEG, matrix(c(rep(proxy[[i]], length(neg_vec)), neg_vec), nrow = 2, byrow = TRUE)) # column bind.



  }

  return(list(entity = entity, POS = POS, NEG = NEG))

}

