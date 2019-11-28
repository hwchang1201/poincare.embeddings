# R-package implementation of Poincaré Embeddings for Learning Hierarchical Representations.

This repository provides the R implementation of the paper ["Poincaré Embeddings for Learning Hierarchical Representations"](https://papers.nips.cc/paper/7213-poincare-embeddings-for-learning-hierarchical-representations) by Maximillian Nickel and Douwe Kiela, accepted as poster presentation at NIPS 2017.

## What is the Poincaré Embedding?

The Poincaré Embedding is concerned with the problem of learning hierarchical structure on the dataset. Phylogenetic tree or the tree of hypernymy are the examples of hierarchical structure dataset. The embedding space is a Poincaré ball, which is an unit ball equipped with poincaré distance. An advantage using Poincaré space compared to the Euclidean space as embedding space is that this space preserve tree-shaped structure well in relatively low dimension. This is because poincaré distance is intuitively continuous version of distance on tree-shaped dataset. We can take advantage of this property to make better visualization of tree-shaped dataset and provide efficient embeddings with comparably less dimensionality.


## Previous Implementation


There are several implementations on this algorithm written in python and C++. You could refer [facebookresearch](https://github.com/facebookresearch/poincare-embeddings) for python or [TatsuyaShirakawa](https://github.com/TatsuyaShirakawa/poincare-embedding) for C++. To the best of my knowledge,  it is not yet done on R.

## Installation

```r
# loading package "poincare.embeddings"
require(devtools)
devtools::install_github("hwchang1201/poincare.embeddings", build_vignettes = T)
library(poincare.embeddings)
```

## Data Preparation

You should provide tree-structured dataset that you want to embed as an input in the main function. The data can be provided with either using .yaml format file or programmatical way by installing the package named "data.tree". This explanation and examples are from Introduction to [data.tree](https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#trees-in-r)

1. Create a tree from a yaml file.

Here is an example that can be an input of the model.

```r
#loading package "yaml" and "data.tree".
library(yaml)
library(data.tree)
# making a yaml structure code.
acme <- "
name: acme Inc.
Accounting:
  New Software:
    leaf:
  New Accounting Standards:
    leaf:
Research:
  New Product Line:
    leaf:
  New Labs:
    leaf:
IT:
  Outsource:
    leaf:
  Go agile:
    leaf:
  Switch to R:
    leaf:
"
# make yaml format as an input.
acme_yamlDataset <- yaml::yaml.load(acme)
# Or make yaml file as an input.
# acme_yamlDataset <- yaml::read_yaml("./acme.yaml") # you should have a yaml file "acme.yaml""

# then convert into tree structure.
acme_treeDataset <- data.tree::as.Node(acme_yamlDataset)

```

2. Create a tree programmatically.

```r
# loading package "data.tree".
library(data.tree)

# defining tree structured dataset.
acme_treeDataset <- Node$new("Acme Inc.")
  accounting <- acme_treeDataset$AddChild("Accounting")
    software <- accounting$AddChild("New Software")
    standards <- accounting$AddChild("New Accounting Standards")
  research <- acme_treeDataset$AddChild("Research")
    newProductLine <- research$AddChild("New Product Line")
    newLabs <- research$AddChild("New Labs")
  it <- acme_treeDataset$AddChild("IT")
    outsource <- it$AddChild("Outsource")
    agile <- it$AddChild("Go agile")
    goToR <- it$AddChild("Switch to R")

print(acme_treeDataset)
```

## Quick Example

<p align="center">
<img src="/Rplot_statistics.png"><br>
</p>

```r
# loading package "poincare.embeddings"
library(poincare.embeddings)

# use example dataset
# 1. use "toy"
toy_yaml <- yaml::yaml.load(toy)
toy_tree <- data.tree::as.Node(toy_yaml)
emb <- poincareEmbeddings(toy_tree, theta_dim = 2, N_epoch = 200, lr = 0.005, n_neg = 10)
# 2. use "statistics"
statistics_yaml <- yaml::yaml.load(statistics)
statistics_tree <- data.tree::as.Node(statistics_yaml)
emb <- poincareEmbeddings(statistics_tree, theta_dim = 2, N_epoch = 200, lr = 0.005, n_neg = 10)
# 3. use "statistics_adv"
statistics_adv_yaml <- yaml::yaml.load(statistics_adv)
statistics_adv_tree <- data.tree::as.Node(statistics_adv_yaml)
emb <- poincareEmbeddings(statistics_adv_tree, theta_dim = 2, N_epoch = 200, lr = 0.005, n_neg = 10)

print(paste("The ranking of the poincare embedding :", emb$rank))
print(paste("The mean average precision of the poincare embedding :", emb$map))
```

## Reference
Nickel, M. and Kiela, D. Poincaré embeddings for learning hierarchical representations. arXiv preprint arXiv:1705.08039, 2017.

Nickel, M. and Kiela, D. Learning continuous hierarchies in the lorentz model of hyperbolic geometry. arXiv preprint arXiv:1806.03417, 2018.

