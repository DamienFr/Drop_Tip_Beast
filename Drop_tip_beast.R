# In phylogeny analysis, it is common to use Beast.
# It usually outputs tree files (.trees) containing multiple trees 
# from the parameter spaces of the Bayesian analysis.
# As trees are all different, TreeAnnotator is then used to produce a summary tree
# that contains values (95% HPD, mean and/or median values) of the parameters (node ages, state of a discrete trait etc).

## This script is to be used on the summary tree file outputed by TreeAnnotator. ##
# It allows to delete one or more individuals of the tree and update
# at the same time the parameter matrix.

# It therefore allows the simple plotting of trees with less individuals
# than the ones used in the Bayesian analysis

########################## Working example ###################################
# Analysis <- drop.tip.beast("treeannotator_tree_file.tree","individuals_to_delete.csv")
# treeannotator_tree_file : tree file in the current directory
# indivs_names_to_delete : csv file with ; separator, no quote,
# first field must indivs to delete names
##############################################################################

# Developped based on tree file format of Beast v1.9 / 1.10 
# This script may or may not work on other versions output trees.

getwd()

library(treeio)
library(ape)


# drop.tip.beast <- function (treeannotator_tree_file="treeannotator_tree_file.tree", individuals_to_delete="individuals_to_delete.csv")
drop.tip.beast <- function (treeannotator_tree_file, individuals_to_delete)
{
  # individuals_to_delete <- "individuals_to_delete.csv"
table_of_indivs_names_to_delete <- read.csv(file=individuals_to_delete,sep=";",header = FALSE,stringsAsFactors=F)
indivs_names_to_delete <- table_of_indivs_names_to_delete[,1]

beast_tree <- read.beast(treeannotator_tree_file)
original_tree <- beast_tree@phylo

####### As an example, this can be used to get individuals to delete #####
# delme_tree <- extract.clade(original_tree, 399)
# indivs_names_to_delete <- c(delme_tree$t, "JJ009-04_1985")
#######

version <- packageVersion("treeio")

if (version == "1.6.0") {
  parameter_matrix <- beast_tree@data
} else{
  parameter_matrix <- beast_tree@stats
}

parameter_matrix$node <- as.numeric(parameter_matrix$node)
# Rows of the matrix containing beast data are not ordered like the order of the nodes of the phylo tree
# stored in beast_tree@phylo$edge[,2], so i'll sort them
# AND the last row must correspond to the parameters of the root node. Root node number if stored in
# min(beast_tree@phylo$edge[,1]) and is not in beast_tree@phylo$edge[,2] because it is not a child of any node.
parameter_matrix <-
  parameter_matrix[match(c(original_tree$edge[, 2], min(beast_tree@phylo$edge[, 1])),
                         as.numeric(parameter_matrix$node)), ]
parameter_matrix_original <- parameter_matrix

new_tree <- original_tree
'%nin%' <- Negate('%in%')

for (x in indivs_names_to_delete) {
  tips_to_delete <- which(new_tree$tip.label == x)
  
  # internal node to delete is the node number apearing in the first column of the edge matrix where the second column is equal to tip to delete
  noeud_interne_a_supprimer <-
    new_tree$edge[new_tree$edge[, 2] == tips_to_delete, 1]
  
  # Am i deleting the tip linked to the root node ?
  if (noeud_interne_a_supprimer == min(new_tree$edge[, 1])) {
    # if yes, the new root number (and parameters) are gonna be the node with a number such as (old-root node number +1) : min(new_tree$edge[,1])+1
    root <-
      parameter_matrix[parameter_matrix$node == min(new_tree$edge[, 1]) + 1, ]
    # i delete the NEW root line from the matrix as it will be updated later
    parameter_matrix <-
      parameter_matrix[!parameter_matrix$node == min(new_tree$edge[, 1]) + 1, ]
  } else{
    # if no, root parameters don't change, root line in matrix is therefore the only line with a node not part of new_tree$edge[,2]
    root <-
      parameter_matrix[parameter_matrix$node %nin% new_tree$edge[, 2], ]
  }
  
  # i remove the root from the matrix, keeping only lines associated with node numbers in new_tree$edge[,2]
  parameter_matrix <-
    parameter_matrix[parameter_matrix$node %in% new_tree$edge[, 2], ]
  # deletion of the tip and the associated internal node
  parameter_matrix <-
    parameter_matrix[parameter_matrix$node %nin% c(tips_to_delete, noeud_interne_a_supprimer), ]
  
  # deletion of the tip of the ctual tree object with drop.tip function
  new_tree <- drop.tip(new_tree, tips_to_delete)
  # parameter_matrix$node node numbers do not correspond anymore to the
  # node numbers of the tree because the latter have been modified by the drop.tip
  # I therefore update them. it's okay to just replace because they are ordered
  parameter_matrix$node <- new_tree$edge[, 2]
  # adding the root line to the matrix ...
  parameter_matrix <- rbind(parameter_matrix, root)
  # modifying the root node number to meet the new node numbers (after they were modified by drop.tip)
  parameter_matrix$node[nrow(parameter_matrix)] <-
  min(new_tree$edge[, 1])
  #  parametres matrix is now in good order, with removed tips and a root line
}

# done
# the tree with deleted tips is in new_tree
# the parameter matrix is stored in parameter_matrix
 return_objects <- list("parameter_matrix" = parameter_matrix, "new_tree" = new_tree, "parameter_matrix_original"=parameter_matrix_original, "original_tree"=original_tree)
 
 return(return_objects)
}



# i can compare the original and new trees with parameters to check if deletion went well

# to plot the new tree
# state <- Analysis$parameter_matrix$clust10011.states
# state[state == "A"] <- "red"
# state[state == "P"] <- "green"
# 
# plot(Analysis$new_tree, edge.color = state, show.tip.label = F)
# 
# # to plot the original tree
# original_state <- Analysis$parameter_matrix_original$clust10011.states
# original_state[original_state == "A"] <- "red"
# original_state[original_state == "P"] <- "green"
# 
# plot(Analysis$original_tree,
#      edge.color = original_state,
#      show.tip.label = F)
# 
# nodelabels(frame = "none", cex = 0.5)
