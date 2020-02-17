# Drop_Tip_Beast
Script to delete tips of a summary tree produced by TreeAnnotator from a Beast .trees file containing multiple trees
 In phylogeny analysis, it is common to use Beast.
 It usually outputs tree files (.trees) containing multiple trees
 from the parameter spaces of the Bayesian analysis.
 As trees are all different, TreeAnnotator is then used to produce a summary tree
 that contains values (95% HPD, mean and/or median values) of the parameters (node ages, state of a discrete trait etc).

 This script is to be used on the summary tree file outputed by TreeAnnotator. 
 It allows to delete one or more individuals of the tree and update
 at the same time the parameter matrix.

 It therefore allows the simple plotting of trees with less individuals
 than the ones used in the Bayesian analysis


########################## Working example ###################################


 Analysis <- drop.tip.beast("treeannotator_tree_file.tree","individuals_to_delete.csv")
  treeannotator_tree_file : tree file in the current directory
  indivs_names_to_delete : csv file with ; separator, no quote,
  first field must indivs to delete names
##############################################################################


 Developped based on tree file format of Beast v1.9 / 1.10
 
 This script may or may not work on other versions output trees.
