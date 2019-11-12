#' Input files for a single BT run
#' 
#' This function creates all input files for a single analysis under the BayesTraits
#' MultiState model.
#'
#'
#' @param tree A fully-bifurcating Phy object
#' @param dataset A data.frame object with two columns (tip name + state[numeric])
#' @param tip.label A \code{character} indicating the name of the column with the trait
#' @param trait A \code{character} indicating the name of the column with the tip names in the dataset
#' @param ML \code{logic} whether parameters should be optimized under a Maximum Likelihood approach. MCMC analyses are available under \code{ML=F}
#' @param model A \code{character} vector used to set the analysis to a "PriorAll" or "RevJump" model
#' @param vrates Whether a VarRates model should be abalyzed, \code{logic}
#' @param Iterations The number of iterations under a bayesian optimization (only when \code{ML=F})
#' @param Burnin Total number of generations assumed as burnin under a bayesian optimization (only when \code{ML=F})
#' @param dist A \code{character} vector indicating whether the distribution is "exp" (exponential) or "uniform" (uniform)
#' @param val_prior A vector of class \code{numeric} with the parameters for the "exp" (1 parameter) or "uniform (2 parameters) distribution
#' @param name The name of the run. This name is used to create a subfolder within the working directory
#' @param run \code{logic} whether analyses should start after input files are created
#'
#' @export



BT_single <- function(tree,
                            dataset,
                            tip.label,
                            trait,
                            ML = ML,
                            model = "RevJump",
                            vrates = T,
                            Iterations = 10000,
                            Burnin = 100,
                            dist = "exp",
                            val_prior = 10,
                            name = "Default",
                            run = F) {
  mainDir <- getwd()
  subDir <- name
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
  setwd(file.path(mainDir, subDir))


  Input_BayesTraits <-
    cbind.data.frame(Tip = dataset[, tip.label], Habitat = dataset[, trait])
  row.names(Input_BayesTraits) <- Input_BayesTraits$Tip
  Input_BayesTraits$Tip <- NULL

  utils::write.table(Input_BayesTraits, "Trait_input.txt", quote = F)
  phytools::writeNexus(tree, "Tree_input.tree")

  val_prior2 <-
    ifelse(length(val_prior) > 1,
           paste(val_prior, collapse = " "),
           val_prior)

  input = c(1, ifelse(ML == T, 1, 2))
  input2 = if (model != "RevJump") {
    c(paste("PriorAll", dist, format(val_prior2, scientific = FALSE)))
  } else{
    c(paste("RevJump", dist, format(val_prior2, scientific = FALSE)))
  }
  input <- c(input, input2)
  if (vrates == T) {
    input = c(input, paste("VarRates"))
  } else{
  }
  input = c(input, paste("Burnin", format(Burnin, scientific = FALSE)))
  input = c(input, paste("Iterations", format(Iterations, scientific = FALSE)))

  ##Parameters


  for (i in 1:tree$Nnode) {
    input = c(input, paste(
      "AddTag",
      paste("internalNode", i, sep = ""),
      paste(ape::extract.clade(tree, length(tree$tip.label) +
                            i)$tip.label, collapse = ' ')
    ))
  }

  write(input, "Input.txt")
  for (i in 1:tree$Nnode) {
    write(
      paste0("AddNode", " RecNode", i, " internalNode", i),
      file = "Input.txt",
      append = TRUE
    )
  }

  write(paste0("run"), file = "Input.txt", append = TRUE)

  ##Run BT
  if (run == F) {
  } else{
    system(paste("bayestraits Tree_input.tree Trait_input.txt < Input.txt"))
    setwd(file.path(mainDir))
  }
  setwd(file.path(mainDir))

}
