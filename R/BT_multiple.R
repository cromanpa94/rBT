#' Input files for multiple BT runs
#' 
#' This function creates imput files for a large combination of the parameters
#'  under the BayesTraits MultiState model.
#'
#'  Specifically, this function is designed to run models differing in the following
#'   parameters: (1) "PriorAll" vs. "RevJump", (2) Variable rates vs. non-Variable rates,
#'  and (3) uniform vs. exponential distributions.
#'
#' @param tree A fully-bifurcating Phy object
#' @param dataset A data.frame object with two columns (tip name + state[numeric])
#' @param tip.label A \code{character} indicating the name of the column with the trait
#' @param trait A \code{character} indicating the name of the column with the tip names in the dataset
#' @param run \code{logic} whether analyses should start after input files are created
#' @param name_general A \code{character} indicating the name of the analuysis (use unique per analysis)
#' @param ML \code{logic} whether parameters should be optimized under a Maximum Likelihood approach. MCMC analyses are available under \code{ML=F}
#' @param it The number of iterations under a bayesian optimization (only when \code{ML=F})
#' @param bur Total number of generations assumed as burnin under a bayesian optimization (only when \code{ML=F})
#' @export


BT_multiple <-
  function(tree,
           dataset,
           tip.label ,
           trait ,
           run = F,
           name_general,
           ML = F,
           it = 11000000,
           bur = 1000000) {
    priors <- c("PriorAll", "RevJump")
    rates <- c(T, F)
    distrib <- c("uniform", "exp")

    for (i in 1:2) {
      for (j in 1:2) {
        for (k in 1:2) {
          BT_single(
            tree = tree,
            dataset = dataset ,
            model = priors[i],
            vrates = rates[j],
            dist = distrib[k],
            val_prior = if (distrib[k] == "exp") {
              10
            } else{
              c(0, 10)
            },
            tip.label = tip.label,
            trait = trait,
            ML = F,
            run = run,
            name = paste0(
              name_general,
              priors[i],
              "_Vrates=",
              rates[j],
              "_",
              distrib[k]
            )
          )
          BT_progress(i / 2, j / 2, k / 3)
        }
      }
    }

  }





