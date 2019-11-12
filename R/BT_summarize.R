#' This function summarizes analyses conducted in BayesTraits
#' 
#' Note that burnin might have been applied in a previous stage of the analysis
#' @param folder where the output files of a BayesTraits analysis are found
#' @param bn burnin (0-1)

BT_summarize <- function(folder = ".", bn = 0) {
  file <- list.files(folder, "Trait_input.txt.Log")
  file2 <- list.files(folder, "Trait_input.txt.Schedule")
  if (length(file) == 0 |  length(file2) == 0) {
    message("Trait_input.txt.Log or Trait_input.txt.Schedule not found in the directory")
  } else{
    Skip = grep("Tree No",
                scan(
                  file = file,
                  what = "c",
                  quiet = T,
                  sep = "\n",
                  blank.lines.skip = FALSE
                )) - 1
    ASR = utils::read.table(
      file,
      skip = Skip,
      sep = "\t",
      quote = "\"",
      header = TRUE
    )

    Lh <- mean(ASR[-1:c(nrow(ASR) * bn), 2])
    mcmc.trace <- coda::mcmc(ASR[-c(1:nrow(ASR) * bn), 2])
    ESS <- coda::effectiveSize(mcmc.trace)

    acc <- utils::read.delim(file2, header = F)[-1,]
    ACC <-
      cbind.data.frame(
        Lh = Lh,
        ESS,
        min_ACCEPTACE = min(as.numeric(as.character(acc[-c(1:4), 2])), na.rm = T),
        max_ACCEPTACE = max(as.numeric(as.character(acc[-c(1:4), 2])), na.rm = T)
      )

    return(list(ACC = ACC, ASR = ASR))
  }
}
