#' Matrix of marginal probabilities at nodes
#' 
#' This function takes a BayesTraits output. The matrix sould be either provided as a data frame()see BT_summarize function or read using this function from a given foldder
#' @param ASR_matrix should have the following structure RecNodeX.P.1. and RecNodeX.P.0.
#' @param nstates Number of discrete states in the analysis
#' @param bn burnin (0-1)
#' @param read \code{logical} whether results should be read from the selected \code{folder}
#' @param folder where the output files of a single BayesTraits analysis are found
#' @export


BT_ASR_matrix <- function(ASR_matrix=NULL, nstates=2, bn=0.1, read=T, folder="."){

  if(read == T){
  file <- list.files(folder, "Trait_input.txt.Log")
  
  Skip = grep("Tree No",
              scan(
                file = file,
                what = "c",
                quiet = T,
                sep = "\n",
                blank.lines.skip = FALSE
              )) - 1
  
  ASR_matrix = utils::read.table(
    file,
    skip = Skip,
    sep = "\t",
    quote = "\"",
    header = TRUE
  )
  }


  nodes_only<-ASR_matrix[,grep("Root", colnames(ASR_matrix))[1]: ncol(ASR_matrix)]
  istate<-seq(1, ncol(nodes_only), by=nstates)
  istate<-istate[-length(istate)]
  Matrix_PS<-lapply(1:length(istate), function(x){
    probs<-nodes_only[,c(istate[x]: c(istate[x]+(nstates-1)) )]
    probs<-probs[-1:c(nrow(probs) * bn),]
    probs<-colMeans(probs)
    names(probs)<-paste0("State_",seq(0,nstates-1))
    probs
  })

  Matrix_PS<-do.call(rbind,Matrix_PS)
  return(Matrix_PS)
}
