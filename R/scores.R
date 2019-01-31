#' @export

mccScores = function(class1, class0, num_bins = 1000, scores_ascending = T){
  if(scores_ascending == T){
    max = max(c(class1,class0))
    result = NULL
    bins = seq(from = 0, to = max, by = max/num_bins)
    for(i in 1:length(bins)){
      TP = sum(class1<=bins[i])
      FP = sum(class0<=bins[i])
      TN = length(class0) - FP

      FN = length(class1) - TP
      result = c(mcc(TP, TN, FP, FN),result)
    }
    return(result)

  }


}