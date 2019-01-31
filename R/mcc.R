#' @export
#'

mcc = function(TP, TN, FP, FN){
  num = (TP*TN - FP*FN)
  if(!(TP+FP)||!(TP+FN)||!(TN+FN)||!(TN+FP)){
    denom = 1
  }else{
    denom = sqrt(TP+FP)*sqrt(TP+FN)*sqrt(TN+FP)*sqrt(TN+FN)
  }
  return(num/denom)
}