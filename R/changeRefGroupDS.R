#' 
#' @title Changes a reference level of a factor
#' @description This function is similar to R function \code{relevel}, 
#' @details In addition to what the R function does, this function
#' allows for the user to re-order the vector, putting the reference
#' group first. If the user chooses the re-order a warning is issued 
#' as this can introduce a mismatch of values if the vector is put back
#' into a table that is not reordered in the same way. Such mismatch
#' can render the results of operations on that table invalid.
#' @param xvect a factor vector
#' @param ref a character, the reference level
#' @param reorderByRef a boolean that tells whether or not the new 
#' vector should be ordered by the reference group.
#' @return  a factor of the same length as xvect
#' @author Isaeva, J., Gaye, A.
#' @export
#'
changeRefGroupDS <- function(xvect, ref=NULL, reorderByRef=NULL){
  
  if(reorderByRef){
    temp_xvect = stats::relevel(xvect, ref)
    # now reorder puting the ref group first
    idx1 <- which(xvect == ref)
    idx2 <- which(xvect != ref)
    new_xvect <- c(temp_xvect[idx1], temp_xvect[idx2])
  }else{
    new_xvect <- stats::relevel(xvect, ref)
  }
  
  return(new_xvect)
  
}
