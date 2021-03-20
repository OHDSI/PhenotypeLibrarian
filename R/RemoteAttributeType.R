#function to remove an attribute type from a query
#Note this is a situation specific fix and needs to be generalized for Capr package
#' @export
removeAttributeType <- function(x, attrName) {
  if (componentType(x) != "Query") {
    stop("function only works for query type components")
  }
  aa <- x@CriteriaExpression[[1]]@Attributes #extract attributes from query
  aa <- sapply(aa, methods::slot, name = "Name") # get names of attributes
  idx <- grep(attrName, aa) #grep the attribute names to the name of attribute you want to remove
  if (length(idx) == 1 & length(aa) == 1) { #if this attribute is the only in list make empty list
    x@CriteriaExpression[[1]]@Attributes <- list()
  } else {#if this attribute is one of several attributes remove only this attribute type
    for (j in seq_along(aa)) {
      x@CriteriaExpression[[1]]@Attributes[[j]] <- NULL
    }
  }
  return(x)
}
