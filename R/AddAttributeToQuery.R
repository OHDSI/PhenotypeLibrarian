#add an attribute to a query
addAttributeToQuery <- function(query, attribute) {
  attrib <- attribute@CriteriaExpression[[1]]
  query@CriteriaExpression[[1]]@Attributes <- append(query@CriteriaExpression[[1]]@Attributes, attrib)
  return(query)
}
