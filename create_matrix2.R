rm(list = ls())




body(create_matrix)[[12]] <- substitute(if (!is.null(originalMatrix)) {
  terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) %in% 
                                             colnames(matrix))])
  weight <- 0
  if (attr(weighting, "acronym") == "tf-idf") 
    weight <- 1e-09
  amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
  colnames(amat) <- terms
  rownames(amat) <- rownames(matrix)
  fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in% 
                                                        colnames(originalMatrix))], amat), weighting = weighting)
  matrix <- fixed
})