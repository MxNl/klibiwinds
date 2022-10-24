label_every_nth = function(x, nth = 3) {
  labels <- rep("", length(x))
  labels_indices <- seq(1, length(x), nth)
  labels[labels_indices] <- x[labels_indices]
  return(labels)
}
