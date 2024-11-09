# Notre deuxième fonction permet d'éffectuer la décomposition QR d'une matrice
#'@export
# Nous allons l'appeler qr_decomp
qr_decomp <- function(A){

  # Définissons la taille de notre matrice et nommons là A
  n <- nrow(A)
  m <- ncol(A)

  # Initialisons nos matrices R et Q
  R <- A
  Q <- diag(n)

  # Créons une boucle qui donnera la décomposition QR de A
  for (j in 1:m) {
    # Sélectionnons la sous-colonne à transformer
    x <- R[j:n,j]

    # Créons la matrice de Householder pour la matrice que nous avons créée
    H_j <- diag(n)
    H_j[j:n,j:n] <- hh_matrix(x)

    # Appliquons maintenant l'algorithme de Householder pour trouver R et Q
    R <- H_j%*%R
    Q <- Q%*%H_j
  }
  return(list(Q=Q,R=R))
}
