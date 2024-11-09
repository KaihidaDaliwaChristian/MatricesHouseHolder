# Ecrivons une fonction qui calcule une matrice de Householder

#'@export

# Nous allons nommer cette fonction hh_matrix
hh_matrix <- function(x){
  # Définissons la taille de la matrice
  n <- length(x)
  # Définisons un vecteur de base
  e1 <- c(1,rep(0,n-1))

  # normalisons notre vecteur v pour obtenir un vecteur de norme 1
  alpha <- -sign(x[1])*sqrt(sum(x^2))
  u <- x-alpha*e1
  v <- u/sqrt(sum(u^2))# Qui est un vecteur normalisé

  # maintenant calculons la matrice de Householder pour notre vecteur
  H <- diag(n)-2*(v%*%t(v))

  # affichons notre matrice de Householder
  return(H)
}
