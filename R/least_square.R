# Notre troisième fonction permet enfin de déterminer
#les coeff de la droite de regresion en utilisant les moindres carrés
#'@export
# Appelons cette fonction least_sqr
least_sqr <- function(A,b){

  # Etape1 Factorisation QR de A
  qr_resul <- qr_decomp(A)
  Q <- qr_resul$Q
  R <- qr_resul$R

  # Etape2 Calcul de t(Q)*b
  b_tilde <- t(Q)%*%b

  # Etape3 Resolution de R*x=b_tilde par substitution inverse
  x <- backsolve(R,b_tilde)

  return(x)
}

