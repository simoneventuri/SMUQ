Module Algebra_Library
  use Numerics_Matrix         ,only:  inv_matrix
  use Matrix_Inversion_Module ,only:  Inverse_Matrix
  implicit none
  private
  public  ::  inv_matrix
  public  ::  Inverse_Matrix
End Module