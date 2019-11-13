module Brent_Library

implicit none

private

public                                                                ::    Brent_Zero

interface Brent_Zero
  module procedure                                                    ::    zero
end interface

contains

# include "split_files/glomin.f90"
# include "split_files/local_min.f90"
# include "split_files/local_min_rc.f90"
# include "split_files/timestamp.f90"
# include "split_files/zero.f90"
# include "split_files/zero_rc.f90"

end module
