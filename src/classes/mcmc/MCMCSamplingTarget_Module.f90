! -*-f90-*-
!!--------------------------------------------------------------------------------------------------------------------------------
!!
!! Stochastic Modeling & Uncertainty Quantification (SMUQ)
!!
!! Copyright (C) 2016 Venturi, Simone & Rostkowski, Przemyslaw (University of Illinois at Urbana-Champaign)
!!
!! This program is free software; you can redistribute it and/or modify it under the terms of the Version 2.1 GNU Lesser General
!! Public License as published by the Free Software Foundation.
!!
!! This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to the Free 
!! Software Foundation, Inc. 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!!
!!--------------------------------------------------------------------------------------------------------------------------------

module MCMCSamplingTarget_Module

use InputDet_Class                                                ,only:    InputDet_Type

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine MCMCSamplingTarget( Input, Value, MiscValues )
    use Parameters_Library
    import                                                            ::    InputDet_Type
    real(rkp), intent(out)                                            ::    Value
    real(rkp), allocatable, dimension(:), intent(inout)               ::    MiscValues
    type(InputDet_Type), intent(in)                                   ::    Input
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

end module
