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

module MHProposalMethod_Class

use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MultiVarDistNorm_Class                                        ,only:    MultiVarDistNorm_Type

implicit none

private

public                                                                ::    MHProposalMethod_Type

type, abstract, extends(MultiVarDistNorm_Type)                        ::    MHProposalMethod_Type
  logical                                                             ::    MeanDefined
  logical                                                             ::    CovDefined
contains
  private
  generic, public                                                     ::    Construct               =>    ConstructProp_Case1
  procedure(ConstructProp_Case1_MHProposalMethod), deferred, private  ::    ConstructProp_Case1
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructProp_Case1_MHProposalMethod(This, X, Cov )
    use                                                               ::    Parameters_Library
    import                                                            ::    MHProposalMethod_Type
    class(MHProposalMethod_Type), intent(inout)                       ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    X
    real(rkp), dimension(:,:), intent(in)                             ::    Cov
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

end module
