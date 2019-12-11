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

module MHProposalLastReject_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MHProposalMethod_Class                                        ,only:    MHProposalMethod_Type

implicit none

private

public                                                                ::    MHProposalLastReject_Type

type, extends(MHProposalMethod_Type)                                  ::    MHProposallastReject_Type

contains
  private
  procedure, private                                                  ::    ConstructProp_Case1
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructProp_Case1(This, X, Cov )

    class(MHProposalLastReject_Type), intent(inout)                   ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    X
    real(rkp), dimension(:,:), intent(in)                             ::    Cov

    character(*), parameter                                           ::    ProcName='ConstructProp_Case1'
    integer                                                           ::    StatLoc=0

    call This%Construct( Mu=X(:,size(X,2)), Cov=Cov )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
