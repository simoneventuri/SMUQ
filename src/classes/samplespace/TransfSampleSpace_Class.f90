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

module TransfSampleSpace_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SampleSpace_Class                                             ,only:    SampleSpace_Type

implicit none

private

public                                                                ::    TransfSampleSpace_Type

type, abstract, extends(SampleSpace_Type)                             ::    TransfSampleSpace_Type

contains
  generic, public                                                     ::    Transform               =>    Transform1D,            &
                                                                                                          Transform2D
  procedure, public                                                   ::    Transform2D
  generic, public                                                     ::    InvTransform            =>    InvTransform1D,         &
                                                                                                          InvTransform2D
  procedure, public                                                   ::    InvTransform2D
  procedure(Transform1D_TransfSampleSpace), deferred, public          ::    Transform1D
  procedure(InvTransform1D_TransfSampleSpace), deferred, public       ::    InvTransform1D
end type

logical, parameter                                                    ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform1D_TransfSampleSpace(This, X)
    use Parameters_Library
    import                                                            ::    TransfSampleSpace_Type
    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:), intent(inout)                            ::    X                                             
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform1D_TransfSampleSpace(This, Z)
    use Parameters_Library
    import                                                            ::    TransfSampleSpace_Type
    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:), intent(inout)                            ::    Z   
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains 

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Transform2D(This, X)

    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    X   

    character(*), parameter                                           ::    ProcName='Transform2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDegen

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (This%NbDim /= size(X,1)) call Error%Raise(Line='Incorrect dimensionality', ProcName=ProcName)

    NbDegen = size(X,2)

    i = 1
    do i = 1, NbDegen
      call This%Transform(X=X(:,i))
    end do
                                          
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine InvTransform2D(This, Z)
 
    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:,:), intent(inout)                          ::    Z   

    character(*), parameter                                           ::    ProcName='InvTransform2D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDegen

    if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

    if (This%NbDim /= size(Z,1)) call Error%Raise(Line='Incorrect dimensionality', ProcName=ProcName)

    NbDegen = size(Z,2)

    i = 1
    do i = 1, NbDegen
      call This%InvTransform(Z=Z(:,i))
    end do
                                            
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
