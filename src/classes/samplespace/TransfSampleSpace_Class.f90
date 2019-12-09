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
use SpaceParam_Class                                              ,only:    SpaceParam_Type

implicit none

private

public                                                                ::    TransfSampleSpace_Type

type, abstract, extends(SampleSpace_Type)                             ::    TransfSampleSpace_Type
  type(ParamSpace_Type)                                               ::    OrigSampleSpace
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
  function Transform1D_TransfSampleSpace( This, X, Debug )
    use Parameters_Library
    import                                                            ::    TransfSampleSpace_Type
    real(rkp), allocatable, dimension(:)                              ::    Transform1D_TransfSampleSpace   
    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:), intent(in)                               ::    X
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvTransform1D_TransfSampleSpace( This, Z, Debug )
    use Parameters_Library
    import                                                            ::    TransfSampleSpace_Type
    real(rkp), allocatable, dimension(:)                              ::    InvTransform1D_TransfSampleSpace   
    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:), intent(in)                               ::    Z
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains 

  !!------------------------------------------------------------------------------------------------------------------------------
  function Transform2D( This, X, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    Transform2D  

    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    X
    logical, optional ,intent(in)                                     ::    Debug   

    character(*), parameter                                           ::    ProcName='Transform2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDegen
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%NbDim /= size(X,1) ) call Error%Raise( Line='Incorrect dimensionality', ProcName=ProcName )

    NbDegen = size(X,2)

    allocate(Transform2D(This%NbDim,NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Transform2D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDegen
      Transform2D(:,i) = This%Transform( X=X(:,i) )
    end do

    if (DebugLoc) call Logger%Exiting
                                          
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvTransform2D( This, Z, Debug )

    real(rkp), allocatable, dimension(:,:)                            ::    InvTransform2D 
 
    class(TransfSampleSpace_Type), intent(inout)                      ::    This
    real(rkp), dimension(:,:), intent(in)                             ::    Z
    logical, optional ,intent(in)                                     ::    Debug   

    character(*), parameter                                           ::    ProcName='InvTransform2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    integer                                                           ::    NbDegen
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( This%NbDim /= size(Z,1) ) call Error%Raise( Line='Incorrect dimensionality', ProcName=ProcName )

    NbDegen = size(Z,2)

    allocate(InvTransform2D(This%NbDim,NbDegen), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Transform2D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbDegen
      InvTransform2D(:,i) = This%InvTransform( Z=Z(:,i) )
    end do

    if (DebugLoc) call Logger%Exiting
                                            
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
