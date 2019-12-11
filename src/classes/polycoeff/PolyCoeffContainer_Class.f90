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

module PolyCoeffContainer_Class

use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use PolyCoeff_Class                                               ,only:    PolyCoeff_Type
use PolyCoeff_Factory_Class                                       ,only:    PolyCoeff_Factory

implicit none

private

public                                                                ::    PolyCoeffContainer_Type

type                                                                  ::    PolyCoeffContainer_Type
  class(PolyCoeff_Type), pointer                                      ::    PolyCoeff=>null()
contains
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Set( This, Object )

    class(PolyCoeffContainer_Type), intent(inout)                     ::    This
    class(PolyCoeff_Type), intent(in)                                 ::    Object

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    allocate(This%PolyCoeff, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Get( This )

    class(PolyCoeff_Type), allocatable                                ::    Get

    class(PolyCoeffContainer_Type), intent(in)                        ::    This

    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0

    if ( .not. associated(This%PolyCoeff) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    allocate(Get, source=This%PolyCoeff, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetPointer( This )

    class(PolyCoeff_Type), pointer                                    ::    GetPointer

    class(PolyCoeffContainer_Type), intent(in)                        ::    This

    character(*), parameter                                           ::    ProcName='GetPointer'

    if ( .not. associated(This%PolyCoeff) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    GetPointer => This%PolyCoeff

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(PolyCoeffContainer_Type), intent(inout)                     ::    LHS
    class(PolyCoeffContainer_Type), intent(in)                        ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (PolyCoeffContainer_Type)
        if ( associated(RHS%PolyCoeff) ) then
          if ( associated(LHS%PolyCoeff) ) deallocate( LHS%PolyCoeff, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='LHS%PolyCoeff', Procname=ProcName, stat=StatLoc )
          allocate(LHS%PolyCoeff, source=RHS%PolyCoeff, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%PolyCoeff', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(PolyCoeffContainer_Type), intent(inout)                      ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc

    if ( associated(This%PolyCoeff) ) deallocate(This%PolyCoeff, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%PolyCoeff', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
