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

module MFileInput_Vec_Class

use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use MFileInput_Class                                              ,only:    MFileInput_Type
use MFileInput_Factory_Class                                      ,only:    MFileInput_Factory

implicit none

private

public                                                                ::    MFileInput_Vec_Type

type                                                                  ::    MFileInput_Vec_Type
  class(MFileInput_Type), pointer                                     ::    MFileInput=>null()
contains
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Set( This, Object )

    class(MFileInput_Vec_Type), intent(inout)                         ::    This
    class(MFileInput_Type), intent(in)                                ::    Object

    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    allocate(This%MFileInput, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%MFileInput', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get( This )

    class(MFileInput_Type), allocatable                               ::    Get

    class(MFileInput_Vec_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0

    if ( .not. associated(This%MFileInput) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    allocate(Get, source=This%MFileInput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPointer( This )

    class(MFileInput_Type), pointer                                   ::    GetPointer

    class(MFileInput_Vec_Type), intent(in)                            ::    This

    character(*), parameter                                           ::    ProcName='GetPointer'

    if ( .not. associated(This%MFileInput) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    GetPointer => This%MFileInput

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(MFileInput_Vec_Type), intent(inout)                          ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc

    if ( associated(This%MFileInput) ) deallocate(This%MFileInput, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%MFileInput', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
