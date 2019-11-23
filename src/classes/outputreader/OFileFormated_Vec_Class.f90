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

module OFileFormated_Vec_Class

use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use OFileFormated_Class                                           ,only:    OFileFormated_Type
use OFileFormated_Factory_Class                                   ,only:    OFileFormated_Factory

implicit none

private

public                                                                ::    OFileFormated_Vec_Type

type                                                                  ::    OFileFormated_Vec_Type
  class(OFileFormated_Type), pointer                                  ::    OFileFormated=>null()
contains
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Set( This, Object, Debug )

    class(OFileFormated_Vec_Type), intent(inout)                      ::    This
    class(OFileFormated_Type), intent(in)                             ::    Object
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(This%OFileFormated, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%OFileFormated', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get( This, Debug )

    class(OFileFormated_Type), allocatable                            ::    Get

    class(OFileFormated_Vec_Type), intent(in)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. associated(This%OFileFormated) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    allocate(Get, source=This%OFileFormated, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPointer( This, Debug )

    class(OFileFormated_Type), pointer                                ::    GetPointer

    class(OFileFormated_Vec_Type), intent(in)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetPointer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. associated(This%OFileFormated) ) call Error%Raise( Line='Member object defined', ProcName=ProcName)

    GetPointer => This%OFileFormated

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(OFileFormated_Vec_Type), intent(inout)                       ::    This

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%OFileFormated) ) deallocate(This%OFileFormated, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%OFileFormated', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
