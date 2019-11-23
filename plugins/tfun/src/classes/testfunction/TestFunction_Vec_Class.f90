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

module TestFunction_Vec_Class

use Logger_Class                                                  ,only:  Logger
use Error_Class                                                   ,only:  Error
use TestFunction_Class                                            ,only:  TestFunction_Type
use TestFunction_Factory_Class                                    ,only:  TestFunction_Factory

implicit none

private

public                                                                ::    TestFunction_Vec_Type

type                                                                  ::    TestFunction_Vec_Type
  class(TestFunction_Type), pointer                                   ::    TestFunction=>null()
contains
  generic, public                                                     ::    assignment(=)           =>    Copy
  procedure, public                                                   ::    Get
  procedure, public                                                   ::    GetPointer
  procedure, public                                                   ::    Set
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Set( This, Object, Debug )

    class(TestFunction_Vec_Type), intent(inout)                       ::    This
    class(TestFunction_Type), intent(in)                              ::    Object
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Set'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%TestFunction) ) deallocate(This%TestFunction, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%TestFunction', ProcName=ProcName, stat=StatLoc)
    
    allocate(This%TestFunction, source=Object, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%TestFunction', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function Get( This, Debug )

    class(TestFunction_Type), allocatable                             ::    Get

    class(TestFunction_Vec_Type), intent(in)                          ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Get'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. associated(This%TestFunction) ) call Error%Raise( Line='Object never defined', ProcName=ProcName)

    allocate(Get, source=This%TestFunction, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Get', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetPointer( This, Debug )

    class(TestFunction_Type), pointer                                 ::    GetPointer

    class(TestFunction_Vec_Type), intent(in)                          ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetPointer'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. associated(This%TestFunction) ) call Error%Raise( Line='Object never defined', ProcName=ProcName)

    GetPointer => This%TestFunction

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(TestFunction_Vec_Type), intent(inout)                       ::    LHS
    class(TestFunction_Vec_Type), intent(in)                          ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (TestFunction_Vec_Type)
        if ( associated(RHS%TestFunction) ) then
          if ( associated(LHS%TestFunction) ) deallocate( LHS%TestFunction, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='LHS%TestFunction', Procname=ProcName, stat=StatLoc )
          allocate(LHS%TestFunction, source=RHS%TestFunction, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%TestFunction', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(TestFunction_Vec_Type), intent(inout)                        ::    This

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( associated(This%TestFunction) ) deallocate(This%TestFunction, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( name='This%TestFunction', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
