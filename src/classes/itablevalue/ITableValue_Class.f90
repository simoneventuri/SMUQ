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

module ITableValue_Class

use Input_Library
use Parameters_Library
use StringConversion_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use SMUQString_Class                                              ,only:    SMUQString_Type

implicit none

private

public                                                                ::    ITableValue_Type

type, abstract                                                        ::    ITableValue_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  procedure, public                                                   ::    GetName
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput
  procedure, public                                                   ::    GetStringValue
  procedure(Initialize_ITableValue), deferred, public                 ::    Initialize
  procedure(Reset_ITableValue), deferred, public                      ::    Reset
  procedure(SetDefaults_ITableValue), deferred, public                ::    SetDefaults
  procedure(ConstructInput_ITableValue), deferred, private            ::    ConstructInput
  procedure(GetInput_ITableValue), deferred, public                   ::    GetInput
  procedure(GetValue_ITableValue), deferred, public                   ::    GetValue
  procedure(Copy_ITableValue), deferred, public                       ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_ITableValue(This)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_ITableValue(This)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_ITableValue(This)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(inout)                            ::    This
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_ITableValue(This, Input, Prefix)
    import                                                            ::    ITableValue_Type
    import                                                            ::    InputSection_Type
    class(ITableValue_Type), intent(inout)                            ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_ITableValue(This, Name, Prefix, Directory)
    import                                                            ::    ITableValue_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_ITableValue
    class(ITableValue_Type), intent(in)                               ::    This
    character(*), intent(in)                                          ::    Name
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine GetValue_ITableValue(This, Input, Abscissa, Values)
    use Parameters_Library
    import                                                            ::    Input_Type
    import                                                            ::    ITableValue_Type  
    class(ITableValue_Type), intent(in)                               ::    This
    type(Input_Type), intent(in)                                      ::    Input
    real(rkp), dimension(:), intent(in)                               ::    Abscissa
    real(rkp), dimension(:), intent(inout)                            ::    Values
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy_ITableValue(LHS, RHS)
    import                                                            ::    ITableValue_Type
    class(ITableValue_Type), intent(out)                              ::    LHS
    class(ITableValue_Type), intent(in)                               ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

!!--------------------------------------------------------------------------------------------------------------------------------
function GetName(This)

  character(:), allocatable                                             ::    GetName
  class(ITableValue_Type), intent(inout)                                ::    This

  character(*), parameter                                               ::    ProcName='GetName'

  GetName = This%Name

end function
!!--------------------------------------------------------------------------------------------------------------------------------

!!--------------------------------------------------------------------------------------------------------------------------------
subroutine GetStringValue(This, Input, Abscissa, Strings, Format)
  
  class(ITableValue_Type), intent(in)                                 ::    This
  type(Input_Type), intent(in)                                        ::    Input
  real(rkp), dimension(:), intent(in)                                 ::    Abscissa
  type(SMUQString_Type), dimension(:), intent(inout)                  ::    Strings
  character(*), optional, intent(in)                                  ::    Format

  character(*), parameter                                             ::    ProcName='GetStringValue'
  integer                                                             ::    StatLoc=0
  real(rkp), allocatable, dimension(:)                                ::    VarR1D
  character(:), allocatable                                           ::    FormatLoc
  integer                                                             ::    i

  if (.not. This%Constructed) call Error%Raise(Line='Object was never constructed', ProcName=ProcName)

  FormatLoc = 'G0'
  if (present(Format)) FormatLoc = Format

  if (size(Strings,1) /= size(Abscissa,1)) call Error%Raise('Incompatible strings array', ProcName=ProcName)

  allocate(VarR1D(size(Abscissa,1)), stat=StatLoc)
  if (StatLoc /= 0) call Error%Allocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)
  
  call This%GetValue(Input=Input, Abscissa=Abscissa, Values=VarR1D)
  call ConvertToStrings(Values=VarR1D, Strings=Strings, Format=FormatLoc)

  deallocate(VarR1D, stat=StatLoc)
  if (StatLoc /= 0) call Error%Deallocate(Name='VarR1D', ProcName=ProcName, stat=StatLoc)

end subroutine
!!--------------------------------------------------------------------------------------------------------------------------------

end module
