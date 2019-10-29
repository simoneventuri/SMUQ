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

module LowDiscSequence_Class

use Parameters_Library
use Input_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    LowDiscSequence_Type

type, abstract                                                        ::    LowDiscSequence_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
contains
  generic, public                                                     ::    Construct               =>    ConstructInput
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Get                     =>    Get_0D,                 &
                                                                                                          Get_1D
  generic, public                                                     ::    GetPoint                =>    GetPoint_0D,            &
                                                                                                          GetPoint_1D
  generic, public                                                     ::    GetPoints               =>    GetPoints_0D,           &
                                                                                                          GetPoints_1D
  procedure(Initialize_LowDiscSequence), deferred, public             ::    Initialize
  procedure(Reset_LowDiscSequence), deferred, public                  ::    Reset
  procedure(SetDefaults_LowDiscSequence), deferred, public            ::    SetDefaults
  procedure(ConstructInput_LowDiscSequence), deferred, private        ::    ConstructInput
  procedure(GetInput_LowDiscSequence), deferred, public               ::    GetInput
  procedure(Get_0D_LowDiscSequence), deferred, public                 ::    Get_0D
  procedure(Get_1D_LowDiscSequence), deferred, public                 ::    Get_1D
  procedure(GetPoint_0D_LowDiscSequence), deferred, public            ::    GetPoint_0D
  procedure(GetPoint_1D_LowDiscSequence), deferred, public            ::    GetPoint_1D
  procedure(GetPoints_0D_LowDiscSequence), deferred, public           ::    GetPoints_0D
  procedure(GetPoints_1D_LowDiscSequence), deferred, public           ::    GetPoints_1D
  procedure(Copy_LowDiscSequence), deferred, public                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_LowDiscSequence( This, Debug )
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_LowDiscSequence( This, Debug )
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_LowDiscSequence( This, Debug )
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_LowDiscSequence ( This, Input, Prefix, Debug )
    import                                                            ::    LowDiscSequence_Type
    import                                                            ::    InputSection_Type
    class(LowDiscSequence_Type), intent(inout)                        ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_LowDiscSequence( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    LowDiscSequence_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_LowDiscSequence
    class(LowDiscSequence_Type), intent(in)                           ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Get_0D_LowDiscSequence( This, NbPoints, Debug )
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    real(rkp), allocatable, dimension(:)                              ::    Get_0D_LowDiscSequence
    integer, intent(in)                                               ::    NbPoints 
    class(LowDiscSequence_Type), intent(in)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Get_1D_LowDiscSequence( This, NbPoints, NbDim, Debug )
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    real(rkp), allocatable, dimension(:,:)                            ::    Get_1D_LowDiscSequence  
    class(LowDiscSequence_Type), intent(in)                           ::    This
    integer, intent(in)                                               ::    NbPoints
    integer, intent(in)                                               ::    NbDim
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetPoint_0D_LowDiscSequence( This, Point, Debug )
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    real(rkp)                                                         ::    GetPoint_0D_LowDiscSequence
    integer, intent(in)                                               ::    Point
    class(LowDiscSequence_Type), intent(in)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetPoint_1D_LowDiscSequence( This, Point, NbDim, Debug )
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    real(rkp), allocatable, dimension(:)                              ::    GetPoint_1D_LowDiscSequence  
    class(LowDiscSequence_Type), intent(in)                           ::    This
    integer, intent(in)                                               ::    Point
    integer, intent(in)                                               ::    NbDim
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetPoints_0D_LowDiscSequence( This, SeqStart, SeqEnd, Debug )
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    real(rkp), allocatable, dimension(:)                              ::    GetPoints_0D_LowDiscSequence
    integer, intent(in)                                               ::    SeqStart
    integer, intent(in)                                               ::    SeqEnd
    class(LowDiscSequence_Type), intent(in)                           ::    This
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetPoints_1D_LowDiscSequence( This, SeqStart, SeqEnd, NbDim, Debug )
    use Parameters_Library
    import                                                            ::    LowDiscSequence_Type
    real(rkp), allocatable, dimension(:,:)                            ::    GetPoints_1D_LowDiscSequence  
    class(LowDiscSequence_Type), intent(in)                           ::    This
    integer, intent(in)                                               ::    SeqStart
    integer, intent(in)                                               ::    SeqEnd
    integer, intent(in)                                               ::    NbDim
    logical, optional ,intent(in)                                     ::    Debug                                             
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_LowDiscSequence( LHS, RHS )
    import                                                            ::    LowDiscSequence_Type
    class(LowDiscSequence_Type), intent(out)                          ::    LHS
    class(LowDiscSequence_Type), intent(in)                           ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName

    class(LowDiscSequence_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetName'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    GetName = This%Name

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
