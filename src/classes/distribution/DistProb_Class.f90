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

module DistProb_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    DistProb_Type

type, abstract                                                        ::    DistProb_Type
  character(:), allocatable                                           ::    Name
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  real(rkp)                                                           ::    A=One
  real(rkp)                                                           ::    B=One
  logical                                                             ::    TruncatedLeft=.false.
  logical                                                             ::    TruncatedRight=.false.
contains
  private
  procedure, public                                                   ::    GetName
  procedure, public                                                   ::    GetA
  procedure, public                                                   ::    GetB
  procedure, public                                                   ::    IsTruncatedLeft
  procedure, public                                                   ::    IsTruncatedRight
  generic, public                                                     ::    PDF                     =>    PDF_R0D
  generic, public                                                     ::    assignment(=)           =>    Copy
  generic, public                                                     ::    Construct               =>    ConstructInput,         &
                                                                                                          HierConstructCase1
  procedure(Initialize_DistProb), deferred, public                    ::    Initialize
  procedure(Reset_DistProb), deferred, public                         ::    Reset
  procedure(ConstructInput_DistProb), deferred, private               ::    ConstructInput
  procedure(HierConstructCase1_DistProb), deferred, private           ::    HierConstructCase1
  procedure(GetInput_DistProb), deferred, public                      ::    GetInput
  procedure(CDF_DistProb), deferred, public                           ::    CDF
  procedure(InvCDF_DistProb), deferred, public                        ::    InvCDF
  procedure(PDF_R0D_DistProb), deferred, private                      ::    PDF_R0D
  procedure(GetMean_DistProb), deferred, public                       ::    GetMean
  procedure(GetVariance_DistProb), deferred, public                   ::    GetVariance
  procedure(SetDefaults_DistProb), deferred, public                   ::    SetDefaults
  procedure(Copy_DistProb), deferred, public                          ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

abstract interface

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize_DistProb( This, Debug )
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset_DistProb( This, Debug )
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults_DistProb( This, Debug )
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput_DistProb( This, Input, Prefix, Debug )
    import                                                            ::    DistProb_Type
    import                                                            ::    InputSection_Type
    class(DistProb_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine HierConstructCase1_DistProb( This, Input, Prefix, Debug )
    import                                                            ::    DistProb_Type
    import                                                            ::    InputDet_Type
    class(DistProb_Type), intent(inout)                               ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput_DistProb( This, MainSectionName, Prefix, Directory, Debug )
    import                                                            ::    DistProb_Type
    import                                                            ::    InputSection_Type
    type(InputSection_Type)                                           ::    GetInput_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Directory
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF_DistProb(This, X, Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    CDF_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF_DistProb(This, P, Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    InvCDF_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D_DistProb(This, X, Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    PDF_R0D_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean_DistProb(This, Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    GetMean_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance_DistProb(This, Debug )
    use                                                               ::    Parameters_Library
    import                                                            ::    DistProb_Type
    real(rkp)                                                         ::    GetVariance_DistProb
    class(DistProb_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug
  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy_DistProb( LHS, RHS )
    import                                                            ::    DistProb_Type
    class(DistProb_Type), intent(out)                                 ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS
  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetName( This, Debug )

    character(:), allocatable                                         ::    GetName
    class(DistProb_Type), intent(in)                                  ::    This
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

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetA( This, Debug )

    real(rkp)                                                         ::    GetA

    class(DistProb_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetA'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( .not. This%TruncatedLeft ) call Error%Raise( Line='Distribution was never left truncated', ProcName=ProcName )

    GetA = This%A

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetB( This, Debug )

    real(rkp)                                                         ::    GetB

    class(DistProb_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetB'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    if ( .not. This%TruncatedRight ) call Error%Raise( Line='Distribution was never right truncated', ProcName=ProcName )

    GetB = This%B

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsTruncatedLeft( This, Debug )

    logical                                                           ::    IsTruncatedLeft

    class(DistProb_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsTruncatedLeft'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    IsTruncatedLeft = This%TruncatedLeft

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsTruncatedRight( This, Debug )

    logical                                                           ::    IsTruncatedRight

    class(DistProb_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsTruncatedRight'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    IsTruncatedRight = This%TruncatedRight

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
