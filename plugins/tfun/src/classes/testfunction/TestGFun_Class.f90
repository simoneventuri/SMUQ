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

module TestGFun_Class

use Input_Library
use Parameters_Library
use String_Library
use StringRoutines_Module
use TestFunction_Class                                            ,only:    TestFunction_Type
use Output_Class                                                  ,only:    Output_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    TestGFun_Type

type, extends(TestFunction_Type)                                      ::    TestGFun_Type
  integer                                                             ::    NbParams=0
  type(String_Type), allocatable, dimension(:)                        ::    InputLabel
  real(rkp), allocatable, dimension(:)                                ::    Parameters
  real(rkp), allocatable, dimension(:)                                ::    c
  real(rkp), dimension(1)                                             ::    Abscissa=Zero
  character(:), allocatable                                           ::    AbscissaName
  character(:), allocatable                                           ::    ResponseName
  character(:), allocatable                                           ::    Label
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, public                                                   ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, nopass, public                                           ::    ComputeGFun
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Initialize( This, Debug )

    class(TestGFun_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
    if ( present(Debug) ) DebugLoc = Debug

    if ( .not. This%Initialized ) then
      This%Name = 'GFun'
      This%Initialized = .true.
    end if

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Reset( This, Debug )

    class(TestGFun_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    This%NbParams = 0

    if ( allocated(This%c) ) deallocate(This%c, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%c', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%InputLabel) ) deallocate(This%InputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Parameters) ) deallocate(This%Parameters, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Parameters', ProcName=ProcName, stat=StatLoc )

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine SetDefaults( This, Debug )

    class(TestGFun_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Abscissa=Zero
    This%ResponseName = 'gfunction'
    This%AbscissaName = 'scalar'
    This%Label = 'gfunction'

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine ConstructInput( This, Input, Prefix, Debug )

    use StringRoutines_Module

    class(TestGFun_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    logical                                                           ::    MandatoryLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    ParameterName = 'response_name'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%ResponseName = VarC0D

    ParameterName = 'abscissa_name'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%AbscissaName = VarC0D

    ParameterName = 'label'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Label = VarC0D

    ParameterName = 'nb_dimensions'
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.true. )
    This%NbParams = VarI0D

    allocate(This%InputLabel(This%NbParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    allocate(This%Parameters(This%NbParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Parameters', ProcName=ProcName, stat=StatLoc )
    This%Parameters = Zero

    SectionName = 'parameters'

    i = 1
    do i = 1, This%NbParams
      This%InputLabel(i) = ''
      ParameterName = 'parameter' // ConvertToString(Value=i) // '_dependency'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.false., Found=Found )
      if ( Found ) This%InputLabel(i) = VarC0D
      MandatoryLoc = .not. Found
      ParameterName = 'parameter' // ConvertToString(Value=i)
      call Input%GetValue( VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=MandatoryLoc, Found=Found )
      if ( Found ) This%Parameters(i) = VarR0D
    end do

    allocate(This%c(This%NbParams), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='c', ProcName=ProcName, stat=StatLoc )
    SectionName = 'c'
    if ( Input%HasSection( SubSectionName=SectionName, CaseSensitive=.false. ) ) then
      i = 1
      do i = 1, This%NbParams
        ParameterName = 'c' // ConvertToString(Value=i)
        call Input%GetValue( Value=VarR0D, ParameterName=ParameterName, SectionName=SectionName, Mandatory=.true. )
        This%c(i) = VarR0D
      end do
    else
      i = 1
      do i = 1, This%NbParams
        This%c(i) = (real(i,8) - Two) / Two
      end do
    end if

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput
    class(TestGFun_Type), intent(in)                                  ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    ParameterName
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='response_name', Value=This%ResponseName )
    call GetInput%AddParameter( Name='abscissa_name', Value=This%AbscissaName )
    call GetInput%AddParameter( Name='nb_dimensions', Value=ConvertToString(Value=This%NbParams) )
    call GetInput%AddParameter( Name='label', Value=This%AbscissaName )

    SectionName='parameters'
    call GetInput%AddSection( SectionName=SectionName )
    i = 1
    do i = 1, This%NbParams
      ParameterName = 'parameter' // ConvertToString(Value=i) // '_dependency'
      if ( len_trim(This%InputLabel(i)%GetValue()) > 0 ) call GetInput%AddParameter( Name=ParameterName,                          &
                                                                    Value=This%InputLabel(i)%GetValue(), SectionName=SectionName )
      ParameterName = 'parameter' // ConvertToString(Value=i)
      call GetInput%AddParameter( Name=ParameterName, Value=ConvertToString(Value=This%Parameters(i)), SectionName=SectionName )
    end do

    SectionName='c'
    call GetInput%AddSection( SectionName=SectionName )
    i = 1
    do i = 1, This%NbParams
      call GetInput%AddParameter( Name='c' // ConvertToString(Value=i), Value=ConvertToString(Value=This%c(i)),                   &
                                                                                                         SectionName=SectionName )
    end do


    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Run( This, Input, Output, Debug )

    class(TestGFun_Type), intent(inout)                               ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ProcessInput'
    integer                                                           ::    StatLoc=0
    real(rkp), allocatable, dimension(:,:)                            ::    Ordinate
    real(rkp), allocatable, dimension(:)                              ::    X
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    character(:), allocatable                                         ::    VarC0D
    type(InputDet_Type)                                               ::    InputDetLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( .not. allocated(Output) ) then
      allocate( Output(1), stat=StatLoc )
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    else
      if ( size(Output,1) /= 1 ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
        allocate( Output(1), stat=StatLoc )
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if

    select type (Input)
      type is (InputDet_Type)
        allocate(Ordinate(1,1), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        allocate(X(This%NbParams), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='X', ProcName=ProcName, stat=StatLoc )
        ii = 1
        do ii = 1, This%NbParams
          if ( len_trim(This%InputLabel(i)%GetValue()) /= 0 ) then
            call Input%GetValue( Value=X(ii), Label=This%InputLabel(i)%GetValue() )
          else
            X(ii) = This%Parameters(ii)
          end if
        end do
        Ordinate(1,1) = This%ComputeGFun( X, This%c )
        deallocate(X, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='X', ProcName=ProcName, stat=StatLoc )

      type is (InputStoch_Type)
        allocate(Ordinate(1,Input%GetNbDegen()), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )
        allocate(X(This%NbParams), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='X', ProcName=ProcName, stat=StatLoc )
        i = 1
        do i = 1, Input%GetNbDegen()
          InputDetLoc = Input%GetDetInput(Num=i)
          ii = 1
          do ii = 1, This%NbParams
            if ( len_trim(This%InputLabel(i)%GetValue()) /= 0 ) then
              call InputDetLoc%GetValue( Value=X(ii), Label=This%InputLabel(i)%GetValue() )
            else
              X(ii) = This%Parameters(ii)
            end if
          end do
          Ordinate(1,i) = This%ComputeGFun( X, This%c )
        end do

      class default
        call Error%Raise( Line='Update input type definitions', ProcName=ProcName )
    end select

    deallocate(X, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='X', ProcName=ProcName, stat=StatLoc )

    call Output(1)%Construct( Abscissa=This%Abscissa, Ordinate=Ordinate, AbscissaName=This%AbscissaName,                          &
                                                                                 OrdinateName=This%ResponseName, Label=This%Label)

    deallocate(Ordinate, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Ordinate', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  function ComputeGFun( X, c, Debug )

    real(rkp)                                                         ::    ComputeGFun

    real(rkp), dimension(:), intent(in)                               ::    X
    real(rkp), dimension(:), intent(in)                               ::    c
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeGFun'
    integer                                                           ::    NbParams
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbParams = size(X,1)

    ComputeGFun = One
    i = 1
    do i = 1, NbParams
        ComputeGFun = ComputeGFun * ( (abs(Four*X(i)-Two)+c(i)) / (One+c(i)) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!----------------------------------------------------------------------------------------------------------------------------!!


  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Copy( LHS, RHS )

    class(TestGFun_Type), intent(out)                                 ::    LHS
    class(TestFunction_Type), intent(in)                              ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
      type is (TestGFun_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        if ( RHS%Constructed ) then
          LHS%Label = RHS%Label
          LHS%AbscissaName = RHS%AbscissaName
          LHS%ResponseName = RHS%ResponseName
          LHS%NbParams = RHS%NbParams
          allocate(LHS%InputLabel, source=RHS%InputLabel, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%InputLabel', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Parameters, source=RHS%Parameters, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Parameters', ProcName=ProcName, stat=StatLoc )
          allocate( LHS%c, source=RHS%c, stat=StatLoc )
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%c', ProcName=ProcName, stat=StatLoc )
          LHS%Abscissa = RHS%Abscissa
        end if
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )
    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

  !!----------------------------------------------------------------------------------------------------------------------------!!
  subroutine Finalizer( This )

    type(TestGFun_Type), intent(inout)                                ::    This

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%InputLabel) ) deallocate(This%InputLabel, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%InputLabel', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%c) ) deallocate(This%c, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%c', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Parameters) ) deallocate(This%Parameters, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Parameters', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!----------------------------------------------------------------------------------------------------------------------------!!

end module
