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

module LikelihoodProduct_Class

use Input_Library
use Parameters_Library
use ArrayRoutines_Module
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type
use Response_Class                                                ,only:    Response_Type
use Output_Class                                                  ,only:    Output_Type
use LikelihoodFunction_Class                                      ,only:    LikelihoodFunction_Type
use LikelihoodFunction_Vec_Class                                  ,only:    LikelihoodFunction_Vec_Type
use LikelihoodFunction_Factory_Class                              ,only:    LikelihoodFunction_Factory

implicit none

private

public                                                                ::    LikelihoodProduct_Type

type, extends(LikelihoodFunction_Type)                                ::    LikelihoodProduct_Type
  type(LikelihoodFunction_Vec_Type), allocatable, dimension(:)        ::    Likelihoods
  integer                                                             ::    NbLikelihoods
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    Evaluate_0D
  procedure, private                                                  ::    Evaluate_1D
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(LikelihoodProduct_Type), intent(inout)                      ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'LikelihoodProduct'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(LikelihoodProduct_Type), intent(inout)                      ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized=.false.
    This%Constructed=.false.

    if ( allocated(This%Likelihoods) ) deallocate(This%Likelihoods, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Likelihoods', ProcName=ProcName, stat=StatLoc )
    This%NbLikelihoods = 0

    call This%SetDefaults()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(LikelihoodProduct_Type), intent(inout)                        ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(LikelihoodProduct_Type), intent(inout)                      ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    integer                                                           ::    StatLoc=0
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    i
    character(:), allocatable                                         ::    SectionName
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(LikelihoodFunction_Type), allocatable                       ::    LikelihoodFunction

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%NbLikelihoods = Input%GetNumberofSubSections()

    allocate(This%Likelihoods(This%NbLikelihoods), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Likelihoods', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbLikelihoods
      SectionName = 'likelihood' // ConvertToString(Value=i)
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call LikelihoodFunction_Factory%Construct( Object=LikelihoodFunction, Input=InputSection, Prefix=PrefixLoc )
      call This%Likelihoods(i)%Set( Object=LikelihoodFunction )
      deallocate(LikelihoodFunction, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='LikelihoodFunction', ProcName=ProcName, stat=StatLoc )
    end do

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(LikelihoodProduct_Type), intent(inout)                      ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i
    class(LikelihoodFunction_Type), pointer                           ::    LFunctionPtr=>null()

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
  
    i = 1
    do i = 1, This%NbLikelihoods
      SectionName = 'likelihood' // ConvertToString(Value=i)
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/likelihood' // ConvertToString(Value=i)
      LFunctionPtr => This%Likelihoods(i)%GetPointer()
      call GetInput%AddSection( Section=LikelihoodFunction_Factory%GetObjectInput( Object=LFunctionPtr,                           &
                                                         MainSectionName=SectionName, Prefix=PrefixLoc, Directory=DirectorySub ) )
    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_1D( This, Responses, Input, Output, LogValue, Debug )

    real(rkp)                                                         ::    Evaluate_1D

    class(LikelihoodProduct_Type), intent(inout)                      ::    This
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    type(InputDet_Type), intent(in)                                   ::    Input
    type(Output_Type), dimension(:), intent(in)                       ::    Output
    logical, optional, intent(in)                                     ::    LogValue
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Evaluate_1D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    class(LikelihoodFunction_Type), pointer                           ::    LFunctionPtr=>null()
    character(:), allocatable                                         ::    LabelLoc
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    LogValueLoc
    real(rkp)                                                         ::    HVarR0D
    real(rkp)                                                         ::    TVarR0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    LogValueLoc = .false.
    if ( present(LogValue) ) LogValueLoc = LogValue

    Evaluate_1D = Zero

    do i = 1, This%NbLikelihoods

      LFunctionPtr => This%Likelihoods(i)%GetPointer()

      VarR0D = LFunctionPtr%Evaluate( Responses=Responses, Input=Input, Output=Output, LogValue=.true. )
      nullify(LFunctionPtr)

      if ( VarR0D <= -huge(One) ) then
        Evaluate_1D = VarR0D
        exit
      end if

      Evaluate_1D = Evaluate_1D + VarR0D
      
    end do

    if ( .not. LogValueLoc ) then
      if ( Evaluate_1D > TVarR0D .and. Evaluate_1D < HVarR0D ) then
        Evaluate_1D = dexp(Evaluate_1D)
      elseif (Evaluate_1D < TVarR0D ) then
        Evaluate_1D= Zero
      else
        call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                           &
                           ConvertToString(Value=Evaluate_1D) // '. Consider changing value of the scalar modifier of responses' )
      end if
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Evaluate_0D( This, Response, Input, Output, LogValue, Debug )

    real(rkp)                                                         ::    Evaluate_0D

    class(LikelihoodProduct_Type), intent(inout)                      ::    This
    type(Response_Type), intent(in)                                   ::    Response
    type(InputDet_Type), intent(in)                                   ::    Input
    type(Output_Type), intent(in)                                     ::    Output
    logical, optional, intent(in)                                     ::    LogValue
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Evaluate_0D'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    class(LikelihoodFunction_Type), pointer                           ::    LFunctionPtr=>null()
    character(:), allocatable                                         ::    LabelLoc
    real(rkp)                                                         ::    VarR0D
    logical                                                           ::    LogValueLoc
    real(rkp)                                                         ::    HVarR0D
    real(rkp)                                                         ::    TVarR0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    HVarR0D = dlog(huge(VarR0D))
    TVarR0D = dlog(tiny(VarR0D))

    LogValueLoc = .false.
    if ( present(LogValue) ) LogValueLoc = LogValue

    Evaluate_0D = Zero

    do i = 1, This%NbLikelihoods

      LFunctionPtr => This%Likelihoods(i)%GetPointer()

      VarR0D = LFunctionPtr%Evaluate( Response=Response, Input=Input, Output=Output, LogValue=.true. )
      nullify(LFunctionPtr)

      if ( VarR0D <= -huge(One) ) then
        Evaluate_0D = VarR0D
        exit
      end if

      Evaluate_0D = Evaluate_0D + VarR0D
      
    end do

    if ( .not. LogValueLoc ) then
      if ( Evaluate_0D > TVarR0D .and. Evaluate_0D < HVarR0D ) then
        Evaluate_0D = dexp(Evaluate_0D)
      elseif (Evaluate_0D < TVarR0D ) then
        Evaluate_0D= Zero
      else
        call Error%Raise( Line='Likelihood Value above machine precision where ln(likelihood) is : ' //                           &
                           ConvertToString(Value=Evaluate_0D) // '. Consider changing value of the scalar modifier of responses' )
      end if
    end if

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(LikelihoodProduct_Type), intent(out)                        ::    LHS
    class(LikelihoodFunction_Type), intent(in)                        ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (LikelihoodProduct_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Name = RHS%Name
          LHS%Label = RHS%Label
          LHS%NbLikelihoods = RHS%NbLikelihoods
          allocate(LHS%Likelihoods, source=RHS%Likelihoods, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Likelihoods', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(LikelihoodProduct_Type), intent(inout)                       ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )
  
    if ( allocated(This%Likelihoods) ) deallocate(This%Likelihoods, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Likelihoods', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
