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

module GENPI_Class

use Input_Library
use Parameters_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use Model_Class                                                   ,only:    Model_Type
use ModelExtTemplate_Class                                        ,only:    ModelExtTemplate_Type
use GENPIModel_Class                                              ,only:    GENPIModel_Type
use ParameterWriter_Class                                         ,only:    ParameterWriter_Type
use OutputReader_Class                                            ,only:    OutputReader_Type
use Output_Class                                                  ,only:    Output_Type
use Input_Class                                                   ,only:    Input_Type
use InputDet_Class                                                ,only:    InputDet_Type
use InputStoch_Class                                              ,only:    InputStoch_Type

implicit none

private

public                                                                ::    GENPI_Type

type, extends(ModelExtTemplate_Type)                                  ::    GENPI_Type
  type(GENPIModel_Type)                                               ::    GENPIModel
  type(ParameterWriter_Type)                                          ::    ParameterWriter
  type(OutputReader_Type)                                             ::    OutputReader
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    RunCase1
  procedure, public                                                   ::    Copy
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(GENPI_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'GENPIModel'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(GENPI_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(GENPI_Type), intent(inout)                                  ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix )

    use String_Library

    class(GENPI_Type), intent(inout)                                  ::    This
    class(InputSection_Type), intent(in)                              ::    Input
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    character(:), allocatable                                         ::    SectionName

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'model'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%GENPIModel%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'parameter_writer'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%ParameterWriter%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    SectionName = 'output_reader'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%OutputReader%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase( This, CaseDir, Prefix )

    use String_Library

    class(GENPI_Type), intent(inout)                                  ::    This
    character(*), intent(in)                                          ::    CaseDir
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructCase'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FileName
    type(InputReader_Type)                                            ::    Input

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    FileName = PrefixLoc // CaseDir // '/input/input.dat'
    call Input%Read( FileName=FileName )

    call This%Construct( Input=Input, Prefix=PrefixLoc // CaseDir )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    use String_Library

    type(InputSection_Type)                                           ::    GetInput

    class(GENPI_Type), intent(in)                                     ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )
    call GetInput%AddSection( Section=This%GENPIModel%GetInput(MainSectionName='model', Prefix=PrefixLoc,                         &
                                                                                                         Directory=DirectorySub) )
    call GetInput%AddSection( Section=This%ParameterWriter%GetInput(MainSectionName='parameter_writer', Prefix=PrefixLoc,         &
                                                                                                         Directory=DirectorySub) )
    call GetInput%AddSection( Section=This%OutputReader%GetInput(MainSectionName='output_reader', Prefix=PrefixLoc,                &
                                                                                                         Directory=DirectorySub) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine RunCase1( This, Input, Output, Stat )

    class(GENPI_Type), intent(inout)                                  ::    This
    class(Input_Type), intent(in)                                     ::    Input
    type(Output_Type), dimension(:), allocatable, intent(inout)       ::    Output
    integer, optional, intent(out)                                    ::    Stat

    character(*), parameter                                           ::    ProcName='RunCase1'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    StatRun=0
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), dimension(:,:), pointer                                ::    OrdinatePointer=>null()
    real(rkp), dimension(:,:), pointer                                ::    OrdinateLocPointer=>null()
    integer                                                           ::    NbDegen
    integer                                                           ::    NbDegenLoc
    integer                                                           ::    i, ii
    type(InputDet_Type)                                               ::    InputLoc
    type(Output_Type), allocatable, dimension(:)                      ::    OutputLoc
    integer                                                           ::    NbOutputs

    NbOutputs = This%OutputReader%GetNbOutputs()

    if ( allocated(Output) ) then
      if ( size(Output,1) /= NbOutputs ) then
        deallocate(Output, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
      end if
    end if
    if ( .not. allocated(Output) ) then
      allocate(Output(NbOutputs), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    end if

    select type (Input)

      type is (InputDet_Type)
        call This%ParameterWriter%WriteInput( Input=Input )

        call This%GENPIModel%Run( Stat=StatRun )

        if ( present(Stat) ) Stat = StatRun

        if ( StatRun == 0 ) call This%OutputReader%ReadOutput( Output=Output )

      type is (InputStoch_Type)
        allocate(OutputLoc(NbOutputs), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

        NbDegen = Input%GetNbDegen()

        i = 1
        do i = 1, NbDegen
          InputLoc = Input%GetDetInput(Num=i)
          call This%ParameterWriter%WriteInput( Input=InputLoc )
          call This%GENPIModel%Run( Stat=StatRun )
          if ( present(Stat) ) Stat = StatRun

          if ( StatRun /= 0 ) exit

          call This%OutputReader%ReadOutput( Output=OutputLoc )

          if ( i == 1 ) then
            ii = 1
            do ii = 1, size(OutputLoc,1)
              allocate(VarR2D(OutputLoc(ii)%GetNbNodes(),NbDegen*OutputLoc(ii)%GetNbDegen()), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
              VarR2D = Zero
              call Output(ii)%Construct( Values=VarR2D, Label=OutputLoc(ii)%GetLabel() )
              deallocate(VarR2D, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
            end do
          end if

          ii = 1
          do ii = 1, size(OutputLoc,1)
            OrdinatePointer => Output(ii)%GetValuesPointer()
            OrdinateLocPointer => OutputLoc(ii)%GetValuesPointer()
            NbDegenLoc = OutputLoc(ii)%GetNbDegen()
            OrdinatePointer(:,(i-1)*NbDegenLoc+1:i*NbDegenLoc) = OrdinateLocPointer(:,:)
            nullify(OrdinatePointer)
            nullify(OrdinateLocPointer)
          end do
        end do

        deallocate(OutputLoc, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='OutputLoc', ProcName=ProcName, stat=StatLoc )

      class default
        call Error%Raise( Line='Update input class definitions', ProcName=ProcName )

    end select


    if ( present(Stat) ) Stat = StatRun

    if ( StatRun /= 0 .and. allocated(Output) ) then
      deallocate(Output, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(GENPI_Type), intent(out)                                    ::    LHS
    class(Model_Type), intent(in)                                     ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (GENPI_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%GENPIModel = RHS%GENPIModel
          LHS%ParameterWriter = RHS%ParameterWriter
          LHS%OutputReader = RHS%OutputReader
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
