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

module SASobol_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use UQMethod_Class                                                ,only:    UQMethod_Type
use InputDet_Class                                                ,only:    InputDet_Type
use Output_Class                                                  ,only:    Output_Type
use SpaceSampler_Class                                            ,only:    SpaceSampler_Type
use SampleSpace_Class                                             ,only:    SampleSpace_Type
use ModelInterface_Class                                          ,only:    ModelInterface_Type
use Response_Class                                                ,only:    Response_Type
use Restart_Class                                                 ,only:    RestartUtility
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use Model_Class                                                   ,only:    Model_Type
use List2D_Class                                                  ,only:    List2D_Type
use Histogram1D_Class                                             ,only:    Histogram1D_Type, BinValues

implicit none

private

public                                                                ::    SASobol_Type

type, extends(UQMethod_Type)                                          ::    SASobol_Type
  integer                                                             ::    CheckpointFreq
  logical                                                             ::    Silent
  type(SpaceSampler_Type)                                             ::    Sampler
  logical                                                             ::    SamplesObtained
  logical                                                             ::    SamplesRan
  logical                                                             ::    SamplesAnalyzed
  integer                                                             ::    Step
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput
  procedure, public                                                   ::    Run
  procedure, public                                                   ::    WriteOutput
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This )

    class(SASobol_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Initialize'

    if ( .not. This%Initialized ) then
      This%Name = 'SASobol'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This )

    class(SASobol_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc = 0

    This%Initialized=.false.
    This%Constructed=.false.

    This%Step = 0

    call This%Sampler%Reset()

    call This%SetDefaults()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This )

    class(SASobol_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='SetDefaults'

    This%CheckpointFreq = -1
    This%Silent = .false.
    This%SamplesObtained = .false.
    This%SamplesRan = .false.
    This%SamplesAnalyzed = .false.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, SectionChain, Prefix )

    class(SASobol_Type), intent(inout)                                ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), intent(in)                                          ::    SectionChain
    character(*), optional, intent(in)                                ::    Prefix

    character(*), parameter                                           ::    ProcName='ConstructInput'
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    VarI0D
    logical                                                           ::    VarL0D
    character(:), allocatable                                         ::    ParameterName
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    integer                                                           ::    i
    integer                                                           ::    ii
    logical                                                           ::    Found
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    logical, allocatable, dimension(:)                                ::    VarL1D
    character(:), allocatable                                         ::    Label1
    character(:), allocatable                                         ::    Label2

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%SectionChain = SectionChain

    ParameterName= 'silent'
    call Input%GetValue( Value=VarL0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%Silent=VarL0D

    ParameterName = "checkpoint_frequency"
    call Input%GetValue( Value=VarI0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%CheckpointFreq = VarI0D

    SectionName = 'sampler'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    call This%Sampler%Construct( Input=InputSection, Prefix=PrefixLoc )
    nullify( InputSection )

    This%Constructed = .true.

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory )

    type(InputSection_Type)                                           ::    GetInput

    class(SASobol_Type), intent(inout)                             ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory

    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    SectionName
    integer                                                           ::    i
    integer, dimension(:,:), pointer                                  ::    VarI2D=>null()
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    type(SMUQFile_Type)                                               ::    File
    character(:), allocatable                                         ::    FileName

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    DirectoryLoc = ''
    PrefixLoc = ''
    if ( present(Directory) ) DirectoryLoc = Directory
    if ( present(Prefix) ) PrefixLoc = Prefix
    DirectorySub = DirectoryLoc

    if ( len_trim(DirectoryLoc) /= 0 ) ExternalFlag = .true.

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    call GetInput%AddParameter( Name='silent', Value=ConvertToString(Value=This%Silent ) )
    call GetInput%AddParameter( Name='checkpoint_frequency', Value=ConvertToString(Value=This%CheckpointFreq ) )

    if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/sampler'
    SectionName = 'sampler'
    call GetInput%AddSection( Section=This%Sampler%GetInput( MainSectionName=SectionName,                                         &
                                                                                        Prefix=PrefixLoc,Directory=DirectorySub) )

    call GetInput%AddParameter( Name='samples_obtained', Value=ConvertToString(Value=This%SamplesObtained ) )
    call GetInput%AddParameter( Name='samples_ran', Value=ConvertToString(Value=This%SamplesRan ) )
    call GetInput%AddParameter( Name='samples_analyzed', Value=ConvertToString(Value=This%SamplesRan ) )
    call GetInput%AddParameter( Name='step', Value=ConvertToString(Value=This%Step ) )

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Run( This, SampleSpace, Responses, Model, OutputDirectory )

    class(SASobol_Type), intent(inout)                             ::    This
    class(SampleSpace_Type), intent(in)                               ::    SampleSpace
    type(Response_Type), dimension(:), intent(in)                     ::    Responses
    class(Model_Type), intent(inout)                                  ::    Model
    character(*), optional, intent(in)                                ::    OutputDirectory

    character(*), parameter                                           ::    ProcName='Run'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    VarC0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D
    integer, allocatable, dimension(:,:)                              ::    VarI2D
    integer, dimension(:,:), pointer                                  ::    VarI2DPtr=>null()
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    real(rkp), dimension(:,:), pointer                                ::    VarR2DPtr=>null()
    integer                                                           ::    NbDim
    logical                                                           ::    StepExceededFlag=.false.
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer                                                           ::    iv
    integer                                                           ::    iStart
    integer                                                           ::    iEnd
    logical                                                           ::    SilentLoc
    character(:), allocatable                                         ::    Line
    type(Output_Type), allocatable, dimension(:)                      ::    Outputs
    integer                                                           ::    NbOutputs
    type(ModelInterface_Type)                                         ::    ModelInterface
    type(InputDet_Type)                                               ::    Input

    call ModelInterface%Construct( Model=Model, Responses=Responses )

    NbDim = SampleSpace%GetNbDim()
    SilentLoc = This%Silent

    if ( .not. allocated(This%BinCounts) ) then
      allocate(This%BinCounts(This%NbHistograms), stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Allocate( Name='This%BinCounts', ProcName=ProcName, stat=StatLoc )
  
      i = 1
      do i = 1, This%NbHistograms
        allocate(VarI2D(This%Histograms(i)%GetNbBins(),ModelInterface%GetResponseNbNodes(Label=This%Labels(i)%GetValue())),       &
                                                                                                                     stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='VarI2D', ProcName=ProcName, stat=StatLoc )
        VarI2D = 0
        call This%BinCounts(i)%Set(Values=VarI2D)
        deallocate(VarI2D, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarI2D', ProcName=ProcName, stat=StatLoc )
      end do
    else
      i = 1
      do i = 1, This%NbHistograms
        call This%BinCounts(i)%GetPointer(Values=VarI2DPtr)
        if ( size(VarI2DPtr,1) /= This%Histograms(i)%GetNbBins() ) call Error%Raise( 'Mismatch in response number of bins ' //    &
                                                ' and size of bin count array: ' // This%Labels(i)%GetValue(), ProcName=ProcName )
        if ( size(VarI2DPtr,2) /= ModelInterface%GetResponseNbNodes(Label=This%Labels(i)%GetValue()) )                            &
            call Error%Raise( 'Mismatch in response number of nodes and number of histograms for response: ' //                   &
                                                                                    This%Labels(i)%GetValue(), ProcName=ProcName )
        nullify(VarI2DPtr)
      end do
    end if

    StepExceededFlag = .false.

    do

      !***************************************************************************************************************************
      ! Obtaining samples
      if ( .not. This%SamplesObtained ) then

        if ( .not. SilentLoc ) then
          if ( This%Step /= 0 ) then
            Line = 'Performing enrichment'
          else
            Line = 'Initial population of the linear system'
          end if
          write(*,'(A)') '' 
          write(*,'(A)') Line
          write(*,'(A)') '' 
        end if

        if ( This%Step == 0 ) then
          This%ParamSample = This%Sampler%Draw(SampleSpace=SampleSpace)
        else
          call This%Sampler%Enrich( SampleSpace=SampleSpace, Samples=This%ParamRecord, EnrichmentSamples=This%ParamSample,        &
                                                                                                        Exceeded=StepExceededFlag)

          if ( StepExceededFlag ) exit
        end if
        allocate(This%ParamSampleRan(size(This%ParamSample,2)), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )
        This%ParamSampleRan = .false.
        iEnd = size(This%ParamSample,2)
        This%SamplesObtained = .true.
      else
        iEnd = size(This%ParamSample,2)
      end if
 
      !***************************************************************************************************************************
      ! Running samples
      if ( .not. This%SamplesRan ) then
        i = This%ParamSampleStep
        do
          i = i + 1
          if ( i > iEnd ) exit
          This%Step = This%Step + 1

          if ( .not. SilentLoc ) then
            Line = 'Model run #' // ConvertToString(Value=This%Step)
            write(*,'(A)') Line
          end if

          This%ParamSampleStep = i
          call Input%Construct( Input=This%ParamSample(:,This%ParamSampleStep), Labels=SampleSpace%GetLabel() )
          call ModelInterface%Run( Input=Input, Output=Outputs, Stat=StatLoc )

          if ( StatLoc /= 0 ) then
            if ( .not. SilentLoc ) then
              Line = 'Model run #' // ConvertToString(Value=This%Step) // ' -- Failed'
              write(*,'(A)') Line
            end if
            StatLoc = 0
            if ( allocated(Outputs) ) deallocate(Outputs, stat=StatLoc)
            if ( StatLoc /= 0 ) call Error%Deallocate( Name='Outputs', ProcName=ProcName, stat=StatLoc )
            This%Step = This%Step - 1
          else
            ! Binning outputs
            This%ParamSampleRan(i) = .true.
            NbOutputs = size(Outputs,1)

            ii = 1
            do ii = 1, This%NbHistograms
              VarC0D = This%Labels(ii)%GetValue()

              iii = 1
              iv = 0
              do iii = 1, NbOutputs
                if ( Outputs(iii)%GetLabel() /= VarC0D ) cycle
                iv = iii
                exit
              end do
              if ( iv == 0 ) call Error%Raise( 'Did not find required output : ' // VarC0D, ProcName=ProcName )  

              call This%BinCounts(ii)%GetPointer(Values=VarI2DPtr)
              VarR2DPtr => Outputs(iv)%GetValuesPointer()
              allocate(VarR1D(Outputs(iv)%GetNbDegen()), stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )
              VarR1D = Zero

              iii = 1
              do iii = 1, Outputs(iv)%GetNbNodes()
                VarR1D = VarR2DPtr(iii,:)
                call BinValues( Values=VarR1D, BinEdges=This%Histograms(ii)%GetBinEdgesPointer(), BinCounts=VarI2DPtr(:,iii) )
              end do  

              deallocate(VarR1D, stat=StatLoc)
              if ( StatLoc /= 0 ) call Error%Deallocate( Name='VarR1D', ProcName=ProcName, stat=StatLoc )

              nullify(VarR2DPtr)
              nullify(VarI2DPtr)

            end do

          end if

          if ( This%CheckpointFreq > 0 .and. mod(i, abs(This%CheckpointFreq)) == 0 ) then
            call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),       &
                            Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )
          end if
    
        end do

        This%SamplesRan = .true.

        allocate(VarR2D(NbDim,This%Step), stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Allocate( Name='VarR2D', ProcName=ProcName, stat=StatLoc )
        if ( allocated(This%ParamRecord) ) then
          iStart = size(This%ParamRecord,2)
          VarR2D(:,1:size(This%ParamRecord,2)) = This%ParamRecord
          deallocate(This%ParamRecord, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )
        else
          iStart = 0
        end if

        i = iStart+1
        ii = 0
        do i = iStart+1, size(VarR2D,2)
          ii = ii + 1
          if ( This%ParamSampleRan(ii) ) then
            VarR2D(:,i) = This%ParamSample(:,ii)
          end if
        end do
        call move_alloc(VarR2D, This%ParamRecord)

        deallocate(This%ParamSample, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )
        deallocate(This%ParamSampleRan, stat=StatLoc)
        if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

        This%ParamSampleStep = 0

      end if

      This%SamplesObtained = .false.
      This%SamplesRan = .false.

    end do

    This%SamplesObtained = .true.
    This%SamplesRan = .true.

    if ( StepExceededFlag ) then
      Line = 'Maximum sampling step exceeded'
      if ( This%Step == 0 ) call Error%Raise( Line='Maximum sampling step exceeded prior to any samples being taken',             &
                                                                                                               ProcName=ProcName )
      write(*,'(A)') ''  
      write(*,'(A)') Line
    end if

    call RestartUtility%Update( InputSection=This%GetInput(MainSectionName='temp', Prefix=RestartUtility%GetPrefix(),             &
                          Directory=RestartUtility%GetDirectory(SectionChain=This%SectionChain)), SectionChain=This%SectionChain )

    if ( present(OutputDirectory) ) call This%WriteOutput( Directory=OutputDirectory, Responses=Responses )

    deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )

    deallocate(This%BinCounts, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%BinCounts', ProcName=ProcName, stat=StatLoc )

    This%Step = 0

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine WriteOutput( This, Directory, Responses )

    class(SASobol_Type), intent(inout)                             ::    This
    character(*), intent(in)                                          ::    Directory
    type(Response_Type), dimension(:), intent(in)                     ::    Responses

    character(*), parameter                                           ::    ProcName='WriteOutput'
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    Line
    character(:), allocatable                                         ::    FileName
    character(:), allocatable                                         ::    PrefixLoc
    logical                                                           ::    SilentLoc
    character(:), allocatable                                         ::    DirectoryLoc
    type(SMUQFile_Type)                                               ::    File
    integer                                                           ::    i
    integer                                                           ::    ii
    integer                                                           ::    iii
    integer, dimension(:,:), pointer                                  ::    VarI2DPtr=>null()


    if ( len_trim(Directory) /= 0 ) then

      call MakeDirectory( Path=Directory, Options='-p' )

      SilentLoc = This%Silent

      if ( .not. SilentLoc ) then
        Line = 'Writing solver data to the output folder'
        write(*,'(A)') ''
        write(*,'(A)') Line
      end if

      PrefixLoc = Directory

      FileName = '/sampled_parameters.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Array=This%ParamRecord, File=File )

      i = 1
      do i = 1, This%NbHistograms

        call MakeDirectory( Path=Directory // '/' // This%Labels(i)%GetValue(), Options='-p' )

        call This%BinCounts(i)%GetPointer(Values=VarI2DPtr)

        FileName = '/' // This%Labels(i)%GetValue() // '/bin_counts.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Array=VarI2DPtr, File=File, RowMajor=.true. )

        FileName = '/' //This%Labels(i)%GetValue() // '/bin_edges.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Array=This%Histograms(i)%GetBinEdgesPointer(), File=File )

        ii = 1
        iii = 0
        do ii = 1, size(Responses,1)
          if ( Responses(ii)%GetLabel() /= This%Labels(i)%GetValue() ) cycle
          iii = ii
          exit
        end do

        if ( iii == 0 ) call Error%Raise( 'Did not find required response: ' // This%Labels(i)%GetValue(), ProcName=ProcName )
        FileName = '/' // This%Labels(i)%GetValue() // '/coordinates.dat'
        call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
        call ExportArray( Array=Responses(iii)%GetCoordinatesPointer(), File=File, RowMajor=.true. )

        nullify(VarI2DPtr)

      end do

    end if

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Copy( LHS, RHS )

    class(SASobol_Type), intent(out)                               ::    LHS
    class(UQMethod_Type), intent(in)                                  ::    RHS

    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0

    select type (RHS)
  
      type is (SASobol_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%Silent = RHS%Silent
          LHS%NbHistograms = RHS%NbHistograms
          LHS%Sampler = RHS%Sampler
          allocate(LHS%Histograms, source=RHS%Histograms, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Histograms', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Labels, source=RHS%Labels, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Labels = RHS%Labels', ProcName=ProcName, stat=StatLoc )
        end if
      
      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  impure elemental subroutine Finalizer( This )

    type(SASobol_Type), intent(inout)                              ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    integer                                                           ::    StatLoc=0

    if ( allocated(This%ParamRecord) ) deallocate(This%ParamRecord, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamRecord', ProcName=ProcName, stat=StatLoc )
    
    if ( allocated(This%ParamSample) ) deallocate(This%ParamSample, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSample', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamSampleRan) ) deallocate(This%ParamSampleRan, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamSampleRan', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Labels) ) deallocate(This%Labels, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Labels', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Histograms) ) deallocate(This%Histograms, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Histograms', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%BinCounts) ) deallocate(This%BinCounts, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%BinCounts', ProcName=ProcName, stat=StatLoc )

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
