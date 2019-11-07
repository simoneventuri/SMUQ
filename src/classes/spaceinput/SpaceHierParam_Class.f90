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

module SpaceHierParam_Class

use Parameters_Library
use Input_Library
use String_Library
use StringRoutines_Module
use ArrayRoutines_Module
use ArrayIORoutines_Module
use CommandRoutines_Module
use InputDet_Class                                                ,only:    InputDet_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use HierDistProb_Class                                            ,only:    HierDistProb_Type
use HierDistProb_Factory_Class                                    ,only:    HierDistProb_Factory
use HierDistProb_Vec_Class                                        ,only:    HierDistProb_Vec_Type
use SpaceParam_Class                                              ,only:    SpaceParam_Type
use SMUQFile_Class                                                ,only:    SMUQFile_Type
use DistProb_Class                                                ,only:    DistProb_Type
use DistProb_Vec_Class                                            ,only:    DistProb_Vec_Type

implicit none

private

public                                                                ::    SpaceHierParam_Type

type                                                                  ::    SpaceHierParam_Type
  character(:), allocatable                                           ::    Name
  integer                                                             ::    NbDim
  logical                                                             ::    Correlated=.false.
  logical                                                             ::    Initialized=.false.
  logical                                                             ::    Constructed=.false.
  type(HierDistProb_Vec_Type), allocatable, dimension(:)              ::    HierDistProb
  type(String_Type), allocatable, dimension(:)                        ::    ParamName
  type(String_Type), allocatable, dimension(:)                        ::    Label
  real(rkp), dimension(:,:), allocatable                              ::    CorrMat
contains
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct                =>    ConstructInput
  procedure, private                                                  ::    ConstructInput
  procedure, public                                                   ::    GetInput             
  procedure, public                                                   ::    Generate
  generic, public                                                     ::    GetLabel                 =>   GetLabel0D,             &
                                                                                                          GetLabel1D
  generic, public                                                     ::    GetParamName             =>   GetParamName0D,        &
                                                                                                          GetParamName1D
  procedure, private                                                  ::    GetParamName0D
  procedure, private                                                  ::    GetParamName1D
  procedure, private                                                  ::    GetLabel0D
  procedure, private                                                  ::    GetLabel1D
  procedure, public                                                   ::    IsCorrelated
  procedure, public                                                   ::    GetNbDim
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer
end type

logical, parameter                                                    ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(SpaceHierParam_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Initialized = .true.
      This%Name = 'SpaceHierParam'
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(SpaceHierParam_Type), intent(inout)                         ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%HierDistProb) ) deallocate(This%HierDistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc )
    This%NbDim = 0

    if ( allocated(This%ParamName) ) deallocate(This%ParamName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamName', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%CorrMat) ) deallocate(This%CorrMat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )
    This%Correlated = .false.

    This%Initialized=.false.
    This%Constructed=.false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(SpaceHierParam_Type),intent(inout)                          ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='SetDefaults'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------     

  !!------------------------------------------------------------------------------------------------------------------------------         
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(SpaceHierParam_Type), intent(inout)                         ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug
    
    character(*), parameter                                           ::    ProcName='ConstructInput'
    logical                                                           ::    DebugLoc
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(HierDistProb_Type), allocatable                             ::    HierDistProb
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    real(rkp)                                                         ::    VarR0D
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_1
    real(rkp), allocatable, dimension(:)                              ::    VarR1D_2
    real(rkp), allocatable, dimension(:,:)                            ::    VarR2D
    integer                                                           ::    VarI0D
    character(:),  allocatable                                        ::    VarC0D
    logical                                                           ::    VarL0D
    integer                                                           ::    i, ii
    logical                                                           ::    Found
    character(:), allocatable                                         ::    SpaceType
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset
    if ( .not. This%Initialized ) call This%Initialize  

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    SectionName = 'parameters'
    call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    This%NbDim = InputSection%GetNumberOfSubSections()

    allocate(This%ParamName(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%ParamName', ProcName=ProcName, stat=StatLoc )

    allocate(This%HierDistProb(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc )

    allocate(This%Label(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      SubSectionName = SectionName // '>parameter' // ConvertToString(Value=i)

      ParameterName = 'name'
      call This%ParamName(i)%Set_Value(Value='<undefined>')
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%ParamName(i) = VarC0D

      ParameterName = 'label'
      call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, SectionName=SubSectionName, Mandatory=.true. )
      This%Label(i) = VarC0D

      SubSectionName = SubSectionName // '>distribution'
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SubSectionName, Mandatory=.true. )
      call HierDistProb_Factory%Construct( Object=HierDistProb, Input=InputSection, Prefix=PrefixLoc )
      nullify( InputSection )
      call This%HierDistProb(i)%Set( Object=HierDistProb )
      deallocate(HierDistProb, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='HierDistProb', ProcName=ProcName, stat=StatLoc )
    end do 

    i = 1
    do i = 1, This%NbDim-1
      ii = 1
      do ii = i+1 ,This%NbDim
        if ( This%Label(i)%GetValue() == This%Label(ii)%GetValue() ) call Error%Raise( Line='Duplicate labels : ' //              &
                                                                                      This%Label(i)%GetValue(), ProcName=ProcName)
      end do
    end do

    SectionName = 'correlation_matrix'
    if ( Input%HasSection( SubSectionName=SectionName ) ) then
      call Input%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
      call ImportArray( Input=InputSection, Array=This%CorrMat, Prefix=PrefixLoc )
      nullify( InputSection )
      This%Correlated = .not. IsDiagonal(Array=This%CorrMat)
    else
      This%CorrMat = EyeR(N=This%NbDim)
      This%Correlated = .false.
    end if

    if ( size(This%Corrmat,1) /= This%NbDim .or. size(This%CorrMat,2) /= This%NbDim ) call Error%Raise(                           &
                                                       Line='Improper sizes for the input correlation matrix', ProcName=ProcName ) 

    This%Constructed=.true.

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    type(InputSection_Type)                                           ::    GetInput

    class(SpaceHierParam_Type), intent(in)                            ::    This
    character(*), intent(in)                                          ::    MainSectionName
    character(*), optional, intent(in)                                ::    Prefix
    character(*), optional, intent(in)                                ::    Directory
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetInput'
    character(:), allocatable                                         ::    PrefixLoc
    character(:), allocatable                                         ::    DirectoryLoc
    character(:), allocatable                                         ::    DirectorySub
    logical                                                           ::    ExternalFlag=.false.
    type(InputSection_Type), pointer                                  ::    InputSection=>null()
    class(HierDistProb_Type), pointer                                 ::    HierDistProb=>null()
    character(:), allocatable                                         ::    FileName
    integer                                                           ::    i
    character(:), allocatable                                         ::    SectionName
    character(:), allocatable                                         ::    SubSectionName
    type(SMUQFile_Type)                                               ::    File

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

    if ( ExternalFlag ) call MakeDirectory( Path=PrefixLoc // DirectoryLoc, Options='-p' )

    call GetInput%SetName( SectionName = trim(adjustl(MainSectionName)) )

    SectionName = 'parameters'
    call GetInput%AddSection(SectionName=SectionName)
    i = 1
    do i = 1, This%NbDim
      SubSectionName = 'parameter' // ConvertToString(Value=i)
      call GetInput%AddSection(SectionName=SubSectionName, To_SubSection=SectionName)
      SubSectionName= SectionName // '>' // SubSectionName
      call GetInput%AddParameter( Name='name', Value=ConvertToString( Value=This%ParamName(i)%GetValue() ),                       &
                                                                                                      SectionName=SubSectionName )
      call GetInput%AddParameter( Name='label', Value=ConvertToString( Value=This%Label(i)%GetValue() ),                          &
                                                                                                      SectionName=SubSectionName )
      HierDistProb => This%HierDistProb(i)%GetPointer()
      if ( ExternalFlag ) DirectorySub = DirectoryLoc // '/distribution' // ConvertToString(i)
      call GetInput%AddSection( Section=HierDistProb_Factory%GetObjectInput( Object=HierDistProb, MainSectionName='distribution', &
                                                        Prefix=PrefixLoc, Directory=DirectorySub ), To_SubSection=SubSectionName )
      nullify(HierDistProb)
    end do

    SectionName='correlation_matrix'
    call GetInput%AddSection( SectionName=SectionName )
    call GetInput%FindTargetSection( TargetSection=InputSection, FromSubSection=SectionName, Mandatory=.true. )
    if ( ExternalFlag ) then
      FileName = DirectoryLoc // '/correlation_matrix.dat'
      call File%Construct( File=FileName, Prefix=PrefixLoc, Comment='#', Separator=' ' )
      call ExportArray( Input=InputSection, Array=This%CorrMat, File=File )
    else
      call ExportArray( Input=InputSection, Array=This%CorrMat )
    end if
    nullify(InputSection)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Generate( This, Input, SpaceParam, Debug )
  
    class(SpaceHierParam_Type), intent(in)                            ::    This
    type(SpaceParam_Type), intent(out)                                ::    SpaceParam
    type(InputDet_Type), intent(in)                                   ::    Input
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Generate'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i
    class(DistProb_Type), allocatable                                 ::    DistProb
    class(HierDistProb_Type), pointer                                 ::    HierDistProbPtr=>null()
    type(DistProb_Vec_Type), allocatable, dimension(:)                ::    DistProbVec

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(DistProbVec(This%NbDim), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='DistProbVec', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, This%NbDim
      HierDistProbPtr => This%HierDistProb(i)%GetPointer()
      call HierDistProbPtr%Generate( Input=Input, Distribution=DistProb )
      call DistProbVec(i)%Set( Object=DistProb )
      deallocate(DistProb, stat=StatLoc)
      if ( StatLoc /= 0 ) call Error%Deallocate( Name='DistProb', ProcName=ProcName, stat=StatLoc )
      nullify(HierDistProbPtr)
    end do

    call SpaceParam%Construct( Distributions=DistProbVec, CorrMat=This%CorrMat, Labels=This%Label, Names=This%ParamName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetParamName0D( This, Num, Debug )

    character(:), allocatable                                         ::    GetParamName0D
    class(SpaceHierParam_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Num
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetParamName0D'
    logical                                                           ::    DebugLoc
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetParamName0D = This%ParamName(Num)%GetValue()      

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetParamName1D( This, Debug )

    type(String_Type), allocatable, dimension(:)                      ::    GetParamName1D
    class(SpaceHierParam_Type), intent(in)                            ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetParamName1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetParamName1D, source=This%ParamName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetParamName1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel0D( This, Num, Debug )

    character(:), allocatable                                         ::    GetLabel0D
    class(SpaceHierParam_Type), intent(in)                            ::    This
    integer, intent(in)                                               ::    Num
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetLabel0D'
    logical                                                           ::    DebugLoc
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetLabel0D = This%Label(Num)%GetValue()      

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetLabel1D( This, Debug )

    type(String_Type), allocatable, dimension(:)                      ::    GetLabel1D
    class(SpaceHierParam_Type), intent(in)                            ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetLabel1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    allocate(GetLabel1D, source=This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='GetLabel1D', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetNbDim( This, Debug )

    integer                                                           ::    GetNbDim
    class(SpaceHierParam_Type), intent(in)                            ::    This
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='GetNbDim'
    logical                                                           ::    DebugLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetNbDim = This%NbDim

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsCorrelated( This, Debug )

    logical                                                           ::    IsCorrelated
    class(SpaceHierParam_Type), intent(in)                            ::    This
    logical, optional, intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='IsCorrelated'
    integer                                                           ::    StatLoc=0
    
    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    IsCorrelated = This%Correlated

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(SpaceHierParam_Type), intent(out)                           ::    LHS
    class(SpaceHierParam_Type), intent(in)                            ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type ( RHS )

      type is (SpaceHierParam_Type)
        call LHS%Reset()

        LHS%Name = RHS%Name
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed
        
        if ( RHS%Constructed ) then
          LHS%NbDim = RHS%NbDim
          LHS%Correlated = RHS%Correlated
          allocate(LHS%HierDistProb, source=RHS%HierDistProb, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%HierDistProb', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%ParamName, source=RHS%ParamName, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Paramname', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%CorrMat, source=RHS%CorrMat, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )
          allocate(LHS%Label, source=RHS%Label, stat=StatLoc)
          if ( StatLoc /= 0 ) call Error%Allocate( Name='LHS%Label', ProcName=ProcName, stat=StatLoc )
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(SpaceHierParam_Type),intent(inout)                               ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( allocated(This%CorrMat) ) deallocate(This%CorrMat, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%CorrMat', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%ParamName) ) deallocate(This%ParamName, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%ParamName', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%Label) ) deallocate(This%Label, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%Label', ProcName=ProcName, stat=StatLoc )

    if ( allocated(This%HierDistProb) ) deallocate(This%HierDistProb, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='This%HierDistProb', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module
