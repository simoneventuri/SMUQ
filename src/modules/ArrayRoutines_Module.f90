module ArrayRoutines_Module

use String_Library
use Input_Library
use Parameters_Library
use StringRoutines_Module
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use SMUQFile_Class                                                ,only:    SMUQFile_Type

implicit none

private

public                                                                ::    Eye
public                                                                ::    EyeI
public                                                                ::    EyeR
public                                                                ::    IsDiagonal
public                                                                ::    LinSpaceVec

logical, parameter                                                    ::    DebugGlobal = .false.

interface IsDiagonal
  module procedure                                                    ::    IsDiagonal_Real8
end interface

interface LinSpaceVec
  module procedure                                                    ::    LinSpaceVecInput_R1D
  module procedure                                                    ::    LinSpaceVec_R1D
end interface

interface Eye
  module procedure                                                    ::    Eye_R2D
  module procedure                                                    ::    Eye_I2D
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function EyeI( N, Debug )

    integer, dimension(:,:), allocatable                              ::    EyeI
    integer, intent(in)                                               ::    N
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='EyeI'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    IOLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate( EyeI(N,N), stat=IOLoc )
    if ( IOLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='EyeI', stat=IOLoc)

    EyeI = 0
    do i = 1,N 
      EyeI(i,i) = 1
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function EyeR( N, Debug )

    real(rkp), dimension(:,:), allocatable                            ::    EyeR
    integer, intent(in)                                               ::    N
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='EyeR'
    logical                                                           ::    DebugLoc
    integer                                                           ::    i
    integer                                                           ::    IOLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate( EyeR(N,N), stat=IOLoc )
    if ( IOLoc /= 0 ) call Error%Allocate( ProcName=ProcName, Name='EyeR', stat=IOLoc)

    EyeR = Zero
    do i = 1,N 
      EyeR(i,i) = One
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eye_R2D( Array, Debug )

    real(rkp), dimension(:,:)                                         ::    Array
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Eye_R2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    N

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    N = size(Array,1)

    if ( size(Array,2) /= N ) call Error%Raise( Line='Not a square array', ProcName=ProcName )

    Array = Zero
    do i = 1, N 
      Array(i,i) = One
    end do

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Eye_I2D( Array, Debug )

    integer, dimension(:,:)                                           ::    Array
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Eye_I2D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc
    integer                                                           ::    i
    integer                                                           ::    N

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    N = size(Array,1)

    if ( size(Array,2) /= N ) call Error%Raise( Line='Not a square array', ProcName=ProcName )

    Array = 0
    do i = 1, N 
      Array(i,i) = 1
    end do

    if (DebugLoc) call Logger%Exiting

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function IsDiagonal_Real8( Array, Debug )

    logical                                                           ::    IsDiagonal_Real8

    real(rkp), dimension(:,:), intent(in)                             ::    Array
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='IsDiagonal_Real8'
    logical                                                           ::    DebugLoc
    integer                                                           ::    M
    integer                                                           ::    N
    integer                                                           ::    i, ii

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    M = size(Array,1)
    N = size(Array,2)

    IsDiagonal_Real8 = .true.

    if ( M /= N ) then
      IsDiagonal_Real8 = .false.
    else if ( M == 1 ) then
      IsDiagonal_Real8 = .true.
    else
      do i = 2, M
        do ii = 1, i-1
          if ( Array(i,ii) > precision(Array) .or. Array(ii,i) > precision(Array) ) then
            IsDiagonal_Real8 = .false.
            exit
          end if
        end do
       if ( .not. IsDiagonal_Real8 ) exit
      end do     
    end if
      
    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LinSpaceVecInput_R1D( Input, Debug )

    real(rkp), allocatable, dimension(:)                              ::    LinSpaceVecInput_R1D

    class(InputSection_Type), intent(inout)                           ::    Input
    logical, optional, intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='LinSpaceVecInput_R1D'
    logical                                                           ::    DebugLoc
    real(rkp), dimension(:,:), allocatable                            ::    InterExtremes
    integer, dimension(:), allocatable                                ::    InterNbNodes
    character(20), dimension(:), allocatable                          ::    InterGenFun
    integer                                                           ::    NbInter
    integer                                                           ::    i
    character(:), allocatable                                         ::    VarC0D
    real(rkp)                                                         ::    VarR0D
    integer                                                           ::    VarI0D
    logical                                                           ::    Found
    character(:), allocatable                                         ::    SubSectionName
    character(:), allocatable                                         ::    ParameterName
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    NbInter = Input%GetNumberOfSubSections()

    allocate( InterExtremes(NbInter,2), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InterExtremes', ProcName=ProcName, stat=StatLoc )
    allocate( InterNbNodes(NbInter), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InterNbNodes', ProcName=ProcName, stat=StatLoc )
    allocate( InterGenFun(NbInter), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( Name='InterGenFun', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, NbInter
      SubSectionName = 'interval' // ConvertToString(Value=i)

      ParameterName = 'min'
      call Input%GetValue( ParameterName=ParameterName, Value=VarR0D, SectionName=SubSectionName, Mandatory=.true. )
      InterExtremes(i,1) = VarR0D

      ParameterName = 'max'
      call Input%GetValue( ParameterName=ParameterName, Value=VarR0D, SectionName=SubSectionName, Mandatory=.true. )
      InterExtremes(i,2) = VarR0D

      ParameterName = 'nb_nodes'
      call Input%GetValue( ParameterName=ParameterName, Value=VarI0D, SectionName=SubSectionName, Mandatory=.true. )
      InterNbNodes(i) = VarI0D

      ParameterName = 'spacing'
      InterGenFun(i) = 'linear'
      call Input%GetValue( ParameterName=ParameterName, Value=VarC0D, SectionName=SubSectionName, Mandatory=.false., Found=Found )      
      if ( Found ) InterGenFun(i) = VarC0D
    end do

    LinSpaceVecInput_R1D = LinSpaceVec( NbInter, InterExtremes, InterNbNodes, InterGenFun )

    deallocate( InterExtremes, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InterExtremes', ProcName=ProcName, stat=StatLoc )
    deallocate( InterNbNodes, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InterNbNodes', ProcName=ProcName, stat=StatLoc )
    deallocate( InterGenFun, stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='InterGenFun', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function LinSpaceVec_R1D( NbInter, InterExtremes, InterNbNodes, InterGenFun, Debug )

    use ComputingRoutines_Module

    real(rkp), dimension(:), allocatable                              ::    LinSpaceVec_R1D

    integer, intent(in)                                               ::    NbInter
    real(rkp), dimension(:,:), intent(in)                             ::    InterExtremes
    integer, dimension(:), intent(in)                                 ::    InterNbNodes
    character(*), dimension(:), optional, intent(in)                  ::    InterGenFun
    logical, optional ,intent(in)                                     ::    Debug


    character(*), parameter                                           ::    ProcName='LinSpaceVec_R1D'
    logical                                                           ::    DebugLoc
    character(:), allocatable                                         ::    VarC0D
    integer                                                           ::    NbNodes
    integer                                                           ::    i
    integer                                                           ::    i_start
    integer                                                           ::    i_end
    integer                                                           ::    ii
    integer                                                           ::    iim1
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if ( DebugLoc ) call Logger%Entering( ProcName )

    if ( NbInter <= 0 ) call Error%Raise( 'Specified number of intervals is invalid ( 0 or lower)' )
    if ( NbInter /= size(InterExtremes,1) ) call Error%Raise( 'Number of intervals does not match size of InterExtremes' )
    if ( NbInter /= size(InterNbNodes,1) ) call Error%Raise( 'Number of intervals does not match size of InternbNodes' )
    if ( .not. any(InterNbNodes > 0) ) call Error%Raise( Line='Zero nodes specified for all intervals', ProcName=ProcName )
    if ( present(InterGenFun) ) then
      if ( NbInter /= size(InterGenFun,1) ) call Error%Raise( 'Number of intervals does not match size of InterGenFun' )
    end if

    NbNodes = sum(InterNbNodes)

    allocate( LinSpaceVec_R1D(NbNodes), stat=StatLoc )
    if ( StatLoc /= 0 ) call Error%Allocate( name='LinSpaceVec_R1D', stat=StatLoc )
    LinSpaceVec_R1D = Zero

    ii = 0
    iim1 = 0
    i = 1
    do i = 1, NbInter
      if( InterNbNodes(i) <= 0 ) cycle

      iim1 = ii
      ii = ii + InterNbNodes(i)

      if ( .not. present(InterGenFun) ) then
        VarC0D = 'linear'
      else
        VarC0D = trim(adjustl(InterGenFun(i)))
      end if

      select case ( VarC0D )
        case ( 'linear' )
          LinSpaceVec_R1D(iim1+1:ii) = LinSpace( InterMin=InterExtremes(i,1), InterMax=InterExtremes(i,2),                        &
                                                                                                         NbNodes=InterNbNodes(i) )  
        case ( 'log10' )
          if ( InterExtremes(i,1) <= 0 ) call Error%Raise( "Minimum interval bound below zero for the log10 spacing" )
          if ( InterExtremes(i,2) <= 0 ) call Error%Raise( "Maximum interval bound below zero for the log10 spacing" )

          LinSpaceVec_R1D(iim1+1:ii) = Log10Space( InterMin=dlog10(InterExtremes(i,1)), InterMax=dlog10(InterExtremes(i,2)),      &
                                                                                                         NbNodes=InterNbNodes(i) )
        case default
          call Error%Raise( Line='Unrecognized spacing scheme: ' // VarC0D, ProcName=ProcName )
      end select

    end do

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------
  

end module
