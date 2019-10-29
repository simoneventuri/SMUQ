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

module DistUnif_Class

use Input_Library
use Parameters_Library
use ComputingRoutines_Module
use DistProb_Class                                                ,only:    DistProb_Type
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error
use InputDet_Class                                                ,only:    InputDet_Type

implicit none

private

public                                                                ::    DistUnif_Type

type, extends(DistProb_Type)                                          ::    DistUnif_Type
  character(:), allocatable                                           ::    ADependency
  character(:), allocatable                                           ::    BDependency
contains
  private
  procedure, public                                                   ::    Initialize
  procedure, public                                                   ::    Reset
  procedure, public                                                   ::    SetDefaults
  generic, public                                                     ::    Construct               =>    ConstructCase1
  procedure, private                                                  ::    ConstructInput
  procedure, private                                                  ::    ConstructCase1
  procedure, private                                                  ::    HierConstructCase1
  procedure, public                                                   ::    GetInput
  procedure, private                                                  ::    PDF_R0D
  procedure, nopass, public                                           ::    ComputePDF
  procedure, public                                                   ::    CDF
  procedure, nopass, public                                           ::    ComputeCDF
  procedure, public                                                   ::    InvCDF
  procedure, nopass, public                                           ::    ComputeInvCDF
  procedure, public                                                   ::    GetMean
  procedure, public                                                   ::    GetVariance
  procedure, private                                                  ::    ComputeMoment1
  procedure, private                                                  ::    ComputeMoment2
  procedure, public                                                   ::    Copy
  final                                                               ::    Finalizer     
end type

logical   ,parameter                                                  ::    DebugGlobal = .false.

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Initialize( This, Debug )

    class(DistUnif_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Initialize'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Initialized ) then
      This%Name = 'uniform'
      This%Initialized = .true.
      call This%SetDefaults()
    end if

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Reset( This, Debug )

    class(DistUnif_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Reset'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%Initialized = .false.
    This%Constructed = .false.

    call This%Initialize()

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine SetDefaults( This, Debug )

    class(DistUnif_Type), intent(inout)                               ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='SetDefaults'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    This%A = Zero
    This%B = One
    This%TruncatedLeft=.true.
    This%TruncatedRight=.true.
    This%ADependency=''
    This%BDependency=''

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructInput( This, Input, Prefix, Debug )

    class(DistUnif_Type), intent(inout)                               ::    This
    type(InputSection_Type), intent(in)                               ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructInput'
    character(:), allocatable                                         ::    ParameterName
    logical                                                           ::    Found
    real(rkp)                                                         ::    VarR0D
    character(:), allocatable                                         ::    VarC0D
    character(:), allocatable                                         ::    PrefixLoc
    integer                                                           ::    StatLoc=0
    logical                                                           ::    MandatoryLoc

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    PrefixLoc = ''
    if ( present(Prefix) ) PrefixLoc = Prefix

    This%TruncatedLeft=.true.
    This%TruncatedRight=.true.

    MandatoryLoc = .true.
    ParameterName = 'a_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%ADependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'a'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%A = VarR0D

    MandatoryLoc = .true.
    ParameterName = 'b_dependency'
    call Input%GetValue( Value=VarC0D, ParameterName=ParameterName, Mandatory=.false., Found=Found )
    if ( Found ) This%BDependency = VarC0D
    MandatoryLoc = .not. Found
    ParameterName = 'b'
    call Input%GetValue( VarR0D, ParameterName=ParameterName, Mandatory=MandatoryLoc, Found=Found )
    if ( Found ) This%B = VarR0D

    if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine ConstructCase1( This, A, B, Debug )
    
    class(DistUnif_Type), intent(inout)                               ::    This
    real(rkp), optional, intent(in)                                   ::    A
    real(rkp), optional, intent(in)                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug 

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ConstructCase1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( This%Constructed ) call This%Reset()
    if ( .not. This%Initialized ) call This%Initialize()

    This%TruncatedLeft=.true.
    This%TruncatedRight=.true.

    This%A = A

    This%B = B

    if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )

    This%Constructed = .true.

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine HierConstructCase1( This, Input, Prefix, Debug )

    class(DistUnif_Type), intent(inout)                               ::    This
    type(InputDet_Type), intent(in)                                   ::    Input
    character(*), optional, intent(in)                                ::    Prefix
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='HierConstructCase1'
    integer                                                           ::    StatLoc=0
    real(rkp)                                                         ::    VarR0D       

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='The object was never constructed', ProcName=ProcName )

    if ( len_trim(This%ADependency) /= 0 ) call Input%GetValue( Value=This%A, Label=This%ADependency )

    if ( len_trim(This%BDependency) /= 0 ) call Input%GetValue( Value=This%B, Label=This%BDependency )

    if ( This%B < This%A ) call Error%Raise( Line='Upper limit < lower limit', ProcName=ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetInput( This, MainSectionName, Prefix, Directory, Debug )

    use StringRoutines_Module

    type(InputSection_Type)                                           ::    GetInput

    class(DistUnif_Type), intent(in)                                  ::    This
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

    call GetInput%AddParameter( Name='a', Value=ConvertToString( Value=This%A ) )
    call GetInput%AddParameter( Name='b', Value=ConvertToString( Value=This%B ) )
    if ( len_trim(This%ADependency) /= 0 ) call GetInput%AddParameter( Name='a_dependency', Value=This%ADependency )
    if ( len_trim(This%BDependency) /= 0 ) call GetInput%AddParameter( Name='b_dependency', Value=This%BDependency )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function PDF_R0D( This, X, Debug )

    real(rkp)                                                         ::    PDF_R0D

    class(DistUnif_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='PDF_R0D'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    PDF_R0D = This%ComputePDF( X, This%A, This%B )
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

!  !!------------------------------------------------------------------------------------------------------------------------------
!  function PDF_R2D( This, NbNodes, Debug )

!    use ComputingRoutines_Module

!    real(rkp), dimension(:,:), allocatable                            ::    PDF_R2D

!    class(DistUnif_Type), intent(in)                                  ::    This
!    integer, intent(in)                                               ::    NbNodes
!    logical, optional ,intent(in)                                     ::    Debug

!    logical                                                           ::    DebugLoc
!    character(*), parameter                                           ::    ProcName='PDF_R2D'
!    real(rkp)                                                         ::    BinMass
!    integer                                                           ::    StatLoc=0
!    integer                                                           ::    i

!    DebugLoc = DebugGlobal
!    if ( present(Debug) ) DebugLoc = Debug
!    if (DebugLoc) call Logger%Entering( ProcName )

!    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

!    if ( NbNodes < 3 ) call Error%Raise( Line='Specified number of points lower than minimum of 3', ProcName=ProcName )

!    BinMass = One / real(NbNodes-1,rkp)

!    allocate(PDF_R2D(NbNodes,2), stat=StatLoc )
!    if ( StatLoc /= 0 ) call Error%Allocate( Name='PDF_R2D', ProcName=ProcName, stat=StatLoc )

!    PDF_R2D(:,1) = LinSpace( InterMin=This%A, InterMax=This%B, NbNodes=NbNodes )
!    PDF_R2D(:,2) = One / ( This%B - This%A )

!    if (DebugLoc) call Logger%Exiting()

!  end function
!  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputePDF( X, A, B, Debug )

    real(rkp)                                                         ::    ComputePDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    A
    real(rkp), intent(in)                                             ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputePDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( X < A .or. X > B) then
      ComputePDF = Zero
    else
      ComputePDF = One/(B-A)
    end if
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function CDF( This, X, Debug )

    real(rkp)                                                         ::    CDF

    class(DistUnif_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    X
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='CDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    CDF = ComputeCDF( X, This%A, This%B )
      
    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeCDF( X, A, B, Debug )

    real(rkp)                                                         ::    ComputeCDF

    real(rkp), intent(in)                                             ::    X
    real(rkp), intent(in)                                             ::    A
    real(rkp), intent(in)                                             ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeCDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( X < A ) then
      ComputeCDF = Zero
    elseif ( X > B ) then
      ComputeCDF = One
    else
      ComputeCDF = (X-A) / (B-A)
    end if      

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function InvCDF( This, P, Debug )

    real(rkp)                                                         ::    InvCDF

    class(DistUnif_Type), intent(in)                                  ::    This
    real(rkp), intent(in)                                             ::    P
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='InvCDF'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    InvCDF = ComputeInvCDF( P, This%A, This%B )

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeInvCDF( P, A, B, Debug )

    real(rkp)                                                         ::    ComputeInvCDF

    real(rkp), intent(in)                                             ::    P
    real(rkp), intent(in), optional                                   ::    A
    real(rkp), intent(in), optional                                   ::    B
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeInvCDF'
    real(rkp)                                                         ::    CDFLeft
    real(rkp)                                                         ::    CDFRight
    real(rkp)                                                         ::    VarR0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( P < Zero ) call Error%Raise( Line='P value below the minimum of 0 in the inverse CDF calculation', ProcName=ProcName )
    if ( P > One ) call Error%Raise( Line='P value above the maximum of 1 in the inverse CDF calculation', ProcName=ProcName )

    ComputeInvCDF = A + P*(B-A)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetMean( This, Debug )

    real(rkp)                                                         ::    GetMean

    class(DistUnif_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetMean'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetMean = (This%A+This%B)/Two

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function GetVariance( This, Debug )

    real(rkp)                                                         ::    GetVariance

    class(DistUnif_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetVariance'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    GetVariance = One/12.0*(This%B-This%A)**2

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment1( This, Debug )

    real(rkp)                                                         ::    ComputeMoment1

    class(DistUnif_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='ComputeMoment1'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    ComputeMoment1 = (This%A+This%B)/Two

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function ComputeMoment2( This, Debug )

    real(rkp)                                                         ::    ComputeMoment2

    class(DistUnif_Type), intent(in)                                  ::    This
    logical, optional ,intent(in)                                     ::    Debug

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='GetVariance'

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    if ( .not. This%Constructed ) call Error%Raise( Line='Object was never constructed', ProcName=ProcName )

    ComputeMoment2 = One/Three*(This%A**2+This%A*This%B+This%B**2)

    if (DebugLoc) call Logger%Exiting()

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Copy( LHS, RHS )

    class(DistUnif_Type), intent(out)                                 ::    LHS
    class(DistProb_Type), intent(in)                                  ::    RHS

    logical                                                           ::    DebugLoc
    character(*), parameter                                           ::    ProcName='Copy'
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    select type (RHS)
  
      type is (DistUnif_Type)
        call LHS%Reset()
        LHS%Initialized = RHS%Initialized
        LHS%Constructed = RHS%Constructed

        if ( RHS%Constructed ) then
          LHS%A = RHS%A
          LHS%B = RHS%B
          LHS%TruncatedLeft=RHS%TruncatedLeft
          LHS%TruncatedRight=RHS%TruncatedRight
          LHS%ADependency = RHS%ADependency
          LHS%BDependency = RHS%BDependency
        end if

      class default
        call Error%Raise( Line='Incompatible types', ProcName=ProcName )

    end select

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  subroutine Finalizer( This )

    type(DistUnif_Type), intent(inout)                                ::    This

    character(*), parameter                                           ::    ProcName='Finalizer'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if (DebugLoc) call Logger%Entering( ProcName )

    if (DebugLoc) call Logger%Exiting()

  end subroutine
  !!------------------------------------------------------------------------------------------------------------------------------

end module DistUnif_Class
