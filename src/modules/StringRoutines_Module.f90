module StringRoutines_Module

use Parameters_Library
use String_Library
use Logger_Class                                                  ,only:    Logger
use Error_Class                                                   ,only:    Error

implicit none

private

public                                                                ::    ConvertToString
public                                                                ::    ConvertToStrings
public                                                                ::    ConvertToInteger
public                                                                ::    ConvertToIntegers
public                                                                ::    ConvertToInteger8
public                                                                ::    ConvertToInteger8s
public                                                                ::    ConvertToReal
public                                                                ::    ConvertToReals
public                                                                ::    ConvertToReal4
public                                                                ::    ConvertToReal4s
public                                                                ::    ConvertToReal8
public                                                                ::    ConvertToReal8s
public                                                                ::    ConvertToLogical
public                                                                ::    ConvertToLogicals
public                                                                ::    ConvertToComplex
public                                                                ::    ConvertToComplexs

logical, parameter                                                    ::    DebugGlobal = .false.


interface ConvertToString
  module procedure                                                    ::    Convert_R0D_To_C0D
  module procedure                                                    ::    Convert_I0D_To_C0D
  module procedure                                                    ::    Convert_I80D_To_C0D
  module procedure                                                    ::    Convert_C0D_To_C0D
  module procedure                                                    ::    Convert_L0D_To_C0D
  module procedure                                                    ::    Convert_CX0D_To_C0D
  module procedure                                                    ::    Convert_String0D_To_C0D
  module procedure                                                    ::    Convert_R1D_To_C0D
  module procedure                                                    ::    Convert_I1D_To_C0D
  module procedure                                                    ::    Convert_I81D_To_C0D
  module procedure                                                    ::    Convert_C1D_To_C0D
  module procedure                                                    ::    Convert_L1D_To_C0D
  module procedure                                                    ::    Convert_CX1D_To_C0D
  module procedure                                                    ::    Convert_String1D_To_C0D
end interface

interface ConvertToStrings
  module procedure                                                    ::    Convert_C0D_To_String1D
  module procedure                                                    ::    Convert_C1D_To_String1D
end interface

interface ConvertToInteger8                                            
  module procedure                                                    ::    Convert_C0D_To_I80D
end interface

interface ConvertToInteger8s                                            
  module procedure                                                    ::    Convert_C0D_To_I81D
  module procedure                                                    ::    Convert_C1D_To_I81D
end interface

interface ConvertToInteger                                            
  module procedure                                                    ::    Convert_C0D_To_I0D
end interface

interface ConvertToIntegers                                            
  module procedure                                                    ::    Convert_C0D_To_I1D
  module procedure                                                    ::    Convert_C1D_To_I1D
end interface

interface ConvertToReal                                            
  module procedure                                                    ::    Convert_C0D_To_R0D
end interface

interface ConvertToReals                                         
  module procedure                                                    ::    Convert_C0D_To_R1D
  module procedure                                                    ::    Convert_C1D_To_R1D
end interface

interface ConvertToReal4                                            
  module procedure                                                    ::    Convert_C0D_To_R40D
end interface

interface ConvertToReal4s                                         
  module procedure                                                    ::    Convert_C0D_To_R41D
  module procedure                                                    ::    Convert_C1D_To_R41D
end interface

interface ConvertToReal8                                           
  module procedure                                                    ::    Convert_C0D_To_R80D
end interface

interface ConvertToReal8s                                           
  module procedure                                                    ::    Convert_C0D_To_R81D
  module procedure                                                    ::    Convert_C1D_To_R81D
end interface

interface ConvertToLogical                                          
  module procedure                                                    ::    Convert_C0D_To_L0D
end interface

interface ConvertToLogicals                                          
  module procedure                                                    ::    Convert_C0D_To_L1D
  module procedure                                                    ::    Convert_C1D_To_L1D
end interface

interface ConvertToComplex                                          
  module procedure                                                    ::    Convert_C0D_To_CX0D
end interface

interface ConvertToComplexs                                          
  module procedure                                                    ::    Convert_C0D_To_CX1D
  module procedure                                                    ::    Convert_C1D_To_CX1D
end interface

contains

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_I80D( String, Debug )

    integer(8)                                                        ::    Convert_C0D_To_I80D

    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_I80D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_I80D
    if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_I81D( String, Separator, Debug )

    integer(8), allocatable, dimension(:)                             ::    Convert_C0D_To_I81D

    character(*), intent(in)                                          ::    String
    character(*), optional, intent(in)                                ::    Separator
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_I81D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable, dimension(:)                           ::    Strings

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    call Parse( Input=String, Separator=SeparatorLoc, Output=Strings )

    Convert_C0D_To_I81D = ConvertToInteger8s( Strings=Strings )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_I81D( Strings, Debug )

    integer(8), allocatable, dimension(:)                             ::    Convert_C1D_To_I81D

    character(*), dimension(:), intent(in)                            ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_I81D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(Convert_C1D_To_I81D(size(Strings,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_I81D', ProcName=ProcName, stat=StatLoc )

    do i = 1, size(Strings,1)
      read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_I81D(i)
      if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_I0D( String, Debug )

    integer                                                           ::    Convert_C0D_To_I0D

    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_I0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_I0D
    if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_I1D( String, Separator, Debug )

    integer, allocatable, dimension(:)                                ::    Convert_C0D_To_I1D

    character(*), intent(in)                                          ::    String
    character(*), optional, intent(in)                                ::    Separator
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_I1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable, dimension(:)                           ::    Strings

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    call Parse( Input=String, Separator=SeparatorLoc, Output=Strings )

    Convert_C0D_To_I1D = ConvertToIntegers( Strings=Strings )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_I1D( Strings, Debug )

    integer, allocatable, dimension(:)                                ::    Convert_C1D_To_I1D

    character(*), dimension(:), intent(in)                            ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_I1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(Convert_C1D_To_I1D(size(Strings,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_I1D', ProcName=ProcName, stat=StatLoc )

    do i = 1, size(Strings,1)
      read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_I1D(i)
      if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_R0D( String, Debug )

    real(rkp)                                                         ::    Convert_C0D_To_R0D

    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_R0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_R0D
    if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_R1D( String, Separator, Debug )

    real(rkp), allocatable, dimension(:)                              ::    Convert_C0D_To_R1D

    character(*), intent(in)                                          ::    String
    character(*), optional, intent(in)                                ::    Separator
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_R1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable, dimension(:)                           ::    Strings

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    call Parse( Input=String, Separator=SeparatorLoc, Output=Strings )

    Convert_C0D_To_R1D = ConvertToReals( Strings=Strings )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_R1D( Strings, Debug )

    real(rkp), allocatable, dimension(:)                              ::    Convert_C1D_To_R1D

    character(*), dimension(:), intent(in)                            ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_R1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(Convert_C1D_To_R1D(size(Strings,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_R1D', ProcName=ProcName, stat=StatLoc )

    do i = 1, size(Strings,1)
      read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_R1D(i)
      if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_R40D( String, Debug )

    real(4)                                                           ::    Convert_C0D_To_R40D

    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_R40D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_R40D
    if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_R41D( String, Separator, Debug )

    real(4), allocatable, dimension(:)                                ::    Convert_C0D_To_R41D

    character(*), intent(in)                                          ::    String
    character(*), optional, intent(in)                                ::    Separator
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_R41D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable, dimension(:)                           ::    Strings

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    call Parse( Input=String, Separator=SeparatorLoc, Output=Strings )

    Convert_C0D_To_R41D = ConvertToReal8s( Strings=Strings )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_R41D( Strings, Debug )

    real(4), allocatable, dimension(:)                                ::    Convert_C1D_To_R41D

    character(*), dimension(:), intent(in)                            ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_R41D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(Convert_C1D_To_R41D(size(Strings,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_R41D', ProcName=ProcName, stat=StatLoc )

    do i = 1, size(Strings,1)
      read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_R41D(i)
      if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_R80D( String, Debug )

    real(8)                                                           ::    Convert_C0D_To_R80D

    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_R80D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_R80D
    if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_R81D( String, Separator, Debug )

    real(8), allocatable, dimension(:)                                ::    Convert_C0D_To_R81D

    character(*), intent(in)                                          ::    String
    character(*), optional, intent(in)                                ::    Separator
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_R81D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable, dimension(:)                           ::    Strings

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    call Parse( Input=String, Separator=SeparatorLoc, Output=Strings )

    Convert_C0D_To_R81D = ConvertToReal8s( Strings=Strings )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_R81D( Strings, Debug )

    real(8), allocatable, dimension(:)                                ::    Convert_C1D_To_R81D

    character(*), dimension(:), intent(in)                            ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_R81D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(Convert_C1D_To_R81D(size(Strings,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_R81D', ProcName=ProcName, stat=StatLoc )

    do i = 1, size(Strings,1)
      read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_R81D(i)
      if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_L0D( String, Debug )

    logical                                                           ::    Convert_C0D_To_L0D

    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_L0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_L0D
    if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_L1D( String, Separator, Debug )

    logical, allocatable, dimension(:)                                ::    Convert_C0D_To_L1D

    character(*), intent(in)                                          ::    String
    character(*), optional, intent(in)                                ::    Separator
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_L1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable, dimension(:)                           ::    Strings

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    call Parse( Input=String, Separator=SeparatorLoc, Output=Strings )

    Convert_C0D_To_L1D = ConvertToLogicals( Strings=Strings )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_L1D( Strings, Debug )

    logical, allocatable, dimension(:)                                ::    Convert_C1D_To_L1D

    character(*), dimension(:), intent(in)                            ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_L1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(Convert_C1D_To_L1D(size(Strings,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_L1D', ProcName=ProcName, stat=StatLoc )

    do i = 1, size(Strings,1)
      read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_L1D(i)
      if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_CX0D( String, Debug )

    complex                                                           ::    Convert_C0D_To_CX0D

    character(*), intent(in)                                          ::    String
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_CX0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    read(unit=String, fmt=*, iostat=StatLoc) Convert_C0D_To_CX0D
    if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_CX1D( String, Separator, Debug )

    complex, allocatable, dimension(:)                                ::    Convert_C0D_To_CX1D

    character(*), intent(in)                                          ::    String
    character(*), optional, intent(in)                                ::    Separator
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_CX1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable, dimension(:)                           ::    Strings

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    call Parse( Input=String, Separator=SeparatorLoc, Output=Strings )

    Convert_C0D_To_CX1D = ConvertToComplexs( Strings=Strings )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_CX1D( Strings, Debug )

    complex, allocatable, dimension(:)                                ::    Convert_C1D_To_CX1D

    character(*), dimension(:), intent(in)                            ::    Strings
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_CX1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    allocate(Convert_C1D_To_CX1D(size(Strings,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_CX1D', ProcName=ProcName, stat=StatLoc )

    do i = 1, size(Strings,1)
      read(unit=Strings(i), fmt=*, iostat=StatLoc) Convert_C1D_To_CX1D(i)
      if ( StatLoc /= 0 ) call Error%Read( Message='Error when performing an internal read', ProcName=ProcName, Status=StatLoc )
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------------------!!
  function Convert_R0D_To_C0D( Value, Format, Debug )

    character(:), allocatable                                         ::    Convert_R0D_To_C0D

    real(rkp), intent(in)                                             ::    Value
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_R0D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    character(1000)                                                   ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    FormatLoc = '(' // FormatLoc // ')'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc )
    Convert_R0D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------------------!!
  function Convert_I0D_To_C0D( Value, Format, Debug )

    character(:), allocatable                                         ::    Convert_I0D_To_C0D

    integer, intent(in)                                               ::    Value
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_I0D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    character(1000)                                                   ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    FormatLoc = '(' // FormatLoc // ')'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc )
    Convert_I0D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------------------!!
  function Convert_I80D_To_C0D( Value, Format, Debug )

    character(:), allocatable                                         ::    Convert_I80D_To_C0D

    integer(8), intent(in)                                            ::    Value
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_I80D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    character(1000)                                                   ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    FormatLoc = '(' // FormatLoc // ')'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc )
    Convert_I80D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------------------!!
  function Convert_C0D_To_C0D( Value, Format, Debug )

    character(:), allocatable                                         ::    Convert_C0D_To_C0D

    character(*), intent(in)                                          ::    Value
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    character(1000)                                                   ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    FormatLoc = '(' // FormatLoc // ')'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc )
    Convert_C0D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------------------!!
  function Convert_L0D_To_C0D( Value, Format, Debug )

    character(:), allocatable                                         ::    Convert_L0D_To_C0D

    logical, intent(in)                                               ::    Value
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_L0D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    character(1000)                                                   ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    FormatLoc = '(' // FormatLoc // ')'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc )
    Convert_L0D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------------------!!
  function Convert_CX0D_To_C0D( Value, Format, Debug )

    character(:), allocatable                                         ::    Convert_CX0D_To_C0D

    complex, intent(in)                                               ::    Value
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_CX0D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    character(1000)                                                   ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    FormatLoc = '(' // FormatLoc // ')'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Value
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc )
    Convert_CX0D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !----------------------------------------------------------------------------------------------------------------------------!!
  function Convert_String0D_To_C0D( Value, Format, Debug )

    character(:), allocatable                                         ::    Convert_String0D_To_C0D

    type(String_Type), intent(in)                                     ::    Value
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_String0D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    FormatLoc
    character(1000)                                                   ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    FormatLoc = '(' // FormatLoc // ')'

    Convert_String0D_To_C0D = Value%GetValue()

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_R1D_To_C0D( Values, Format, Separator, Debug )

    character(:), allocatable                                         ::    Convert_R1D_To_C0D

    real(rkp), dimension(:), intent(in)                               ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_R1D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    integer                                                           ::    Size1
    character(10000)                                                  ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Values
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
    Convert_R1D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_I1D_To_C0D( Values, Format, Separator, Debug )

    character(:), allocatable                                         ::    Convert_I1D_To_C0D

    integer, dimension(:), intent(in)                                 ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_I1D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    integer                                                           ::    Size1
    character(10000)                                                  ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Values
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
    Convert_I1D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_I81D_To_C0D( Values, Format, Separator, Debug )

    character(:), allocatable                                         ::    Convert_I81D_To_C0D

    integer(8), dimension(:), intent(in)                              ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_I81D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    integer                                                           ::    Size1
    character(10000)                                                  ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Values
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
    Convert_I81D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_C0D( Values, Format, Separator, Debug )

    character(:), allocatable                                         ::    Convert_C1D_To_C0D

    character(*), dimension(:), intent(in)                            ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    integer                                                           ::    Size1
    character(10000)                                                  ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Values
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
    Convert_C1D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_L1D_To_C0D( Values, Format, Separator, Debug )

    character(:), allocatable                                         ::    Convert_L1D_To_C0D

    logical, dimension(:), intent(in)                                 ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_L1D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    integer                                                           ::    Size1
    character(10000)                                                  ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Values
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
    Convert_L1D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_CX1D_To_C0D( Values, Format, Separator, Debug )

    character(:), allocatable                                         ::    Convert_CX1D_To_C0D

    complex, dimension(:), intent(in)                                 ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_CX1D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    integer                                                           ::    Size1
    character(10000)                                                  ::    VarC0D

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    write(unit=VarC0D, fmt=FormatLoc, iostat=StatLoc) Values
    if ( StatLoc /= 0 ) call Error%Write( Line='Error when performing an internal write', ProcName=ProcName, iostat=StatLoc)
    Convert_CX1D_To_C0D = trim(adjustl(VarC0D))

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_String1D_To_C0D( Values, Format, Separator, Debug )

    character(:), allocatable                                         ::    Convert_String1D_To_C0D

    type(String_Type), dimension(:), intent(in)                       ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_String1D_To_C0D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    integer                                                           ::    Size1

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    Convert_String1D_To_C0D = Values(1)%GetValue()
    i = 2
    do i = 2, size(Values,1)
      Convert_String1D_To_C0D = Convert_String1D_To_C0D // SeparatorLoc // Values(i)%GetValue()
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C0D_To_String1D( Value, Format, Separator, Debug )

    type(String_Type), dimension(:), allocatable                      ::    Convert_C0D_To_String1D

    character(*), intent(in)                                          ::    Value
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C0D_To_String1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i
    character(:), allocatable, dimension(:)                           ::    Output

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    call Parse( Input=Value, Separator=SeparatorLoc, Output=Output )

    allocate(Convert_C0D_To_String1D(size(Output,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C0D_To_String1D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, size(Output,1)
      Convert_C0D_To_String1D(i) = trim(adjustl(Output(i)(:)))
    end do

    deallocate(Output, stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Deallocate( Name='Output', ProcName=ProcName, stat=StatLoc )

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

  !!------------------------------------------------------------------------------------------------------------------------------
  function Convert_C1D_To_String1D( Values, Format, Separator, Debug )

    type(String_Type), dimension(:), allocatable                      ::    Convert_C1D_To_String1D

    character(*), dimension(:), intent(in)                            ::    Values
    character(*), optional, intent(in)                                ::    Separator
    character(*), optional, intent(in)                                ::    Format
    logical, optional ,intent(in)                                     ::    Debug

    character(*), parameter                                           ::    ProcName='Convert_C1D_To_String1D'
    logical                                                           ::    DebugLoc
    integer                                                           ::    StatLoc=0
    character(:), allocatable                                         ::    SeparatorLoc
    character(:), allocatable                                         ::    FormatLoc
    integer                                                           ::    i

    DebugLoc = DebugGlobal
    if ( present(Debug) ) DebugLoc = Debug
    if (DebugLoc) call Logger%Entering( ProcName )

    SeparatorLoc = ' '
    if ( present(Separator) ) SeparatorLoc = Separator

    FormatLoc = 'G0'
    if ( present(Format) ) FormatLoc = Format

    if ( SeparatorLoc == ' ' ) SeparatorLoc = '1X'

    FormatLoc = '(' // FormatLoc // ',*(' // SeparatorLoc // ',' // FormatLoc // '))'

    allocate(Convert_C1D_To_String1D(size(Values,1)), stat=StatLoc)
    if ( StatLoc /= 0 ) call Error%Allocate( Name='Convert_C1D_To_String1D', ProcName=ProcName, stat=StatLoc )

    i = 1
    do i = 1, size(Values,1)
      Convert_C1D_To_String1D(i) = trim(adjustl(Values(i)(:)))
    end do

    if (DebugLoc) call Logger%Exiting

  end function
  !!------------------------------------------------------------------------------------------------------------------------------

end module
