Module TecplotLine_Class

  implicit none

  private
  public  ::  TecplotLine_Type

  Type    :: TecplotLine_Type
    logical                     ::  OneFilePerZone  = .False.
    integer                     ::  Unit      =   0
    integer                     ::  NumZones  =   0       ! Number of zones
    integer                     ::  NVar      =   0       ! Number of variables
    integer       ,allocatable  ::  Np(:)                 ! Number of points per zone Dim=(Nz)
    real(8)       ,allocatable  ::  VarData(:,:,:)        ! Dim=(NPts,This%NVar,This%Nz) )
    character(:)  ,allocatable  ::  VarNames(:)
    character(:)  ,allocatable  ::  FileName
  contains
    private
    procedure ,public ::  Initialize  =>  InitializeTecplotLine
    procedure ,public ::  WriteGnuplotFile


    generic   ,public ::  GetVarsIndex  =>  GetVarsIndexFromNames
    procedure         ::  GetVarsIndexFromNames

    generic   ,public ::  GetVarIndex   =>  GetVarIndexFromName, GetVarIndexFromNames
    procedure         ::  GetVarIndexFromName
    procedure         ::  GetVarIndexFromNames

    generic   ,public ::  GetVar   =>  GetVarFromName
    procedure         ::  GetVarFromName

    procedure ,public ::  RemoveVarNamesQuotes
    procedure ,public ::  RemoveVarNamesPrefix
    procedure ,public ::  RemovePoints
    generic   ,public ::  RemoveVar =>  RemoveVarFromIndex, RemoveVarFromIndexes
    procedure         ::  RemoveVarFromIndex
    procedure         ::  RemoveVarFromIndexes


    generic   ,public ::  AddVar    =>  AddVarFromValueName, AddVarFromNamePrefix
    procedure         ::  AddVarFromValueName
    procedure         ::  AddVarFromNamePrefix

    procedure ,public ::  Invert    =>  InvertTecplotLine

    generic   ,public ::  GetVarsProperties =>  GetVarsProperties_2d  ! GetVarsProperties_1d,
!     procedure         ::  GetVarsProperties_1d
    procedure         ::  GetVarsProperties_2d
    procedure ,public ::  GetMatchingVarIndex
    procedure ,public ::  AddNewZone


  End Type

  Interface

    Module Subroutine InitializeTecplotLine( This, FileName, OneFilePerZone, LogLevel )
      class(TecplotLine_Type)                               ,intent(out)    ::  This
      character(*)                                          ,intent(in)     ::  FileName
      logical                                     ,optional ,intent(in)     ::  OneFilePerZone
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine WriteGnuplotFile( This, FileName, VarNames )
      class(TecplotLine_Type)                               ,intent(in)     ::  This
      character(*)                                ,optional ,intent(in)     ::  FileName
      character(*)                                ,optional ,intent(in)     ::  VarNames(:)
    End Subroutine

    Module Subroutine AddNewZone( This, TecplotLine, LogLevel )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
      type(TecplotLine_Type)                                ,intent(in)     ::  TecplotLine
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Function GetVarIndexFromName( This, VarName, CaseSensitive ) result(iVar)
      class(TecplotLine_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  VarName
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      integer                                                               ::  iVar
    End Function

    Module Function GetVarIndexFromNames( This, VarNames, CaseSensitive ) result(iVar)
      class(TecplotLine_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  VarNames(:)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      integer                                                               ::  iVar
    End Function

    Module Function GetVarsIndexFromNames( This, VarNames, CaseSensitive ) result(iVar)
      class(TecplotLine_Type)                               ,intent(in)     ::  This
      character(*)                                          ,intent(in)     ::  VarNames(:)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      integer   ,allocatable                                                ::  iVar(:)
    End Function

    Module Subroutine GetVarFromName( This, Var, VarName, CaseSensitive, iVar, iZone )
      class(TecplotLine_Type)                               ,intent(in)     ::  This
      real(8)   ,allocatable                                ,intent(out)    ::  Var(:)
      character(*)                                          ,intent(in)     ::  VarName
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      integer                                     ,optional ,intent(out)    ::  iVar
      integer                                     ,optional ,intent(in)     ::  iZone
    End Subroutine

    Module Subroutine AddVarFromValueName( This, Value, Name, iZone, LogLevel )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
      real(8)   ,allocatable                                ,intent(in)     ::  Value(:)
      character(*)                                          ,intent(in)     ::  Name
      integer                                     ,optional ,intent(in)     ::  iZone
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine AddVarFromNamePrefix( This, Name, PrefixVarToSum, iZone, LogLevel )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Name
      character(*)                                          ,intent(in)     ::  PrefixVarToSum
      integer                                     ,optional ,intent(in)     ::  iZone
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine





    Module Subroutine RemovePoints( This, Idx, LogLevel )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  Idx(:)    ! Index of data points to be removed
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine RemoveVarNamesQuotes( This )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
    End Subroutine

    Module Subroutine RemoveVarNamesPrefix( This, Prefix, LogLevel )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
      character(*)                                          ,intent(in)     ::  Prefix
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine RemoveVarFromIndexes( This, VarIdx, iZone, LogLevel )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  VarIdx(:)
      integer                                     ,optional ,intent(in)     ::  iZone
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine

    Module Subroutine RemoveVarFromIndex( This, VarIdx, iZone, LogLevel )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
      integer                                               ,intent(in)     ::  VarIdx
      integer                                     ,optional ,intent(in)     ::  iZone
      integer                                     ,optional ,intent(in)     ::  LogLevel
    End Subroutine


    Module Subroutine InvertTecplotLine( This )
      class(TecplotLine_Type)                               ,intent(inout)  ::  This
    End Subroutine



    Module Subroutine GetVarsProperties_2d( This, Value, VarNameStartWith, CaseSensitive, iVar, Names, iZone, VarNames )
      class(TecplotLine_Type)                               ,intent(in)     ::  This
      real(8)       ,allocatable                  ,optional ,intent(out)    ::  Value(:,:)  ! Dim=(Np,NVar)
      character(*)                                ,optional ,intent(in)     ::  VarNameStartWith
      character(*)                                ,optional ,intent(in)     ::  VarNames(:)
      logical                                     ,optional ,intent(in)     ::  CaseSensitive
      integer       ,allocatable                  ,optional ,intent(out)    ::  iVar(:)
      character(:)  ,allocatable                  ,optional ,intent(out)    ::  Names(:)
      integer                                     ,optional ,intent(in)     ::  iZone
    End Subroutine

!     Module Subroutine GetVarsProperties_1d( This, Value, VarNameStartWith, VarName, CaseSensitive, iVar, Name )
!       class(TecplotLine_Type)                               ,intent(in)     ::  This
!       real(8)       ,allocatable                  ,optional ,intent(out)    ::  Value(:)  ! Dim=(Np)
!       character(*)                                ,optional ,intent(in)     ::  VarNameStartWith
!       character(*)                                ,optional ,intent(in)     ::  VarName
!       logical                                     ,optional ,intent(in)     ::  CaseSensitive
!       integer       ,allocatable                  ,optional ,intent(out)    ::  iVar
!       character(:)  ,allocatable                  ,optional ,intent(out)    ::  Name
!     End Subroutine

    Module Subroutine GetMatchingVarIndex( This, iVar, StartWith )
      class(TecplotLine_Type)                                 ,intent(in)     ::  This
      integer ,allocatable                                    ,intent(out)    ::  iVar(:)
      character(*)                                  ,optional ,intent(in)     ::  StartWith
    End Subroutine

  End Interface

End Module