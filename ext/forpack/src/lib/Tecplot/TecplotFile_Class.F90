Module TecplotFile_Class

  implicit none

  private
  public  ::  TecplotFile_Type

  Type    :: TecplotFile_Type
    integer                     ::  Unit
    integer                     ::  NVar              ! Total number of variables (both nodal and cell-centered)
    integer                     ::  NNodeVar          ! Number of nodal variables
    integer                     ::  NCellVar          ! Number of cell-centered variables
    integer                     ::  NNodes            ! Number of nodes
    integer                     ::  NCells            ! Number of cells
    integer                     ::  iLineConn
    integer       ,allocatable  ::  IdxCellVar(:)     ! Index mapping from total to cell-centered variables
    integer       ,allocatable  ::  IdxNodeVar(:)     ! Index mapping from total to nodal variables
    character(:)  ,allocatable  ::  FileName          ! Name of the tecplot file
    character(:)  ,allocatable  ::  VarNames(:)       ! Name of all variables
    character(:)  ,allocatable  ::  VarNamesNode(:)   ! Name of nodal variables
    character(:)  ,allocatable  ::  VarNamesCell(:)   ! Name of cell-centered variables
    real(8)       ,allocatable  ::  NodeVar(:,:)      ! Data associated to nodal variables. Dim=(NNodes,NNodeVar)
    real(8)       ,allocatable  ::  CellVar(:,:)      ! Data associated to nodal variables. Dim=(NCells,NCellVar)
  contains
    private
    procedure ,public   ::  Initialize      =>  InitializeTecplotFile
    procedure ,public   ::  Write           =>  WriteTecplotFile
!     procedure ,public   ::  GetCellVar
    procedure ,public   ::  GetCellVarIndex =>  GetCellVarIndexFromTecplotFile
    procedure ,public   ::  GetMatchingCellVarIndex
    generic   ,public   ::  AddVariable     =>  AddVariableToTecplotFile_0d, AddVariableToTecplotFile_1d
    procedure           ::  AddVariableToTecplotFile_0d
    procedure           ::  AddVariableToTecplotFile_1d
  End Type

  Interface

    Module Subroutine InitializeTecplotFile( This, FileName, LogLevel )
      class(TecplotFile_Type)                                 ,intent(out)    ::  This
      character(*)                                            ,intent(in)     ::  FileName
      integer                                       ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
    End Subroutine

    Module Subroutine AddVariableToTecplotFile_0d( This, Data, Name, Type, LogLevel )
      class(TecplotFile_Type)                                 ,intent(inout)  ::  This
      real(8)       ,dimension(:)                             ,intent(in)     ::  Data
      character(*)                                            ,intent(in)     ::  Name
      character(*)                                  ,optional ,intent(in)     ::  Type
      integer                                       ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
    End Subroutine

    Module Subroutine AddVariableToTecplotFile_1d( This, Data, Names, Type, LogLevel )
      class(TecplotFile_Type)                                 ,intent(inout)  ::  This
      real(8)       ,dimension(:,:)                           ,intent(in)     ::  Data
      character(*)  ,dimension(:)                             ,intent(in)     ::  Names
      character(*)                                  ,optional ,intent(in)     ::  Type
      integer                                       ,optional ,intent(in)     ::  LogLevel                         !< Debugging indicator
    End Subroutine

    Module Subroutine WriteTecplotFile( This, FileName )
      class(TecplotFile_Type)                                 ,intent(in)   ::  This
      character(*)                                  ,optional ,intent(in)   ::  FileName
    End Subroutine

    Module Function GetCellVarIndexFromTecplotFile( This, VarName ) result(iVar)
      class(TecplotFile_Type)                                 ,intent(in)     ::  This
      character(*)                                            ,intent(in)     ::  VarName
      integer                                                                 ::  iVar
    End Function

!     Module Function GetCellVar( This, VarName ) result(Var)
!       class(TecplotFile_Type)                                 ,intent(in)     ::  This
!       character(*)                                            ,intent(in)     ::  VarName
!       real(8) ,allocatable                                                    ::  Var(:,:)
!     End Function

    Module Subroutine GetMatchingCellVarIndex( This, iVar, StartWith )
      class(TecplotFile_Type)                                 ,intent(in)     ::  This
      integer ,allocatable                                    ,intent(out)    ::  iVar(:)
      character(*)                                  ,optional ,intent(in)     ::  StartWith
    End Subroutine




  End Interface

End Module