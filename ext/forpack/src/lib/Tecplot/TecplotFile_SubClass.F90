SubModule(TecplotFile_Class) TecplotFile_SubClass

  use Logger_Class        ,only:  Logger, LogLevel_NOLOGS, LogLevel_DEBUG

  implicit none

  integer ,parameter  ::  ix    = 1
  integer ,parameter  ::  iy    = 2
  integer ,parameter  ::  iz    = 3
  integer ,parameter  ::  irho  = 4
  integer ,parameter  ::  icsN2n= 5
  integer ,parameter  ::  icsN  = 6
  integer ,parameter  ::  iu    = 7
  integer ,parameter  ::  iv    = 8
  integer ,parameter  ::  iw    = 9
  integer ,parameter  ::  it    = 10
  integer ,parameter  ::  itv   = 11
  integer ,parameter  ::  ip    = 12
  integer ,parameter  ::  im    = 13


  contains

Module Procedure InitializeTecplotFile

  use String_Library        ,only:  Convert_To_Integer, VecTrim
  use File_Library          ,only:  FileExist
  use Error_Class           ,only:  Error

  character(*)                                              ,parameter  ::  ProcName = 'InitializeTecplotFile'

  call Logger%Entering( ProcName )

  This%FileName   =   FileName

  if ( .Not. FileExist(This%FileName) ) call Error%Raise( "Input file '"//This%FileName//"' not found" )
  open(NewUnit=This%Unit,File=This%FileName,Status="OLD", Action="READ")

  call Logger%Write( "Reading variables names" )
  call ReadVarNames( This, LogLevel=0 )
  call Logger%Write( "-> This%NVar      = ", This%NVar )
  call Logger%Write( "-> This%VarNames  = ", This%VarNames )

  call Logger%Write( "Reading number of nodes/cells" )
  call ReadNumberNodesCells( This, LogLevel )
  call Logger%Write( "-> This%Nnodes  = ", This%Nnodes )
  call Logger%Write( "-> This%Ncells  = ", This%Ncells )

  call Logger%Write( "Reading the data" )
  call ReadVariables( This )

  close(This%Unit)

  call Logger%Exiting()

End Procedure

Subroutine ReadVarNames( This, LogLevel )
  use String_Library      ,only:  EmptyString
  use Utilities_Library   ,only:  AddElementToArray
  class(TecplotFile_Type)                               ,intent(inout)  ::  This
  integer                                     ,optional ,intent(in)     ::  LogLevel
  character(*)                                              ,parameter  ::  ProcName = 'ReadVarNames'
  integer                                                               ::  ios, j
  character(1000)                                                       ::  Line
  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )
  call Logger%Write( "Extracting variable names" )
  rewind(This%Unit)
  This%NVar   =   0
  call EmptyString(This%VarNames)
  do
    read(This%Unit,"(a)",iostat=ios) Line
    if (ios/=0) exit
    if ( Line(1:11) /= "VARIABLES =" ) cycle
    j = index(Line,"=")+2
    This%NVar  = 1
    j = index(Line,"=")+2
    call AddElementToArray( trim(Line(j:)), This%VarNames )
    do
      read(This%Unit,"(a)",iostat=ios) Line
      if ( Line(1:1) /= '"' ) exit
      This%NVar = This%NVar + 1
      call AddElementToArray( trim(Line), This%VarNames )
    end do
  end do
  call Logger%Write( "-> Number of variables: This%NVar     = ", This%NVar )
  call Logger%Write( "-> Names of variables:  This%VarNames  = ", This%VarNames )
  call Logger%Exiting()
End Subroutine

Subroutine ReadNumberNodesCells( This, LogLevel )
  use String_Library      ,only:  RemoveSpace, Convert_To_Integer, Parse
  class(TecplotFile_Type)                               ,intent(inout)  ::  This
  integer                                     ,optional ,intent(in)     ::  LogLevel
  character(*)                                              ,parameter  ::  ProcName = 'ReadNumberNodesCells'
  integer                                                               ::  ios
  character(1000)                                                       ::  Line
  character(:)  ,allocatable                                            ::  List(:)
  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )
  This%Nnodes    =   0
  This%Ncells    =   0
  rewind(This%Unit)
  do
    read(This%Unit,"(a)",iostat=ios) Line
    if (ios/=0) exit
    if ( Line(1:4) /= "ZONE" ) cycle
    Line    =   RemoveSpace(Line)

    call Parse(Line,"N=",List)
    if ( size(List) >= 2 ) then
      call Parse(List(2),",",List)
      This%Nnodes  =   Convert_To_Integer( List(1) )
    end if

    call Parse(Line,"E=",List)
    if ( size(List) >= 2 ) then
      call Parse(List(2),",",List)
      This%Ncells  =   Convert_To_Integer( List(1) )
    end if
    exit
  end do
  call Logger%Exiting()
End Subroutine
! ZONE N=21720, E=10800, T="US3D_Data", VARLOCATION=([1-3]=NODAL,[4-31]=CELLCENTERED)
! ZONE T="Extracted Points"


!   call Logger%Write( "Getting number of zones" )
!   rewind(This%Unit)
!   This%Nz     =   0
!   do
!     read(This%Unit,"(a)",iostat=ios) Line
!     if (ios/=0) exit
!     if ( Line(1:4) /= "ZONE" ) cycle
!     This%Nz   =   This%Nz + 1
!   end do
!   allocate( This%Np(This%Nz) )
!   allocate( NRows(This%Nz) )
!   allocate( NLin2(This%Nz) )
!   call Logger%Write( "-> This%Nz = ", This%Nz )
!

Subroutine ReadVariables( This )

  use String_Library        ,only:  Parse, GetSubString, UpperCase, Convert_To_Integer
  use Error_Class           ,only:  Error

  class(TecplotFile_Type)                               ,intent(inout)  ::  This

  integer                                                   ,parameter  ::  CellVar = 1
  integer                                                   ,parameter  ::  NodeVar = 2
  integer                                                               ::  ios, iLine
  integer                                                               ::  i, j, in, ic
  integer                                                               ::  iIni, iFin, NVar
  integer       ,allocatable                                            ::  Var2Type(:)
  character(1000)                                                       ::  Line
  character(:)  ,allocatable                                            ::  String, VarType, StrIdx
  character(:)  ,allocatable                                            ::  List(:), List2(:)

! Setting the number of cell-centered/nodal variables and their index
  rewind(This%Unit)
  do
    read(This%Unit,"(a)",iostat=ios) Line
    if (ios/=0) exit
    i   =   index(Line,"VARLOCATION")
    if ( i == 0 ) cycle
    String    =   GetSubString( Line, ["(",")"] )
    call Parse( String, ",", List )
    do i = 1,size(List)
      call Parse( List(i), "=", List2 )
      StrIdx  =   GetSubString( List2(1), ["[","]"] )
      VarType =   trim( UpperCase(List2(2)) )
      call Parse( StrIdx, "-", List2 )
      select case (size(List2))
        case (1)
          iIni    =   Convert_To_Integer(List2(1))
          iFin    =   iIni
        case (2)
          iIni    =   Convert_To_Integer(List2(1))
          iFin    =   Convert_To_Integer(List2(2))
      end select
      NVar        =   iFin - iIni + 1
      select case (VarType)
        case ("NODAL")
          This%NNodeVar   =   NVar
          This%IdxNodeVar =   [ (j,j=iIni,iFin) ]
        case ("CELLCENTERED")
          This%NCellVar   =   NVar
          This%IdxCellVar =   [ (j,j=iIni,iFin) ]
      end select
    end do
    exit
  end do

  allocate( This%NodeVar(This%Nnodes,This%NNodeVar) )
  allocate( This%CellVar(This%Ncells,This%NCellVar) )
  This%VarNamesNode   =   [ ( This%VarNames( This%IdxNodeVar(i) ) , i=1,This%NNodeVar ) ]
  This%VarNamesCell   =   [ ( This%VarNames( This%IdxCellVar(i) ) , i=1,This%NCellVar ) ]

  call Logger%Write( "-> This%NNodeVar      = ", This%NNodeVar )
  call Logger%Write( "-> This%NCellVar      = ", This%NCellVar )
  call Logger%Write( "-> This%IdxNodeVar    = ", This%IdxNodeVar )
  call Logger%Write( "-> This%IdxCellVar    = ", This%IdxCellVar )
  call Logger%Write( "-> This%VarNamesNode  = ", This%VarNamesNode )
  call Logger%Write( "-> This%VarNamesCell  = ", This%VarNamesCell )

  NVar    =   This%NNodeVar + This%NCellVar
  if ( NVar /= This%Nvar ) then
  call Error%Raise( "Number of variables names and sum of cell-centered/nodal variables do not match")
  end if


! Going to the start of the variables data
  rewind(This%Unit)
  iLine   =   0
  do
    read(This%Unit,"(a)",iostat=ios) Line
    if (ios/=0) exit
    iLine =   iLine + 1
    if ( Line(1:12) /= "SOLUTIONTIME" ) cycle
    exit
  end do

  allocate( Var2Type(This%NVar) )
  do i = 1,size(This%IdxCellVar)
    j   =   This%IdxCellVar(i)
    Var2Type(j) =   CellVar
  end do
  do i = 1,size(This%IdxNodeVar)
    j   =   This%IdxNodeVar(i)
    Var2Type(j) =   NodeVar
  end do

  call Logger%Write( "-> Var2Type    = ", Var2Type )


  in    =   0
  ic    =   0
  do i = 1,This%NVar

    select case (Var2Type(i))

      case (CellVar)
        ic    =   ic + 1
        call Logger%Write( "-> Reading variable i = ", i, "Cell var ic = ", ic, "=> ", This%VarNamescell(ic), Fi="i3" )
        do j = 1,This%NCells
          read(This%Unit,*) This%CellVar(j,ic)
          iLine =   iLine + 1
        end do

      case (NodeVar)
        in    =   in + 1
        call Logger%Write( "-> Reading variable i = ", i, "Node var in = ", in, "=> ", This%VarNamesNode(in), Fi="i3" )
        do j = 1,This%NNodes
          read(This%Unit,*) This%NodeVar(j,in)
          iLine =   iLine + 1
        end do

    end select

  end do

  This%iLineConn   =   iLine
  call Logger%Write( "-> This%iLineConn = ", This%iLineConn )
  read(This%Unit,"(a)") Line
  call Logger%Write( "-> Line  = ", Line )

!
!
!   rewind(This%Unit)
!   do i = 1,This%iLineConn
!     read(This%Unit,*)
!   end do
!   read(This%Unit,"(a)") Line
!   call Logger%Write( "-> Line  = ", Line )



!   do i = 1,20!size(T,1)
!     call Logger%Write( "-> i = ", i, "This%CellVar(i,:) = ", This%CellVar(i,:), Fi="i5", Fr="es15.8" )
!   end do


End Subroutine



Module Procedure AddVariableToTecplotFile_0d!( This, Data, Name, Type, LogLevel )
!   class(TecplotFile_Type)                                 ,intent(inout)  ::  This
!   real(8)       ,dimension(:)                             ,intent(in)     ::  Data
!   character(*)                                            ,intent(in)     ::  Name
!   character(*)                                  ,optional ,intent(in)     ::  Type
!   integer                                       ,optional ,intent(in)     ::  LogLevel
End Procedure

Module Procedure AddVariableToTecplotFile_1d!( This, Data, Name, Type, LogLevel )
  use Utilities_Library     ,only:  GetOptArgValue, AddElementToArray
!   class(TecplotFile_Type)                                 ,intent(inout)  ::  This
!   real(8)       ,dimension(:,:)                           ,intent(in)     ::  Data
!   character(*)  ,dimension(:)                             ,intent(in)     ::  Names
!   character(*)                                  ,optional ,intent(in)     ::  Type
!   integer                                       ,optional ,intent(in)     ::  LogLevel

  character(*)                                              ,parameter  ::  ProcName = 'AddVariableToTecplotFile_1d'

  character(:)  ,allocatable                                            ::  VarType
  integer                                                               ::  i, iIni, iFin, NVar

  call Logger%Entering( ProcName )

  VarType   =   GetOptArgValue("CELLCENTERED",Type)

  NVar    =   size(Names)
  iIni    =   This%NVar + 1
  iFin    =   iIni + NVar

  This%NVar   =   This%NVar + NVar
  call AddElementToArray( Names, This%VarNames )


  call Logger%Write( "-> This%NVar          = ", This%NVar )
  call Logger%Write( "-> This%VarNames      = ", This%VarNames )

  select case (VarType)

    case ("NODAL")
      call Logger%Write( "-> This%NNodeVar      = ", This%NNodeVar )
      call Logger%Write( "-> This%IdxNodeVar    = ", This%IdxNodeVar )
      call Logger%Write( "-> This%VarNamesNode  = ", This%VarNamesNode )
      This%NNodeVar   =   This%NNodeVar + NVar
      call AddElementToArray( [ (i,i=iIni,iFin) ] , This%IdxNodeVar   )
      call AddElementToArray( Names               , This%VarNamesNode )
      call AddElementToArray( Data                , This%NodeVar      , Dim=2 )

    case ("CELLCENTERED")

!       call Logger%Write( "-> Before: This%NCellVar      = ", This%NCellVar )
!       call Logger%Write( "-> Before: This%IdxCellVar    = ", This%IdxCellVar )
!       call Logger%Write( "-> Before: This%VarNamesCell  = ", This%VarNamesCell )
!       call Logger%Write( "-> Before: size(This%CellVar,1)  = ", size(This%CellVar,1) )
!       call Logger%Write( "-> Before: size(This%CellVar,2)  = ", size(This%CellVar,2) )

      This%NCellVar   =   This%NCellVar + NVar
      call AddElementToArray( [ (i,i=iIni,iFin) ] , This%IdxCellVar   )
      call AddElementToArray( Names               , This%VarNamesCell )
      call AddElementToArray( Data                , This%CellVar      , Dim=2 )
      call Logger%Write( "-> After:  This%NCellVar      = ", This%NCellVar )
      call Logger%Write( "-> After:  This%IdxCellVar    = ", This%IdxCellVar )
      call Logger%Write( "-> After:  This%VarNamesCell  = ", This%VarNamesCell )
      call Logger%Write( "-> After:  size(This%CellVar,1)  = ", size(This%CellVar,1) )
      call Logger%Write( "-> After:  size(This%CellVar,2)  = ", size(This%CellVar,2) )
  end select


  call Logger%Exiting()

End Procedure

Module Procedure WriteTecplotFile

  use String_Library        ,only:  Inline, Parse, Convert_To_String
  use File_Library          ,only:  AddFileSuffix
  use Utilities_Library     ,only:  GetOptArgValue

  character(*)                                              ,parameter  ::  ProcName = 'WriteTecplotFile'
  integer                                                               ::  OutUnit, InpUnit
  integer                                                               ::  ios
  integer                                                               ::  i, j, iIni, iFin
  character(1000)                                                       ::  Line
  character(:)  ,allocatable                                            ::  OutputFile, BaseName
  character(:)  ,allocatable                                            ::  NewLine, OutFile, List(:)

  call Logger%Entering( ProcName )

  OutputFile    =   GetOptArgValue(AddFileSuffix(This%FileName,"new",Separator="-"),FileName)
!   OutputFile  =   AddFileSuffix(BaseName,"output",Separator="-")
  call Logger%Write( "OutputFile = ", OutputFile )


  open( NewUnit=InpUnit , File=This%FileName , Status="OLD"     , Action="READ"  )
  open( NewUnit=OutUnit , File=OutputFile    , Status="REPLACE" , Action="WRITE" )

  rewind(InpUnit)
  do

!     Line  =  ""
    read(InpUnit,"(a)",iostat=ios) Line
    if (ios/=0) exit

    if ( Line(1:9) == "VARIABLES" ) then
      NewLine   =   "VARIABLES="
      do i = 1,This%NVar
        NewLine =   NewLine//trim(This%VarNames(i))
        if ( i /= This%NVar ) NewLine =   NewLine//","
      end do
      Line      =   NewLine
      call Logger%Write( "-> Line = ", trim(Line) )
      write(OutUnit,"(a)") trim(Line)
      cycle
    end if

    if ( Line(1:8) == "ZONETYPE" ) then
    call Logger%Write( "-> ZONETYPE: Line = ", trim(Line) )
      write(OutUnit,"(a)") trim(Line)
      cycle
    end if

    if ( Line(1:4) == "ZONE" ) then
      call Logger%Write( "-> ZONE: Line = ", trim(Line) )
      call Parse( Line, "VARLOCATION=", List )
      NewLine   =   List(1) // " VARLOCATION=("
      iIni      =   This%IdxNodeVar(1)
      iFin      =   This%IdxNodeVar(size(This%IdxNodeVar))
      NewLine   =   NewLine // "["//Convert_To_String(iIni)//"-" //Convert_To_String(iFin)//"]=NODAL,"
      iIni      =   This%IdxCellVar(1)
      iFin      =   This%IdxCellVar(size(This%IdxCellVar))
      NewLine   =   NewLine // "["//Convert_To_String(iIni)//"-" //Convert_To_String(iFin)//"]=CELLCENTERED)"
      Line      =   NewLine
      call Logger%Write( "-> Line = ", trim(Line) )
      write(OutUnit,"(a)") trim(Line)
      cycle
    end if



    if ( Line(1:12) == "SOLUTIONTIME" ) then
    call Logger%Write( "-> ZONETYPE: Line = ", trim(Line) )
      write(OutUnit,"(a)") trim(Line)
    end if


    do i = 1,This%NNodeVar
      do j = 1,This%NNodes
        write(OutUnit,"(1x,es16.9)") This%NodeVar(j,i)
      end do
    end do
    do i = 1,This%NCellVar
      do j = 1,This%NCells
        write(OutUnit,"(1x,es16.9)") This%CellVar(j,i)
      end do
    end do

    rewind(InpUnit)
    do i = 1,This%iLineConn
      read(InpUnit,*)
    end do

    do
      read(InpUnit,"(a)",iostat=ios) Line
      if (ios/=0) exit
      write(OutUnit,"(a)") trim(Line)
    end do

    exit

  end do

  close(InpUnit)
  close(OutUnit)

  call Logger%Exiting()

End Procedure



Module Procedure GetCellVarIndexFromTecplotFile
  integer                                                                 ::  i
  iVar    =   0
  do i = 1,This%NVar
    if ( trim(This%VarNamesCell(i)) /= trim(VarName) ) cycle
    iVar  =   i
    exit
  end do
End Procedure


! Module Procedure GetCellVar
!   integer                                                                 ::  i
!   i     =   This%GetCellVarIndex(VarName)
!   if ( i /= 0 ) then
!     Var =   This%CellVar(:,i)
!   else
!     allocate( Var(0,0) )
!   end if
! End Procedure

Module Procedure GetMatchingCellVarIndex

  use String_Library        ,only:  DoStartWith => StartWith
  use Utilities_Library     ,only:  AddElementToArray

  integer                                                                 ::  i

  allocate( iVar(0) )

  if ( present(StartWith) ) then
    do i = 1,This%NVar
      if ( DoStartWith( This%VarNamesCell(i), StartWith, Trimed=.True., CaseSensitive=.True. ) ) then
        call AddElementToArray( i, iVar )
      end if
    end do
  end if

End Procedure



End SubModule
