SubModule(TecplotLine_Class) TecplotLine_SubClass

  use Logger_Class        ,only:  Logger, LogLevel_NOLOGS, LogLevel_DEBUG

  implicit none


  contains

Module Procedure InitializeTecplotLine

  use String_Library        ,only:  Convert_To_Integer, VecTrim, GetFormat, EmptyString, Parse, GetSubString
  use File_Library          ,only:  FileExist
  use Error_Class           ,only:  Error
  use Utilities_Library     ,only:  GetOptArgValue

  character(*)                                              ,parameter::  ProcName = 'InitializeTecplotLine'
  integer                                                   ,parameter::  NCol = 5
  integer                                                             ::  ios
  integer                                                             ::  iz, iline
  integer   ,allocatable                                              ::  NRows(:), NLin2(:)
  integer                                                             ::  NPts, j, k, iVar, iini, ifin
  integer                                                             ::  NPassiveVar
  character(1000)                                                     ::  Line
  character(:)  ,allocatable                                          ::  Fi, LineParts(:), String
  logical       ,allocatable                                          ::  PassiveVar(:)

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )

  This%FileName   =   FileName
  This%OneFilePerZone =   GetOptArgValue(.False.,OneFilePerZone)
  call Logger%Write( "This%FileName = ", This%FileName )
  call Logger%Write( "This%OneFilePerZone = ", This%OneFilePerZone )

  if ( .Not. FileExist(This%FileName) ) call Error%Raise( "Input file '"//This%FileName//"' not found" )
  open(NewUnit=This%Unit,File=This%FileName,Status="OLD", Action="READ")

  call Logger%Write( "Reading variables names" )
  call ReadVarNames( This, LogLevel )
  call Logger%Write( "-> This%NVar      = ", This%NVar )
  call Logger%Write( "-> This%VarNames  = ", This%VarNames )

!   call Logger%Write( "Getting number of variables" )
!   This%NVar   =   0
!   call EmptyString(This%VarNames)
!   do
!     read(This%Unit,"(a)",iostat=ios) Line
!     if (ios/=0) exit
!     if ( Line(1:11) /= "VARIABLES =" ) cycle
!     j = index(Line,"=")+2
!     This%NVar  = 1
!     j = index(Line,"=")+2
!     VarNames_(This%NVar) = Line(j:)
!     do
!       read(This%Unit,"(a)",iostat=ios) Line
!       if ( Line(1:1) /= '"' ) exit
!       This%NVar = This%NVar + 1
!       VarNames_(This%NVar) = Line
!     end do
!   end do
! !   allocate( character(100) :: This%VarNames(This%NVar) )
!   This%VarNames    =   VecTrim( VarNames_(1:This%NVar) )
!   call Logger%Write( "-> Number of variables: This%NVar     = ", This%NVar )
!   call Logger%Write( "-> Names of variables:  This%VarNames  = ", This%VarNames )




  call Logger%Write( "Getting number of zones" )
  rewind(This%Unit)
  This%NumZones     =   0
  do
    read(This%Unit,"(a)",iostat=ios) Line
    if (ios/=0) exit
    if ( Line(1:4) /= "ZONE" ) cycle
    This%NumZones   =   This%NumZones + 1
  end do
  allocate( This%Np(This%NumZones) )
  allocate( NRows(This%NumZones) )
  allocate( NLin2(This%NumZones) )
  call Logger%Write( "-> This%NumZones = ", This%NumZones )






  call Logger%Write( "Getting number of points per zone" )
  rewind(This%Unit)
  This%Np   =   0
  iz          =   0
  do
    read(This%Unit,"(a)",iostat=ios) Line
    if (ios/=0) exit
    if ( Line(1:4) == "ZONE" ) iz = iz + 1
    if ( Line(1:3) == " I=" ) then
      j = index(Line,"=")+1
      k = index(Line,",")-1
      NPts          =   Convert_To_Integer( Line(j:k) )
      This%Np(iz) =   NPts
      NLin2(iz)     =   mod(NPts,NCol)
      NRows(iz)     =   ( NPts - NLin2(iz) ) / NCol
    end if
  end do
  if ( Logger%On() ) then
    Fi    =   GetFormat( maxval([maxval(This%Np),maxval(NRows),maxval(NLin2)]) )
    call Logger%Write( "-> This%Np  = ", This%Np, Fi=Fi )
    call Logger%Write( "-> NRows    = ", NRows  , Fi=Fi )
    call Logger%Write( "-> NLin2    = ", NLin2  , Fi=Fi )
  end if
  NPts    =   maxval(This%Np)
  allocate( This%VarData(NPts,This%NVar,This%NumZones) )




  call Logger%Write( "Getting data" )

  allocate( PassiveVar(This%NVar) )

  rewind(This%Unit)
  iz      =   0
  iline   =   0

  do
    iline = iline + 1
    read(This%Unit,"(a)",iostat=ios) Line
    if (ios/=0) exit
!     if ( Line(1:4) == "ZONE" ) iz = iz + 1

    if ( Line(1:5)  /= " DT=(" ) then
      cycle
    end if
!     call Logger%Write( "-> Line = ", trim(Line), Debug=.True. )
    iz = iz + 1

    PassiveVar    =   .False.

    NPassiveVar   =   0
    do

      read(This%Unit,"(a)",iostat=ios) Line
      call Parse( Line, "=", LineParts )
!       call Logger%Write( "*** -> Line = ", trim(Line), Debug=.True. )
!       call Logger%Write( "*** -> size(LineParts)  = ", size(LineParts) , Debug=.True. )


      if ( size(LineParts) == 1 ) then
        backspace(This%Unit)
        exit
      end if

!     Getting the number of passive variables
      k   =   index(Line,"PASSIVEVARLIST")
      if ( k /= 0 ) then
!         call Logger%Write( "-> PASSIVEVARLIST", Debug=.True. )
        String    =   GetSubString( Line, InBetween=["[","]"] )
        call Parse( String, "=", LineParts )
        NPassiveVar   =   size(LineParts)
        do k = 1,NPassiveVar
          iVar    =   Convert_To_Integer( LineParts(k) )
          PassiveVar(iVar)  = .True.
        end do
      end if
    end do


!     do iVar = 1,This%NVar
!       call Logger%Write( "-> iVar = ", iVar, "Name = ", This%VarNames(iVar), "Passive = ", PassiveVar(iVar), Debug=.True. )
!     end do



    do iVar = 1,This%NVar
      if ( PassiveVar(iVar) ) cycle
      do k = 1,NRows(iz)
        iini    =   (k-1) * NCol + 1
        ifin    =   iini  + NCol - 1
        iline   =   iline + 1
!         call Logger%Write( "-> reading line: [iline,iini,ifin,iVar,iz] = ", [iline,iini,ifin,iVar,iz], Debug=.True. )
        read(This%Unit,*) This%VarData(iini:ifin,iVar,iz)
      end do
      iini    =   ifin + 1
      ifin    =   iini + NLin2(iz) - 1
      if ( NLin2(iz) /= 0 ) then
        iline   =   iline + 1
!         call Logger%Write( "-> reading line: [iline,iini,ifin,iVar,iz] = ", [iline,iini,ifin,iVar,iz], Debug=.True. )
        read(This%Unit,*) This%VarData(iini:ifin,iVar,iz)
      end if
    end do
  end do
  close(This%Unit)

  call Logger%Exiting()

End Procedure

Module Procedure WriteGnuplotFile

  use File_Library          ,only:  AddFileSuffix
  use String_Library        ,only:  Convert_To_String, Convert_Ratio
  use Utilities_Library     ,only:  GetOptArgValue

  implicit none

  character(*)                                              ,parameter::  ProcName = 'WriteGnuplotFile'
  integer                                                             ::  Unit, i, k, iz, NVar
  integer       ,allocatable                                          ::  iVar(:)
  character(:)  ,allocatable                                          ::  OutputFile, BaseName
  character(:)  ,allocatable                                          ::  VarNames_(:)

  call Logger%Entering( ProcName )


  BaseName    =   GetOptArgValue(This%FileName,FileName)
  OutputFile  =   BaseName
  call Logger%Write( "BaseName = ", BaseName )

  call Logger%Write( "Setting list of variables to write" )
  if ( present(VarNames) ) then
    allocate( VarNames_, source = VarNames )
    iVar    =   This%GetVarsIndex(VarNames)
  else
    allocate( VarNames_ , source = This%VarNames )
    allocate( iVar      , source = [(i,i=1,This%NVar)] )
  end if
  NVar    =   size(iVar)
  call Logger%Write( "-> NVar      = ", NVar      )
  call Logger%Write( "-> VarNames_ = ", VarNames_ )
  call Logger%Write( "-> iVar      = ", iVar      )

!   This%OneFilePerZone

  do iz = 1,This%NumZones
    if ( This%NumZones /= 1 ) OutputFile =  AddFileSuffix(BaseName,Convert_To_String(iz),Separator="-")
    call Logger%Write( "Opening output file for zone " //Convert_Ratio(iz,This%NumZones)//": '"//OutputFile//"'" )
    open( NewUnit = Unit, File = OutputFile, Status = "REPLACE" , Action = "WRITE" )

    write(Unit,"('#',*(1x,a15))") ( trim(This%VarNames(iVar(k))) , k=1,NVar )
    call Logger%Write( "-> Writing data" )
    do i = 1,This%Np(iz)
      write(Unit,"(1x,*(1x,es15.8))") ( This%VarData(i,iVar(k),iz) , k=1,NVar )
    end do
    close(Unit)
  end do

  call Logger%Exiting()

End Procedure



Module Procedure GetVarIndexFromName
  use String_Library      ,only:  RemoveQuotes, Equal
  integer                                                               ::  i
  logical                                                               ::  CaseSensitive_
  CaseSensitive_ = .False.; if ( present(CaseSensitive) ) CaseSensitive_ = CaseSensitive
  iVar    =   0
  do i = 1,This%NVar
    if ( .Not. Equal( VarName, RemoveQuotes(This%VarNames(i)), Trimed=.True., CaseSensitive=CaseSensitive_ ) ) cycle
    iVar  =   i
    exit
  end do
End Procedure

Module Procedure GetVarIndexFromNames
  use String_Library      ,only:  RemoveQuotes
  integer                                                               ::  k
  iVar    =   0
  do k = 1,size(VarNames)
    iVar  =   This%GetVarIndex( trim(VarNames(k)), CaseSensitive=CaseSensitive )
    if ( iVar /= 0 ) exit
  end do
End Procedure

Module Procedure GetVarsIndexFromNames
  use String_Library      ,only:  RemoveQuotes
  integer                                                               ::  k
  allocate( iVar(size(VarNames)) )
  iVar    =   0
  do k = 1,size(VarNames)
    iVar(k)   =   This%GetVarIndex( trim(VarNames(k)), CaseSensitive=CaseSensitive )
  end do
End Procedure


Module Procedure GetVarFromName
  use Utilities_Library     ,only:  GetOptArgValue
  integer                                                               ::  iV, iZ
  iZ    =   GetOptArgValue( 1, iZone )
  iV    =   This%GetVarIndex( VarName, CaseSensitive=CaseSensitive )
  if ( present(iVar) ) iVar = iV
  if ( iV /= 0 ) then
    allocate( Var , source = This%VarData(:,iV,iZ) )
  end if
End Procedure






Module Procedure AddVarFromValueName

  use Utilities_Library     ,only:  AddElementToArray, GetOptArgValue

  character(*)                                              ,parameter  ::  ProcName = 'AddVarFromValueName'
  integer                                                               ::  iP, iV, iZ
  integer                                                               ::  iV_new, iZ_new
  integer                                                               ::  Nz, Nv, Np
  real(8)       ,allocatable                                            ::  VarData(:,:,:)  ! Dim=(NPts,This%NVar,This%NumZones) )

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )

  if ( Logger%On() ) then
    call Logger%Write( "Adding new variable to TecplotLine object" )
    call Logger%Write( "-> Name = ", Name )
    call Logger%Write( "-> iZ   = ", GetOptArgValue( 1, iZone ) )
    call Logger%Write( "-> Np   = ", size(Value) )
  end if

  Nz      =   This%NumZones
  Nv      =   This%NVar
  Np      =   maxval(This%Np)
  iV_new  =   This%GetVarIndex( Name )
  iZ_new  =   GetOptArgValue( 1, iZone )

  if ( iV_new == 0 ) then
    call Logger%Write( "Variable '"//Name//"' does not exist" )
    iV_new  =   Nv + 1
    VarData =   This%VarData
    deallocate( This%VarData )
    allocate( This%VarData(Np,Nv+1,Nz) )
    call Logger%Write( "Copying old data" )
    do iZ = 1,Nz
      do iV = 1,Nv
        do iP = 1,Np
          This%VarData(iP,iV,iZ)   =   VarData(iP,iV,iZ)
        end do
      end do
    end do
    call AddElementToArray( Name , This%VarNames )
    This%NVar   =   size(This%VarNames)
  else
    call Logger%Write( "Variable '"//Name//"' already exists" )
  end if
  call Logger%Write( "-> Writing new variable at posiiton: iV_new = ", iV_new )

  call Logger%Write( "Adding new data" )
  call Logger%Write( "-> iZ_new = ", iZ_new )
  call Logger%Write( "-> iV_new = ", iV_new )
  do iP = 1,Np
    This%VarData(iP,iV_new,iZ_new)   =   Value(iP)
  end do

  call Logger%Exiting()

End Procedure


Module Procedure AddVarFromNamePrefix

  character(*)                                              ,parameter  ::  ProcName = 'AddVarFromNamePrefix'

  integer       ,allocatable                                            ::  VarIdx(:)
  real(8)       ,allocatable                                            ::  OldValue(:,:) ! Dim=(Np,NVar)
  real(8)       ,allocatable                                            ::  NewValue(:)   ! Dim=(Np)

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )

  call Logger%Write( "Creating a new variables called '"//Name//"' by summing all variable strating by '"//PrefixVarToSum//"'" )

  call Logger%Write( "-> Calling This%GetVarsProperties" )
  call This%GetVarsProperties(                    &
          Value             =   OldValue        , &
          VarNameStartWith  =   PrefixVarToSum  , &
          iVar              =   VarIdx            )
  call Logger%Write( "-> VarIdx = ", VarIdx )

!   call Logger%Write( "-> Calling This%RemoveVar" )
!   call This%RemoveVar(            &
!           VarIdx    =   VarIdx  , &
!           iZone     =   1       , &
!           LogLevel  =   LogLevel  )

  if ( size(VarIdx) > 0 ) then
    call Logger%Write( "-> Calling This%AddVar" )
    NewValue    =   sum(OldValue,2)
    call This%AddVar( NewValue, Name, iZone=1 )
  else
    call Logger%Write( "-> No variable to add" )
  end if

  call Logger%Exiting()

End Procedure








! TODO: Only done on zone 1
Module Procedure RemovePoints
  use String_Library        ,only:  Convert_To_String
  use Utilities_Library     ,only:  RemoveElementFromArray
  character(*)                                              ,parameter  ::  ProcName = 'RemovePoints'
  integer                                                               ::  i, k, l, N
  integer ,dimension(size(Idx))                                         ::  Idx_
  character(:)  ,allocatable                                            ::  Name
  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )

  Idx_  =   Idx
  N     =   size(Idx_)
  call Logger%Write( "Removing "//Convert_To_String(N)//" points" )


  do k = 1,N
    i   =   Idx_(k)
    if ( (i<0) .or. (i>This%Np(1)) ) cycle
!     call Logger%Write( "-> k = ", k, "i = ", i, "Data = ", This%VarData(i,33:,1), Fi="i3", Fr="es15.8" )
    call RemoveElementFromArray( This%VarData, Idx=i, Dim=1 )
    This%Np(1)  =   This%Np(1) - 1
    do l = k+1,N
      if ( i <= Idx_(l) ) Idx_(l) = Idx_(l) - 1
    end do
  end do

  call Logger%Exiting()
End Procedure


Module Procedure RemoveVarNamesPrefix
  use String_Library        ,only:  RemovePrefix, VecTrim
  character(*)                                              ,parameter  ::  ProcName = 'RemoveVarNamesPrefix'
  integer                                                               ::  iV, N
  character(:)  ,allocatable                                            ::  Name
  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )
  do iV = 1,This%NVar
    Name    =   trim(This%VarNames(iV))
    This%VarNames(iV)   =   RemovePrefix(Name,Prefix)
    if ( len_trim(Name) /= len_trim(This%VarNames(iV)) ) &
    call Logger%Write( "-> iV = ", iV, "VarName = '"//Name//"' => '"//trim(This%VarNames(iV))//"'")
  end do
  This%VarNames   =   VecTrim(This%VarNames)
  call Logger%Exiting()
End Procedure

! TODO: Not strinclty correct... if prefix appear at other places than the start of the string, it will also be remove... ok for now
Module Procedure RemoveVarNamesQuotes
  use String_Library        ,only:  RemoveQuotes, VecTrim
  integer                                                               ::  iV
  do iV = 1,This%NVar
    This%VarNames(iV)  =   RemoveQuotes(This%VarNames(iV))
  end do
  This%VarNames   =   VecTrim(This%VarNames)
End Procedure


Module Procedure RemoveVarFromIndexes

  use Utilities_Library     ,only:  GetOptArgValue

  character(*)                                              ,parameter  ::  ProcName = 'RemoveVarFromIndexes'
  integer                                                               ::  i, j, iV, jV
  integer                                                               ::  VarIdx_(size(VarIdx))
  integer                                                               ::  NVarOld, NVarNew

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )

  if ( Logger%On() ) then
    call Logger%Write( "Removing variable from index" )
    call Logger%Write( "-> Index of variables to removed:             VarIdx = ", VarIdx )
    call Logger%Write( "-> Zone number where var. need to be removed: iZ     = ", GetOptArgValue( 1, iZone ) )
  end if

  VarIdx_   =   VarIdx

  do i = 1,size(VarIdx_)
    iV    =   VarIdx_(i)
    if ( (iV<=0) .or. (iV>This%NVar) ) cycle
    call Logger%Write( "-> Processing element: i = ", i, "iV = ", iV, "This%VarNames(iV) = ", trim(This%VarNames(iV)) )
    NVarOld   =   This%NVar
    call This%RemoveVar(iV,LogLevel=LogLevel)
    if ( NVarOld /= This%NVar ) then ! If variable has been removed, then update the index of the variables which still need to be removed
      do j = i+1,size(VarIdx_)
        jV    =   VarIdx_(j)
        if ( (jV<=0) .or. (jV>This%NVar) ) cycle
        if ( (jV<iV) ) cycle
        VarIdx_(j)    =   VarIdx_(j) - 1
!         call Logger%Write( "    -> Updating index of variable still to be removed: j = ", j, "jV = ", jV, "=> ", VarIdx_(j) )
      end do
    end if
  end do

  call Logger%Exiting()

End Procedure


Module Procedure RemoveVarFromIndex

  use Utilities_Library     ,only:  RemoveElementFromArray

  character(*)                                              ,parameter  ::  ProcName = 'RemoveVarFromIndex'
  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )
  if ( (VarIdx<=0) .or. (VarIdx>This%NVar) ) then
    call Logger%Write( "-> Index out of range => Exiting" )
    call Logger%Exiting()
    return
  end if

  This%NVar   =   This%NVar - 1
  call RemoveElementFromArray( This%VarData , VarIdx, Dim=2 )
  call RemoveElementFromArray( This%VarNames, VarIdx )

  call Logger%Exiting()
End Procedure



Module Procedure InvertTecplotLine
  integer                                                               ::  iZ, iV, Np
  Np    =   maxval(This%Np)
  do iZ = 1,This%NumZones
    do iV = 1,This%NVar
      This%VarData(1:Np,iV,iZ)   =   This%VarData(Np:1:-1,iV,iZ)
    end do
  end do
End Procedure




Module Procedure GetMatchingVarIndex
  use String_Library        ,only:  DoStartWith => StartWith, RemoveQuotes
  use Utilities_Library     ,only:  AddElementToArray
  use String_Library      ,only:
  integer                                                                 ::  i
  allocate( iVar(0) )
  if ( present(StartWith) ) then
    do i = 1,This%NVar
      if ( DoStartWith( RemoveQuotes(This%VarNames(i)), StartWith, Trimed=.True., CaseSensitive=.True. ) ) then
        call AddElementToArray( i, iVar )
      end if
    end do
  end if
End Procedure


! Module Procedure GetVarsProperties_1d
!
!   use Utilities_Library     ,only:  AddElementToArray
!   use String_Library        ,only:  EmptyString, RemoveQuotes
!
!   integer                                                   ,parameter  ::  iZ = 1
!   integer                                                               ::  i, j, Nv, Np
!   integer     ,allocatable                                              ::  ListIndex(:)
!
!   Np    =   This%Np(iZ)
!   Nv    =   0
!
! !   if ( present(VarNameStartWith) ) then
! !
! !     call This%GetMatchingVarIndex( ListIndex, StartWith=VarNameStartWith )
! !     Nv  =   size(ListIndex)
! !
! !     if ( present(iVar) ) then
! !       allocate( iVar, source = ListIndex )
! !     end if
! !
! !     if ( present(Value) ) then
! !       allocate( Value(Np,Nv) )
! !       do i = 1,Nv
! !         j           =   ListIndex(i)
! !         Value(:,i)  =   This%VarData(:,j,1)
! !       end do
! !     end if
! !
! !     if ( present(Names) ) then
! !       do i = 1,Nv
! !         j       =   ListIndex(i)
! !         VarName_ =   trim(RemoveQuotes(This%VarNames(j)))
! !         call AddElementToArray( VarName_, Names )
! !       end do
! !     end if
! !
! !   end if
!
!   if ( present(VarName) ) then
!
!     call This%GetMatchingVarIndex( ListIndex, StartWith=VarName )
!     Nv  =   size(ListIndex)
!
!     if ( present(iVar) ) then
!       allocate( iVar, source = ListIndex )
!     end if
!
!     if ( present(Value) ) then
!       allocate( Value(Np,Nv) )
!       do i = 1,Nv
!         j           =   ListIndex(i)
!         Value(:,i)  =   This%VarData(:,j,1)
!       end do
!     end if
!
!     if ( present(Names) ) then
!       do i = 1,Nv
!         j       =   ListIndex(i)
!         call AddElementToArray( trim(RemoveQuotes(This%VarNames(j))), Names )
!       end do
!     end if
!
!   end if
!
!   if ( present(iVar) ) iVar =  0
!
!   if ( present(Value) ) then
!     if ( .Not. allocated(Value) ) allocate( Value(Np) )
!   end if
!
!   if ( present(Names) ) then
!     if ( .Not. allocated(Names) ) call EmptyString(Names)
!   end if
!
! End Procedure

Module Procedure GetVarsProperties_2d

  use Utilities_Library     ,only:  AddElementToArray, GetOptArgValue
  use String_Library        ,only:  EmptyString, RemoveQuotes

  integer                                                               ::  i, j, iZ, Nv, Np
  integer     ,allocatable                                              ::  ListIndex(:)

  iZ    =   GetOptArgValue( 1, iZone )
  Np    =   This%Np(iZ)
  Nv    =   0

  if ( present(VarNameStartWith) ) then
    call This%GetMatchingVarIndex( ListIndex, StartWith=VarNameStartWith )
  else if ( present(VarNames) ) then
    ListIndex   =   This%GetVarsIndex( VarNames, CaseSensitive=CaseSensitive )
  end if

  if ( allocated(ListIndex) ) then
    Nv  =   size(ListIndex)

    if ( present(Value) ) then
      allocate( Value(Np,Nv) )
      do i = 1,Nv
        j           =   ListIndex(i)
        Value(:,i)  =   This%VarData(:,j,1)
      end do
    end if

    if ( present(Names) ) then
      do i = 1,Nv
        j       =   ListIndex(i)
        call AddElementToArray( trim(RemoveQuotes(This%VarNames(j))), Names )
      end do
    end if

  end if




  if ( present(iVar) ) then
    if ( allocated(ListIndex) ) allocate( iVar, source = ListIndex )
    if ( .Not. allocated(iVar) )  allocate( iVar(Nv) )
  end if

  if ( present(Value) ) then
    if ( .Not. allocated(Value) ) allocate( Value(Np,Nv) )
  end if

  if ( present(Names) ) then
    if ( .Not. allocated(Names) ) call EmptyString(Names)
  end if

End Procedure













Subroutine ReadVarNames( This, LogLevel )
  use String_Library      ,only:  EmptyString
  use Utilities_Library   ,only:  AddElementToArray
  class(TecplotLine_Type)                               ,intent(inout)::  This
  integer                                     ,optional ,intent(in)   ::  LogLevel
  character(*)                                              ,parameter::  ProcName = 'ReadVarNames'
  integer                                                             ::  ios, j
  character(1000)                                                     ::  Line
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



Module Procedure AddNewZone

  character(*)                                              ,parameter::  ProcName = 'AddNewZone'

  call Logger%Entering( ProcName, LogLevel=LogLevel, DefLogLevel=LogLevel_NOLOGS, MsgLogLevel=LogLevel_DEBUG )

  if ( This%NumZones == 0 ) then
    This%Unit           =   TecplotLine%Unit
    This%NumZones       =   TecplotLine%NumZones
    This%NVar           =   TecplotLine%NVar
    This%Np             =   TecplotLine%Np
    This%VarData        =   TecplotLine%VarData
    This%VarNames       =   TecplotLine%VarNames
    This%FileName       =   TecplotLine%FileName
    This%OneFilePerZone =   TecplotLine%OneFilePerZone
  call Logger%Exiting()
    return
  end if



  call Logger%Write( "-> This%NumZones  = ", This%NumZones )
  call Logger%Write( "-> This%NVar      = ", This%NVar )
  call Logger%Write( "-> This%Np        = ", This%Np )

  call Logger%Write( "-> TecplotLine%NumZones  = ", TecplotLine%NumZones )
  call Logger%Write( "-> TecplotLine%NVar      = ", TecplotLine%NVar )
  call Logger%Write( "-> TecplotLine%Np        = ", TecplotLine%Np )

  This%NumZones               =   This%NumZones + TecplotLine%NumZones

!     integer                     ::  NVar  = 0       ! Number of variables
!     integer       ,allocatable  ::  Np(:)           ! Number of points per zone Dim=(Nz)
!     real(8)       ,allocatable  ::  VarData(:,:,:)  ! Dim=(NPts,This%NVar,This%NumZones) )
!     character(:)  ,allocatable  ::  VarNames(:)
!     character(:)  ,allocatable  ::  FileName
!     logical                     ::  OneFilePerZone  = .False.
!
!
  call Logger%Exiting()

End Procedure
!



End SubModule