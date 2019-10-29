SubModule(GPF_Parameters_Class) GPF_Parameters_SubClass

! @TODO: Some prameters are related to Graph objects... and so there will be a problem is a figure is made of several graphs

  implicit none

  contains

Module Procedure GPFParametersConstructor
  call This%Initialize()
End Procedure

Module Procedure Initialize_Parameters
  This%i_Directory              =  .False.
  This%i_FileName_Prefix        =  .False.
  This%i_File_Extension         =  .False.
  This%i_X_Label                =  .False.
  This%i_Y_Label                =  .False.
  This%i_Z_Label                =  .False.
  This%i_X_LogScale             =  .False.
  This%i_Y_LogScale             =  .False.
  This%i_Z_LogScale             =  .False.
  This%i_X_Format               =  .False.
  This%Group_Name               =  ""
  This%Directory                =  ""
  This%FileName_Prefix          =  ""
  This%FileName_Suffix          =  ""
  This%File_Extension           =  ""
  This%X_Label                  =  ""
  This%Y_Label                  =  ""
  This%Z_Label                  =  ""
  This%X_LogScale               =  .False.
  This%Y_LogScale               =  .False.
  This%Z_LogScale               =  .False.
  This%X_Format                 =  ""
  allocate( This%X_2D(0,0) )
  allocate( This%Y_2D(0,0) )
End Procedure

! This Passed-object dummy argument must have the "inout" attribut otherwise each call to this procedure reset the component to the default values.
Module Procedure Set_Parameters

  if ( present(Group_Name) )    This%Group_Name         =  Group_Name

  if ( present(Directory) ) then
    This%i_Directory            =  .True.
    This%Directory              =  Directory
  end if

  if ( present(FileName_Prefix) ) then
    This%i_FileName_Prefix      =  .True.
    This%FileName_Prefix        =  FileName_Prefix
  end if

  if ( present(FileName_Suffix) ) then
    This%FileName_Suffix        =  FileName_Suffix
  end if

  if ( present(File_Extension) ) then
    This%i_File_Extension       =  .True.
    This%File_Extension         =  File_Extension
  end if

  if ( present(X_Label) ) then
    This%i_X_Label              =  .True.
    This%X_Label                =  X_Label
  end if

  if ( present(Y_Label) ) then
    This%i_Y_Label              =  .True.
    This%Y_Label                =  Y_Label
  end if

  if ( present(Z_Label) ) then
    This%i_Z_Label              =  .True.
    This%Z_Label                =  Z_Label
  end if

  if ( present(X_LogScale) ) then
    This%i_X_LogScale           =  .True.
    This%X_LogScale             =  X_LogScale
  end if

  if ( present(Y_LogScale) ) then
    This%i_Y_LogScale           =  .True.
    This%Y_LogScale             =  Y_LogScale
  end if

  if ( present(Z_LogScale) ) then
    This%i_Z_LogScale           =  .True.
    This%Z_LogScale             =  Z_LogScale
  end if

  if ( present(X_Format) ) then
    This%i_X_Format             =  .True.
    This%X_Format               =  X_Format
  end if

  if ( present(X_2D) ) then
    if ( allocated(This%X_2D) ) deallocate(This%X_2D)
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( This%X_2D(size(X_2D,1),size(X_2D,2)) )
!     This%X_2D = X_2D
    allocate( This%X_2D(size(X_2D,1),size(X_2D,2)), source = X_2D )
#else
    allocate( This%X_2D, source = X_2D )
#endif
  end if

  if ( present(Y_2D) ) then
    if ( allocated(This%Y_2D) ) deallocate(This%Y_2D)
#ifdef WORKAROUND_GFORTRAN_SOURCE_ALLOCATION
!     allocate( This%Y_2D(size(Y_2D,1),size(Y_2D,2)) )
!     This%Y_2D = Y_2D
    allocate( This%Y_2D(size(Y_2D,1),size(Y_2D,2)), source = Y_2D )
#else
    allocate( This%Y_2D , source = Y_2D )
#endif
  end if

End Procedure

Module Procedure Get_Group_Name
  Group_Name    =  This%Group_Name
End Procedure

Module Procedure GetDirectory
  Directory     =  This%Directory
End Procedure

Module Procedure GetTitle
  Title   =   BaseTitle
  if ( len_trim(This%Group_Name) /= 0 ) Title = Title // " ("//trim(This%Group_Name)// ")"
End Procedure

Module Procedure GetFileName
  if ( present(BaseName) ) then
    FileName    =   This%Get_FileName_Prefix()        //  &
                    BaseName                          //  &
                    This%Get_FileName_Suffix()
  else
    FileName    =   This%Get_FileName_Prefix()        //  &
                      This%Get_FileName_Suffix()
  end if
  if ( This%Get_File_Extension() /= '' ) then
    FileName    =   FileName // "." // This%Get_File_Extension()
  end if
End Procedure

Module Procedure Get_FileName_Prefix
  FileName_Prefix       =  This%FileName_Prefix
End Procedure

Module Procedure Get_FileName_Suffix
  FileName_Suffix       =  This%FileName_Suffix
End Procedure

Module Procedure Get_File_Extension
  File_Extension        =  This%File_Extension
End Procedure

Module Procedure Get_X_Label
  X_Label               =  This%X_Label
End Procedure

Module Procedure Get_Y_Label
  Y_Label               =  This%Y_Label
End Procedure

Module Procedure Get_X_LogScale
  X_LogScale            =  This%X_LogScale
End Procedure

! PROBLEM: This Pure Function only work if This%X_2D is allocated
Pure Module Function Get_X_2D( This ) result(X_2D)
  class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)     ,dimension(size(This%X_2D,1),size(This%X_2D,2))         ::  X_2D
  X_2D                  =  This%X_2D
End Function

Pure Module Function Get_Y_2D( This ) result(Y_2D)
  class(GPF_Parameters_Type)                            ,intent(in)     ::  This                            !< Passed-object dummy argument
  real(rkp)     ,dimension(size(This%Y_2D,1),size(This%Y_2D,2))         ::  Y_2D
  Y_2D                  =  This%Y_2D
End Function

End SubModule