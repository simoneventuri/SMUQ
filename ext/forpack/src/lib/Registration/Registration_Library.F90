Module Registration_Library

  use Registration_Class            ,only:  RegisterObject_Type, RegisterList_Type, RegisterObjectsList
  use RegisterObjectFactory_Class   ,only:  RegisterObjectFactory_Type

  implicit none

  private

  public  ::  RegisterObjectsList
  public  ::  RegisterObject_Type
  public  ::  RegisterList_Type
  public  ::  RegisterObjectFactory_Type

End Module