@test
subroutine Test_StringClass_Assign()

   use String_Library   ,only:  String_Type
   use pfunit_mod

   implicit none

   type(String_Type)    ::  String, StringFound

   character(:) ,allocatable  ::  Initial
   character(:) ,allocatable  ::  Obtained
   character(:) ,allocatable  ::  Expected

   Initial    =   '0123456789'
   Expected   =   '9876543210'
   String     =   Initial
   StringFound=   String%Reverse()
   @assertEqual(StringFound//'',Expected)

   Initial    =   'abcdefghijklmnopqrstuvwxyz'
   Expected   =   'zyxwvutsrqponmlkjihgfedcba'
   String     =   Initial
   StringFound=   String%Reverse()
   @assertEqual(StringFound//'',Expected)

end subroutine
