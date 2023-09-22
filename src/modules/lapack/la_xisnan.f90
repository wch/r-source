# 1 "SRC/la_xisnan.F90"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 399 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "SRC/la_xisnan.F90" 2
module LA_XISNAN
   interface LA_ISNAN

   module procedure SISNAN
   module procedure DISNAN

   end interface

contains
   
   logical function SISNAN( x )
   use LA_CONSTANTS, only: wp=>sp





   real(wp) :: x





   sisnan = SLAISNAN(x,x)

   contains
   logical function SLAISNAN( x, y )
   use LA_CONSTANTS, only: wp=>sp
   real(wp) :: x, y
   SLAISNAN = ( x.ne.y )
   end function SLAISNAN

   end function SISNAN

   logical function DISNAN( x )
   use LA_CONSTANTS, only: wp=>dp





   real(wp) :: x





   DISNAN = DLAISNAN(x,x)

   contains
   logical function DLAISNAN( x, y )
   use LA_CONSTANTS, only: wp=>dp
   real(wp) :: x, y
   DLAISNAN = ( x.ne.y )
   end function DLAISNAN

   end function DISNAN

end module LA_XISNAN

