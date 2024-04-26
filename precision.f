      PROGRAM precision

      implicit none

      integer, parameter :: ikind = selected_real_kind(p=15)
      real :: X, Y, Z
      real(kind=ikind) :: rX, rY, rZ
      double precision :: doubleX, doubleY, doubleZ

C     Calculate in single precision
      Y = 1.0
      X = 3.0
      Z = Y/X
      print *, 'The value calculated of Z in single precision is ', Z

C     Calculate in double precision
      doubleY = 1.0
      doubleX = 3.0
      doubleZ = doubleY/doubleX
      print *, 'The value calculated of Z in double precision is ',
     $ doubleZ

C     Calculate in modified precision
      rY = 3.0
      rX = 1.0
      rZ = rY/rX
      print *, 'The value calculated of Z in modified precision is',
     $ rZ

      end PROGRAM precision