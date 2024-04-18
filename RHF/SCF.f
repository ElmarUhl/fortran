C*************************************************************************
C
C  MINIMAL BASIS STO-3G CALCULATION ON HEH+
C
C  THIS IS A LITTLER DUMMY MAIN PROGRAM WHICH CALLS HFCALC
C 
C*************************************************************************
c      implicit double precision (a-h,o-z)
      iop = 2
      n = 3
      r = 1.4632D0
      zeta1 = 2.0925D0
      zeta2 = 1.24D0
      zA = 2.0D0
      zB = 1.0D0

c      print *, iop, n, r, zeta1, zeta2, zA, zB

      call hfcalc(iop, n, r, zeta1, zeta2, zA, zB)
      end


      
C*************************************************************************
      subroutine hfcalc(iop, n, r, zeta1, zeta2, zA, zB)
c      print *, iop, n, r, zeta1, zeta2, zA, zB

C     DOES A HARTREE FOCK CALCULATION FOR A TWO ELECTRON DIATOMIC
C     USING THE 1S MINIMAL STO-NG BASIS SET
C     MINIMAL BASIS SET HAS BASIS FUNCTIONS 1 AND 2 ON NUCLEI A AND B
C
C     INTEGER OPTION PRINT
C     IOP=O NO PRINTING WHATSOEVER (TO OPTIMIZE EXPONENTS, SAY)
C     IOP=1 PRINT ONLY CONVERGED RESULTS
C     IOP=2 PRINT EVERY INTERATION
C     N STO-NG CALCULATION (N=1,2 OR 3)
C     R BOND LENGTH (U)
C     ZETA1 SLATER ORBITAL EXPONENT (FUNCTION 1)
C     ZETA2 SLATER ORBITAL EXPONENT (FUNCTION 2)
C     ZA    ATOMIC NUMBER (ATOM A)
C     ZB    ATOMIC NUMBER (ATOM B)
C
C***************************************************************************
c      implicit double precision (a-h,o-z)
      if (iop .eq. 0) go to 20
      print 10, n, zA, zB
c   10 format(1H1,2X,4HSTO-,i1,21HG FOR ATOMIC NUMBERS , F5.2, 5H AND , F5.2)
   10 format(1H1,2x, 4HSTO-, i1, 21HG FOR ATOMIC NUMBERS , F5.2, F5.2)

   20 continue
C     calculate all the one and two-electron integrals
c      call intgrl(iop, n, r, zeta1, zeta2, zA, zB)
C     be inefficient and put all integrals in pretty arrays
c      call colect(iop, n, r, zeta1, zeta2, zA, zB)
C     perform the scf calculation
c      call scf(iop, n, r, zeta1, zeta2, zA, zB)
c      return
      end