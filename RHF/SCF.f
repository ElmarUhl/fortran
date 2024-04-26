C*************************************************************************
C
C  MINIMAL BASIS STO-3G CALCULATION ON HEH+
C
C  THIS IS A LITTLER DUMMY MAIN PROGRAM WHICH CALLS HFCALC
C 
C*************************************************************************
      implicit double precision (a-h,o-z)

      iop = 2
      n = 3
      r = 1.4632D0
      zeta1 = 2.0925D0
      zeta2 = 1.24D0
      zA = 2.0D0
      zB = 1.0D0

      call hfcalc(iop, n, r, zeta1, zeta2, zA, zB)

      end


      
C*************************************************************************
      subroutine hfcalc(iop, n, r, zeta1, zeta2, zA, zB)

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
      implicit double precision (a-h,o-z)

      if (iop .eq. 0) go to 20
      print 10, n, zA, zB
   10 format(1H1,2x, 4HSTO-, i1, 21HG FOR ATOMIC NUMBERS , F5.2, 5H AND ,
     $F5.2,/)

   20 continue
C     calculate all the one and two-electron integrals
      call intgrl(iop, n, r, zeta1, zeta2, zA, zB)

C     be inefficient and put all integrals in pretty arrays
c      call colect(iop, n, r, zeta1, zeta2, zA, zB)
C     perform the scf calculation
c      call scf(iop, n, r, zeta1, zeta2, zA, zB)
c      return
      end

c ***************************************************************************
      subroutine intgrl(iop, n, r, zeta1, zeta2, zA, zB)
c
c     calculates all the basica integrals needed for scf calculation
c
c ******************************************************************************
      implicit double precision (a-h, o-z)

      common/int/s12,t11,t12, t22, v11a, v12a, v22a, v11b, v12b, v22b,v1111, v2111, v2121, v2211, v2221, v2222
      dimension coef(3,3), expon(3,3),d1(3), a1(3), d2(3), a2(3)
      data pi/3.1415926536898/
c these are the contraction coefficients and exponents for a normalized 1s slater orbital with exponent 1.0 in terms
c of normalized 1s primitive gaussians
      data coef, expon/1.0D0,2*0.0D0,0.678914D0,0.430129D0,0.0D0,
     $ 0.444635D0,0.535328D0,0.154329D0,0.27095D0,2*0.0D0,0.151523D0,
     $ 0.851819D0, 0.0D0, 0.109818D0, 0.405771D0, 2.22766D0/
      R2=R*R

c      print *, coef
c      print *, expon
c      print *, r2

c     scale the exponents (A) of primitive gaussians
c     include normalization in contraction coefficients (D)
      do 10 i=1, n
        a1(i) = expon(i,n)*(zeta1**2)
        d1(i) = coef(i,n)*((2.0D0*a1(i)/pi)**0.75D0)
        a2(i) = expon(i,n)*(zeta2**2)
        d2(i) = coef(i,n)*((2.0D0*a2(i)/pi)**0.75D0)
   10 continue
      end
c     D and A are now the contraction coefficients and exponents
c     in terms of unnormalized primitive gaussians
      s12 = 0.0D0
      t11 = 0.0D0
      t12 = 0.0D0
      t22 = 0.0D0
      v11a = 0.0D0
      v12a = 0.0D0
      v22a = 0.0D0
      v11b = 0.0D0
      v12b = 0.0D0
      v22b = 0.0D0
      v1111 = 0.0D0
      v2111 = 0.0D0
      v2121 = 0.0D0
      v2211 = 0.0D0
      v2221 = 0.0D0
      v2222 = 0.0D0
c     calculate one-electron integrals
c     center A is first atom, center B is second atom
c     origin is on center A
C     v12a = off-diagonal nuclear attraction to center A, etc.
c      do 20 i=1, n
c            do 20 j=1, n
c     rap2 = squared distance between center A and cneter B, etc.
      RAP = A2(J)*R/(A1(1)+A2(J))
      RAP2 = RAP**2
      RBP2 = (R - RAP)**2
      S12 = S12 + S(A1(I),A2(J),R2)*D1(I)*D2(J)
      T11 = T11 + T(A1(I),A1(J),O.ODO)*D1(I)*DI(J)
      T12 = T12 + T(A1(I),A2(J),R2)*D1(I)*D2(J)
      T22 = T22 + T(A2(1),A2(J).0.ODO)*D2(I)*D2(J)
      V11A = V11A + V(A1(I),A1(J),O.ODO,O.ODO,ZA)*D1(1)*D1(J)
      V12A = V12A + V(A1(1),A2(J),R2,RAP2,ZA)*D1(I)*D2(J)
      V22A = V22A + V(A2(1),A2(J),O.ODO,R2,ZA)*D2(I)*D2(J)
      V11B = V11B + V(A1(I),A1(J),0.ODO,R2,ZB)*D1(I).D1(J)
      V12B = V12B + V(A1(I),A2(J),R2,RBP2,ZB)*D1(I)*D2(J)
      V22B = V22B + V(A2(I),A2(J).0.ODO,0.ODO,ZB)*D2(I)*D2(J)
   20 CONTINUE
C CALCULATE TWO-ELECTRON INTEGRALS
      DO 30 I = 1, N
      DO 30 J = 1, N
      DO 30 K = 1, N
      DO 30 L = 1, N
      RAP = A2(I)*R/(A2(I) + A1(J))
      RBP = R - RAP
      RAQ = A2(K)*R/(A2(K)+A1(L))
      RBQ = R - RAQ
      RPQ = RAP - RAQ
      RAP2 = RAP * RAP
      RBP2 = RBP * RBP
      RAQ2 = RAQ * RAQ
      RBQ2 = RBQ * RBQ
      RPQ2 = RPQ * RPQ
      V1111 = V1111+TWOE(A1(I),A1(J),A1(K),A1(L),0.ODO,0.ODO.0.ODO)
     $ *D1(I)*D1(J)*D1(K)*D1(L)
      V2111 = V2111+TWOE(A2(I),A1(J),A1(K),A1(L),R2,0.0DO,RAP2)
     $ *D2(I)*D1(J)*D1(K)*D1(L)
      V2121 = V2121+TWOE(A2(I),Al(J),A2(K),A1(L),R2,R2,RPQ2)
     $ *D2(I)*D1(J)*D2(K)*D1(L)
      V2211 = V2211+TWOE(A2(I),A2(J),A1(K),A1(L),0.ODO,0.ODO,RZ)
     $ *D2(I)*D2(J)*D1(K)*D1(L)
      V2221 = V2221+TWOE(A2(I),A2(J),A2(K),A1(L),O.ODO,R2,RBQ2)
     $ *D2(I)*D2(J)*D2(K)*D1(L)
      V2222 = V2222+TWOE(AZ(I),A2(J),A2(K),A2(L),0.ODO,0.ODO,0.000)
     $ *D2(I)*D2(J)*D2(K)*D2(L)
   30 CONTINUE
      IF (IOP .EQ. O) GO TO 90
      PRINT 40
   40 FORMAT(3X,1HR,10X,5HZETA1,6X,5HZETA2,6X,3HS12,8X,3HT11/)
      PRINT 50, R.ZETA1,ZETA2,S12,T11
   50 FORMAT(5F11.6//)
      PRINT 60
   60 FORMAT(3X,3HT12,8X,3HT22,8X,4HV11A,7X,4HV12A,7X,4HV22A/)
      PRINT 50, T12,T22,V11A,V12A,V22A
      PRINT 70
   70 FORMAT(3X,4HV11B,7X,4HV12B,7X,4HV22B,7X,5HV1111,6X,5HV2111/)
      PRINT 50, V11B,V12B,V22B,V1111,V2111
      PRINT 80
   80 FORMAT(3X,5HV2121,6X,6HV2211,6X,5HV2221,6X,5HV2222/)
      PRINT 50, V2121,V2211,V2221.V2222
   90 RETURN
      END
C*******************************************************************
      FUNCTION FO(ARG)
C
C     CALCULATES THE F FUNCTION
C     FO ONLY (S-TYPE ORBITALS)
C
C*******************************************************************
      IMPLICIT DOUBLE PRECISION(A-H.O-Z)
      DATA PI/3.1416926636898DO/
      IF (ARG .LT. 1.0D-6) GO TO 10
C     FO IN TERMS OF THE ERROR FUNCTION
      FO = DSQRT(PI/ARG)*DERF(DSQRT(ARG))/2.0DO
      GO TO 20
C     ASYMPTOTIC VALUE FOR SMALL ARGUMENTS
   10 FO = 1.0DO-ARG/3.0DO
   20 CONTINUE
      RETURN
      END
C********************************************************************
      FUNCTION DERF(ARG)
C
C     CALCULATES THE ERROR FUNCTION ACCORDING TO A RATIONAL
C     APPROXIMATION FROM M. ABRAMOWITZ AND I.A. STEGUN,
C     HANDBOOK OF MATHEMATICAL FUNCTIONS. DOVER.
C     ABSOLUTE ERROR IS LESS THAN 1.5.10..(-7)
C     CAN BE REPLACED BY A BUILT-IN FUNCTION ON SOME MACHINES
C
C*********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H.O-Z)
      DIMENSION A(5)
      DATA P/0.3275911DO/
      DATA A/0.254829592DO,-0.284496736DO,1.421413741DO,
     $ -1.463162027DO,1.061405429DO/
      T = 1.0DO/(1.0DO+P*ARG)
      TN = T
      POLY = A(1)*TN
      DO 10 I = 2, 5
      TN = TN*T
      POLY = POLY+A(I)*TN
   10 CONTINUE
      DERF = 1.0DO-POLY*DEXP(-ARG*ARG)
      RETURN
      END
C**********************************************************************
      FUNCTION S(A, B, RAB2)
C
C     CALCULATES OVERLAPS FOR UN-NORMALIZED PRIMITIVES
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA PI/3.1416928536898DO/
      S = (PI/(A+B))**1.5DO*DEXP(-A*B*RAB2/(A+B))
      RETURN
      END
C************************************************************************
      FUNCTION T(A, B, RAB2)
C
C     CALCULATES KINETIC ENERGY INTEGRALS FOR UN-NORMALIZED PRIMITIVES
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H.O-Z)
      DATA PI/3.141592663&898DO/
