      PROGRAM RHF
C*************************************************************************
C
C     MINIMAL BASIS STO-3G CALCULATION ON HEH+
C
C     THIS IS A LITTLER DUMMY MAIN PROGRAM WHICH CALLS HFCALC
C 
C*************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      IOP = 2
      N = 3
      R = 1.4632D0
      ZETA1 = 2.0925D0
      ZETA2 = 1.24D0
      ZA = 2.0D0
      ZB = 1.0D0

      CALL HFCALC(IOP, N, R, ZETA1, ZETA2, ZA, ZB)

      END PROGRAM RHF
     

C*************************************************************************
      SUBROUTINE HFCALC(IOP, N, R, ZETA1, ZETA2, ZA, ZB)

C     DOES A HARTREE FOCK CALCULATION FOR A TWO ELECTRON DIATOMIC
C     USING THE 1S MINIMAL STO-NG BASIS SET
C     MINIMAL BASIS SET HAS BASIS FUNCTIONS 1 AND 2 ON NUCLEI A AND B
C
C     INTEGER OPTION PRINT
C     IOP = O NO PRINTING WHATSOEVER (TO OPTIMIZE EXPONENTS, SAY)
C     IOP = 1 PRINT ONLY CONVERGED RESULTS
C     IOP = 2 PRINT EVERY INTERATION
C     N STO-NG CALCULATION (N = 1,2 OR 3)
C     R BOND LENGTH (U)
C     ZETA1 SLATER ORBITAL EXPONENT (FUNCTION 1)
C     ZETA2 SLATER ORBITAL EXPONENT (FUNCTION 2)
C     ZA    ATOMIC NUMBER (ATOM A)
C     ZB    ATOMIC NUMBER (ATOM B)
C
C***************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)

      IF (IOP .EQ. 0) GO TO 20
      PRINT 10, N, ZA, ZB
   10 FORMAT(1H1, 2X, 4HSTO-, I1, 21HG FOR ATOMIC NUMBERS , F5.2,
     $ 5H AND, F5.2, /)
   20 CONTINUE
C     CALCULATE ALL THE ONE AND TWO-ELECTRON INTEGRALS
      CALL INTGRL(IOP, N, R, ZETA1, ZETA2, ZA, ZB)
C     BE INEFFICIENT AND PUT ALL INTEGRALS IN PRETTY ARRAYS
      CALL COLECT(IOP, N, R, ZETA1, ZETA2, ZA, ZB)
C     PERFORM THE SCF CALCULATION
      CALL SCF(IOP, N, R, ZETA, ZETA2, ZA, ZB)
      RETURN
      END

C***************************************************************************
      SUBROUTINE INTGRL(IOP, N, R, ZETA1, ZETA2, ZA, ZB)
C
C     CALCULATES ALL THE BASIC INTEGRALS NEEDED FOR SCF CALCULATION
C
C******************************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      COMMON/INT/S12, T11, T12, T22, V11A, V12A, V22A, V11B, V12B, V22B,
     $ V1111, V2111, V2121, V2211, V2221, V2222
      DIMENSION COEF(3,3), EXPON(3,3), D1(3), A1(3), D2(3), A2(3)
      DATA PI/3.145926536898D0/
C     THESE ARE THE CONTRACTION COEFFICIENTS AND EXPONENTS FOR A
C     NORMALIZED 1S SLATER ORBITAL WITH EXPONENT 1.0 IN TERMS
      DATA COEF, EXPON/1.0D0,2*0.0D0,0.678914D0,0.430129D0,0.0D0,
     $ 0.444635D0,0.535328D0,0.154329D0,0.27095D0,2*0.0D0,0.151523D0,
     $ 0.851819D0, 0.0D0, 0.109818D0, 0.405771D0, 2.22766D0/
      R2 = R*R
C     SCALE THE EXPONENTS (A) OF PRIMITIVE GAUSSIANS
C     INCLUDE NORMALIZATION IN CONTRACTION COEFFICIENTS (D)
      DO 10 I = 1, N
        A1(I) = EXPON(I,N)*(ZETA1**2)
        D1(I) = COEF(I,N)*((2.0D0*A1(I)/PI)**0.75D0)
        A2(I) = EXPON(I,N)*(ZETA2**2)
        D2(I) = COEF(I,N)*((2.0D0*A2(I)/PI)**0.75D0)
   10 CONTINUE
C     D AND A ARE NOW THE CONTRACTION COEFFICIENTS AND EXPONENTS
C     IN TERMS OF UNNORMALIZED PRIMITIVE GAUSSIANS
      S12 = 0.0D0
      T11 = 0.0D0
      T12 = 0.0D0
      T22 = 0.0D0
      V11A = 0.0D0
      V12A = 0.0D0
      V22A = 0.0D0
      V11B = 0.0D0
      V12B = 0.0D0
      V22B = 0.0D0
      V1111 = 0.0D0
      V2111 = 0.0D0
      V2121 = 0.0D0
      V2211 = 0.0D0
      V2221 = 0.0D0
      V2222 = 0.0D0
C     CALCULATE ONE-ELECTRON INTEGRALS
C     CENTER A IS FIRST ATOM, CENTER B IS SECOND ATOM
C     ORIGIN IS ON CENTER A
C     V12A = OFF-DIAGONAL NUCLEAR ATTRACTION TO CENTER A, ETC.
      DO 20 I = 1, N
      DO 201 J = 1, N
C     RAP2 = SQUARED DISTANCE BETWEEN CENTER A AND CNETER B, ETC.
      RAP = A2(J)*R/(A1(1) + A2(J))
      RAP2 = RAP**2
      RBP2 = (R - RAP)**2
      S12 = S12 + S(A1(I), A2(J), R2)*D1(I)*D2(J)
      T11 = T11 + T(A1(I), A1(J), 0.0D0)*D1(I)*D1(J)
      T12 = T12 + T(A1(I), A2(J), R2)*D1(I)*D2(J)
      T22 = T22 + T(A2(1), A2(J), 0.0D0)*D2(I)*D2(J)
      V11A = V11A + V(A1(I), A1(J), 0.0D0, 0.0D0, ZA)*D1(1)*D1(J)
      V12A = V12A + V(A1(1), A2(J), R2, RAP2, ZA)*D1(I)*D2(J)
      V22A = V22A + V(A2(1), A2(J), 0.0D0, R2,ZA)*D2(I)*D2(J)
      V11B = V11B + V(A1(I), A1(J), 0.0D0, R2, ZB)*D1(I)*D1(J)
      V12B = V12B + V(A1(I), A2(J), R2, RBP2, ZB)*D1(I)*D2(J)
      V22B = V22B + V(A2(I), A2(J), 0.0D0, 0.0D0, ZB)*D2(I)*D2(J)
  201 CONTINUE
   20 CONTINUE
C     CALCULATE TWO-ELECTRON INTEGRALS
      DO 303 I = 1, N
      DO 302 J = 1, N
      DO 301 K = 1, N
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
      V1111 = V1111 + TWOE(A1(I),A1(J),A1(K),A1(L),0.0D0,0.0D0,0.0D0)
     $ *D1(I)*D1(J)*D1(K)*D1(L)
      V2111 = V2111 + TWOE(A2(I),A1(J),A1(K),A1(L),R2,0.0D0,RAP2)
     $ *D2(I)*D1(J)*D1(K)*D1(L)
      V2121 = V2121 + TWOE(A2(I),A1(J),A2(K),A1(L),R2,R2,RPQ2)
     $ *D2(I)*D1(J)*D2(K)*D1(L)
      V2211 = V2211 + TWOE(A2(I),A2(J),A1(K),A1(L),0.0D0,0.0D0,RZ)
     $ *D2(I)*D2(J)*D1(K)*D1(L)
      V2221 = V2221 + TWOE(A2(I),A2(J),A2(K),A1(L),0.0D0,R2,RBQ2)
     $ *D2(I)*D2(J)*D2(K)*D1(L)
      V2222 = V2222 + TWOE(A2(I),A2(J),A2(K),A2(L),0.0D0,0.0D0,0.0D0)
     $ *D2(I)*D2(J)*D2(K)*D2(L)
   30 CONTINUE
  301 CONTINUE
  302 CONTINUE
  303 CONTINUE
      IF (IOP .EQ. 0) GO TO 90
      PRINT 40
   40 FORMAT(3X,1HR,10X,5HZETA1,6X,5HZETA2,6X,3HS12,8X,3HT11/)
      PRINT 50, R,ZETA1,ZETA2,S12,T11
   50 FORMAT(5F11.6//)
      PRINT 60
   60 FORMAT(3X,3HT12,8X,3HT22,8X,4HV11A,7X,4HV12A,7X,4HV22A/)
      PRINT 50, T12,T22,V11A,V12A,V22A
      PRINT 70
   70 FORMAT(3X,4HV11B,7X,4HV12B,7X,4HV22B,7X,5HV1111,6X,5HV2111/)
      PRINT 50, V11B,V12B,V22B,V1111,V2111
      PRINT 80
   80 FORMAT(3X,5HV2121,6X,6HV2211,6X,5HV2221,6X,5HV2222/)
      PRINT 50, V2121,V2211,V2221,V2222
   90 RETURN
      END

C*******************************************************************
      FUNCTION FO(ARG)
C
C     CALCULATES THE F FUNCTION
C     FO ONLY (S-TYPE ORBITALS)
C
C*******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI/3.1416926636898D0/
      IF (ARG .LT. 1.0D-6) GO TO 10
C     FO IN TERMS OF THE ERROR FUNCTION
      FO = DSQRT(PI/ARG)*DERF(DSQRT(ARG))/2.0D0
      GO TO 20
C     ASYMPTOTIC VALUE FOR SMALL ARGUMENTS
   10 FO = 1.0D0-ARG/3.0D0
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
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION A(5)
      DATA P/0.3275911D0/
      DATA A/0.254829592D0,-0.284496736D0,1.421413741D0,
     $ -1.463162027D0,1.061405429D0/
      T = 1.0D0/(1.0D0+P*ARG)
      TN = T
      POLY = A(1)*TN
      DO 10 I = 2, 5
      TN = TN*T
      POLY = POLY + A(I)*TN
   10 CONTINUE
      DERF = 1.0D0-POLY*DEXP(-ARG*ARG)
      RETURN
      END


C**********************************************************************
      FUNCTION S(A, B, RAB2)
C
C     CALCULATES OVERLAPS FOR UN-NORMALIZED PRIMITIVES
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI/3.1416928536898D0/
      S = (PI/(A+B))**1.5D0*DEXP(-A*B*RAB2/(A+B))
      RETURN
      END


C************************************************************************
      FUNCTION T(A, B, RAB2)
C
C     CALCULATES KINETIC ENERGY INTEGRALS FOR UN-NORMALIZED PRIMITIVES
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI/3.1416928536898D0/
      T = A*B/(A+B)*(3.0D0-2.0D0*A*B*RAB2/(A+B))*(PI/(A+B))**1.5D0
     $ *DEXP(-A*B*RAB2/(A+B))
      RETURN
      END


C***********************************************************************
      FUNCTION V(A,B,RAB2,RCP2,ZC)
C
C CALCULATES UN-NORMALIZED NUCLEAR ATTRACTION INTEGRALS
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI/3.1416926636898D0/
      V = 2.0D0*PI/(A+B)*FO((A+B)*RCP2)*DEXP(-A*B*RAB2/(A+B))
      V=-V*ZC
      RETURN
      END


C***********************************************************************
      FUNCTION TWOE(A,B,C,D,RAB2,RCD2,RPQ2)
C
C     CALCULATES TWO-ELECTRON INTEGRALS FOR UN-NORMALIZED PRIMITIVES
C     A,B,C,D ARE THE EXPONENTS ALPHA,BETA, ETC.
C     RAB2 EQUALS SQUARED DISTANCE BETWEEN CENTER A AND CENTER B, ETC.
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PI/3.1416928636898D0/
      TWOE = 2.0D0*(PI**2.5D0)/((A+B)*(C+0)*DSQRT(A+B+C+0))
     $ *FO((A+B)*(C+D)*RPQ2/(A+B+C+0))
     $ *DEXP(-A*B*RAB2/(A+B)-C*D*RCD2/(C+0))
      RETURN
      END


C***********************************************************************
      SUBROUTINE COLECT(IOP,N,R,ZETA1,ZETA2,ZA,ZB)
C
C     THIS TAKES THE BASIC INTEGRALS FROM COMMON AND ASSEMBLES THE
C     RELEVANT MATRICES, THAT IS S,H,X,XT, AND TWO-ELECTRON INTEGRALS
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON/MATRIX/S(2,2),X(2,2),XT(2,2),H(2,2),F(2,2),G(2,2),C(2,2),
     $ FPRIME(2,2),CPRIME(2,2),P(2,2),OLDP(2,2),TT(2,2,2,2),E(2,2)
      COMMON/INT/S12,T11,T12,T22,V11A,V12A,V22A,V11B,V12B,V22B,
     $ V1111,V2111,V2121,V2211,V2221,V2222
C     FORM CORE HAMILTONIAN
      H(1,1) = T11 + V11A + V11B
      H(1,2) = T12 + V12A + V12B
      H(2,1) = H(1,2)
      H(2,2) = T22 + V22A + V22B
C     FORM OVERLAP MATRIX
      S(1,1) = 1.0D0
      S(1,2) = S12
      S(2,1) = S(1,2)
      S(2,2) = 1.0D0
C     USE CANONICAL ORTHOGONALIZATION
      X(1,1) = 1.0D0/DSQRT(2.0D0*(1.0D0+S12))
      X(2,1) = X(1,1)
      X(1,2) = 1.0D0/DSQRT(2.0D0*(1.0D0-S12))
      X(2,2) = -X(1,2)
C     TRANSPOSE OF TRANSFORMATION MATRIX
      XT(1,1) = X(1,1)
      XT(1,2) = X(2,1)
      XT(2,1) = X(1,2)
      XT(2,2) = X(2,2)
C     MATRIX OF TWO-ELECTRON INTEGRALS
      TT(1,1,1,1) = V1111
      TT(2,1,1,1) = V2111
      TT(1,2,1,1) = V2111
      TT(1,1,2,1) = V2111
      TT(1,1,1,2) = V2111
      TT(2,1,2,1) = V2121
      TT(1,2,2,1) = V2121
      TT(2,1,1,2) = V2121
      TT(1,2,1,2) = V2121
      TT(2,2,1,1) = V2211
      TT(1,1,2,2) = V2211
      TT(2,2,2,1) = V2221
      TT(2,2,1,2) = V2221
      TT(2,1,2,2) = V2221
      TT(1,2,2,2) = V2221
      TT(2,2,2,2) = V2222
      IF (IOP .EQ. 0) GO TO 40
      CALL MATOUT(S, 2, 2, 2, 2, 4HS   )
      CALL MATOUT(X, 2, 2, 2, 2, 4HX   )
      CALL MATOUT(H, 2, 2, 2, 2, 4HH   )
      PRINT 10
   10 FORMAT(//)
      DO 30 I = 1, 2
      DO 31 J = 1, 2
      DO 32 K = 1, 2
      DO 33 L = 1, 2
      PRINT 20, I, J, K, L, TT(I, J, K, L)
   20 FORMAT(3X, 1H(, 4I2, 2H ), F10.6)
   33 CONTINUE
   32 CONTINUE
   31 CONTINUE
   30 CONTINUE
   40 RETURN
      END

C***********************************************************************
      SUBROUTINE SCF(IOP, N, R, ZETA1, ZETA2, ZA, Z8)
C
C     PERFORMS THE SCF ITERATIONS
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      COMMON/MATRIX/S(2,2), X(2,2), XT(2,2), H(2,2), F(2,2), G(2,2),
     $ C(2,2), FPRIME(2,2), CPRIME(2,2), P(2,2), OLDP(2,2),
     $ TT(2,2,2,2), E(2,2)
      DATA PI/3.1416926636898D0/
C     CONVERGENCE CRITERION FOR DENSITY MATRIX
      DATA CRIT/1.0D-41/
C     MAXIMUM NUMBER OF ITERATIONS
      DATA MAXIT/25/
C     ITERATION NUMBER
      ITER = 0
C     USE CORE-HAMILTONIAN FOR INITIAL GUESS AT F. I.E. (P-O)
      DO 11 I = 1, 2
      DO 10 J = 1, 2
      P(I,J) = 0.0D0
   10 CONTINUE
   11 CONTINUE
      IF (IOP .LT. 2) GO TO 20
      CALL MATOUT(P, 2, 2, 2, 2, 4HP   )
C     START OF ITERATION LOOP
   20 ITER = ITER + 1
      IF (IOP .LT. 2) GO TO 40
      PRINT 30, ITER
   30 FORMAT(/, 4X, 28HSTART OF ITERATION NUMBER = , I2)
   40 CONTINUE
C     FORM TUO-ELECTRON PART OF FOCR MATRIX FROM P
      CALL FORMG
!      IF (IOP .LT. 2) GO TO 60
      IF (IOP .LT. 2) GO TO 50
      CALL MATOUT(G, 2, 2, 2, 2, 4HG   )
   50 CONTINUE
C     ADD CORE HAMILTONIAN TO GET FOCK MATRIX
      DO 60 I = 1, 2
      DO 60 J = 1, 2
      F(I,J) = H(I,J) + G(I,J)
   60 CONTINUE
C     CALCULATE ELECTRONIC ENERGY
      EN = 0.0D0
      DO 70 I = 1, 2
      DO 70 J = 1, 2
      EN = EN + 0.5D0*P(I,J)*(H(I, J) + F(I,J))
   70 CONTINUE
      IF (IOP .LT. Z) GO TO 90
      CALL MATOUT(F, 2, 2, 2, 2, 4HF   )
      PRINT 80, EN
   80 FORMAT(///, 4X, 20HELECTRONIC ENERGY = , D20.12)
   90 CONTINUE
C     TRANSFORM FOCK MATRIX USING G FOR TEMPORARY STORAGE
      CALL MULT(F,X,G,2,2)
      CALL MULT(XT,G,FPRIME,2,2)
C     DIAGONALIZE TRANSFORMED FOCK MATRIX
      CALL DIAG(FPRIME,CPRIME,E)
C     TRANSFORM EIGENVECTORS TO GET MATRIX C
      CALL MULT(X,CPRIME,C,2,2)
C     FORM NEW DENSITY MATRIX
      DO 100 I = 1, 2
      DO 100 J = 1, 2
C     SAVE PRESENT DENSITY MATRIX
C     BEFORE CREATING NEW ONE
      OLDP(I,J) = P(I,J)
      P(I,J) = 0.0D0
      DO 100 K = 1, 1
      P(I,J) = P(I,J) + 2.0D0*C(I,K)*C(J,K)
  100 CONTINUE
      IF (IOP .LT. 2) GO TO 110
      CALL MATOUT(FPRIME,2,2,2,2,4HF'  )
      CALL MATOUT(CPRIME,2,2,2,2,4HC'    )
      CALL MATOUT(E,2,2,2,2,4HE   )
      CALL MATOUT(C,2,2,2,2,4HC   )
      CALL MATOUT(P,2,2,2,2,4HP   )
  110 CONTINUE
C     CALCULATE DELTA
      DELTA = 0.0D0
      DO 120 I = 1, 2
      DO 120 J = 1, 2
      DELTA = DELTA + (P(I,J) - OLDP(I,J))**2
  120 CONTINUE
      DELTA = DSQRT(DELTA/4.0D0)
      IF (IOP .EQ. 0) GO TO 140
      PRINT 130, DELTA
  130 FORMAT(/,4X,39HDELTA(CONVERGENCE OF DENSITY MATRIX) =
     $ F10.6,/)
  140 CONTINUE
C     CHECK FOR CONVERGENCE
      IF (DELTA .LT. CRIT) GO TO 160
C     NOT YET CONVERGED
C     TEST FOR MAXIMUM NUMBER OF ITERATIONS
C     IF MAXIMUM NUMBER NOT YET REACHED THEN
C     GO BACK FOR ANOTHER ITERATION
      IF (ITER .LT. MAXIT) GO TO 20
C     SOMETHING WRONG HERE
      PRINT 150
  150 FORMAT(4X,21HNO CONVERGENCE IN SCF)
      STOP
  160 CONTINUE
C     CALCULATION CONVERGED IF IT GOT HERE
C     ADD NUCLEAR REPULSION TO GET TOTAL ENERGY
      ENT = EN + ZA*ZB/R
      IF (IOP .EQ. 0) GO TO 180
      PRINT 170, EN, ENT
  170 FORMAT(//,4X,21HCALCULATION CONVERGED,
     $ //,4X,20HELECTRONIC ENERGY = ,D20.12,
     $ //,4X,20HTOTAL ENERGY =     ,D20.12)
  180 CONTINUE
      IF (IOP .NE. 1) GO TO 190
C     PRINT OUT THE FINAL RESULTS IF
C     HAVE NOT DONE SO ALREADY
      CALL MATOUT(G,2,2,2,2,4HG     )
      CALL MATOUT(F,2,2,2,2,4HF     )
      CALL MATOUT(E,2,2,2,2,4HE     )
      CALL MATOUT(C,2,2,2,2,4HC     )
      CALL MATOUT(P,2,2,2,2,4HP     )
  190 CONTINUE
C     PS MATRIX HAS MULLIKEN POPULATIONS
      CALL MULT(P,S,OLDP,2,2)
      IF (IOP .EQ. 0) GO TO 200
      CALL MATOUT(OLDP,2,2,2,2,4HPS    )
  200 CONTINUE
      RETURN
      END


C***********************************************************************
      SUBROUTINE FORMG
C
C     CALCULATES THE G MATRIX FROM THE DENSITY MATRIX
C     AND TWO-ELECTRON INTEGRALS
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      COMMON/MATRIX/S(2,2), X(2,2), XT(2,2), H(2,2), F(2,2), G(2,2),
     $ C(2,2), FPRIME(2,2), CPRIME(2,2), P(2,2), OLDP(2,2),
     $ TT(2,2,2,2),E(2,2)
      DO 10 I = 1, 2
      DO 10 J = 1, 2
      G(I,J) = 0.0D0
      DO 10 K = 1, 2
      DO 10 L = 1, 2
      G(I,J) = G(I,J) + P(K,L)*(TT(I,J,K,L) - 0.5D0*TT(I,L,K,J))
   10 CONTINUE
      RETURN
      END


C***********************************************************************
      SUBROUTINE DIAG(F, C, E)
C
C     DIAGONALIZES F TO GIVE EIGENVECTORS IN C AND EIGENVALUES IN E
C     THETA IS THE ANGLE DESCRIBING SOLUTION
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION F(2,2),C(2,2),E(2,2)
      DATA PI/3.1416928636898D0/
      IF (DABS(F(1,1)-F(2,2)) .GT. 1.0D-20) GO TO 10
C     HERE IS SYMMETRY DETERMINED SOLUTION (HOMONUCLEAR DIATOMIC)
      THETA = PI/4.0D0
      GO TO 20
   10 CONTINUE
C     SOLUTION FOR HETERONUCLEAR DIATOMIC
      THETA = 0.6D0*DATAN(2.0D0*F(1,2)/(F(1,1) - F(2,2)))
   20 CONTINUE
      C(1,1) = DCOS(THETA)
      C(2,1) = DSIN(THETA)
      C(1,2) = DSIN(THETA)
      C(2,2) = -DCOS(THETA)
      E(1,1) = F(1,1)*DCOS(THETA)**2 + F(2,2)*DSIN(THETA)**2
     $ + F(1,2)*DSIN(2.0D0*THETA)
      E(2,2) = F(2,2)*DCOS(THETA)**2 + F(1,1)*DSIN(THETA)*2
     $ -F(1,2)*DSIN(2.0D0*THETA)
      E(2,1) = 0.0D0
      E(1,2) = 0.0D0
C     ORDER EIGENVALUES AND EIGENVECTORS
      IF (E(2,2) .GT. E(1,1)) GO TO 30
      TEMP = E(2,2)
      E(2,2) = E(1,1)
      E(1,1) = TEMP
      TEMP = C(1,2)
      C(1,2) = C(1,1)
      C(1,1) = TEMP
      TEMP = C(2,2)
      C(2,2) = C(2,1)
      C(2,1) = TEMP
   30 RETURN
      END


C******************************************************************
      SUBROUTINE MULT(A,B,C,IM,M)
C
C     MULTIPLIES TWO SQUARE MATRICES A AND B TO GET C
C
C******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION A(IM,IM),B(IM,IM),C(IM,IM)
      DO 10 I = 1, M
      DO 10 J = L, M
      C(I,J) = 0.0D0
      DO 10 K = 1, M
   10 C(I,J) = C(I,J) + A(I,K)*B(K,J)
      RETURN
      END


C******************************************************************
      SUBROUTINE MATOUT (A, IM, IN, M, N, LABEL)
C
C     PRINT MATRICES OF SIZE M BY N
C
C******************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(IM,IN)
      IHIGH = 0
   10 LOW = IHIGH + 1
      IHIGH = IHIGH + 5
      IHIGH = MIN0(IHIGH, N)
      PRINT 20, LABEL, (I, I = LOW, IHIGH)
   20 FORMAT(///, 3X, 5H THE , A4, 6H ARRAY, /, 15X, 5(10X, I3, 6X)//)
      DO 30 I = 1, M
      PRINT 40, I, (A(I, J), J = LOW, IHIGH)
   40 FORMAT(I10, 5X, 5(1X, D18.10))
   30 CONTINUE
      IF (N-IHIGH) 50, 50, 10
   50 RETURN
      END