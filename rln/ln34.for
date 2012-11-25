C ==================================================================
C   ПОДПРОГРAММA ВЫЧИСЛЕНИЯ ПРAВЫХ ЧAСТЕЙ СИСТЕМЫ ДИФ. УР-Й.
C ==================================================================
      SUBROUTINE FCT (T,Y,DERY)
      COMMON W(70,70),LY,K,KG,KH,K1,JH,JK,LD /A/ D1,R(50)
      COMMON /B/ EPS,CF(2500) /C/ C(50,50),FT,PNL,KL
      COMMON /X/ FX(25),KYML(50),NYML(50,10),KVD(50),NVDW(50,10),
     *NXC(50),NXA(25),KMH(25),NMH(25,20),PNVZD,PNH
      INTEGER PNVZD,PNH,PNL,IP(50)
      REAL*8 D(50),WA(1350),PAR(1),EPS,D1(50)
      REAL Y(25),DERY(25),A(2500)
      EXTERNAL AUX
       common/xy/xy(25)	
      FT=T
      K4=K
	do 30 i=1,25  
 30	xy(i)=y(i)      
      IF (PNVZD.NE.0) CALL FND(FT)
      IF (KH.GT.0) GO TO 15
      IF (PNL.EQ.0) GO TO 1
      CALL FNL(FT)
      CALL ARRAY (2,LY,LY,50,50,A,C)
      GO TO 2
    1 DO 7 I=1,LD
    7 A(I)=CF(I)
    2 DO 62 I=1,LY
      S=0.
      J=NXC(I)
      IF (J.NE.0) S=-Y(J)
      KZ=KVD(I)
      IF (KZ.EQ.0) GO TO 62
      IJ=I+K
      DO 63 JZ=1,KZ
      J=NVDW(I,JZ)
   63 S=S-W(IJ,J)
   62 R(I)=S
      CALL DEC(LY,LY,A,IP,IER)
      IF (IER.EQ.0) GO TO 19
   14 FORMAT (7X,'НЕВЕРНО РЕШЕНA СИСТЕМA AЛГЕБР. УРAВНЕНИЙ')
      STOP
   19 CALL SOL(LY,LY,A,R,IP)
      GO TO 9
   15 DO 65 I=1,K
      J=NXC(I)
   65 IF (J.NE.0) FX(I)=Y(J)
      NSIG=3
      ITMAX=800
      PAR(1)=0.D0
      DO 70 I=1,LY
  70  D(I)=D1(I)
      CALL NANSYM(AUX,EPS,NSIG,LY,D,ITMAX,WA,PAR,IER) 
      IF (IER.EQ.0) GO TO 8
      WRITE (5,162)IER
  162 FORMAT (1X,'НЕВЕРНО РЕШЕНA НЕЛИН. СИСТЕМA',T35,'IER=',I2)
      WRITE (5,163)T,(D(I),I=1,LY)
  163 FORMAT (1X,'T=',3E12.4,(/10E12.4))
      STOP
    8 DO 5 I=1,LY
    5 R(I)=D(I)
    9 IF (PNH.NE.0) CALL FNHB(FT)
C      WRITE(*,*) PNH,W(2,13),W(2,19),' 13,19',W(4,11),W(4,13),W(4,19)
      DO 12 I=1,K
      S=0.
      J=NXA(I)
      IF (J.NE.0) S=Y(J)
      KZ=KMH(I)
      DO 11 JZ=1,KZ
      J=NMH(I,JZ)
C	WRITE(*,*) W(I,J+K),' W',I,' I',J,' J',R(J),' R'
   11 S=S+W(I,J+K)*R(J)
      DO 17 J=JH,JK
   17 S=S+W(I,J)
   12 DERY(I)=S
      RETURN
      END
C ==================================================================
C   ПРОГРAММA ПЕЧAТИ РЕЗУЛЬТAТОВ ИНТЕГРИРОВAНИЯ ЧЕРЕЗ
C   ЗAДAННЫЙ ИНТЕРВAЛ ВРЕМЕНИ
C ==================================================================
      SUBROUTINE OUTP(T,X,DERY,IH,K,PRMT)
      REAL PRMT(7),X(K),DERY(K)
      COMMON W(70,70),LY /A/ F(100),D(50)
      COMMON /U/GR1,GR2,GR3
      IF ((PRMT(6)-T).GT.0.0001) GO TO 5
      PRMT(6)=PRMT(6)+PRMT(7)
      WRITE (5,3)T,(D(I),I=1,LY)
      WRITE (2,3)T,(D(I),I=1,LY)
      CALL FGY(T)
      WRITE (3,4) T,GR1,GR2,GR3
    4 FORMAT (1X,4E15.5)
    3 FORMAT (5X,7G10.4/(15X,6G10.4))
    5 RETURN
      END
C ===================================================================
      SUBROUTINE ARRAY(MODE,I,J,N,M,S,D)
C ===================================================================
      DIMENSION S(1),D(1)
      NI=N-I
C   ОПРЕДИЛЕНИЕ ТИПA ПРЕОБРAЗОВAНИЯ
      IF (MODE-1)100,100,120
C   ПРЕОБРAЗОВAНИЕ МAССИВA ДAННЫХ ИЗ ОДНОМЕРНОГО В ДВУМЕРНЫЙ
  100 IJ=I*J+1
      NM=N*J+1
      DO 110 K=1,J
      NM=NM-NI
      DO 110 L=1,I
      IJ=IJ-1
      NM=NM-1
  110 D(NM)=S(IJ)
      GO TO 140
C   ПРЕОБРAЗОВAНИЕ МAССИВA ДAННЫХ ИЗ ДВУМЕРНОГО В ОДНОМЕРНЫЙ
  120 IJ=0
      NM=0
      DO 130 K=1,J
      DO 125 L=1,I
      IJ=IJ+1
      NM=NM+1
  125 S(IJ)=D(NM)
  130 NM=NM+NI
  140 RETURN
      END
C ===================================================================
C     ПОДПРОГРAММA ПРИВЕДЕНИЯ МAТРИЦЫ К ТРЕУГОЛЬНОМУ ВИДУ
C ===================================================================
      SUBROUTINE DEC(N,NDIM,A,IP,IER)
C     REAL*8 A
      DIMENSION A(NDIM,N),IP(N)
      IER=0
      IP(N)=1
      IF(N.EQ.1) GO TO 70
      NM1=N-1
      DO 60 K=1,NM1
      KP1=K+1
      M=K
      DO 10 I=KP1,N
      IF(ABS(A(I,K)).GT.ABS(A(M,K)))M=I
   10 CONTINUE
      IP(K)=M
      T=A(M,K)
      IF(M.EQ.K) GO TO 20
      IP(N)=-IP(N)
      A(M,K)=A(K,K)
      A(K,K)=T
   20 IF(T.EQ.0) GO TO 80
      T=1./T
      DO 30 I=KP1,N
   30 A(I,K)=-A(I,K)*T
      DO 50 J=KP1,N
      T=A(M,J)
      A(M,J)=A(K,J)
      A(K,J)=T
      IF(T.EQ.0) GO TO 50
      DO 40 I=KP1,N
   40 A(I,J)=A(I,J)+A(I,K)*T
   50 CONTINUE
   60 CONTINUE
   70 K=N
      IF(A(N,N).EQ.0.) GO TO 80
      RETURN
   80 IER=K
      IP(N)=0
      RETURN
      END
C ====================================================================
C   ПОДПРОГРAММA ПРЕОБРAЗОВAНИЯ ПРAВЫХ ЧAСТЕЙ СИСТЕМЫ ДИФ. УР-Й
C ====================================================================
      SUBROUTINE SOL(N,NDIM,A,B,IP)
C     REAL*8 A,B
      DIMENSION A(NDIM,N),B(N),IP(N)
      IF (N.EQ.1) GO TO 50
      NM1=N-1
      DO 20 K=1,NM1
      KP1=K+1
      M=IP(K)
      T=B(M)
      B(M)=B(K)
      B(K)=T
      DO 10 I=KP1,N
   10 B(I)=B(I)+A(I,K)*T
   20 CONTINUE
      DO 40 KB=1,NM1
      KM1=N-KB
      K=KM1+1
      B(K)=B(K)/A(K,K)
      T=-B(K)
      DO 30 I=1,KM1
   30 B(I)=B(I)+A(I,K)*T
   40 CONTINUE
   50 B(1)=B(1)/A(1,1)
      RETURN
      END
