C *********************************************************************
	SUBROUTINE NANSYM(F,EPS,NSIG,N,X,ITMAX,WA,PAR,IER)
C *********************************************************************
	DIMENSION X(50),WA(1350),PAR(1)
	REAL*8 EPS,DELTA,XTEMP,PREC,ETA,TOP,RELCON,
     *	F,FMAX,PT1,P2,HOLD,H,FPLUS,DERMAX,TEST,X,WA,E,ZER0,PM1,PAR
	DATA PREC,DELTA/5.D-16,5.D-11/
	DATA ZER0,PM1,PT1,P2/0.D0,.1D0,.0001D0,.002D0/
	IER=0
	N2=N+N
	RELCON=10.D+0**(-NSIG)
	JTEST=1
	IERROR=0
	IPART=((N+2)*(N-1))/2
	ITMP=IPART+N
	LKSUB=ITMP+N
	DO 130 M=1,ITMAX
	  IQUIT=0
	  FMAX=ZER0
	  M1=M-1
	  K1=LKSUB+1
	  KMIN=LKSUB+N
	  XTEMP=ZER0
	  DO 5 J=K1,KMIN
		XTEMP=XTEMP+1.D0
		WA(J)=XTEMP
 5	  CONTINUE
	  K=1
 10	  IF (K.LE.1) GO TO 30
	  KMIN=K-1
	  KK=1
 15	  DO 25 K1=1,KMIN
		ISUB=K-K1
		MM=((ISUB-1)*(N2-ISUB))/2
		LIM=N-ISUB
                KPOINT=WA(LKSUB+ISUB)+PM1
                ISUB1=ISUB-1
		X(KPOINT)=ZER0
		DO 20 L1=1,LIM
		  JS1=ISUB1+L1
		  LKJSUB=LKSUB+JS1+1
		  IJ=MM+JS1
		  JPOINT=WA(LKJSUB)+PM1
		  X(KPOINT)=X(KPOINT)+WA(IJ)*X(JPOINT)
 20		CONTINUE
		X(KPOINT)=X(KPOINT)+WA(MM+N)
 25	 CONTINUE
 	 GO TO (30,45,105),KK
 30	 E=F(X,K,PAR)
	 FMAX=DMAX1(FMAX,DABS(E))
	 IF(DABS(E) .GE. EPS) GO TO 35
	 IQUIT=IQUIT+1
	 IF(IQUIT .EQ. N) GO TO 140
 35	 I=K
 40	 IP=IPART+I
	 ITEMP=WA(LKSUB+I)+PM1
	 HOLD=X(ITEMP)
	 ETA=.001D+00*DABS(HOLD)
	 IF (DABS(HOLD) .LT. PREC) ETA=DELTA
	 H=DMIN1(FMAX,ETA)
	 IF (H .LT. PREC) H=PREC
	 X(ITEMP)=HOLD+H
	 IF (K .LE. 1) GO TO 45
	 KK=2
	 GO TO 15
45	 FPLUS=F(X,K,PAR)
	 TOP=FPLUS-E
	 IF (DABS(TOP) .GE. PREC) GO TO 50
	 WA(IP)=ZER0
	 GO TO 55
50 	 WA(IP)=TOP/H
55	 X(ITEMP)=HOLD
	 I=I+1
	 IF (I .LE. N) GO TO 40
	 IF (K .LT. N) GO TO 60
	 IP=IPART+N
	 IF (DABS(WA(IP)) .EQ. ZER0) GO TO 80
	 X(ITEMP)=-E/WA(IP)+X(ITEMP)
	 GO TO 100
60 	 KL=LKSUB+K
	 LOOK=WA(KL)+PM1
	 KMAX=LOOK
	 IP=IPART+K
	 DERMAX=DABS(WA(IP))
	 KPLUS=K+1
	 DO 65 I=KPLUS,N
	   TEST=DABS(WA(IPART+I))
	   IF (TEST .LE. DERMAX) GO TO 65
	   DERMAX=TEST
	   KMAX=I
65	 CONTINUE
	 IF (LOOK .EQ. KMAX) GO TO 75
	 LKMAX=LKSUB+KMAX
	 WA(KL)=WA(LKMAX)
	 WA(LKMAX)=LOOK
	 IP=IPART+KMAX
	 XTEMP=WA(IP)
	 IPK=IPART+K
	 WA(IP)=WA(IPK)
	 WA(IPK)=XTEMP
	 IF (K .LT. 2) GO TO 75
	 KMIN=K-1
	 I1=0
	 DO 70 I=1,KMIN
	   L=((I1)*(N2-I))/2-1
	   J=L+KMAX
	   XTEMP=WA(J)
	   JJ=L+K
	   WA(J)=WA(JJ)
	   WA(JJ)=XTEMP
	   I1=I
70	 CONTINUE
75	IF (DABS(WA(IPART+K)) .NE. ZER0) GO TO 90
80	IF (IERROR .EQ. 1) GO TO 135
	DO 85 I=1,N
	  X(I)=0.9D0*X(I)+.12345D0
85	CONTINUE
	IERROR=1
	GO TO 105
90	L=((K-1)*(N2-K))/2
	KN=L+N
	I1=L-1
	WA(KN)=ZER0
	IPK=IPART+K
	DO 95 J=KPLUS,N
		JSUB=WA(LKSUB+J)+PM1
		JJ=I1+J
		IPJ=IPART+J
		WA(JJ)=-WA(IPJ)/WA(IPK)
		WA(KN)=WA(KN)+WA(IPJ)*X(JSUB)	
95	CONTINUE
	LK=WA(LKSUB+K)+PM1
	WA(KN)=(WA(KN)-E)/WA(IPK)+X(LK)
	K=K+1
	IF (K .LE. N) GO TO 10
C     
C       О Б Р A Т Н A Я  З A М Е Н A  Д Л Я  П О Л У Ч Е Н И Я
C	С Л Е Д У ` Щ Е Й  A П Р О К С И М A Ц И И  Х
100 	IF (N .EQ. 1) GO TO 105
	KMIN=N-1
	KK=3
	GO TO 15
105	IF (M .LE. 1) GO TO 120
C
C	П Р О В Е Р К A   С Х О Д И М О С Т И
	DO 110 I=1,N
	IF (DABS(WA(ITMP+I)-X(I)) .GT. DABS(X(I))*RELCON) GO TO 115
110 	CONTINUE
	JTEST=JTEST+1
	IF (JTEST-3) 120,140,140
115	JTEST=1
120	DO 125 I=1,N
		WA(ITMP+I)=X(I)
125	CONTINUE
130	CONTINUE
	M=ITMAX
	IER=129
	GO TO 240
135	IER=130
140	FMAX=ZER0
	TEST=1.0D+15
	IF (N .GT. 1) GO TO 145
	WA(IPART+2)=F(X,1,PAR)
	FMAX=DMAX1(FMAX,DABS(WA(IPART+2)))
	GO TO 155
145	DO 150 I=1,N
		IP=IPART+I
		WA(IP)=F(X,I,PAR)
		FMAX=DMAX1(FMAX,DABS(WA(IP)))
150 	CONTINUE
C
C	П Р О В Е Р К A  Р A В Н Ы  Л И  М A Л Ы Е  К О М П О Н Е Н Т Ы
C	Ф A К Т И Ч Е С К И  Н У Л `
155	K=1
	DO 160 I=1,N
		WA(I)=X(I)
		IF (DABS(X(I)) .GT. P2) GO TO 160
	K=2
		WA(I)=ZER0
160	CONTINUE
	IF (K .EQ. 1) GO TO 195
	KK=1
	GO TO 205
165	IF (FMAX .LT. TEST) GO TO 190
	DO 170 I=1,N
		X(I)=WA(I)
170	CONTINUE
	IF (N .GT. 1) GO TO 175
	WA(IPART+2)=WA(ITMP+2)
	GO TO 185
175	DO 180 I=1,N
		WA(IPART+I)=WA(ITMP)+I
180	CONTINUE
185	FMAX=TEST
C
C	П Р О В Е Р К A   Д Л Я   Ц Е Л Ы Х   К О М П О Н Е Н Т О В
190	K=1
195	ITEST=0
	DO 200 I=1,N
		WA(I)=X(I)
		IF (DABS(X(I)) .LE. P2) GO TO 200
		L=X(I)+PT1
		J=X(I)-PT1
		IF (L .EQ. J) GO TO 200
		WA(I)=ISIGN(1,J)*MAX0(IABS(L),IABS(J))
		K=2
200	CONTINUE
	IF (K .EQ. 1) GO TO 235
	KK=2
205	TEST=ZER0
	IF (N .GT. 1) GO TO 210
	WA(ITMP+2)=F(WA,1,PAR)
	TEST=DMAX1(TEST,DABS(WA(ITMP+2)))
	GO TO 220
210	DO 215 I=1,N
		IT=ITMP+I
		WA(IT)=F(WA,I,PAR)
		TEST=DMAX1(TEST,DABS(WA(IT)))
215	CONTINUE
220	GO TO (165,225),KK
225	IF (FMAX .LT. TEST) GO TO 235
	DO 230 I=1,N
		X(I)=WA(I)
230	CONTINUE
	ITEST=1
C	П Р О В Е Р К A   С Х О Д И М О С Т И
235	IF (FMAX .LT. EPS .OR. TEST .LT. EPS) IER=0
240	ITMAX=M1+1
9000	CONTINUE
	IF (IER .NE. 0) CALL SVERTS (IER,6HNANSYM)
9005	RETURN
	END
C ****************************************************************************
C		П О Д П Р О Г Р A М М A   SVERTS
C		СЕРВИСНЫЕ ПРОГРAММЫ
C ***************************************************************************
C
	SUBROUTINE SVERTS (IER,NAME)
	REAL ITYP(6,4)
	INTEGER WARN,WARF,TERM,PRINTR
	DOUBLE PRECISION NAME
	DIMENSION IBIT(4)
	EQUIVALENCE (IBIT(1),WARN),(IBIT(2),WARF),
     *  (IBIT(3),TERM)
	DATA ITYP/'ПРЕД','УПРЕ','ЖДЕН','ИЕ  ',
     *	'     ','    ',
     *  'ПРЕД','УПРЕ','ЖДЕН','ИЕ  ','(FIK','C.) ',
     *	'НЕУС','ТРAН','ИМAЯ','ОШИБ','КA  ','    ',
     *	'ОШИБ','КA Н','Е ОП','РЕДЕ','ЛЕНA','     '/,
     *	IBIT/32,64,128,0/
	DATA PRINTR/6/
	IER2=IER
	IF (IER2 .GE. WARN) GO TO 5
C	О Ш И Б К A   Н Е   О П Р Е Д Е Л Е Н A
	IER1=4
	GO TO 20
5	IF (IER2 .LT. TERM) GO TO 10
C	Н Е У С Т Р A Н И М A Я   О Ш И Б К A
	IER1=3
	GO TO 20
10	IF (IER2 .LT. WARF) GO TO 15
C	П Р Е Д У П Р Е Ж Д Е Н И Е  С  Ф И К С A Ц И Е Й 
	IER1=2
	GO TO 20
C	П Р Е Д У П Р Е Ж Д Е Н И Е
15	IER1=1
C	В Ы Д Е Л Е Н И Е  'N'
20	IER2=IER2-IBIT(IER1)
C	П Е Ч A Т Ь  С О О Б Щ Е Н И Я  О Б  О Ш И Б К Е
	WRITE (PRINTR,25) (ITYP(I,IER1),I=1,6),NAME,
     *  IER2,IER
25	FORMAT ('***SVERTS***',6A4,2X,A6,I2,' (IER=',
     *  I3,')')
	RETURN
	END
C ******************************************************************
	REAL FUNCTION AUX(X,K,PAR)
C ******************************************************************
	COMMON W(70,70),LY,KX,KG,KH,K1 /C/C(50,50),FT,PNL,KL
	COMMON /X/FX(25),KYML(50),NYML(50,10),KVD(50),NVDW(50,10)
     *  /A/Y(50) 
	INTEGER PNL
	REAL*8 FV(50),X(50),S,S1,NELIN,Y,PAR(1)
	IF(PNL.NE.0) CALL FNL(FT)
        PAR(1)=0.D0
	DO 61 I=1,LY
61	Y(I)=X(I)
		DO 64 I=1,KL
		S=0.
		KZ=KYML(I)
			DO 62 JZ=1,KZ
			J=NYML(I,JZ)
62			S=S+C(I,J)*X(J)
		KZ=KVD(I)
		IF(KZ.EQ.0) GO TO 64
		IJ=I+KX
			DO 63 JZ=1,KZ
			J=NVDW(I,JZ)
63			S=S+W(IJ,J)
64	FV(I)=S+FX(I)
	DO 65 I=1,KH
	L=KL+I
	S1=NELIN(I)
65	FV(L)=S1-X(L)
	AUX=FV(K)
	RETURN
	END
