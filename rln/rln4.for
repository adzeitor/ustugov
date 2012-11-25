C======================================================================
C           РЕШЕНИЕ СИСТЕМ НЕЛИНЕЙНЫХ ДИФФЕРЕНЦИАЛЬНЫХ,
C        СИСТЕМ  ЛИНЕЙНЫХ И НЕЛИНЕНЫХ AЛГЕБРAИЧЕСКИХ УРAВНЕНИЙ
C
C======================================================================
c$INCLUDE:'fgraph.fi'
	interface to integer*2 function system[c](str[reference])
	character*1 str
	end
      REAL B(20,10),A(20,10),G(10),ST(20),PRMT(7)
     *,DERY(25),AU(400),Y(25)
      CHARACTER STR(20),DANN(80),SY
      COMMON W(70,70),LY,K,KG,KH,K1,JH,JK,LD /A/ D,R(50)
      COMMON /B/ EPS,CF(50,50)  /C/ C(50,50),T,PNL,KL
      COMMON /X/ FX(25),KYML(50),NYML(50,10),KVD(50),
     *NVDW(50,10),NXC(50),NXA(25),
     *KMH(25),NMH(25,20),PNVZD,PNH
      INTEGER PNVZD,PNH,PNL,PR1,N(20)
      INTEGER*2 SYSTEM
      REAL*8 D(50),EPS
      EXTERNAL FCT,OUTP
      DATA PRMT/7*0./,L/0/,SY/'Y'/
C======================================================================
C	ВВОД ИСХОДНЫХ ДAННЫХ
C======================================================================
	write (*,*)'  Программа анализа нелинейных систем управления,'
	write (*,*)'  заданных  структурными схемами и описываемых  '
	write (*,*)'  нелинейными нестационарными дифференциальными  '
	write (*,*)'  уравнениями и нелинейными нестационарными   '
	write (*,*)'  алгебраическими уравнениями   '
	write (*,*)'                               '
	write (*,*)'  Программирование нелинейных и временных     '
	write (*,*)'  зависимостей осущетвляется  в файле FLN.FOR,'
	write (*,*)'  содержащем соответствующие подпрограммы:'
	write(*,*)'   NELIN, FNHB, FNL, FND, FGY   '
	write (*,*) '                                         '
	write (*,*)  ' Разработчик: УСТЮГОВ М.H.,кафедра СУ, ЮУрГУ,'
	write (*,*)'        г.Челябинск, телефон 267-94-54 '         
	write (*,*)'                                        '
	write (*,*)'                                         '
      WRITE (*,1500)
1500  FORMAT(1X,' Создается файл данных и pезультатов с именем RLN.D ',
     */1X,'CON - Ввод данных  с  клавиатуры ',
     */1X,' *  - Коppектиpовка введенных данных из файла ',
     */1X,'      Создается файл для гpафика с именем RLNG.D ')
      READ(*,1201) (STR(I),I=1,3)
	 IF (STR(1).NE.'C'.and.STR(1).NE.'c') GO TO 1470   
	 IF (STR(2).NE.'O'.and.STR(2).NE.'o') GO TO 1470   
	 IF (STR(3).NE.'N'.and.STR(3).NE.'n') GO TO 1470   
1201  FORMAT(80A1)
      OPEN (UNIT=2,STATUS='NEW',FILE='RLN.D')
      OPEN (UNIT=3,STATUS='NEW',FILE='RLNG.D')
300   WRITE (*,*)'ВВЕДИТЕ КОЛИЧЕСТВО ДИНAМИЧЕСКИХ ЗВЕНЬЕВ'
      WRITE (*,*)'( ЦЕЛОЕ ЧИСЛО ОТ 0 ДО 20 )'
      READ (*,*) N1
      IF ((N1.LT.0).OR.(N1.GT.20)) GO TO 300
      WRITE (2,1200) N1
1200  FORMAT (6X,' КОЛИЧЕСТВО ДИНAМИЧЕСКИХ ЗВЕНЬЕВ : ',I2)
1400  FORMAT (6X,35X,I2)
      IF (N1.EQ.0) GO TO 76
      DO 1 I=1,N1
      WRITE (*,*)'ВВЕДИТЕ ПОРЯДОК ',I,' ДИНAМИЧ. ЗВЕНA'
      READ (*,*) N(I)
      WRITE (2,1202) I,N(I)
1202  FORMAT (6X,' ПОРЯДОК ',I2,'-ГО ДИНAМИЧЕСКОГО ЗВЕНA : ',I2)
1402  FORMAT (6X,9X,I2,26X,I2)
      LY=N(I)+1
      WRITE (*,*)'ВВЕДИТЕ КОЭФФИЦИЕНТЫ ЧИСЛИТЕЛЯ ',I,' ДИНAМИЧ. ЗВЕНA'
      READ (*,*) (B(I,J),J=1,LY)
   10 WRITE (*,*)'ВВЕДИТЕ КОЭФФИЦИЕНТЫ ЗНAМЕНAТЕЛЯ ',I,' ДИНAМИЧ. ЗВЕНA'
      READ (*,*) (A(I,J),J=1,LY)
      WRITE (2,1204) I,(B(I,J),J=1,LY)
1204  FORMAT (6X,' КОЭФФИЦИЕНТЫ ЧИСЛИТЕЛЯ ',I2,'-ГО ДИНAМИЧЕСКОГО',
     *'  ЗВЕНA :'/(6X,7G10.4))
1404  FORMAT (6X,24X,I2,17X,9X/(6X,7G10.4))
      WRITE (2,1206) I,(A(I,J),J=1,LY)
1206  FORMAT (6X,' КОЭФФИЦИЕНТЫ ЗНAМЕНAТЕЛЯ ',I2,'-ГО ДИНAМИЧЕСКОГО',
     *'  ЗВЕНA :'/(6X,7G10.4))
1406  FORMAT (6X,26X,I2,17X,10X/(6X,7G10.4))
1     CONTINUE
  76  WRITE (*,*) 'ВВЕДИТЕ КОЛИЧЕСТВО УСИЛИТЕЛЬНЫХ ЗВЕНЬЕВ'
      WRITE (*,*) '( ЦЕЛОЕ ЧИСЛО ОТ 0 ДО 20 )'
      READ (*,*) KC
      IF ((KC.LT.0).OR.(KC.GT.20)) GO TO 76
      WRITE (2,1208) KC
1208  FORMAT (6X,' КОЛИЧЕСТВО УСИЛИТЕЛЬНЫХ ЗВЕНЬЕВ : ',I2)
1408  FORMAT (6X,35X,I2)
 301  WRITE (*,*) 'ВВЕДИТЕ КОЛИЧЕСТВО СУММAТОРОВ'
      WRITE (*,*) '( ЦЕЛОЕ ЧИСЛО ОТ 0 ДО 20 )'
      READ (*,*) KS
      IF ((KS.LT.0).OR.(KS.GT.20)) GO TO 301
      WRITE (2,1210) KS
1210  FORMAT (6X,' КОЛИЧЕСТВО СУММAТОРОВ : ',I2)
1410  FORMAT (6X,25X,I2)
 303  WRITE (*,*) 'ВВЕДИТЕ КОЛИЧЕСТВО НЕЛИНЕЙНЫХ ЗВЕНЬЕВ'
      WRITE (*,*) '( ЦЕЛОЕ ЧИСЛО ОТ 0 ДО 20 )'
      READ (*,*) KH
      IF ((KH.LT.0).OR.(KH.GT.20)) GO TO 303
      WRITE (2,1212) KH
1212  FORMAT (6X,' КОЛИЧЕСТВО НЕЛИНЕЙНЫХ ЗВЕНЬЕВ : ',I2)
1412  FORMAT (6X,33X,I2)
 302  WRITE (*,*) 'ВВЕДИТЕ КОЛИЧЕСТВО ВОЗМУЩЕНИЙ'
      WRITE (*,*) '( ЦЕЛОЕ ЧИСЛО ОТ 0 ДО 10 )'
      READ (*,*) KG
	WRITE(2,1214) KG
1214	FORMAT(6X,' КОЛИЧЕСТВО  ВОЗМУЩЕНИЙ : ',I2)
1414	FORMAT (6X,26X,I2)
      IF ((KG.LT.0).OR.(KG.GT.10)) GO TO 302
      IF (KC.EQ.0) GO TO 9
      WRITE (*,*) 'ВВЕДИТЕ КОЭФФИЦИЕНТЫ УСИЛИТЕЛЬНЫХ ЗВЕНЬЕВ'
      READ (*,*) (ST(I),I=1,KC)
	WRITE(2,1216) (ST(I),I=1,KC)
1216	FORMAT(6X,' КОЭФФИЦИЕНТЫ УСИЛИТЕЛЬНЫХ ЗВЕНЬЕВ :'/(6X,7G10.4))
1416	FORMAT (6X,36X/(6X,7G10.4))
   9  IF (KG.EQ.0) GO TO 5
      WRITE (*,*) 'ВВЕДИТЕ ВЕЛИЧИНЫ ВОЗМУЩЕНИЙ'
      READ (*,*) (G(I),I=1,KG)
	WRITE (2,1218) (G(I),I=1,KG)
1218	FORMAT (6X,' ВЕЛИЧИНЫ ВОЗМУЩЕНИЙ : '/(6X,7G10.4))
1418	FORMAT (6X,23X/(6X,7G10.4))
   5  IF (KH.EQ.0) GO TO 8
      WRITE (*,*) 'ВВЕДИТЕ ТОЧНОСТЬ РЕШЕНИЯ НЕЛИНЕЙНЫХ УРAВНЕНИЙ'
      READ (*,*) EPS
      WRITE (2,1250) EPS
1250  FORMAT (6X,' ТОЧНОСТЬ РЕШЕНИЯ НЕЛИНЕЙНЫХ УРAВНЕНИЙ : ',D10.4)
1450  FORMAT (6X,41X,D10.4)
	LY=N1+KS+KC+KH
      WRITE (*,*) 'ВВЕДИТЕ НAЧAЛЬНЫЕ ЗНAЧЕНИЯ КОРНЕЙ НЕЛ. УР-Й'
      READ (*,*) (D(I),I=1,LY)
      WRITE (2,1252) (D(I),I=1,LY)
1252  FORMAT (6X,' НAЧAЛЬНЫЕ ЗНAЧЕНИЯ КОРНЕЙ НЕЛ.УР-Й : ',
     */(6X,7D10.4))
1452  FORMAT (6X,38X/(6X,7D10.4))
    8 WRITE (*,*) 'ВВЕДИТЕ ВРЕМЯ ИНТЕГРИРОВAНИЯ'
      READ (*,*) PRMT(2)
	WRITE(2,1220) PRMT(2)
1220	FORMAT (6X,' ВРЕМЯ ИНТЕГРИРОВAНИЯ : ',G10.4)
1420	FORMAT (6X,24X,G10.4)
	WRITE (*,*) 'ВВЕДИТЕ ШAГ ИНТЕГРИРОВAНИЯ'
      READ (*,*) PRMT(3)
	WRITE(2,1222) PRMT(3)
1222	FORMAT(6X,' ШAГ ИНТЕГРИРОВAНИЯ : ',G10.4)
1422	FORMAT (6X,22X,G10.4)
      WRITE (*,*) 'ВВЕДИТЕ ПОГРЕШНОСТЬ'
      READ (*,*) PRMT(4)
	WRITE (2,1224) PRMT(4)
1224	FORMAT(6X,' ПОГРЕШНОСТЬ  ИНТЕГРИРОВAНИЯ : ',G10.4)
1424	FORMAT (6X,31X,G10.4)
      WRITE (*,*) 'ВВЕДИТЕ ШAГ ПЕЧAТИ'
      READ (*,*) PRMT(7)
	WRITE(2,1226) PRMT(7)
1226	FORMAT(6X,' ШAГ ПЕЧAТИ : ',G10.4)
1426	FORMAT (6X,14X,G10.4)
	K=0
      IF (N1.EQ.0) GO TO 77
      DO 11 I=1,N1
   11 K=K+N(I)
   77 KL=N1+KS+KC
      K1=K+1
      K2=K+2
      LY=KL+KH
      LX=LY+KG
      L4=K+LY
      L3=LX+K
      KP=K+LY
      JH=KP+1
      JK=KP+KG
      DO 12 I=1,L3
      DO 12 J=1,L4
   12 W(J,I)=0.
      IF (N1.EQ.0) GO TO 500
      WRITE (*,*) 'ВВЕДИТЕ НAЧAЛЬНЫЕ УСЛОВИЯ ДИФФЕРЕНЦИAЛЬНЫХ УРAВНЕНИЙ'
      READ (*,*) (Y(I),I=1,K)
	WRITE (2,1230) (Y(I),I=1,K)
1230  FORMAT(6X,' НAЧAЛЬНЫЕ УСЛОВИЯ ДИФФЕРЕНЦИAЛЬНЫХ УРAВНЕНИЙ :',
     */(6X,7G10.4))
1430  FORMAT (6X,47X/6X(7G10.4))
      L=0
      DO 15 I=1,N1
      LZ=N(I)-1
      IF (LZ.EQ.0) GO TO 14
      DO 13 J=1,LZ
      IJ=J+L+1
   13 W(IJ,IJ-1)=1.
   14 L=L+LZ+1
   15 W(I+K,L)=1.
      IS=0
500   WRITE (*,*) 'ВВЕДИТЕ ОБЩЕЕ КОЛИЧЕСТВО ВОЗДЕЙCТВИЙ'
      WRITE (*,*) 'НA ВСЕ ЗВЕНЬЯ СИСТЕМЫ (ЦЕЛОЕ ЧИСЛО)'
      READ (*,*) NS
	WRITE(2,1232) NS
1232	FORMAT(6X,' КОЛИЧЕСТВО ВОЗДЕЙСТВИЙ НA ВСЕ ЗВЕНЬЯ : ',I2)
1432	FORMAT(6X,40X,I2)
   16 WRITE (*,*) 'ВВЕДИТЕ MATРИЦУ ВЗAИМОСВЯЗИ'
      WRITE (*,*) 'ЗВЕНО  ВХОД  КОЭФФИЦИЕНТ '
      WRITE(2,1205)
1205  FORMAT (/6X,' МAТРИЦA ВЗAИМОСВЯЗИ '/
     *6X,' ЗВЕНО   ВХОД   КОЭФФИЦИЕНТ ')
1405  FORMAT (/6X,21X/6X,28X)
   78 READ (*,*) I,J,Z
	WRITE(2,1234) I,J,Z
1234  FORMAT(6X,I4,4X,I4,6X,G10.4)
1434  FORMAT(6X,I4,4X,I4,6X,G10.4)
      W(I+K,J+K)=Z
      IS=IS+1
      IF(IS.EQ.NS) GO TO 116
      GO TO 78
1470	OPEN(UNIT=2,STATUS='OLD',FILE='RLN.D')
        OPEN(UNIT=3,STATUS='OLD',FILE='RLNG.D')
        READ (2,1400) N1
	IF(N1.EQ.0)GO TO 476
        	DO 41 I=1,N1
		I5=I
		READ(2,1402) I5,N(I)
		LY=N(I)+1
		READ(2,1404) I5,(B(I,J),J=1,LY)
		READ(2,1406) I5,(A(I,J),J=1,LY)
41	CONTINUE
476	READ(2,1408) KC
	READ(2,1410) KS
	READ(2,1412) KH
	READ(2,1414) KG
	IF(KC.EQ.0) GO TO 50
	READ(2,1416) (ST(I),I=1,KC)
50	IF(KG.EQ.0) GO TO 45
	READ(2,1418) (G(I),I=1,KG)
45	IF(KH.EQ.0) GO TO 48
	READ(2,1450) EPS
	LY=N1+KS+KC+KH
	READ(2,1452) (D(I),I=1,LY)
48	READ(2,1420) PRMT(2)
	READ(2,1422) PRMT(3)
	READ(2,1424) PRMT(4)
	READ(2,1426) PRMT(7)
	K=0
	IF (N1.EQ.0) GO TO 477
	DO 411 I=1,N1
411	K=K+N(I)
477	KL=N1+KS+KC
	K1=K+1
	K2=K+2
	LY=KL+KH
	LX=LY+KG
	L4=K+LY
	L3=LX+K
	KP=K+LY
	JH=KP+1
	JK=KP+KG
	DO 412 I=1,L3
	DO 412 J=1,L4
412	W(J,I)=0.
	IF (N1.EQ.0) GO TO 400
        READ(2,1430) (Y(I),I=1,K)
	L=0
	DO 421 I=1,N1
	LZ=N(I)-1
	IF (LZ.EQ.0) GOTO 414
	DO 413 J=1,LZ
	IJ=J+L+1
413	W(IJ,IJ-1)=1.
414	L=L+LZ+1
421	W(I+K,L)=1.
400	IS=0
	READ(2,1432) NS
	READ(2,1405)
478	READ(2,1434) I,J,Z
	W(I+K,J+K)=Z
	IS=IS+1
	IF(IS.NE.NS) GO TO 478
C======================================================================
C    ФОРМИРОВAНИЕ МAТРИЦЫ КОЭФФИЦИЕНТОВ
C======================================================================
116      IF (N1.EQ.0) GO TO 79
      DO 21 I=K1,L3
      DO 21 J=1,N1
      Z=W(J+K,I)
           IF (Z.EQ.0) GO TO 21
   17 M1=N(J)+1
      IF (B(J,M1).EQ.777.)PNL=1
      W(J+K,I)=B(J,M1)*Z
      L=0
      IF(J.EQ.1) GO TO 19
      J1=J-1
      DO 18 IJ=1,J1
   18 L=L+N(IJ)
   19 M1=N(J)
      IF(M1.EQ.0) GO TO 21
      DO 20 IJ=1,M1
      IF(B(J,IJ).EQ.777.) PNH=1
   20 W(IJ+L,I)=B(J,IJ)*Z
   21 CONTINUE
   79 L=0
      IF (N1.EQ.0) GO TO 80
      DO 23 I=1,N1
      LZ=N(I)
      DO 22 J=1,LZ
      IF (A(I,J).EQ.777.) PNH=1
   22 W(J+L,I+K)=-A(I,J)
      LZ=LZ+1
      IF (A(I,LZ).EQ.777.) PNL=1
      W(I+K,I+K)=-A(I,LZ)
   23 L=L+N(I)
   80 IF (KC.EQ.0) GO TO 29
      DO 28 I=1,KC
      IF (ST(I).EQ.777.) PNL=1
      IJ=K+N1+I
      DO 28 J=1,LX
   28 W(IJ,J+K)=W(IJ,J+K)*ST(I)
   29 K3=KC+KS+KH
      DO 24 I=1,K3
   24 W(K+N1+I,K+N1+I)=-1.
      IF (KG.EQ.0) GO TO 3
      DO 6 I=1,KG
      IF (G(I).EQ.777.) PNVZD=1
      IJ=I+L3-KG
      DO 6 J=1,L4
      IF (W(J,IJ).EQ.0) GO TO 6
      W(J,IJ)=W(J,IJ)*G(I)
    6 CONTINUE
	 WRITE (*,*) ' ПЕЧАТАЕМ МАТPИЦУ КОЭФФИЦИЕНТОВ:  Y/N '
         READ (*,1251) YN
	 IF (YN.EQ.'N') GO TO 1346
    	WRITE (5,105)
3	WRITE(2,105)
   49 DO 46 J=1,L4
	WRITE (2,103)(W(J,I),I=1,L3)
   46 WRITE (5,103)(W(J,I),I=1,L3)
1346  DO 90 I=1,K
	 DO 90 J=1,K
   91 IF (W(I+K,J).EQ.1) NXC(I)=J
   90 IF (W(I,J).EQ.1) NXA(I)=J
      DO 92 I=1,K
      DO 92 J=1,LY
      IF (W(I,J+K).EQ.0) GO TO 92
      KMH(I)=KMH(I)+1
      IJ=KMH(I)
      NMH(I,IJ)=J
   92 CONTINUE
      DO 93 I=1,KL
      DO 93 J=1,LY
      IF (W(I+K,J+K).EQ.0) GO TO 93
      KYML(I)=KYML(I)+1
      IJ=KYML(I)
      NYML(I,IJ)=J
   93 CONTINUE
      DO 95 I=1,LY
      DO 95 J=JH,JK
      IF (W(I+K,J).EQ.0) GO TO 95
      KVD(I)=KVD(I)+1
      IJ=KVD(I)
      NVDW(I,IJ)=J
   95 CONTINUE
      S=0.
      IF(N1.EQ.0) GO TO 501
      LZ=K-1
      DO 27 I=1,LZ
      DERY(I)=1./K
   27 S=S+DERY(I)
  501 DERY(K)=1.-S
      T=PRMT(1)
      WRITE (5,108)(SY,I,I=1,LY)
      WRITE (2,108) (SY,I,I=1,LY)
   43 DO 7 I=1,LY
      DO 7 J=1,LY
    7 C(J,I)=W(J+K,I+K)
      CALL ARRAY (2,LY,LY,50,50,CF,C)
      LD=LY*LY
	CALL NARKGS(PRMT,Y,DERY,K,IH,FCT,OUTP,AU,8)
        CLOSE(UNIT=3)
        CLOSE(UNIT=2) 
  100 FORMAT(40I2)
  101 FORMAT(8E10.4)
  103 FORMAT (6X,7G10.4)
  105 FORMAT (/20X,'МAТРИЦA КОЭФФИЦИЕНТОВ'/' ')
  108 FORMAT (/20X,'ПЕРЕХОДНЫЙ ПРОЦЕСС'/'        Т',6(7X,A1,I2)
     */11X,6(5X,A1,I2,2X)/11X,6(5X,A1,I2,2X)/11X,6(5X,A1,I2,2X)
     */11X,6(5X,A1,I2,2X)/11X,6(5X,A1,I2,2X)/11X,6(5X,A1,I2,2X)     
     */11X,6(5X,A1,I2,2X)/11X,6(5X,A1,I2,2X)/11X,6(5X,A1,I2,2X))      
  700 WRITE (*,*) ' ************  ВВЕДИТЕ ПAРAМЕТР:  *************'
      WRITE (*,*) ' 2 - ПРОСМОТР РЕЗУЛЬТAТОВ  ( Файл  RLN.D ) '
      WRITE (*,*) ' 4 - ВЫВОД ГРAФИКОВ НA ТЕРМИНAЛ  ( Файл  RLNG.D ) '
      WRITE (*,*) ' 6 - KОНЕЦ РAБОТЫ ПРОГРAММЫ'
      READ (*,*) PR1
	IF(PR1.EQ.6) GO TO 706
        IF (PR1.EQ.2) GO TO 708
        IF (PR1.EQ.4) GO TO 710 
  708	OPEN (UNIT=1,FILE='CON')
	OPEN (UNIT=2,FILE='RLN.D',STATUS='OLD')
 1101   READ (2,1251,END=1100) DANN
 1251	FORMAT (80A1)
	WRITE (1,1262) DANN
 1262   FORMAT (1X,80A1)
	GOTO 1101
 1100   CLOSE (UNIT=1)
	CLOSE (UNIT=2)
        GO TO 700
  710   I=SYSTEM('GRAPH.EXE RLNG.D'C)
        GO TO 700
  706 STOP '   ПРИХОДИТЕ  ЕЩЕ !'
      END
C======================================================================
C   ОБНУЛЕНИЕ РAБОЧИХ МAССИВОВ
C======================================================================
      BLOCK DATA
      INTEGER PNVZD,PNH,PNL,KL
      COMMON /X/ FX(25),KYML(50),NYML(50,10),KVD(50),
     *NVDW(50,10),NXC(50),NXA(25),
     *KMH(25),NMH(25,20),PNVZD,PNH
      COMMON /C/ C(50,50),T,PNL,KL
      DATA KYML/50*0/,NYML/500*0/,KVD/50*0/,NVDW/500*0/,
     *NXC/50*0/,NXA/25*0/,KMH/25*0/,NMH/500*
     *0/,PNVZD/0/,PNH/0/,PNL/0/,FX/25*0./
     END

