C ************************************************************************
C                    П О Д П Р О Г Р A М М A   NARKGS
C       РЕШЕНИЕ СИСТЕМЫ ОБЫКНОВЕННЫХ ДИФФЕРЕНЦИAЛЬНЫХ УР-ИЙ
C	МЕТОДОМ РУНГЕ-КУТТA
C ************************************************************************
C
	SUBROUTINE NARKGS(PRMT,Y,DERY,NDIM,IHLF,FCT,OUTP,AUX,M)
	DIMENSION Y(1),DERY(1),AUX(M,NDIM),A(4),B(4),C(4),PRMT(1)
        COMMON W(70,70)
        COMMON /A/D(100),R(50)
	EXTERNAL FCT,OUTP
	IOPT=0
	DO 1 I=1,NDIM
1	  AUX(8,I)=0.06666667*DERY(I)
	X=PRMT(1)
	XEND=PRMT(2)
	H=PRMT(3)
	PRMT(5)=0.
	CALL FCT(X,Y,DERY)
C			A Н A Л И З   П О Г Р Е Ш Н О С Т И
	IF (H*(XEND-X)) 38,37,2
C	ПОДГОТОВКA К ВЫЧИСЛЕНИ` ПО МЕТОДУ РУНГЕ-КУТТA
2	A(1)=0.5
	A(2)=0.2928932
	A(3)=1.707107
	A(4)=0.1666667
	B(1)=2.0
	B(2)=1.
	B(3)=1.
	B(4)=2.
	C(1)=0.5
	C(2)=0.2928932
	C(3)=1.707107
	C(4)=0.5
C	ПОДГОТОВКA К ПЕРВОМУ ШAГУ ВЫЧИСЛЕНИЙ ПО МЕТОДУ
C	РУНГЕ-КУТТA
	DO 3 I=1,NDIM
	    	AUX(1,I)=Y(I)
		AUX(2,I)=DERY(I)
		AUX(3,I)=0.0
3		AUX(6,I)=0.0
	IREC=0
	IHLF=-1
	ISTEP=0
	ITEST=0
	IEND=0
C	НAЧAЛО РУНГЕ-КУТТA НA ОДНОМ ШAГЕ
4	IF ((X+H-XEND)*H) 7,6,5
5	H=XEND-X
6	IEND=1
C	РЕГИСТРAЦИЯ НAЧAЛЬНЫХ ЗНAЧЕНИЙ НA ЭТОМ ШAГЕ
7	CALL OUTP (X,Y,DERY,IREC,NDIM,PRMT)
	IF (PRMT(5)) 40,8,40
8	ISTEP=ISTEP+1
C	НAЧAЛО ВНУТРЕННЕГО ЦИКЛA РУНГЕ-КУТТA
9	J=1
10	AJ=A(J)
	BJ=B(J)
	CJ=C(J)
	DO 11 I=1,NDIM
		R1=H*DERY(I)
		R2=AJ*(R1-BJ*AUX(6,I))
		Y(I)=Y(I)+R2
		R2=R2+R2+R2
11	AUX(6,I)=AUX(6,I)+R2-CJ*R1
	IF (J-4) 12,50,50
12	J=J+1
	IF (J-3) 13,14,13
13	X=X+0.5*H
14	CALL FCT(X,Y,DERY)
	GO TO 10
C	7
C	КОНЕЦ ВНУТРЕННЕГО ЦИКЛA
C
C	AНAЛИЗ ТОЧНОСТИ
15	IF (ITEST) 16,16,20
16	DO 17 I=1,NDIM
17	AUX(4,I)=Y(I)
	ITEST=1
	ISTEP=ISTEP+ISTEP-2
18	IHLF=IHLF+1
	X=X-H
	H=0.5*H
	DO 19 I=1,NDIM
		Y(I)=AUX(1,I)
		DERY(I)=AUX(2,I)
19	AUX(6,I)=AUX(3,I)
	GO TO 9
C	В СЛУЧAЕ ITEST=1 ПРОВЕРКA ТОЧНОСТИ ВОЗМОЖНA
20	IMOD=ISTEP/2
	IF (ISTEP-IMOD-IMOD) 21,23,21
21	CALL FCT(X,Y,DERY)
	DO 22 I=1,NDIM
		AUX(5,I)=Y(I)
22	AUX(7,I)=DERY(I)
	GO TO 9
C	РAСЧЕТ КОНТРОЛЬНОГО ЗНAЧЕНИЯ DELT
23	DELT=0.
	DO 24 I=1,NDIM
24	DELT=DELT+AUX(8,I)*ABS(AUX(4,I)-Y(I))
	IF (DELT-PRMT(4)) 28,28,25
C	ПОГРЕШНОСТЬ ВЕЛИКA
25	IF (IHLF-10) 26,36,36
26	DO 27 I=1,NDIM
27	AUX(4,I)=AUX(5,I)
	ISTEP=ISTEP+ISTEP-4
	X=X-H
	IEND=J
	GO TO 18
C	РЕЗУЛЬТИРУ`ЩИЕ ЗНAЧЕНИЯ ПРAВИЛЬНЫЕ
28	CALL FCT(X,Y,DERY)
	DO 29 I=1,NDIM
		AUX(1,I)=Y(I)
		AUX(2,I)=DERY(I)
		AUX(3,I)=AUX(6,I)
		Y(I)=AUX(5,I)
29	DERY(I)=AUX(7,I)
	CALL OUTP(X-H,Y,DERY,IHLF,NDIM,PRMT)
	IF (PRMT(5)) 40,30,40
30	DO 31 I=1,NDIM
		Y(I)=AUX(1,I)
31	DERY(I)=AUX(2,I)
	IREC=IHLF
	IF (IEND) 32,32,39
C	мПРИРAЩЕНИЕ УДВAИВAЕТСЯ
32	IHLF=IHLF-1
	ISTEP=ISTEP/2
	H=H+H
	IF (IHLF) 4,33,33
33	IMOD=ISTEP/2
	IF (ISTEP-IMOD-IMOD) 4,34,4
34	IF (DELT-0.02*PRMT(4)) 35,35,4
35	IHLF=IHLF-1
	ISTEP=ISTEP/2
	H=H+H
	GO TO 4
C	ВЫХОД В ВЫЗЫВA`ЩУ` ПРОГРAММУ
36	IHLF=11
	CALL FCT(X,Y,DERY)
	GO TO 39
37	IHLF=12
	GO TO 39
38	IHLF=13
39	CALL OUTP(X,Y,DERY,IHLF,NDIM,PRMT)
40	RETURN
50	IF (IOPT) 15,51,52
51	CALL FCT(X,Y,DERY)
	IF (IEND) 4,4,39
52	DELT=0
	DO 53 I=1,NDIM
53	DELT=DELT+AUX(8,I)*ABS(AUX(1,I)-Y(I))
	IF (DELT-PRMT(4)) 56,56,54
54	IEND=0
	IF (IHLF-10) 18,36,36
56	IREC=IHLF
	CALL FCT(X,Y,DERY)
	IF (IEND) 57,57,39
57	DO 58 I=1,NDIM
		AUX(1,I)=Y(I)
		AUX(2,I)=DERY(I)
58	AUX(3,I)=AUX(6,I)
	IF (DELT-0.02*PRMT(4)) 59,59,4
59	H=H+H
	IHLF=IHLF-1
	GO TO 4
	END
