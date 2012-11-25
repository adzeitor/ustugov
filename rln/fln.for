C********************************************************************
      REAL FUNCTION NELIN(N)
	COMMON /A/Y(50)/C/C(50,50),T
	INTEGER N
	REAL*8 Y,F,G1
	goto(1,2,3,4,5,6),N
1	nelin=1.d0*y(20)
	return
2	nelin=2.d0*y(21)          
	return                    
3	nelin=3.d0*y(22)          
	return                    
4	nelin=4.d0*y(23)          
	return                    
5	nelin=5.d0*y(24)
        return
6	nelin=1.d0*y(25)  
      RETURN
      END
C********************************************************************
      REAL FUNCTION G1(C1,C2,C3,OM)
	REAL*8 C1,C2,C3,OM
      IF(DABS(OM).LT.C3) GO TO 1
      G1=DSIGN(C2,OM)
      RETURN
1     IF(DABS(OM).GT..1D-04) GO TO 2
      G1=0.D0
      RETURN
2     G1=DSIGN(C1,OM)+(C2-C1)/C3*OM
      RETURN
      END
C********************************************************************
      SUBROUTINE FNHB(T)
	COMMON W(70,70)
	COMMON /A/ F(100),Y(50)/xy/x(25)
C      W(3,9)=3*Y(6)
C        W(1,12)=Y(6)
C      IF((ABS(Y(3)).LT.5.).AND.(ABS(Y(1)).LT..00001)) W(1,12)=0.
      RETURN
      END
C********************************************************************
      SUBROUTINE FNL(T)
	COMMON /C/ L(50,50)
	REAL L
      RETURN
      END
C********************************************************************
      SUBROUTINE FND(T)
	COMMON W(70,70)
	COMMON /A/ F(100),Y(50)/xy/x(25)
      RETURN
      END
C********************************************************************
      SUBROUTINE FGY(T)
	COMMON W(70,70)
	COMMON /A/F(100),Y(50)/xy/x(25)/c/L(50,50)
	COMMON /U/GR1,GR2,GR3
	REAL L
	GR1=Y(1)
	GR2=Y(2)
	GR3=Y(3)
      RETURN
      END
