      SUBROUTINE DISPLAY(STATUS,N)
      DIMENSION STATUS(35,35)
      CHARACTER*70 STRING
      
      DO 7 I=1,N
      STRING=' '
      DO 14 J=1,N
      IF (STATUS(I,J).EQ.1.) THEN
      STRING=TRIM(STRING)//' @ '
      ELSE IF (STATUS(I,J).EQ.2)THEN
      STRING=TRIM(STRING)//' # '
      ELSE
      STRING=TRIM(STRING)//' . '
      ENDIF
      
   14 CONTINUE
      WRITE(*,*)STRING
    7 CONTINUE
! *** UNFORTUNATELY THIS IS QUANTIZED IN UNITS OF 1 SEC c 
!     CALL SLEEP(1)
! *** ANOTHER WAY TO DO THIS (WASTE SOME TIME)
      
      DO 17 J=1,9000000
      RN1=RAND()

   17 CONTINUE

      RETURN
      END SUBROUTINE
! **************************************************
      PROGRAM LIFE
      DIMENSION STATUS(35,35),NEXT(35,35)
      
      N=35
      
      WRITE(*,*)"ENTER 1 FOR MANUAL SETUP, 0 FOR RANDOM"
      READ(*,*)RAN
      
! *** SET UP THE BOARD
      DO 7 I=1,N
      DO 14 J=1,N
      STATUS(I,J)=0.
      NEXT(I,J)=0.
   14 CONTINUE
    7 CONTINUE
      IF(RAN.EQ.1.)THEN
! *** INITIALIZE LIVE CELLS

   13 WRITE(*,*)'ENTER COORDINATES OF C-CELLS, 0,0 WHEN DONE'
      READ(*,*)I,J
      IF((I.EQ.0).AND.(J.EQ.0))GOTO 15
      STATUS(I,J)=2.
      CALL DISPLAY(STATUS,N)
      GOTO 13

   15 WRITE(*,*)'ENTER COORDINATES OF LIVE CELLS, 0,0 WHEN DONE'
      READ(*,*)I,J
      IF((I.EQ.0).AND.(J.EQ.0))GOTO 18
      STATUS(I,J)=1. 
! *** DISPLAY MOVE
      CALL DISPLAY(STATUS,N)
      GOTO 15
      ELSE
      WRITE(*,*)"ENTER A FIVE DIGIT INTEGER TO CHANGE SEED"
      READ(*,*)NSEED
      CALL SRAND(NSEED)
         
      DO 57 I=1,N
      DO 54 J=1,N
      RN1=RAND()
      IF(RN1.EQ.0.5)THEN
      STATUS(I,J)=2
      ELSE IF(RN1.LT.0.8)THEN
      STATUS(I,J)=0.
      ELSE
      STATUS(I,J)=1.
      ENDIF
   54 CONTINUE
   57 CONTINUE

      ENDIF

   18 CONTINUE 
     
      
! *** MAKE IT ALIVE!!! *** C *** SEE http://www.bitstorm.org/gameoflife/ 
! *** RULES:
! *** For a space that is 'populated': 
! *** Each cell with one or no neighbors dies, as if by loneliness.
! *** Each cell with four or more neighbors dies, as if by overpopulation.
! *** Each cell with two or three neighbors survives.
! *** For a space that is 'empty' or 'unpopulated': 
! *** Each cell with three neighbors becomes populated.
      

! *** Count the Number of Live Cells - Be Careful not to go over matrix!
! *** N = size of matrix

      
      WRITE(*,*)"ADJUST YOUR SCREEN TO BE 36 LINES LONG"
      CALL SLEEP(3)

      STUCK=0


  100 DO 20 I=1,N
      DO 21 J=1,N
      LIVE=0
      CCELL=0
      IF((I-1.GE.1).AND.(J-1.GE.1).AND.(STATUS(I-1,J-1).EQ.1))
     MLIVE=LIVE+1
      IF((I-1.GE.1).AND.(STATUS(I-1,J).EQ.1))LIVE=LIVE+1
      IF((J-1.GE.1).AND.(STATUS(I,J-1).EQ.1))
     MLIVE=LIVE+1
      IF((I+1.LE.N).AND.(J+1.LE.N).AND.(STATUS(I+1,J+1).EQ.1))
     MLIVE=LIVE+1
      IF((I+1.LE.N).AND.(STATUS(I+1,J).EQ.1))LIVE=LIVE+1
      IF((J+1.LE.N).AND.(STATUS(I,J+1).EQ.1))LIVE=LIVE+1
      IF((I+1.LE.N).AND.(J-1.GE.1).AND.(STATUS(I+1,J-1).EQ.1))
     MLIVE=LIVE+1
      IF((I-1.LE.N).AND.(J+1.GE.1).AND.(STATUS(I-1,J+1).EQ.1))
     MLIVE=LIVE+1
      IF((I-1.GE.1).AND.(J-1.GE.1).AND.(STATUS(I-1,J-1).EQ.2))
     MCCELL=CCELL+1
      IF((I-1.GE.1).AND.(STATUS(I-1,J).EQ.2))CCELL=CCELL+1
      IF((J-1.GE.1).AND.(STATUS(I,J-1).EQ.2))
     MCCELL=CCELL+1
      IF((I+1.LE.N).AND.(J+1.LE.N).AND.(STATUS(I+1,J+1).EQ.2))
     MCCELL=CCELL+1
      IF((I+1.LE.N).AND.(STATUS(I+1,J).EQ.2))CCELL=CCELL+1
      IF((J+1.LE.N).AND.(STATUS(I,J+1).EQ.2))CCELL=CCELL+1
      IF((I+1.LE.N).AND.(J-1.GE.1).AND.(STATUS(I+1,J-1).EQ.2))
     MCCELL=CCELL+1
      IF((I-1.LE.N).AND.(J+1.GE.1).AND.(STATUS(I-1,J+1).EQ.2))
     MCCELL=CCELL+1


! *** Cell Fate

      RN2=RAND()
      IF((STATUS(I,J).EQ.2))NEXT(I,J)=2
      
      IF((STATUS(I,J).EQ.0))THEN
      IF((LIVE.EQ.3))NEXT(I,J)=1
      ELSE
      IF((LIVE.LE.1))NEXT(I,J)=0
      IF((LIVE.GE.4))NEXT(I,J)=0
      IF((LIVE.EQ.2))NEXT(I,J)=1
      IF((LIVE.EQ.3))NEXT(I,J)=1
      IF((CCELL.GE.2))NEXT(I,J)=2
      IF((RN2.LT.0.01))NEXT(I,J)=2
      ENDIF
   21 CONTINUE
   20 CONTINUE


! *** Refresh

      DO 22 I=1,N
      DO 23 J=1,N
      IF((STATUS(I,J)).EQ.(NEXT(I,J)))STUCK=STUCK+1
      STATUS(I,J)=NEXT(I,J)
   23 CONTINUE
   22 CONTINUE

      CALL DISPLAY(STATUS,N)
      IF(STUCK.NE.1200)THEN 
      GOTO 100
      ELSE
      PRINT*, "Press Ctrl C to Quit"      
      END IF

      STOP
      END PROGRAM





