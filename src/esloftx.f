C**********************************************************************
C    ESLOFTX
C    Subroutine for rotor lofting within XROTOR.
C    Based on ESLOFT for DFDC v070-ES2.
C    Copyright (C) 2009-2014, Philip Carter, Esotec Developments.
C    philip (at) esotec (dot) org
C 
C    This program is free software; you can redistribute it and/or modify
C    it under the terms of the GNU General Public License as published by
C    the Free Software Foundation; either version 2 of the License, or
C    (at your option) any later version.
C
C    This program is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU General Public License for more details.
C
C    You should have received a copy of the GNU General Public License
C    along with this program; if not, write to the Free Software
C    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
C***********************************************************************

      SUBROUTINE ESLOFTX
C--------------------------------------------------
C     Rotor lofting in XROTOR
C     Version 1.0  February 2015
C     - Write ESBLADE files for CAD import.
C     Version 0.95 January 2014
C     - Circular root blends!
C     - Hi-res loft plot
C     - Live xfoil plots
C     Version 0.9  January 2014
C     - Windmill friendly
C--------------------------------------------------
      INCLUDE 'XROTOR.INC'
C
      CHARACTER*1 CHKEY, ANS, CIND
      CHARACTER*16 PROMPT
      CHARACTER*8 PROMPT2
      CHARACTER*4 COMAND,COMOLD
      CHARACTER*128 COMARG, ARGOLD, ARGPLT, FNAM
      LOGICAL LOPMOD
C
C---- local arrays for calling GETINT,GETFLT...
      DIMENSION IINPUT(20),IPTEMP(NLSX)
      DIMENSION RINPUT(20)
      LOGICAL ERROR
      CHARACTER*80 LINE,PLIN
C
C---- local arrays for repaneling and saving airfoils
      DIMENSION XXP(NPX,NAFX),YYP(NPX,NAFX),NNP(NAFX)
      DIMENSION IPFSAVE(NLSX)
C
C---- "last" command (nothing yet)
      COMAND = '****'
      COMARG = ' '
      FNAM   = ' '
      ISPARS = ' '
C
      NROTOR = 1
      NDSK = 1
C
      COMOLD = COMAND
      ARGOLD = COMARG
      GREEK = .FALSE.
C
C---- check if rotor geometry loaded
C
      IF(LROTOR) THEN
C---- import geometry
        DO I=1,II
          RZ1(I) = CH(I) * RAD
          RZ2(I) = BETA(I)
          RZ3(I) = XI(I) * RAD
        ENDDO
C
        CALL SEGSPL(RZ1,RZ1S,RZ3,II)
        CALL SEGSPL(RZ2,RZ2S,RZ3,II)
        TIPCHD = SEVAL(RAD,RZ1,RZ1S,RZ3,II) 
C
C--- round-tip criterion 
C   
        IF(TIPCHD/RAD .LT. RTCRIT) THEN  
          LROUND = .TRUE.
          WRITE(*,2100) TIPCHD/RAD, TIPCHD*1000.
        ELSE
          LROUND = .FALSE.
          WRITE(*,2200) TIPCHD/RAD, TIPCHD*1000.
        ENDIF
C
C---- calculate blend root radius
C
        CIRRAD = CIRRAT*RZ1(1)
C
C---no geometry
C
      ELSE
         WRITE(*,*)
         WRITE(*,*)'No geometry loaded'
         WRITE(*,*)'You may work with airfoils only'
      ENDIF
C
C---- assume input data has changed
C
      LCALC  = .FALSE. !  Loft data is not calculated
      LBLEN  = .FALSE. !  Blended sections not calculated
      LTRAN  = .FALSE. !  Transformed sections not calculated
      LGEOPLX= .FALSE. !  No current xfoil plot
      ERROR  = .FALSE. !  Let things work smoothly...
      LTDEF  = .FALSE. !  No thickness data for spline routines
      LONAME =  NAME   !  Loft name begins as rotor name
C
C---- check for windmill anomalies
C
      AZ  = AERODATA(1,1)
      CMX = AERODATA(2,1)
      CMN = AERODATA(3,1)
C      
      IF(AZ.GE.0. .AND.(CMN+CMX).LT.0. .AND..NOT.WIND)THEN
        WRITE(*,*)
        WRITE(*,*) 'Inverted airfoil specified in AERO'
        WRITE(*,*) 'Windmill mode turned on'
        WIND = .TRUE.
      ENDIF
C
C---- show loft if geometry and airfoils are present
C
      IF(LROTOR .AND. NAF.GE.2) THEN
        CALL SHOWLOFT(NDSK)
      ELSEIF(NAF.LT.2) THEN
        WRITE(*,*) 
     & 'Load airfoil set (LSET) or airfoils (LAF) to proceed'
      ENDIF
C
C-------------------------------------------------------------
C---- begin user-interaction loop
C............................................................
C
 500  CONTINUE
      LSMOD = .FALSE.  ! don't user-modify splined t or t/c
      PROMPT = '.LOFT^'
      CALL ASKC(PROMPT,COMAND,COMARG)
C
C---- process previous command ?
      IF(COMAND(1:1).EQ.'!') THEN
        IF(COMOLD.EQ.'****') THEN
          WRITE(*,*) 'Previous .LOFT command not valid'
          GO TO 500
        ELSE
          COMAND = COMOLD
          COMARG = ARGOLD
        ENDIF
      ENDIF
C
      IF(COMAND.EQ.'    ') THEN
        IF(LPLOT) THEN
          CALL PLEND
          CALL CLRZOOM
        ENDIF
        LPLOT = .FALSE.
        RETURN
      ENDIF
C
C---- can jump in here if COMARG string needs processing
 505  CONTINUE
C
C---- extract numbers (if any) from command argument string
      DO I=1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
      ENDDO
      NINPUT = 20
      CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
      NINPUT = 20
      CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C
 1000 FORMAT(A)
 1010 FORMAT(1X,A4,' command not recognized.  Type a "?" for list')
C
C---- can jump in here if COMAND string has been set elsewhere
 510  CONTINUE
C
C------------------------------------------------------------------
C
      IF(    COMAND.EQ.'HELP'
     &  .OR. COMAND.EQ.'?   ') THEN
C
C------ Menu for commands
C
        WRITE(*,1110)
C
 1110   FORMAT(
     &  /'   SHOW <s> Show parent airfoil set',
     &  /'   LSET f   Load parent airfoil set from file',
     &  /'   SSET f   Save parent airfoil set to file',
     &  /'   NSET s   Name parent airfoil set',
     &  /'   DAF  i   Delete  airfoil from current set',
     &  /'   LAF  f   Load an airfoil from file',
     &  /'   SAF  i   Save an airfoil to file',
     &  /'   ALFZ i   Specify parent airfoil zero-lift alpha',
     &  /'   PANE ir  Specify parameters and repanel parents',
C
     & //'   DISP <d> Display current loft configuration',
     &  /'   THIC <t> Specify thickness distribution mode',
     &  /'   PARA r   Specify parabolic axis location',
     &  /'   THUB r   Specify blade thickness at hub',
     &  /'   TTIP r   Specify blade thickness at tip',
     &  /'   STN  ir  Specify station number and density (hub/tip)',
     &  /'   TSTN rr  Specify tip region and refinement factor',
     &  /'   TCUT r   Specify tip cut (round tips)',
     &  /'   OSHO rr  Specify overshoot at hub and tip',     
     &  /'   PAX  rr  Specify pitch axes at hub and tip (x/c)',
     &  /'   DIH  r   Specify dihedral at tip (circular arc)',
C
     & //'   CIRC <c> Toggle  circular root blend',
     &  /'   DISC     Display circular root blend parameters',
     &  /'   RRAD  r  Specify blade root radius (mm)',
     &  /'   LENG  r  Specify blend_length/bladelength',
     &  /'   CPAR  r  Specify blend chordline parameter (1-99)',
     &  /'   TPAR  r  Specify blend thickness parameter   "',
     &  /'   BETR  r  Specify root beta relaxation (deg)',
     &  /'   EXPO     Export loft geometry for analysis',
C
     & //'   MODE     Toggle round/square tip mode',
     &  /'   DEF      Reset loft parameters to default settings',
     &  /'   NLOF s   Specify base name for loft output',
     &  /'   UNIT i   Specify units for loft output',
     &  /'   DIM      Toggle 2D/3D points files',
     &  /'   ROTA     Toggle left/right hand rotation',
     &  /'   WRIT f   Save current loft configuration data to file',
C
     & //'   SAVN ii  Save Normalized section(s) to file',
     &  /'   SAVT ii  Save Transformed section(s)   "  |',
     &  /'   SAVR     Save loft station Radii       "  | output',
     &  /'   DISR     Display loft station Radii       | units',
     &  /'   SAVB     Save ESBLADE loft data file      ',
C
     & //'   PLOP ii  Plot parent section(s)',
     &  /'   PLON ii  Plot normalized interpolated section(s)',
     &  /'   PLOT ii  Plot transformed section(s)',
     &  /'  .DATA     Plot transformed section data vs radius',
C
     & //'   BLOW <b> Blowup airfoil plot region',
     &  /'   RESE <r> Reset current airfoil plot scale and origin',
     &  /'   REPL     Replot current airfoil plot',
     &  /'   HARD     Hardcopy current plot',
     &  /'  .ANNO     Annotate plot',
     &  /'   SIZE r   Change absolute plot size',
     &  /'   Z        Zoom  ',
     &  /'   U        Unzoom',/)
C
 2100 FORMAT(/,' Tip_chord/radius = ', F7.5, ' =',F6.2,' mm',
     &     /,' Round tip is assumed')
 2200 FORMAT(/,' Tip_chord/radius = ', F7.5, ' =',F6.2,' mm',
     &     /,' Square tip is assumed')
 2300 FORMAT(/,' New tip chord =',F6.2,' mm')
C
C
C---------------------------------------------------------
C---- toggle circular root blend
C
      ELSEIF(COMAND.EQ.'CIRC'.OR.COMAND.EQ.'C   ') THEN
        LCIRC = .NOT.LCIRC
        IF(LCIRC) THEN
          WRITE(*,2500)
c          WRITE(*,2510)CIRRAD*1000.,CIRLEN,CIRCHD,CIRTHK,CIRBET
          IF(OSHUB.NE.0.) THEN
            OSHUB = 0.0
            WRITE(*,*)
            WRITE(*,*)'Hub overshoot zeroed for root blend'
          ENDIF
        ELSE
          WRITE(*,*)
          WRITE(*,*)'Circular root blend turned off'
        ENDIF
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWLOFT(NDSK)
C
 2500 FORMAT(/,' Circular root blend selected')
 2505 FORMAT(/,' Circular root blend parameters')
 2510 FORMAT(  ' ---------------------------------------',
     &       /,' Blade root radius           = ',F6.2,' mm',
     &       /,' Blend_length / bladelength  = ',F6.3,
     &       /,' Chord contour parameter     = ',F5.1,
     &       /,' Thickness contour parameter = ',F5.1,
     &       /,' Root beta relaxation        = ',F5.2,' deg')
C
C---------------------------------------------------------
C---- display circular root blend parameters
C
      ELSEIF(COMAND.EQ.'DISC') THEN
        WRITE(*,2505)
        WRITE(*,2510)CIRRAD*1000.,CIRLEN,CIRCHD,CIRTHK,CIRBET
C
C---------------------------------------------------------
C---- specify blade root radius
C
      ELSEIF(COMAND.EQ.'RRAD') THEN
        CIRRADMM = CIRRAD*1000.
        IF(NINPUT.EQ.1) THEN
          CIRRADMM = RINPUT(1)
        ELSE
          CALL ASKR('Enter blade root radius (mm)^',CIRRADMM)
        ENDIF
        CIRRAD = CIRRADMM/1000.
        CIRRAT = CIRRAD / RZ1(1)
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(LGEOPLX) THEN
          CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
        ELSE
          CALL SHOWLOFT(NDSK)
        ENDIF
C
C---------------------------------------------------------
C---- specify blend length / blade length
C
      ELSEIF(COMAND.EQ.'LENG') THEN
        IF(NINPUT.EQ.1) THEN
          CIRLEN = RINPUT(1)
        ELSE
          CALL ASKR('Enter blend_length/bladelength^',CIRLEN)
        ENDIF
C
        IF(CIRLEN.LT.0.05) CIRLEN = 0.05
        IF(CIRLEN.GT.0.95) CIRLEN = 0.95
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWLOFT(NDSK)
C
C---------------------------------------------------------
C---- specify chord contour parameter
C
      ELSEIF(COMAND.EQ.'CPAR') THEN
        CCT = CIRCHD
        IF(NINPUT.EQ.2) THEN
          CCT = RINPUT(1)
        ELSEIF(NINPUT.EQ.1) THEN
          CCT = RINPUT(1)
        ELSE 
       CALL ASKR('Enter chord parameter: slim 1<-->99 full^',CCT)
        ENDIF
C
        IF(CCT.LT.1.0.OR.CCT.GT.99.0) THEN
          WRITE(*,*)
          WRITE(*,*)'Parameter outside range of 1. to 99.'
          GOTO 500
        ENDIF
C
        IF(CCT.EQ.CIRCHD) THEN
          WRITE(*,*)
          WRITE(*,*)'Parameter unchanged'
          GOTO 500
        ENDIF
C
        CIRCHD = CCT
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(LGEOPLX) THEN
          CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
        ELSE
          CALL SHOWLOFT(NDSK)
        ENDIF
C
C---------------------------------------------------------
C---- specify thickness contour parameter
C
      ELSEIF(COMAND.EQ.'TPAR') THEN
        CTT = CIRTHK
C
        IF(NINPUT.EQ.2) THEN
          CTT = RINPUT(2)
        ELSEIF(NINPUT.EQ.1) THEN
          CTT = RINPUT(1)
        ELSE 
       CALL ASKR('Enter thickness parameter: thin 1<-->99 thick^',CTT)
        ENDIF
C
        IF(CTT.LT.1.0.OR.CTT.GT.99.0) THEN
          WRITE(*,*)
          WRITE(*,*)'Parameter outside range of 1. to 99.'
          GOTO 500
        ENDIF
C
        IF(CTT.EQ.CIRTHK) THEN
          WRITE(*,*)
          WRITE(*,*)'Parameter unchanged'
          GOTO 500
        ENDIF
C
        CIRTHK = CTT
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(LGEOPLX) THEN
          CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
        ELSE
          CALL SHOWLOFT(NDSK)
        ENDIF
C
C---------------------------------------------------------
C---- specify root beta relaxation
C
      ELSEIF(COMAND.EQ.'BETR') THEN
        IF(NINPUT.EQ.1) THEN
          CIRBET = RINPUT(1)
        ELSE
          CALL ASKR('Enter root beta relaxation (degrees)^',CIRBET)
        ENDIF
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(LGEOPLX) THEN
          CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
        ELSE
          CALL SHOWLOFT(NDSK)
        ENDIF
C
C---------------------------------------------------------
C---- export modified chord and beta to XROTOR
C
      ELSEIF(COMAND.EQ.'EXPO') THEN
        IF(LCIRC) THEN 
          CALL EXPGEOM
          WRITE(*,5000)
        ELSE
          WRITE(*,*)
          WRITE(*,*)'No modified geometry to export'
        ENDIF
C
 5000 FORMAT(/,' Modified geometry exported to CRotor''s arrays',
     &       /,' Returning to LOFT with this geometry will fail')
C
C------------------------------------------------------------
C------------------------------------------------------------
C---- load airfoils from esloft file
C
      ELSEIF(COMAND.EQ.'LSET') THEN
        FNAM = COMARG
        IF(FNAM(1:1).EQ.' ') THEN
          CALL ASKS(' Enter esloft airfoil filename^',FNAM)
        ENDIF
C
        CALL AFLOAD(FNAM, ERROR)
        IF(.NOT.ERROR) THEN
          CALL SHOWAF
          LCALC = .FALSE.
          LBLEN = .FALSE.
          LTRAN = .FALSE.
        ENDIF
C
C---------------------------------------------------------
C---- save esloft airfoil file
C
      ELSEIF(COMAND.EQ.'SSET') THEN
        IF(NAF.LT.2) THEN
          WRITE(*,*)
          WRITE(*,*)'Minimum 2 airfoils required for esloft file'
          GO TO 500
        ENDIF
C
        CALL AFSAVE(COMARG)
C
C--------------------------------------------------------------------
C---- load airfoil from generic file and process
C
      ELSEIF(COMAND.EQ.'LAF ') THEN
        IF(NAF.EQ.NAFX-1) THEN
          WRITE(*,1115) NAFX-1
          GO TO 500
        ENDIF
C
        FNAM = COMARG
        IF(FNAM(1:1).EQ.' ') THEN
          CALL ASKS('Enter airfoil filename^',FNAM)
        ENDIF
C
        CALL DATLOAD(FNAM,ERROR)
        IF(.NOT.ERROR) THEN
          CALL SHOWAF
          LCALC = .FALSE.
          LBLEN = .FALSE.
          LTRAN = .FALSE.
        ENDIF
C
 1115 FORMAT(/,' Maximum of ',I2,' parent airfoils',
     &       /,' One reserved for circle. Increase NAFX')
C
C--------------------------------------------------------------------
C---- save paneled airfoil from set
C
      ELSEIF(COMAND.EQ.'SAF ') THEN
        IF(NAF.LT.1) THEN
          WRITE(*,*)
          WRITE(*,*)'No airfoils loaded'
          GO TO 500
        ENDIF
C
        IF(NINPUT.EQ.1) THEN
          IDAT = IINPUT(1)
        ELSE
          CALL ASKI('Enter airfoil index^',IDAT)
        ENDIF
C
        IF(IDAT.LT.1 .OR. IDAT.GT.NAF) THEN
          WRITE(*,*)
          WRITE(*,*)'Index out of range'
          GO TO 500
        ENDIF
C
        CALL DATSAVE(IDAT)
C
C--------------------------------------------------------------------
C---- delete airfoil and process arrays
C
      ELSEIF(COMAND.EQ.'DAF ') THEN
        IF(NAF.LT.1) THEN
          WRITE(*,*)
          WRITE(*,*)'No airfoils loaded'
          GO TO 500
        ENDIF
C
        IF(NINPUT.EQ.1) THEN
          NDEL = IINPUT(1)
        ELSE
          CALL ASKI('Enter index of airfoil to delete^',NDEL)
        ENDIF
C
        IF(NDEL.LT.1 .OR. NDEL.GT.NAF) THEN
          WRITE(*,*)
          WRITE(*,*)'Index out of range'
          GO TO 500
        ENDIF
C
        DO I=NDEL,NAF-1
           DO K=1,NPP
             XXE(K,I)= XXE(K,I+1)
             YYE(K,I)= YYE(K,I+1)
           ENDDO
           PAREA (I)= PAREA (I+1)
           PLERAD(I)= PLERAD(I+1)
           PANGTE(I)= PANGTE(I+1)
           TOCE  (I)= TOCE  (I+1)
           TLOC  (I)= TLOC  (I+1)
           CAMLO (I)= CAMLO (I+1)
           CAMLOC(I)= CAMLOC(I+1)
           TETH  (I)= TETH  (I+1)
           ENAME (I)= ENAME (I+1)
           ALFZ  (I)= ALFZ  (I+1)
        ENDDO
C
        NAF=NAF-1
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(NAF.GE.1) THEN
          CALL SHOWAF
        ELSE
          WRITE(*,*)
          WRITE(*,*)'No airfoils loaded'
        ENDIF
C
C--------------------------------------------------------------------
C---- specify A0
C
      ELSEIF(COMAND.EQ.'ALFZ') THEN
        IF(NAF.LT.1) THEN
          WRITE(*,*)
          WRITE(*,*)'No airfoils loaded'
          GO TO 500
        ENDIF
C
        IF(NINPUT.EQ.1) THEN
          NZER = IINPUT(1)
        ELSE
          CALL ASKI('Enter airfoil index^',NZER)
        ENDIF
C
        IF(NZER.LT.1 .OR. NZER.GT.NAF) THEN
          WRITE(*,*)
          WRITE(*,*)'Index out of range'
          GO TO 500
        ENDIF
C
        WRITE(*,1120) NZER
 1120   FORMAT(/,' Airfoil ',I3)
        CALL ASKR('Enter zero-lift alpha (deg)^',ALFZ(NZER))
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWAF        
C
C---------------------------------------------------------------
C---- display airfoil set
C
      ELSEIF(COMAND.EQ.'SHOW' .OR. COMAND.EQ.'S   ') THEN
        IF(NAF.LT.1) THEN
          WRITE(*,*)
          WRITE(*,*)'No airfoils loaded'
          GO TO 500
        ENDIF
C
        CALL SHOWAF
C
C---------------------------------------------------------------
C---- specify airfoil set name
C
      ELSEIF(COMAND.EQ.'NSET') THEN
        FNAM = COMARG
        IF(FNAM(1:1).EQ.' ') THEN
          WRITE(*,1125) SNAME
          CALL ASKS(' Enter parent airfoil set name^',SNAME)
        ELSE
          SNAME = FNAM
        ENDIF
        CALL STRIP(SNAME,NSNAME)
C
 1125   FORMAT(/' Current airfoil set name is: ', A30)
C
C---------------------------------------------------------------
C---- change airfoil paneling
C
      ELSEIF(COMAND.EQ.'PANE') THEN
        IF(NAF.LT.1) THEN
          WRITE(*,*)
          WRITE(*,*)'No airfoils loaded'
          GO TO 500
        ENDIF
C
        NPP2T  = NPP2
        PAFT   = PAF
        NPMAX  = NPX/2
C
        IF(NINPUT.EQ.2)THEN
          NPP2T= IINPUT(1)
          PAFT = RINPUT(2)
        ELSEIF(NINPUT.EQ.1)THEN
          NPP2T= IINPUT(1)
        ELSE
          CALL ASKI('Enter no. of points per side^',NPP2T)
          CALL ASKR('Enter panel density factor (le/te)^',PAFT)
        ENDIF
C
        IF(NPP2T.GT.NPMAX) THEN
          WRITE(*,1130) NPMAX
          GO TO 500
        ELSEIF(NPP2T.LT.20 .OR. PAFT.LT.1.0) THEN
          WRITE(*,*)
          WRITE(*,*)'Parameters out of range'
          GO TO 500
        ELSEIF(NPP2T.EQ.NPP2.AND.PAFT.EQ.PAF) THEN
          WRITE(*,*)
          WRITE(*,*)'Parameters unchanged'
          GO TO 500
        ENDIF
C
        DO I=1,NAF
          NNP(I)=NPP
        ENDDO
        NPP2= NPP2T
        NPP = NPP2*2
        PAF = PAFT
C
        WRITE(*,*)
        DO I=1,NAF
          CALL AFPANEL(NPX,NAFX,I,NNP,XXE,YYE,NPP2,XXP,YYP,PAF)
          DO K=1,NPP
            XXE(K,I)= XXP(K,I)
            YYE(K,I)= YYP(K,I)
          ENDDO
          WRITE(*,1132) I
        ENDDO
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(LGEOPLX) THEN
          CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
        ELSE
          CALL AFDISP(6)
        ENDIF
C
 1130 FORMAT(/' Maximum ',I3,' points per side',
     &       /' Increase NPX')
 1132 FORMAT( ' Airfoil ',I2,' repaneled')
C
C----------------------------------------------------------------
C---- change loft station parameters
C
      ELSEIF(COMAND.EQ.'STN ') THEN
        NLOFTT  = NLOFT
        PLOFTT  = PLOFT
        NLMAX   = NLSX-2
C
        IF(NINPUT.EQ.2) THEN
          NLOFTT = IINPUT(1)
          PLOFTT = RINPUT(2)
        ELSEIF(NINPUT.EQ.1) THEN
          NLOFTT = IINPUT(1)
        ELSE 
          CALL ASKI('Enter no. of loft stations - hub->tip^',NLOFTT)
          CALL ASKR('Enter station density factor - hub/tip^',PLOFTT)
        ENDIF
C
        IF(NLOFTT.GT.NLMAX) THEN
           WRITE(*,1135) NLMAX
           GO TO 500
        ELSEIF(NLOFTT.LT.3 .OR. PLOFTT.LE.0.0) THEN
           WRITE(*,*)
           WRITE(*,*)'Parameters out of range'
           GO TO 500
        ELSEIF(NLOFTT.EQ.NLOFT.AND.PLOFTT.EQ.PLOFT) THEN
           WRITE(*,*)
           WRITE(*,*)'Parameters unchanged'
           GO TO 500
        ENDIF
C
        NLOFT = NLOFTT
        PLOFT = PLOFTT
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWLOFT(NDSK)
C
 1135 FORMAT(/,' Maximum ',I2,' loft stations',
     &       /,' Increase NLSX')
C
C----------------------------------------------------------------
C---- change tip station parameters
C
      ELSEIF(COMAND.EQ.'TSTN') THEN
        TSFACT = TSFAC
        TSLOCT = TSLOC
C
        IF(NINPUT.EQ.2) THEN
          TSLOCT = RINPUT(1)
          TSFACT = RINPUT(2)
        ELSEIF(NINPUT.EQ.1) THEN
          TSLOCT = RINPUT(1)
        ELSE 
          CALL ASKR('Enter tip station region / bladelength^',TSLOCT)
          CALL ASKR('Enter tip station refinement factor^',TSFACT)
        ENDIF
C
        IF(TSFACT.LT.1.0) THEN
         WRITE(*,*)
         WRITE(*,*)'Tip station refinement factor cannot be less than 1'
          GOTO 500
        ENDIF
C
        IF(TSLOCT.LT.0.0 .OR. TSLOCT.GT.0.5) THEN
          WRITE(*,*)
          WRITE(*,*)'Tip station region must be in range 0 <--> 0.5'
          GOTO 500
        ENDIF
C
        IF(TSFACT.EQ.TSFAC.AND.TSLOCT.EQ.TSLOC) THEN
          WRITE(*,*)
          WRITE(*,*)'Parameters unchanged'
          GOTO 500
        ENDIF
C
        TSFAC = TSFACT
        TSLOC = TSLOCT
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWLOFT(NDSK)
C
C---------------------------------------------------------
C---- change working tip cut 
C
      ELSEIF(COMAND.EQ.'TCUT') THEN
        IF(.NOT.LROTOR)THEN
           WRITE(*,*)
           WRITE(*,*)'Load rotor first'
           GOTO 500
        ENDIF
C        
        IF(NINPUT.EQ.1) THEN
          TCUTMM = RINPUT(1)
        ELSE
          TCUTMM = TCUTW*(1.-XI0)*RAD*1000.
          CALL ASKR('Enter tip cut (mm)^',TCUTMM)
        ENDIF
        TCUTT = TCUTMM/((1.-XI0)*RAD*1000.)
C
        IF(TCUTT.LT.0.0 .OR. TCUTT.GT.0.1)THEN
          WRITE(*,*)
          WRITE(*,*)'Tip cut entry out of range'
          GOTO 500
        ENDIF
C
        TCRAD = RAD - TCUTT*(1.-XI0)*RAD
        TCHD  = SEVAL(TCRAD,RZ1,RZ1S,RZ3,II) 
        WRITE(*,2300) TCHD*1000.
        IF(TCHD .LT. 0.001) THEN  ! Change minimum tip chord here
          WRITE(*,*)
          WRITE(*,*)'Tip chord too small - 1 mm minimum'
          WRITE(*,*)'Specify larger tip cut'
          GOTO 500
        ENDIF
C
        TCUTW = TCUTT
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWLOFT(NDSK)
C
C---------------------------------------------------------
C---- specify root thickness
C
      ELSEIF(COMAND.EQ.'THUB') THEN
        TKHUBT  = TKHUB
        TKHUBOLD= TKHUB
        IF(NINPUT.EQ.1) THEN
          TKHUBT = RINPUT(1)
        ELSE
          WRITE(*,*)
          WRITE(*,*)'0 to set thickest airfoil at hub'
          CALL ASKR('Enter blade thickness at hub (mm)^',TKHUBT)
        ENDIF
C
        IF(TKHUBT.EQ.TKHUBOLD) GO TO 500
C
        TKHUB = TKHUBT
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(LGEOPLX) THEN
          CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
        ELSE
          CALL SHOWLOFT(NDSK)
        ENDIF
C
C---------------------------------------------------------
C---- specify tip thickness
C
      ELSEIF(COMAND.EQ.'TTIP') THEN
        TKTIPT  = TKTIP
        TKTIPOLD= TKTIP
        IF(NINPUT.EQ.1) THEN
          TKTIPT = RINPUT(1)
        ELSE
          WRITE(*,*)
          WRITE(*,*)'0 to set thinnest airfoil at tip'
          CALL ASKR('Enter blade thickness at tip (mm)^',TKTIPT)
        ENDIF
C
        IF(TKTIPT.EQ.TKTIPOLD) GO TO 500
C
        TKTIP = TKTIPT
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(LGEOPLX) THEN
          CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
        ELSE
          CALL SHOWLOFT(NDSK)
        ENDIF
C
C---------------------------------------------------------
C---- specify thickness distribution
C
      ELSEIF(COMAND.EQ.'THIC' .OR. COMAND.EQ.'T   ') THEN
        ITTT    = ITTYPE
        ITTYPOLD= ITTYPE
        IF(NINPUT.EQ.1) THEN
         ITTT = IINPUT(1)
        ELSE
          WRITE(*,1140)
          CALL ASKI('Enter thickness distribution index^',ITTT)
        ENDIF
C
        IF(ITTT.LT.1 .OR. ITTT.GT.6) THEN
           WRITE(*,*)'Index out of range'
           GOTO 500
        ELSE
           ITTYPE=ITTT
           IF    (ITTYPE.EQ.1) THEN
              WRITE(*,*)
              WRITE(*,*)'Linear t/c selected'
           ELSEIF(ITTYPE.EQ.2) THEN
              WRITE(*,*)
              WRITE(*,*)'Parabolic t/c selected'
           ELSEIF(ITTYPE.EQ.3) THEN
              WRITE(*,*)
              WRITE(*,*)'Splined t/c selected'
              LSMOD = .TRUE.
           ELSEIF(ITTYPE.EQ.4) THEN
              WRITE(*,*)
              WRITE(*,*)'Linear thickness selected'
           ELSEIF(ITTYPE.EQ.5) THEN
              WRITE(*,*)
              WRITE(*,*)'Parabolic thickness selected'
           ELSEIF(ITTYPE.EQ.6) THEN
              WRITE(*,*)
              WRITE(*,*)'Splined thickness selected'
              LSMOD = .TRUE.
           ENDIF  
        ENDIF
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
C
        IF(ITTYPE.EQ.3 .OR. ITTYPE.EQ.6) THEN
          CALL PRINTLOFT(6,NDSK)
        ELSE
          CALL SHOWLOFT(NDSK)
        ENDIF
C
 1140   FORMAT(
     &  /, ' 1  Linear    t/c',
     &  /, ' 2  Parabolic t/c',
     &  /, ' 3  Splined   t/c',
     &  /, ' 4  Linear    t ',
     &  /, ' 5  Parabolic t ',
     &  /, ' 6  Splined   t ')
C
C---------------------------------------------------------
C---- specify parabolic axis
C
      ELSEIF(COMAND.EQ.'PARA') THEN
        PAXIST =  PAXIS
        PAXISOLD= PAXIS
        IF(NINPUT.EQ.1) THEN
          PAXIST = RINPUT(1)
        ELSE
          WRITE(*,*)
          WRITE(*,*)'Units of bladelengths inboard of hub'
          CALL ASKR('Locate parabolic axis^',PAXIST)
        ENDIF
C
        IF(.NOT.LCALC) THEN
           IF(PAXIST.LE.0.0) THEN
             WRITE(*,*)
             WRITE(*,*)'PARA must be set greater than zero'
           ELSE
             PAXIS = PAXIST
           ENDIF
        ELSE
          YPARAT = PAXIST*(YLOFT(ITIP)-YLOFT(IHUB))
          PLOC   = YLOFT(IHUB)-YPARAT
          IF(PLOC.GE.YLOFT(1)) THEN
            WRITE(*,*)
            WRITE(*,*)'Axis must be inboard of Station 1'
            YPARAT= PAXIS*(YLOFT(ITIP)-YLOFT(IHUB))
          ELSE
            PAXIS = PAXIST
          ENDIF
        ENDIF
Cs'
        WRITE(*,1145) PAXIS
        IF(LCALC) THEN
          YPARACM = YPARAT*100.0
          WRITE(*,1147) YPARACM
        ENDIF
C
        IF(PAXIS.NE.PAXISOLD) THEN
          LCALC = .FALSE.
          LBLEN = .FALSE.
          LTRAN = .FALSE.
          CALL SHOWLOFT(NDSK)
        ENDIF
C
 1145   FORMAT(/,1X,'Parabolic axis at',F6.3,' bladelengths')
 1147   FORMAT(1X,F7.3,' cm inboard of hub')
C
C
C---------------------------------------------------------
C---- toggle between round and square tip treatment 
C
      ELSEIF(COMAND.EQ.'MODE' .OR. COMAND.EQ.'M   ') THEN
        LROUND = .NOT.LROUND
        IF(LROUND) THEN
          WRITE(*,*)
          WRITE(*,*)'Round tip mode selected'
        ELSE
          WRITE(*,*)
          WRITE(*,*)'Square tip mode selected'
        ENDIF
C
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWLOFT(NDSK)
C
C---------------------------------------------------------
C---- specify overshoot 
C
      ELSEIF(COMAND.EQ.'OSHO') THEN
        OSHUBT = OSHUB
        OSTIPT = OSTIP
        OSHUBOLD= OSHUB
        OSTIPOLD= OSTIP
        IF(NINPUT.EQ.2) THEN
          OSHUBT = RINPUT(1)
          OSTIPT = RINPUT(2)
        ELSEIF(NINPUT.EQ.1) THEN
          OSHUBT = RINPUT(1)
          IF(.NOT.LROUND) THEN
            CALL ASKR('Enter overshoot at tip (mm)^',OSTIPT)
          ENDIF
        ELSE
          CALL ASKR('Enter overshoot at hub (mm)^',OSHUBT)
          IF(.NOT.LROUND) THEN
            CALL ASKR('Enter overshoot at tip (mm)^',OSTIPT)
          ENDIF
        ENDIF
C
        IF(OSHUBT.LT.0.0 .OR. OSTIPT.LT.0.0) THEN
          WRITE(*,*)
          WRITE(*,*)'Negative overshoot not allowed'
          GO TO 500
        ELSEIF(OSHUBT.EQ.OSHUBOLD.AND.OSTIPT.EQ.OSTIPOLD) THEN
          GO TO 500
        ENDIF
C
        IF(LCIRC.AND.OSHUBT.NE.0.0) THEN
          WRITE(*,*)
          WRITE(*,*)'Hub overshoot must be zero for root blends'
          OSHUBT = 0.0
        ENDIF
C
        OSHUB = OSHUBT
        OSTIP = OSTIPT
        LCALC = .FALSE.
        LBLEN = .FALSE.
        LTRAN = .FALSE.
        CALL SHOWLOFT(NDSK)
C
C---------------------------------------------------------
C---- change airfoil pitch axes x/c
C
      ELSEIF(COMAND.EQ.'PAX') THEN
        AXHUBOLD= AXHUB
        AXTIPOLD= AXTIP
        IF(NINPUT.EQ.2) THEN
          AXHUB = RINPUT(1)
          AXTIP = RINPUT(2)
        ELSEIF(NINPUT.EQ.1) THEN
          AXHUB = RINPUT(1)
          CALL ASKR('Enter blade pitch axis at tip (x/c)^',AXTIP)
        ELSE
          CALL ASKR('Enter blade pitch axis at hub (x/c)^',AXHUB)
          CALL ASKR('Enter blade pitch axis at tip (x/c)^',AXTIP)
        ENDIF
C
        IF(AXTIP.NE.AXTIPOLD.OR.AXHUB.NE.AXHUBOLD) THEN
          LCALC = .FALSE.
          LBLEN = .FALSE.
          LTRAN = .FALSE.
          CALL SHOWLOFT(NDSK)
        ENDIF
C
C---------------------------------------------------------
C---- change airfoil cone angle - dihedral
C
      ELSEIF(COMAND.EQ.'DIH ') THEN
        AZTIPOLD= AZTIP
        IF(NINPUT.EQ.1) THEN
          AZTIP = RINPUT(1)
        ELSE
          CALL ASKR('Enter dihedral at tip (mm)^',AZTIP)
        ENDIF
C
        IF(AZTIP.NE.AZTIPOLD) THEN
          LTRAN = .FALSE.
          IF(LGEOPLX) THEN
            CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
          ELSE
            CALL SHOWLOFT(NDSK)
          ENDIF
        ENDIF
C
C---------------------------------------------------------
C---- display current loft data
C
      ELSEIF(COMAND.EQ.'DISP'.OR.COMAND.EQ.'D   ') THEN
        CALL SHOWLOFT(NDSK)
C
C---------------------------------------------------------
C---- write loft data to disk
C
      ELSEIF(COMAND.EQ.'WRIT') THEN
        IF(.NOT.LCALC) THEN
           CALL LOFTGEOM(NDSK,ERROR)
           IF(ERROR) GO TO 500
        ENDIF
C
        FNAM = COMARG
        IF(FNAM(1:1).EQ.' ') THEN
          CALL ASKS(' Enter loft data filename^',FNAM)
        ENDIF
        CALL STRIP(FNAM,NF)
C
        LU = 19
        OPEN(LU,FILE=FNAM,STATUS='OLD',ERR=605)
C
        WRITE(*,*)
        WRITE(*,*) 'Output file exists. Overwrite? (Y/n)'
        READ (*,1000) ANS
        IF(INDEX('Nn',ANS).EQ.0) GO TO 610
C
        CLOSE(LU)
        WRITE(*,*)
        WRITE(*,*) 'Output file not saved'
        GO TO 615
C
 605    OPEN(LU,FILE=FNAM,STATUS='NEW',ERR=615)
 610    REWIND(LU)
C
        CALL PRINTLOFT(LU,NDSK)
        IF(LCIRC) THEN
          WRITE(LU,2505)
          WRITE(LU,2510)CIRRAD*1000.,CIRLEN,CIRCHD,CIRTHK,CIRBET
        ENDIF
        CALL AFDISP(LU)
        CLOSE(LU)
C
 615    CONTINUE
C
C---------------------------------------------------------
C---- reset loft parameters to default settings
C
      ELSEIF(COMAND.EQ.'DEF ') THEN
        IF(LROTOR) THEN
          CALL LOFTINIT2
          CIRRAD = CIRRAT*RZ1(1)
          LONAME = NAME
          LCALC = .FALSE.
          LBLEN = .FALSE.
          LTRAN = .FALSE.
          CALL SHOWLOFT(NDSK)
C
          WRITE(*,*)
          WRITE(*,*) 'Loft parameters reset to defaults'
          WRITE(*,*) 'Airfoil set and output settings unchanged'
        ELSE
          WRITE(*,*)
          WRITE(*,*) 'No geometry loaded'
        ENDIF
C
C---------------------------------------------------------
C---- toggle between 2D and 3D points file output
C
      ELSEIF(COMAND.EQ.'DIM ') THEN
        LL2D = .NOT.LL2D
        WRITE(*,*)
        IF(LL2D) THEN
          WRITE(*,*)'2D points files will be saved'
        ELSE
          WRITE(*,*)'3D points files will be saved'
        ENDIF
C
C---------------------------------------------------------
C---- toggle between left and right hand rotation output
C
      ELSEIF(COMAND.EQ.'ROTA') THEN
        LROTATE = .NOT.LROTATE
        WRITE(*,*)
        IF(LROTATE) THEN
          WRITE(*,*)'Left hand rotation selected'
        ELSE
          WRITE(*,*)'Right hand rotation selected'
        ENDIF
        LTRAN= .FALSE.
C
C---------------------------------------------------------
C---- change loft output units
C
      ELSEIF(COMAND.EQ.'UNIT') THEN
        IF(NINPUT.EQ.1) THEN
           IUNTYPE = IINPUT(1)
        ELSE              
           WRITE(*,1150)
           IF(OUTFAC.EQ.1.) THEN
              IUNTYPE= 1
              WRITE(*,*)'Currently meters'
           ELSEIF(OUTFAC.EQ.100.) THEN
              IUNTYPE= 2
              WRITE(*,*)'Currently centimeters'
           ELSEIF(OUTFAC.EQ.1000.) THEN
              IUNTYPE= 3        
              WRITE(*,*)'Currently millimeters'
           ELSEIF(OUTFAC.EQ.MTI) THEN
              IUNTYPE= 4     
              WRITE(*,*)'Currently inches'
           ENDIF
           CALL ASKI('Specify output units^',IUNTYPE)
        ENDIF
C 
        WRITE(*,*)
        IF(IUNTYPE.EQ.1) THEN
           OUTFAC = 1.0
           WRITE(*,*) 'Output units set to meters'
        ELSEIF(IUNTYPE.EQ.2) THEN
           OUTFAC = 100.0
           WRITE(*,*) 'Output units set to centimeters'
        ELSEIF(IUNTYPE.EQ.3) THEN
           OUTFAC = 1000.0
           WRITE(*,*) 'Output units set to millimeters'
        ELSEIF(IUNTYPE.EQ.4) THEN
           OUTFAC = MTI
           WRITE(*,*) 'Output units set to inches'
        ELSE
           WRITE(*,*) 'Wrong index'
           WRITE(*,*) 'Output units set to meters'
           IUNTYPE= 1
           OUTFAC = 1.0
        ENDIF
C
 1150   FORMAT(
     &  /, '  1  meters',
     &  /, '  2  centimeters',
     &  /, '  3  millimeters',
     &  /, '  4  inches')
C
C---------------------------------------------------------
C---- change loft output base name
C
      ELSEIF(COMAND.EQ.'NLOF') THEN
        FNAM = COMARG
        IF(FNAM(1:1).EQ.' ') THEN
          WRITE(*,*)
          WRITE(*,1160) LONAME
          CALL ASKS(' Enter new base name^',LONAME)
        ELSE
          LONAME = FNAM
        ENDIF
        CALL STRIP(LONAME,NLONAME)
C
 1160   FORMAT(1X,'Current loft output base name: ', A30)
C
C
C---------------------------------------------------------
C---- PLOTTING
C---- plot individual parent airfoils
C
      ELSEIF(COMAND.EQ.'PLOP') THEN
        IF(NAF.LT.1) THEN
          WRITE(*,*)
          WRITE(*,*)'No airfoils loaded'
          GO TO 500
        ENDIF
C
        DO I=1,NLSX
           IPTEMP(I)= 0
        ENDDO
C
        IF(NINPUT.GE.1) THEN
           DO I=1,NINPUT
              IPTEMP(I)= IINPUT(I)
           ENDDO
           NXF= NINPUT
        ELSE
           NXF = NAF
           CALL ASKS(' Enter airfoil indices - 0 for all^',PLIN)
           CALL GETINT(PLIN,IPTEMP,NXF,ERROR)
           IF(NXF.LT.1) GO TO 500
        ENDIF
C
        IF(IPTEMP(1).EQ.0) THEN
           DO I=1,NAF
             IXPLOT(I)=I
           ENDDO
           NXPLOT = NAF
        ELSE
           DO I=1,NXF
              IF(IPTEMP(I).LT.1 .OR. IPTEMP(I).GT.NAF) THEN
                 WRITE(*,*)
                 WRITE(*,*)'Airfoil index out of range'
                 GO TO 500
              ENDIF
           ENDDO
C
           NXPLOT = NXF
           DO I=1,NXPLOT
              IXPLOT(I)=IPTEMP(I)
           ENDDO
        ENDIF
C
        NAFOLD = NAF
        IXTYPE = 1
        CALL XFOILPLOT(NDSK, 1)  ! no blowup
        CALL AFDISP(6)
C
C----------------------------------------------------------------
C---- plot blended or transformed sections
C
      ELSEIF(COMAND.EQ.'PLON' .OR. COMAND.EQ.'PLOT') THEN
C
        IF(COMAND.EQ.'PLON') THEN
          IF(.NOT.LBLEN) CALL AFBLEND(NDSK,ERROR)
          IXTYPE = 2
        ELSE
          IF(.NOT.LTRAN) CALL AFTRANS(NDSK,ERROR)
          IXTYPE = 3
        ENDIF
C
        IF(ERROR) THEN
          LGEOPLX= .FALSE.
          WRITE(*,*)
          WRITE(*,*) 'LCALC error'
          GO TO 500
        ENDIF
C
        DO I=1,NLSX
           IPTEMP(I)= 0
        ENDDO
C
        IF(NINPUT.GE.1) THEN
           DO I=1,NINPUT
              IPTEMP(I)= IINPUT(I)
           ENDDO
           NXF= NINPUT
        ELSE
           NXF = NLOFT2
           CALL ASKS(' Enter airfoil indices - 0 for all^',PLIN)
           CALL GETINT(PLIN,IPTEMP,NXF,ERROR)
           IF(NXF.LT.1) GO TO 500
        ENDIF
C
        IF(IPTEMP(1).EQ.0) THEN
           DO I=1,NLOFT2
             IXPLOT(I)=I
           ENDDO
           NXPLOT = NLOFT2
        ELSE
           DO I=1,NXF
              IF(IPTEMP(I).LT.1 .OR. IPTEMP(I).GT.NLOFT2) THEN
                 WRITE(*,*)
                 WRITE(*,*)'Airfoil index out of range'
                 GO TO 500
              ENDIF
           ENDDO
C
           NXPLOT = NXF
           DO I=1,NXPLOT
              IXPLOT(I)=IPTEMP(I)
           ENDDO
        ENDIF
C
        NLOFTOLD= NLOFT2
        CALL XFOILPLOT(NDSK, 1)  ! no blowup
        CALL PRINTLOFT(6,NDSK)
C
C--------------------------------------------------------
C---- blow up current plot
C
      ELSEIF(COMAND.EQ.'BLOW' .OR.
     &       COMAND.EQ.'B   '      ) THEN
C
        IF(.NOT.LGEOPLX) THEN
          WRITE(*,*)
          WRITE(*,*)'No current xfoil plot to blowup'
          GO TO 500
        ENDIF
C
        IF(IXTYPE.EQ.1 .AND. NAFOLD.NE.NAF) THEN
          WRITE(*,*)
          WRITE(*,*)'Parent airfoil numbers have changed'
          WRITE(*,*)'Use PLOP to set airfoil indices'
          GO TO 500
C
        ELSEIF(IXTYPE.EQ.2) THEN
          IF(.NOT.LBLEN) THEN
            CALL AFBLEND(NDSK,ERROR)
            IF(ERROR) GO TO 500
          ENDIF
C
          IF(NLOFTOLD.NE.NLOFT2) THEN
            WRITE(*,*)
            WRITE(*,*)'Station numbers have changed'
            WRITE(*,*)'Use PLON to set station indices'
            GO TO 500
          ENDIF
C
        ELSEIF(IXTYPE.EQ.3) THEN
          IF(.NOT.LTRAN) THEN
            CALL AFTRANS(NDSK,ERROR)
            IF(ERROR) GO TO 500
          ENDIF
C
          IF(NLOFTOLD.NE.NLOFT2) THEN
            WRITE(*,*)
            WRITE(*,*)'Station numbers have changed'
            WRITE(*,*)'Use PLOT to set station indices'
            GO TO 500
          ENDIF
        ENDIF
C
        CALL OFFGETX
        CALL XFOILPLOT(NDSK, 2)  ! do blowup
C
C--------------------------------------------------------
C---- reset current airfoil plot scale and offset
C
      ELSEIF(COMAND.EQ.'RESE' .OR.
     &       COMAND.EQ.'R   '    ) THEN
C
        IF(.NOT.LGEOPLX) THEN
          WRITE(*,*)
          WRITE(*,*)'No current xfoil plot to reset'
          GO TO 500
        ENDIF
C
        IF(IXTYPE.EQ.1 .AND. NAFOLD.NE.NAF) THEN
          WRITE(*,*)
          WRITE(*,*)'Parent airfoil numbers have changed'
          WRITE(*,*)'Use PLOP to set airfoil indices'
          GO TO 500
C
        ELSEIF(IXTYPE.EQ.2) THEN
          IF(.NOT.LBLEN) THEN
            CALL AFBLEND(NDSK,ERROR)
            IF(ERROR) GO TO 500
          ENDIF
C
          IF(NLOFTOLD.NE.NLOFT2) THEN
            WRITE(*,*)
            WRITE(*,*)'Station numbers have changed'
            WRITE(*,*)'Use PLON to set station indices'
            GO TO 500
          ENDIF
C
        ELSEIF(IXTYPE.EQ.3) THEN
          IF(.NOT.LTRAN) THEN
            CALL AFTRANS(NDSK,ERROR)
            IF(ERROR) GO TO 500
          ENDIF
C
          IF(NLOFTOLD.NE.NLOFT2) THEN
            WRITE(*,*)
            WRITE(*,*)'Station numbers have changed'
            WRITE(*,*)'Use PLOT to set station indices'
            GO TO 500
          ENDIF
        ENDIF
C
        CALL XFOILPLOT(NDSK, 1)  ! no blowup
C
C--------------------------------------------------------
C---- replot current airfoil plot
C
      ELSEIF(COMAND.EQ.'REPL' ) THEN
        CALL REGEOPL(NAFOLD,NLOFTOLD,NDSK)
C
C---------------------------------------------------------------
C---- plot section data
C
      ELSEIF(COMAND.EQ.'DATA') THEN
        IF(.NOT.LBLEN) THEN
           CALL AFBLEND(NDSK,ERROR)
           IF(ERROR) GO TO 500
        ENDIF
C
        CALL PLOTLOFDATA(NDSK)
        LGEOPLX = .FALSE.
C
C--------------------------------------------------------------
C---- save blended and transformed sections to disk
C
      ELSEIF(COMAND.EQ.'SAVN' .OR. COMAND.EQ.'SAVT') THEN
C
        IF(COMAND.EQ.'SAVN') THEN
          IF(.NOT.LBLEN) CALL AFBLEND(NDSK,ERROR)
          IF(ERROR) GO TO 500
          IPFTYPE = 1
        ELSE
          IF(.NOT.LTRAN) CALL AFTRANS(NDSK,ERROR)
          IF(ERROR) GO TO 500
          IPFTYPE = 2
        ENDIF
C
        DO I=1,NLSX
           IPTEMP(I)= 0
        ENDDO
C
        IF(NINPUT.GE.1) THEN
           DO I=1,NINPUT
              IPTEMP(I)= IINPUT(I)
           ENDDO
           NPF= NINPUT
        ELSE
           NPF = NLOFT2
           CALL ASKS(' Enter station indices - 0 for all^',PLIN)
           CALL GETINT(PLIN,IPTEMP,NPF,ERROR)
           IF(NPF.LT.1) GO TO 500
        ENDIF
C
        IF(IPTEMP(1).EQ.0) THEN
           DO I=1,NLOFT2
             IPFSAVE(I)=I
           ENDDO
           NPFSAVE = NLOFT2
        ELSE
           DO I=1,NPF
              IF(IPTEMP(I).LT.1 .OR. IPTEMP(I).GT.NLOFT2) THEN
                 WRITE(*,*)
                 WRITE(*,*)'Station index out of range'
                 GO TO 500
              ENDIF
           ENDDO
C
           NPFSAVE = NPF
           DO I=1,NPFSAVE
              IPFSAVE(I)= IPTEMP(I)
           ENDDO
        ENDIF
C
        CALL SAVLOFT(NDSK,NPFSAVE,IPFSAVE,IPFTYPE)
C
C--------------------------------------------------------------
C---- save station radii to disk
C
      ELSEIF(COMAND.EQ.'SAVR') THEN
        IF(.NOT.LCALC) THEN
          CALL LOFTGEOM(NDSK,ERROR)
          IF(ERROR) GO TO 500
        ENDIF
        LU = 19
        CALL SAVRAD(LU,NDSK,.FALSE.,.FALSE.)
C
C--------------------------------------------------------------
C---- display station radii
C
      ELSEIF(COMAND.EQ.'DISR') THEN
        IF(.NOT.LCALC) THEN
          CALL LOFTGEOM(NDSK,ERROR)
          IF(ERROR) GO TO 500
        ENDIF
        CALL SAVRAD(6,NDSK,.FALSE.,.FALSE.)
C
C--------------------------------------------------------------
C---- save ESBLADE file
C
      ELSEIF(COMAND.EQ.'SAVB') THEN
        IF(.NOT.LTRAN) THEN
          CALL AFTRANS(NDSK,ERROR)
          IF(ERROR) GO TO 500
        ENDIF
        CALL SAVEBLADE
C
C---------------------------------------------------------------
C---- standard plot controls
C---- annotate plot
C
      ELSEIF(COMAND.EQ.'ANNO') THEN
       IF(LPLOT) THEN
        CALL ANNOT(CSIZE)
       ELSE
        WRITE(*,*)
        WRITE(*,*) 'No active plot to annotate'
       ENDIF
C
C--------------------------------------------------------------------
C---- hard copy current plot
C
      ELSEIF(COMAND.EQ.'HARD') THEN
        IF(LPLOT) CALL PLEND
        LPLOT = .FALSE.
        CALL REPLOT(IDEVRP)
C
C--------------------------------------------------------------------
C---- change plot size
C
      ELSEIF(COMAND.EQ.'SIZE') THEN
        IF(NINPUT.GE.1) THEN
         SIZE = RINPUT(1)
        ELSE
         CALL ASKR('Enter plot size^',SIZE)
        ENDIF
C
C--------------------------------------------------------------------
C---- zoom plot
C
      ELSEIF(COMAND.EQ.'Z   ') THEN
        IF(LPLOT) THEN
         CALL USETZOOM(.TRUE.,.TRUE.)
         CALL REPLOT(IDEV)
        ENDIF
C
C--------------------------------------------------------------------
C---- unzoom plot
C
      ELSEIF(COMAND.EQ.'U   ') THEN
        IF(LPLOT) THEN
         CALL CLRZOOM
         CALL REPLOT(IDEV)
        ENDIF
C
C--------------------------------------------------------------------
C
C---- that's it!
C
      ELSE
        WRITE(*,1010) COMAND
      ENDIF
C
      GO TO 500
C
      END !  ESLOFT
C
C------------------------------------------------------------------




C------------------------------------------------------------------
C---- ESLOFT SUBROUTINES
C------------------------------------------------------------------


      SUBROUTINE SHOWLOFT(NDSK)
      INCLUDE 'XROTOR.INC'
      LOGICAL ERROR
C
      PLFACD= 0.67  !  This controls size of LOFTPLT vs window
      XORG  = 0.12  !  These control the origin
      YORG  = 0.11
C
      IF(.NOT.LCALC) THEN
         CALL LOFTGEOM(NDSK,ERROR)
         IF(ERROR) RETURN
      ENDIF
C
      CALL PRINTLOFT(6,NDSK)
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFACD*SIZE,LPLOT,.FALSE.)
      CALL PLOT(XORG,YORG,-3)
      CALL LOFTPLT(NDSK)
C
      LGEOPLX = .FALSE.
C
      RETURN
      END  !  SHOWLOFT
C
C-------------------------------------------------------------------

      SUBROUTINE SHOWAF
      INCLUDE 'XROTOR.INC'
C
      CALL AFDISP(6)
      CALL PLTINI(SCRNFR,IPSLU,IDEV,SIZE,LPLOT,.FALSE.)
      CALL AFPLOT
C
      LGEOPLX = .FALSE.
C
      RETURN
      END  !  SHOWAF
C-------------------------------------------------------------------


      SUBROUTINE REGEOPL(NAFOLD,NLOFTOLD,NDSK)
      INCLUDE 'XROTOR.INC'
      LOGICAL ERROR
C
      IF(.NOT.LGEOPLX) THEN
        WRITE(*,*)
        WRITE(*,*)'No current xfoil plot to replot'
        RETURN
      ENDIF
C
      IF(IXTYPE.EQ.1 .AND. NAFOLD.NE.NAF) THEN
        WRITE(*,*)
        WRITE(*,*)'Parent airfoil numbers have changed'
        WRITE(*,*)'Use PLOP to set airfoil indices'
        RETURN
C
      ELSEIF(IXTYPE.EQ.2) THEN
        IF(.NOT.LBLEN) THEN
          CALL AFBLEND(NDSK,ERROR)
          IF(ERROR) RETURN
        ENDIF
C
        IF(NLOFTOLD.NE.NLOFT2) THEN
          WRITE(*,*)
          WRITE(*,*)'Station numbers have changed'
          WRITE(*,*)'Use PLON to set station indices'
          RETURN
        ENDIF
C
      ELSEIF(IXTYPE.EQ.3) THEN
        IF(.NOT.LTRAN) THEN
          CALL AFTRANS(NDSK,ERROR)
          IF(ERROR) RETURN
        ENDIF
C
        IF(NLOFTOLD.NE.NLOFT2) THEN
          WRITE(*,*)
          WRITE(*,*)'Station numbers have changed'
          WRITE(*,*)'Use PLOT to set station indices'
          RETURN
        ENDIF
      ENDIF
C
      CALL XFOILPLOT(NDSK, 3)  ! use current scale and origin
      CALL PRINTLOFT(6,NDSK)   ! keep terminal updated 
C
      RETURN
      END ! REGEOPL
C-------------------------------------------------------------------



      SUBROUTINE AFDISP(LU)
C-----------------------------
C     Prints airfoil set data
C-----------------------------
      INCLUDE 'XROTOR.INC'
C
      WRITE(LU,1050) NPP2,PAF
      WRITE(LU,1000)
      WRITE(LU,1100)SNAME
      DO I=1,NAF
        WRITE(LU,1120) I,ENAME(I),TOCE(I),TLOC(I),
     &        CAMLO(I),CAMLOC(I),PLERAD(I),TETH(I),ALFZ(I)
      ENDDO
      WRITE(LU,1000)
C
 1000 FORMAT(1X,83('-'))
 1050 FORMAT(/,1X,'Parent Airfoils', 30X,
     &          'Points/side:',I3,3X,'Density-le/te:',F6.2)
 1100 FORMAT(  1X,A28,
     &   't/c  @  x/c    camber @  x/c     r_le     t_te    A0deg')
C
 1120 FORMAT(1X,I2,3X,A21,F6.4,2X,F6.4,3X,F6.4,2X,F6.4,3X,
     &       F6.4,3X,F6.4,2X,F6.2)
C
      RETURN
      END   ! AFDISP
C
C-------------------------------------------------------------------



      SUBROUTINE TOVC (NPX,NAFX,IAF,NP,XX,YY,THOVCD,THLOC)
C------------------------------------------------------------------
C    Generalized routine for calculating approx. max thickness/chord 
C    and location (x/c) of arbitrary airfoils. Assumes unit chord.
C------------------------------------------------------------------
      REAL XX(NPX,NAFX),YY(NPX,NAFX)
C
      NP2=NP/2
      THICKL=0.0
C
      DO 100 J=10,NP2
C            
         XPNT1=XX(J,IAF)
         YPNT1=YY(J,IAF)

         DO K=J+1,NP
            IF (XX(K,IAF) .GE. XPNT1)THEN
               GO TO 50
            END IF
         ENDDO
C
 50      XPNT2=XX(K,IAF)
         YPNT2=YY(K,IAF)
C
         THICK=YPNT1-YPNT2
C
         IF (THICK .LE. THICKL)THEN
            GO TO 200
         END IF
C           
         THICKL=THICK
C
 100  CONTINUE
C
 200  THOVCD = THICKL
      THLOC  = XX(J-1,IAF)
C
      RETURN
      END  ! TOVC
C
C-------------------------------------------------------------------




      SUBROUTINE AFPANEL(NPX,NAFX,IAF,NP,XX,YY,NPP2,XXP,YYP,PAF)
C-------------------------------------------------------------------
C    AFPANEL
C    Repaneling of arbitrary airfoils with equivalent no. of points
C    on upper and lower surfaces with spacing proportional to S.
C    Sets up airfoils for blending.
C
C    NPP2 -  no of points per side
C    PAF  -  coordinate spacing_TE/spacing_LE
C------------------------------------------------------------------- 
      REAL XX(NPX,NAFX),YY(NPX,NAFX),XXP(NPX,NAFX),YYP(NPX,NAFX),
     &XB(NPX),YB(NPX),XBP(NPX),YBP(NPX),SB(NPX),STINT(NPX),SPNT(NPX)
C
      INTEGER NP(NAFX)
C
      NB=NP(IAF)
      NPP=NPP2*2
C
      DO J=1,NB
         XB(J)=XX(J,IAF)
         YB(J)=YY(J,IAF)
      ENDDO
C
C---- initialize spline
C
      CALL SCALC (XB,YB, SB,NB)
      CALL SEGSPL(XB,XBP,SB,NB)
      CALL SEGSPL(YB,YBP,SB,NB)
C
C---- get le
C
      CALL LEFIND(SLE,XB,XBP,YB,YBP,SB,NB)
C
      XLE=SEVAL(SLE,XB,XBP,SB,NB)
      YLE=SEVAL(SLE,YB,YBP,SB,NB)
C      WRITE(*,1000)IAF,XLE,YLE
C
C----Work out raw panel intervals
C
      NINT2=2*NPP2-1
C
      DO I=1,NINT2
         STINT(I) = PAF - FLOAT(I-1)*(PAF-1.)/FLOAT(NINT2-1)
      ENDDO
C
C----Accumulate panel intervals to get unscaled S points
C
      SPNT(1)=0.0
C
      DO I=2,NPP2
         J=I-1
         K=2*I-3
         L=2*I-2
         SPNT(I)=SPNT(J)+STINT(K)+STINT(L)
      ENDDO
C
C----Calculate scale factors
C
      FACTUP = SLE/(SPNT(NPP2)+STINT(NINT2))
      SLOWER = SB(NB)-SLE
      FACTDN = SLOWER/(SPNT(NPP2)+STINT(NINT2))
C
C---- Scale lower S points
C
      DO I=2,NPP2
         K=NPP-I+1
         SPNT(K)=SB(NB)-FACTDN*SPNT(I)
      ENDDO
      SPNT(NPP)=SB(NB)
C
C---- Scale upper S points
C
      DO I=1,NPP2
         SPNT(I)= FACTUP*SPNT(I)        
      ENDDO
C
C----Calculate coordinates from S points
C
      DO I=2,NPP-1
         XXP(I,IAF)=SEVAL(SPNT(I),XB,XBP,SB,NB)
         YYP(I,IAF)=SEVAL(SPNT(I),YB,YBP,SB,NB)
      ENDDO
C
      XXP(1,IAF)=XB(1)
      YYP(1,IAF)=YB(1)
C
      XXP(NPP,IAF)=XB(NB)
      YYP(NPP,IAF)=YB(NB)
C
C----Airfoil is repaneled!
C
C      WRITE(*,1010) IAF
C
c 1010 FORMAT(' Airfoil ',I2,' repaneled')
C
 800  RETURN
      END  ! AFPANEL
C
C-------------------------------------------------------------------




      SUBROUTINE AFSAVE(FNAMEIN)
C-------------------------------------------------------------------
C     Save airfoil set to esloft airfoil file    
C-------------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      CHARACTER*(*) FNAMEIN
      CHARACTER ANS*1, FNAM*128, FBLNK*80
      PARAMETER (NBUFX = 2000)
      INTEGER ILFRST(NAFX),ILLAST(NAFX)
      DIMENSION XT(NBUFX),YT(NBUFX)
C
      LU = 19
      FNAM = FNAMEIN
      CALL STRIP(FNAM,NF)
C
      IF(FNAM(1:1) .EQ. ' ') THEN
        CALL ASKS(' Enter airfoil set filename^',FNAM)
      ENDIF
C
      OPEN(LU,FILE=FNAM,STATUS='OLD',ERR=5)
C
      WRITE(*,*)
      WRITE(*,*) 'Output file exists. Overwrite?  Y/n'
      READ (*,1000) ANS
      IF(INDEX('Nn',ANS).EQ.0) GO TO 6
C
      CLOSE(LU)
      WRITE(*,*) 'Current airfoil set not saved.'
      RETURN
C
 5    OPEN(LU,FILE=FNAM,STATUS='NEW',ERR=90)
 6    REWIND(LU)
C
C      
C--- ESLOFT header and general data
C
      WRITE(LU,1100)
      WRITE(LU,1120) NAF
      WRITE(LU,1120) NPP2
      WRITE(LU,1130) PAF
C
      DO I=1,NAF
        WRITE(LU,1000) ENAME(I)
      ENDDO
C
      DO I=1,NAF
        WRITE(LU,1130) ALFZ(I)
      ENDDO
C
C--- Move coordinates to linear arrays and write
C
      DO I=1,NAF
        ILFRST(I)= 1 + NPP*(I-1)
        ILLAST(I)= NPP*I
        DO K=1,NPP
          L = ILFRST(I) + K-1
          XT(L) = XXE(K,I)
          YT(L) = YYE(K,I)
        ENDDO
      ENDDO
C
      IFTYPE = 2
      FBLNK = ' '
C
      CALL AWRITE(FBLNK,LU,
     &            NAF,ILFRST,ILLAST,XT,YT,
     &            SNAME, ISPARS, IFTYPE)
C
      CLOSE(LU)
      RETURN      
C
 90   WRITE(*,*) 'Bad filename.'
      WRITE(*,*) 'Current airfoil set not saved.'
      RETURN
C
 1000 FORMAT(A)
 1100 FORMAT('ESLOFT Airfoil File ')
 1120 FORMAT(I6)
 1130 FORMAT(F10.4)
C
      END  !  AFSAVE
C
C-----------------------------------------------------------



      SUBROUTINE AFLOAD(FNAMIN,ERROR)
C-----------------------------------------------------------
C     Reads previously saved esloft airfoil file
C-----------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      CHARACTER*(*)  FNAMIN
      CHARACTER*128  FNAM,LINE
      CHARACTER*80   NAMET
      LOGICAL LOPEN, ERROR
C
C---- local array for calling AREAD
      DIMENSION NT(NAFX)
C
C---- open file
C
      ERROR = .FALSE.
      LOPEN = .FALSE.
      LU = 19
      FNAM = FNAMIN
      CALL STRIP(FNAM,NF)
      IF(FNAM.NE.' ') THEN
        OPEN(LU,FILE=FNAM,STATUS='OLD',ERR=98)
        LOPEN = .TRUE.
      ELSE
        RETURN
      ENDIF
C
      ICNT = 0
C
C---- Read header line from ESLOFT input file
C
      CALL RDLINE2(LU,LINE,ICNT)
      IF(LINE.EQ.'END' .OR. LINE.EQ.'ERR') GO TO 210
C
      IF(LINE(1:6).NE.'ESLOFT') THEN
        WRITE(*,*) 'Not an ESLOFT airfoil file'
        CLOSE(LU)
        ERROR = .TRUE.
        RETURN
      ENDIF
C
C--- Read in general data
C
      CALL RDLINE2(LU,LINE,ICNT)
      READ(LINE,*,ERR=210) NAF
C
      CALL RDLINE2(LU,LINE,ICNT)
      READ(LINE,*,ERR=210) NPP2
      NPP = 2*NPP2
C
      CALL RDLINE2(LU,LINE,ICNT)
      READ(LINE,*,ERR=210) PAF
C
      DO I=1,NAF
        CALL RDLINE2(LU,LINE,ICNT)
        READ(LINE,*,ERR=210) NAMET
        CALL STRIP(NAMET,NFT)
        ENAME(I)=NAMET
      ENDDO
C
      DO I=1,NAF
        CALL RDLINE2(LU,LINE,ICNT)
        READ(LINE,*,ERR=210) ALFZ(I)
      ENDDO
C
C---- read in airfoil coordinates
C
      CALL AREADNR(LU, NPX,NAFX, XXE,YYE,
     &             NT,NTEL,
     &             SNAME,ISPARS,IFTYPE)
C
      CALL STRIP(SNAME,NSNAME)
C
      IF(IFTYPE.EQ.0 .OR. NTEL.NE.NAF) THEN
         WRITE(*,1180) NTEL
         NAF = 0
         GOTO 300
      ENDIF  
C
      DO I=1,NAF
        IF(NT(I).NE.NPP) THEN
           WRITE(*,1190) I
           NAF = 0
           ERROR = .TRUE.
           GO TO 300
        ENDIF
      ENDDO
C
C---- calculate geometry parameters
C
      NBB = NPP
      DO I=1,NAF
C
        DO J=1,NBB
          XBB(J)=XXE(J,I)
          YBB(J)=YYE(J,I)
        ENDDO
C
        CALL SCALC (XBB,YBB, SBB,NBB)
        CALL SEGSPL(XBB,XBBP,SBB,NBB)
        CALL SEGSPL(YBB,YBBP,SBB,NBB)
C
        CALL GEOPARX(XBB,XBBP,YBB,YBBP,SBB,NBB, WXX1,
     &            SBLEX,CHORDBX,AREABX,RADBLEX,ANGBTEX,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKBX,CAMBRBX,XTHICKBX,XCAMBRBX)
C
        TOCE(I)  = THICKBX
        TLOC(I)  = XTHICKBX
        CAMLO(I) = CAMBRBX
        CAMLOC(I)= XCAMBRBX
        PAREA(I) = AREABX
        PLERAD(I)= RADBLEX
        PANGTE(I)= ANGBTEX
        TETH(I)  = ABS(YBB(1)-YBB(NPP))
      ENDDO
C
      DO I=1,NAF-1
        IF(TOCE(I).LT.TOCE(I+1)) THEN
          WRITE(*,1200)
          NAF = 0
          ERROR = .TRUE.
          GO TO 300
        ENDIF
      ENDDO
C
C---- end of input, close file
C
      GO TO 300
C...............................................................
   98 CONTINUE
      WRITE(*,1050) FNAM(1:NF)
      ERROR = .TRUE.
      GO TO 300
C
   99 CONTINUE
      WRITE(*,1100) FNAM(1:NF)
      NAF = 0
      ERROR = .TRUE.
      GO TO 300
C
 210  CONTINUE
      WRITE(*,1150) ICNT,FNAM(1:NF)
      NAF = 0
      ERROR = .TRUE.
C
 1050 FORMAT(/' File OPEN error:  ', A)
 1100 FORMAT(/' File READ error:  ', A)
 1150 FORMAT(/' File READ error on line ',I3,':  ', A)
 1180 FORMAT(/,
     $ ' Error reading airfoil data',/,
     $ ' Coordinates read for',I2,' foils')
 1190 FORMAT(/' Airfoil ',I2,' has differing no. of points',/,
     &        ' File read aborted')
 1200 FORMAT(/' File airfoils not of descending thickness',
     &       /' File read aborted')
C
 300  IF(LOPEN) CLOSE(LU)
      RETURN
      END  ! AFLOAD
C
C-----------------------------------------------------------




      SUBROUTINE DATLOAD(FNAMIN,ERROR)
C-----------------------------------------------------------
C     Reads previously saved airfoil file, 
C     repanels airfoil and sorts airfoil array
C-----------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      CHARACTER*(*)  FNAMIN
      CHARACTER*128  FNAM
      CHARACTER*80   NAMET
      CHARACTER*1    ANS
      DIMENSION XT(NPX,NAFX),YT(NPX,NAFX),XXP(NPX,NAFX),YYP(NPX,NAFX)
      INTEGER NT(NAFX)
      LOGICAL ERROR
C
      LU=19
      FNAM = FNAMIN
      ERROR = .FALSE.
C
      CALL AREAD(FNAM,LU,NPX,NAFX, XT,YT,
     &           NT,NRD,
     &           NAMET,ISPARS,IFTYPE)
C
      IF(IFTYPE.EQ.0) THEN
        WRITE(*,*) 'File read error. Aborted'
        ERROR = .TRUE.
        GO TO 500
      ENDIF
C
      IF(IFTYPE.EQ.1) THEN
        WRITE(*,*)'Plain airfoil file'
        CALL ASKS('Enter airfoil name^',NAMET)
      ENDIF
C
      IF(IFTYPE.EQ.4) THEN
        WRITE(*,*) 'Multi-element file, first element used'
      ENDIF
C
      CALL STRIP(NAMET,NNAMET)
C
C---- normalize and repanel airfoil
C
      IAF = 1
      CALL NORMX(NPX,NAFX,IAF,XT,YT,NT)
      CALL AFPANEL(NPX,NAFX,IAF,NT,XT,YT,NPP2,XXP,YYP,PAF)
C
      WRITE(*,*)
      WRITE(*,*)'Airfoil normalized and repaneled'
C
C---- calculate geometry parameters
C
      NPP = 2*NPP2
      NBB = NPP
C
      DO J=1,NBB
        XBB(J)=XXP(J,IAF)
        YBB(J)=YYP(J,IAF)
      ENDDO
C
      TETHT = ABS(YBB(1)-YBB(NPP)) ! the ABS thanks to Dan Angie
      IF(TETHT.LT.0.0001) THEN
        WRITE(*,*)'TE thickness/chord must exceed 0.0001'
        WRITE(*,*)'Airfoil not loaded'
        ERROR = .TRUE.
        GOTO 500
      ENDIF
C
      CALL SCALC (XBB,YBB, SBB,NBB)
      CALL SEGSPL(XBB,XBBP,SBB,NBB)
      CALL SEGSPL(YBB,YBBP,SBB,NBB)
C
      CALL GEOPARX(XBB,XBBP,YBB,YBBP,SBB,NBB, WXX1,
     &            SBLEX,CHORDBX,AREABX,RADBLEX,ANGBTEX,
     &            EI11BA,EI22BA,APX1BA,APX2BA,
     &            EI11BT,EI22BT,APX1BT,APX2BT,
     &            THICKBX,CAMBRBX,XTHICKBX,XCAMBRBX)
C
C---- deal with duplicate t/c
C---- adjust DELTC for min del t/c
C
      DELTC = 0.005
C
      DO I=1,NAF
         DTOC= ABS(THICKBX-TOCE(I))
         IF(DTOC.LT.DELTC) THEN
            WRITE(*,1230)I,I
            READ(*,1000) ANS
            IF(INDEX('Nn',ANS).EQ.0) THEN
              L=I
              NAF= NAF-1
              GO TO 50
            ELSE
              WRITE(*,*)'Airfoil not loaded'
              GO TO 500
            ENDIF
         ENDIF
      ENDDO
C
C---- shuffle airfoil array with decreasing t/c
C
      DO I=1,NAF
        IF(THICKBX.GT.TOCE(I)) THEN
          DO J=NAF,I,-1
            DO K=1,NPP
              XXE(K,J+1)= XXE(K,J)
              YYE(K,J+1)= YYE(K,J)
            ENDDO
C
            TOCE  (J+1)= TOCE(J)
            TLOC  (J+1)= TLOC(J)
            CAMLO (J+1)= CAMLO(J)
            CAMLOC(J+1)= CAMLOC(J)
            PAREA (J+1)= PAREA(J)
            PLERAD(J+1)= PLERAD(J)
            PANGTE(J+1)= PANGTE(J)
            TETH  (J+1)= TETH(J)
            ENAME (J+1)= ENAME(J)
            ALFZ  (J+1)= ALFZ(J)
          ENDDO
C
          L=I
          GO TO 50
        ENDIF
      ENDDO
C
      L=NAF+1
C
C---- load arrays
C
 50   DO K=1,NPP
        XXE(K,L)=XXP(K,IAF)
        YYE(K,L)=YYP(K,IAF)
      ENDDO
C
      TOCE(L)  = THICKBX
      TLOC(L)  = XTHICKBX
      CAMLO(L) = CAMBRBX
      CAMLOC(L)= XCAMBRBX
      PAREA(L) = AREABX
      PLERAD(L)= RADBLEX
      PANGTE(L)= ANGBTEX
      ENAME(L) = NAMET
      TETH(L)  = ABS(YYE(1,L)-YYE(NPP,L))
C
      ALFZ(L) = 0.0
      CALL ASKR('Enter zero-lift alpha (deg)^',ALFZ(L))
      NAF = NAF+1
C
C
 1000 FORMAT(A)          
 1020 FORMAT(/' Max thickness = ',F7.4,' @ ',F7.4 )
 1230 FORMAT(
     & /,' Imported airfoil has similar t/c as airfoil ',I2,
     & /,' Overwrite airfoil ',I2,' ?  Y/n')
C
 500  RETURN
      END  !  DATLOAD
C
C----------------------------------------------------------------
C



      SUBROUTINE DATSAVE(IDAT)
C----------------------------------------------------------------
C     Saves section from parent airfoil set to .dat file
C----------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      CHARACTER ANS*1, FNAM*128
      LU = 19
C
      CALL ASKS(' Enter airfoil filename^',FNAM)
      OPEN(LU,FILE=FNAM,STATUS='OLD',ERR=5)
C
      WRITE(*,*) 'Output file exists. Overwrite?  Y/n'
      READ (*,1000) ANS
      IF(INDEX('Nn',ANS).EQ.0) GO TO 6
C
      CLOSE(LU)
      WRITE(*,*) 'Airfoil not saved.'
      RETURN
C
 5    OPEN(LU,FILE=FNAM,STATUS='NEW',ERR=90)
 6    REWIND(LU)
C 
C--- Write data
C
      WRITE(LU,1040) ENAME(IDAT)
      DO I=1,NPP
         WRITE(LU,1045) XXE(I,IDAT), YYE(I,IDAT)
      ENDDO
C
      CLOSE(LU)
      RETURN
C
 90   WRITE(*,*) 'Bad filename. Airfoil not saved.'
      RETURN
C
 1000 FORMAT(A)
 1040 FORMAT(1X,A40)
 1045 FORMAT(2(F12.6))
C
      END   !  DATSAVE
C
C-------------------------------------------------------------------




      SUBROUTINE LOFTGEOM(NR,ERROR)
C-------------------------------------------------------------------
C    Panels blade to lofting station parameters
C    Splines chord and beta to lofting stations
C    Calculates blade thickness distribution
C      ITTYPE = 1   Linear thickness/chord
C      ITTYPE = 2   Parabolic    "
C      ITTYPE = 3   Splined      "
C      ITTYPE = 4   Linear   thickness
C      ITTYPE = 5   Parabolic    "
C      ITTYPE = 6   Splined      "
C    Interpolates aero and esloft A0 data to lofting stations
C    Corrects Beta using the A0 data
C    Sets flag: data is in place to display loft or generate sections
C--------------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      DIMENSION DSTN(NLSX),DTSTN(NLSX)
      LOGICAL ERROR,SERROR
C
      N=NR
      ERROR=.FALSE.
C
C---- first make sure data is available
C
      IF(.NOT.LROTOR) THEN
        WRITE(*,*)
        WRITE(*,*) 'Rotor not loaded'
        ERROR=.TRUE.
        RETURN
      ENDIF
C
      IF(NAF.LT.2) THEN
         WRITE(*,*)
         WRITE(*,*) 'Minimum of 2 airfoils required'
         ERROR=.TRUE.
         RETURN
      ENDIF
C
C---- all is present and correct----------------------------------- 
C---- calculate stations
C   
      IF(LROUND) THEN
        TCUTM = TCUTW*(1.-XI0)*RAD
      ELSE
        TCUTM  = 0.0
      ENDIF
C
      RHUB = XI0*RAD
      YTOT = RAD - RHUB - TCUTM
      NLINT = NLOFT-1
      DTOT = 0.0
C
      DO I=1,NLINT
         DSTN(I)= 1.0 + FLOAT(I)*(PLOFT-1.0)/FLOAT(NLINT-1)
         DTOT = DTOT + DSTN(I)
      ENDDO
C
      FAC = YTOT/DTOT
      DO I=1,NLINT
         DSTN(I)=DSTN(I)*FAC
      ENDDO
C
      YLOFT(1)= RHUB
      DO I=2,NLOFT
         YLOFT(I)=YLOFT(I-1)+DSTN(I-1)
      ENDDO
C
C---- increase station density at round tip
C
      IF(LROUND) THEN
        SFAC = 0.5 + 1./(2.*TSFAC)
        TSLOC2 = TSLOC / (SFAC + TSLOC*(1.-SFAC))
        BLT = RAD-RHUB
C
        ITSTN = 0.0     
        DO I=NLOFT,1,-1
          IF((RAD-YLOFT(I))/BLT.GE.TSLOC2 .AND. ITSTN.EQ.0.)THEN
            ITSTN = I
          ENDIF
        ENDDO
C
        DO I=ITSTN+1,NLOFT
          DTFAC = 1.0 - (TSFAC-1.)/TSFAC*
     &    (YLOFT(I)-YLOFT(ITSTN))/(YLOFT(NLOFT)-YLOFT(ITSTN))
          DTSTN(I)=(YLOFT(I)-YLOFT(I-1))*DTFAC
        ENDDO
C
        DO I=ITSTN+1,NLOFT
          YLOFT(I)=YLOFT(I-1) + DTSTN(I)
        ENDDO
C
        DTTOT = 0.0
        DO I=2,NLOFT
          DTSTN(I) = YLOFT(I)-YLOFT(I-1)
          DTTOT = DTTOT + DTSTN(I)
        ENDDO
C
        FAC = YTOT/DTTOT
        DO I=2,NLOFT
          YLOFT(I)=YLOFT(I-1) + DTSTN(I)*FAC
        ENDDO
      ENDIF
C
C---- add overshoot stations (if any)
C---- store hub and root station indices
C
      NLOFT2 = NLOFT
      IF(OSHUB.NE.0.0) THEN
         IHUB = 2
         NLOFT2 = NLOFT2+1
         DO I=NLOFT2,2,-1
           YLOFT(I)=YLOFT(I-1)
         ENDDO
         YLOFT(1)=YLOFT(2)-OSHUB/1000.
      ELSE
         IHUB = 1
      ENDIF
C
      ITIP = IHUB+NLOFT-1
C
      IF(LROUND) THEN
        IF(TCUTW.EQ.0.0) THEN
          NLOFTX = NLOFT2
          ITIPX = ITIP
        ELSE
          NLOFTX = NLOFT2+1
          ITIPX  = ITIP+1
          YLOFT(NLOFTX) = RAD
        ENDIF
      ELSE 
        IF(OSTIP.NE.0.0) THEN
          NLOFT2 = NLOFT2+1
          YLOFT(NLOFT2)=YLOFT(NLOFT2-1)+OSTIP/1000.
        ENDIF
        NLOFTX = NLOFT2
        ITIPX = ITIP
      ENDIF       
C
C---- spline blade data to lofting stations
C
      DO I=1,NLOFTX
         CDLOFT(I) = SEVAL(YLOFT(I),RZ1,RZ1S,RZ3,II)
         BELOFT(I) = SEVAL(YLOFT(I),RZ2,RZ2S,RZ3,II)
      ENDDO
C
C--------------------------------------------------------------------
C---- thickness routines
C---- get thickness at root and tip
C
      IF(TKHUB.EQ.0.0) THEN
         THUB=TOCE(1)*CDLOFT(IHUB)
      ELSE
         THUB=TKHUB/1000.
      ENDIF
      TOCH=THUB/CDLOFT(IHUB)
C
      IF(TKTIP.EQ.0.0) THEN
         TTIP=TOCE(NAF)*CDLOFT(ITIP)
      ELSE
         TTIP=TKTIP/1000.
      ENDIF
      TOCT=TTIP/CDLOFT(ITIP)
C
C---- direct to thickness distribution types
C
      IF(ITTYPE.EQ.1) GO TO 100
      IF(ITTYPE.EQ.4) GO TO 200
      IF(ITTYPE.EQ.2) GO TO 300
      IF(ITTYPE.EQ.5) GO TO 400
      IF(ITTYPE.EQ.3) GO TO 500
      IF(ITTYPE.EQ.6) GO TO 550
C
      ITTYPE = 1
      WRITE(*,*) 'Thickness index out of bounds'
      WRITE(*,*) 'Set to linear t/c'
      GO TO 800
C
C---- linear thickness/chord---------------------------------
C
 100  CONTINUE
C
      DO I=1,NLOFT2
        TCLOFT(I)= TOCH - (TOCH-TOCT)*
     &    (YLOFT(I)-YLOFT(IHUB))/(YLOFT(ITIP)-YLOFT(IHUB))
C
        THLOFT(I) = TCLOFT(I)*CDLOFT(I)
      ENDDO
C
C---- locate airfoils
C
      DO I=1,NAF
        NTC(I) = 1
        TCLOC(I,1)= YLOFT(IHUB)+(TCLOFT(IHUB)-TOCE(I))*
     &  (YLOFT(ITIP)-YLOFT(IHUB))/(TCLOFT(IHUB)-TCLOFT(ITIP))
      ENDDO
C
      GO TO 600
C
C---- linear thickness---------------------------------------
C
 200  CONTINUE
C
      DO I=1,NLOFT2
        THLOFT(I)= THUB - (THUB-TTIP)*
     &  (YLOFT(I)-YLOFT(IHUB))/(YLOFT(ITIP)-YLOFT(IHUB))
C
        TCLOFT(I) = THLOFT(I)/CDLOFT(I)
      ENDDO
C
      CALL TCLOCATE
C
      GO TO 600
C
C---- parabolic thickness/chord---------------------------------
C
 300  CONTINUE
C
C---- traps to prevent parabolic calcs blowing up
C
      BLENGTH= YLOFT(ITIP)-YLOFT(IHUB)
      YPARA  = PAXIS*BLENGTH
      PLOC   = YLOFT(IHUB)-YPARA   
C
      IF(PLOC.GE.YLOFT(1)) THEN
         WRITE(*,*)
         WRITE(*,*)'Parabolic axis must be inboard of Station 1'
         WRITE(*,*)'Increase the value of PARA'
         ERROR = .TRUE.
         GO TO 800
      ENDIF
C
      DTC = TOCH - TOCT
      IF(DTC.LT.0.0001) THEN
         WRITE(*,*)
         WRITE(*,*)'Parabolic thickness/chord:'
         WRITE(*,*)'Hub t/c must be greater than tip t/c'
         ERROR = .TRUE.
         GO TO 800
      ENDIF
C
C---- the big equation that took so long to figure out...
C      
      APARA = (BLENGTH + 2.0*YPARA 
     &         - 2.0*SQRT(YPARA*BLENGTH+YPARA**2))/DTC**2
C
      XPARA = SQRT(YPARA/APARA)
      DO I=1,NLOFT2
         YTC = YLOFT(I)+ YPARA - YLOFT(IHUB)
         XTC = SQRT(YTC/APARA)
         TCLOFT(I) = TOCH - XTC + XPARA
         THLOFT(I) = TCLOFT(I)*CDLOFT(I)
      ENDDO
C
C---- locate airfoils
C
C      DO I=1,NAF
C         XTCAF = XPARA + TOCH - TOCE(I)
C         YAF(I)= APARA*XTCAF**2 - YPARA + YLOFT(IHUB)
C         NTC(I)=1
C         TCLOC(I,1) = YAF(I)
C      ENDDO
C
      CALL TCLOCATE
      GO TO 600
C
C---- parabolic thickness---------------------------------------
C
 400  CONTINUE
C
C---- traps
C
      BLENGTH= YLOFT(ITIP)-YLOFT(IHUB)
      YPARA  = PAXIS*BLENGTH
      PLOC   = YLOFT(IHUB)-YPARA
C
      IF(PLOC.GE.YLOFT(1)) THEN
         WRITE(*,*)
         WRITE(*,*)'Parabolic axis must be inboard of Station 1'
         WRITE(*,*)'Increase the value of PARA'
         ERROR = .TRUE.
         GO TO 800
      ENDIF
C
      DTH = THUB - TTIP
      IF(DTH.LT.0.0001) THEN
         WRITE(*,*)
         WRITE(*,*)'Parabolic thickness:'
         WRITE(*,*)'Blade must be thicker at hub than at tip'
         ERROR = .TRUE.
         GO TO 800
      ENDIF
C
C---- the equation again
C      
      APARA = (BLENGTH + 2.0*YPARA 
     &         - 2.0*SQRT(YPARA*BLENGTH+YPARA**2))/DTH**2
C
      XPARA = SQRT(YPARA/APARA)
      DO I=1,NLOFT2
         YTH = YLOFT(I)-YLOFT(IHUB)+YPARA
         XTH = SQRT(YTH/APARA)
         THLOFT(I) = THUB - XTH + XPARA
         TCLOFT(I) = THLOFT(I)/CDLOFT(I)
      ENDDO
C
      CALL TCLOCATE
      GO TO 600
C
C
C---- splined t/c------------------------------------------------
C
 500  CONTINUE
C
      IF(.NOT.LTDEF) THEN
        DO I=1,NLOFT2
          TCLOFT(I) = TOCH - (TOCH-TOCT)*
     &    (YLOFT(I)-YLOFT(IHUB))/(YLOFT(ITIP)-YLOFT(IHUB))
          THLOFT(I) = TCLOFT(I)*CDLOFT(I)
        ENDDO
      ENDIF
C
      IF(LSMOD) THEN
        CALL MODTC
        DO I=1,NLOFT2
          THLOFT(I) = TCLOFT(I)*CDLOFT(I)
        ENDDO
        TKHUB = THLOFT(IHUB)*1000.
        TKTIP = THLOFT(ITIP)*1000.
      ELSE 
        DO I=1,NLOFT2
          TCLOFT(I) = SEVAL(YLOFT(I),TT1,TT1S,TT3,NLOLD) 
        ENDDO
C
        DHUB = TOCH-TCLOFT(IHUB)
        DTIP = TOCT-TCLOFT(ITIP)
        DO I=1,NLOFT2
          TCLOFT(I) = TCLOFT(I)
     &    +DHUB*(YLOFT(ITIP)-YLOFT(I))/(YLOFT(ITIP)-YLOFT(IHUB))
     &    +DTIP*(YLOFT(I)-YLOFT(IHUB))/(YLOFT(ITIP)-YLOFT(IHUB))
          THLOFT(I) = TCLOFT(I)*CDLOFT(I)
        ENDDO
      ENDIF
C
C---- store splines for future use
C
      DO I=1,NLOFT2
        TT1(I) = TCLOFT(I)
        TT2(I) = THLOFT(I)
        TT3(I) = YLOFT(I)
      ENDDO
C
      NLOLD = NLOFT2
      CALL SEGSPL(TT1,TT1S,TT3,NLOLD)
      CALL SEGSPL(TT2,TT2S,TT3,NLOLD)
C
      CALL TCLOCATE
      GO TO 600
C
C
C---- splined t---------------------------------------------------
C
 550  CONTINUE
C
      IF(.NOT.LTDEF) THEN
        DO I=1,NLOFT2
          THLOFT(I)= THUB - (THUB-TTIP)*
     &    (YLOFT(I)-YLOFT(IHUB))/(YLOFT(ITIP)-YLOFT(IHUB))
          TCLOFT(I) = THLOFT(I)/CDLOFT(I)
        ENDDO
      ENDIF
C
      IF(LSMOD) THEN
        CALL MODTH
        DO I=1,NLOFT2
          TCLOFT(I) = THLOFT(I)/CDLOFT(I)
        ENDDO
        TKHUB = THLOFT(IHUB)*1000.
        TKTIP = THLOFT(ITIP)*1000.
      ELSE 
        DO I=1,NLOFT2
          THLOFT(I) = SEVAL(YLOFT(I),TT2,TT2S,TT3,NLOLD) 
        ENDDO
C
        DHUB = THUB-THLOFT(IHUB)
        DTIP = TTIP-THLOFT(ITIP)
        DO I=1,NLOFT2
          THLOFT(I) = THLOFT(I)
     &    +DHUB*(YLOFT(ITIP)-YLOFT(I))/(YLOFT(ITIP)-YLOFT(IHUB))
     &    +DTIP*(YLOFT(I)-YLOFT(IHUB))/(YLOFT(ITIP)-YLOFT(IHUB))
          TCLOFT(I) = THLOFT(I)/CDLOFT(I)
        ENDDO
      ENDIF
C
C---- store splines for future use
C
      DO I=1,NLOFT2
        TT1(I) = TCLOFT(I)
        TT2(I) = THLOFT(I)
        TT3(I) = YLOFT(I)
      ENDDO
C
      NLOLD = NLOFT2
      CALL SEGSPL(TT1,TT1S,TT3,NLOLD)
      CALL SEGSPL(TT2,TT2S,TT3,NLOLD)
C
      CALL TCLOCATE
C
C---- Thickness routines all done----------------------------------------
C---- store pitch axis at loft stations
C
 600  DO I = 1,NLOFTX
        AXLOFT(I) = AXHUB-(AXHUB-AXTIP)*
     &      (YLOFT(I)-YLOFT(IHUB))/(YLOFT(ITIP)-YLOFT(IHUB))
      ENDDO
C
C---- circular root blend or not
C
      IF(LCIRC) CALL ROOTBLEND
C
C---- Beta calcs: interpolate AERO and ESLOFT A0 data to lofting stations 
C
      CALL GETA0LOFT
      CALL GETA0AERO
C
C---- perform Beta correction
C
      DO I=1,NLOFT2
        BDEL = (AZLOFT(I)-AZXROT(I))*DTRX
        BECORR(I) = BELOFT(I)+BDEL
      ENDDO
C
C---- all data required to generate sections is now in place
C
      LCALC=.TRUE.
      LBLEN=.FALSE.
      LTRAN=.FALSE.
      LTDEF=.TRUE.  ! thickness is defined for spline routines
C
 800  RETURN
      END   ! LOFTGEOM
C
C--------------------------------------------------------------------



      SUBROUTINE GETA0LOFT
C--------------------------------------------------------------------
C    Linear interpolation of ESLOFT airfoils A0 to loft stations
C    Interpolation is according to t/c, not blade radial location
C    Allows extrapolation to meet loft stations
C    AZERO reversed in sign for windmills
C--------------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
C
      IF(WIND) THEN
        WFAC = -1.0
      ELSE
        WFAC =  1.0
      ENDIF
C
      IF(LCIRC) THEN
        NAFT = NAF+1
      ELSE
        NAFT = NAF
      ENDIF
C
      DO 200 L=1,NLOFT2
        DO I=1,NAFT
          IF(TOCE(I).EQ.TCLOFT(L)) THEN
             AZLOFT(L)= ALFZ(I) * WFAC
             GO TO 200
          ENDIF
        ENDDO
C
        IF(LCIRC.AND.TCLOFT(L).GT.TOCE(1))THEN
          I1 = NAF+1
          I2 = 1
        ELSE
          DO I = 1,NAF
            IF(TOCE(I).LT.TCLOFT(L)) THEN
              IF(I.EQ.1) THEN
                I1 = 1
                I2 = 2
              ELSE
                I1 = I-1
                I2 = I
              ENDIF
              GO TO 150
            ELSEIF(I.EQ.NAF) THEN
              I1 = NAF-1
              I2 = NAF
            ENDIF
          ENDDO
        ENDIF
C
 150    FAC = (TOCE(I1)-TCLOFT(L))/(TOCE(I1)-TOCE(I2))
        AZLOFT(L)=(ALFZ(I1)-(ALFZ(I1)-ALFZ(I2))*FAC)*WFAC
C
 200  CONTINUE
C
      RETURN
      END   ! GETA0LOFT
C
C-------------------------------------------------------------------




      SUBROUTINE GETA0AERO
C--------------------------------------------------------------------
C    Linear interpolation of AERO airfoils A0 to loft stations
C    Does the same calculation as the (fixed) GETCLCDCM in AERO,but
C    allows extrapolation if required to meet the loft stations
C--------------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      DIMENSION YADIM(NAX)
C
      N = NAERO
C
      IF(N.LT.1) THEN
         WRITE(*,*) 'AERO sections not defined'
         GO TO 300
      ENDIF
C
C---- single section needs no interpolation
C
      IF(N.EQ.1) THEN
         DO I=1,NLOFT2
           AZXROT(I)= AERODATA(1,1)/DTRX
         ENDDO
         GO TO 300
      ENDIF
C
C---- put XI into dimensioned Y
C
      DO I=1,N
         YADIM(I)= XIAERO(I)*RAD
      ENDDO
C
C---- interpolate A0 to loft stations
C
      DO 200 I=1,NLOFT2
        DO J=1,N
          IF(YADIM(J).GT.YLOFT(I))GO TO 100
        ENDDO
        J=N+1
C
 100    CONTINUE
        IF(J.EQ.1) THEN
          I1 = 1
          I2 = 2
        ELSE IF(J.GT.N) THEN
          I1 = N-1
          I2 = N
        ELSE
          I1 = J-1
          I2 = J
        ENDIF
C
        A01 = AERODATA(1,I1)
        A02 = AERODATA(1,I2)
C
        FAC = (YLOFT(I)-YADIM(I1))/(YADIM(I2)-YADIM(I1))
        AZXROT(I)= (A01-(A01-A02)*FAC)/DTRX  
C
 200  CONTINUE
C
 300  CONTINUE
      RETURN
      END   ! GETA0AERO
C
C-------------------------------------------------------------------




      SUBROUTINE GETLNAMES(LONAME,NR,NDSK,ISTN,NAMTYPE,NAMOUT)
C---------------------------------------------------------------------
C     Assembles titles and filenames for various types of loft output
C     NAMTYPE= 1  Returns root name (LONAME) + disk index if NR>1
C     NAMTYPE= 2  Returns root name (+ disk index) + loft station index
C     NAMTYPE= 3  Returns root name (+ disk index) + stn index + '.txt'
C     NAMTYPE= 4  Returns root name (+ disk index) + stn index + '.dat'
C     NAMTYPE= 5  Returns root name (+ disk index) + '-radii.txt'
C     NAMTYPE= 6  Returns 'Stn ' + stn index
C     NAMTYPE= 9  Returns root name + '-ESBLADE.txt'
C
C     NR     - number of disks
C     NDSK  - current disk index
C     ISTN   - current loft station index
C     NAMOUT - output string
C---------------------------------------------------------------------
      CHARACTER*(*)LONAME
      CHARACTER*80 NAMOUT
      CHARACTER  SINDEX(99)*3, DINDEX(4)*5, SIND*3, DIND*5
C
      DATA SINDEX/ 
     $      '-01','-02','-03','-04','-05','-06','-07','-08','-09',
     $'-10','-11','-12','-13','-14','-15','-16','-17','-18','-19',
     $'-20','-21','-22','-23','-24','-25','-26','-27','-28','-29',
     $'-30','-31','-32','-33','-34','-35','-36','-37','-38','-39',
     $'-40','-41','-42','-43','-44','-45','-46','-47','-48','-49',
     $'-50','-51','-52','-53','-54','-55','-56','-57','-58','-59',
     $'-60','-61','-62','-63','-64','-65','-66','-67','-68','-69',
     $'-70','-71','-72','-73','-74','-75','-76','-77','-78','-79',
     $'-80','-81','-82','-83','-84','-85','-86','-87','-88','-89',
     $'-90','-91','-92','-93','-94','-95','-96','-97','-98','-99'/
C
      DATA DINDEX/'-Dsk1','-Dsk2','-Dsk3','-Dsk4'/
C
      IF(ISTN.LT.1 .OR. ISTN.GT.99)THEN
         WRITE(*,*) 'Station index outside range (1-99)'
         SIND = '-**'
      ELSE
         SIND = SINDEX(ISTN)
      END IF
C
      IF(NDSK.LT.1 .OR. NDSK.GT.4)THEN
         WRITE(*,*) 'Disk index outside range (1-4)'
         DIND = '-Dsk*'
      ELSE
         DIND = DINDEX(NDSK)
      END IF
C
      L=LEN(LONAME)
      DO J=L,1,-1
         IF(LONAME(J:J).NE.' ')THEN
            L=J
            GO TO 20
         ENDIF
      ENDDO
C
 20   IF(NAMTYPE.EQ.1) THEN
        IF(NR.GT.1) THEN
          NAMOUT = LONAME(1:L)//DIND
        ELSE
          NAMOUT = LONAME(1:L)
        ENDIF
C
      ELSEIF(NAMTYPE.EQ.2) THEN
        IF(NR.GT.1) THEN
          NAMOUT = LONAME(1:L)//DIND//SIND
        ELSE
          NAMOUT = LONAME(1:L)//SIND
        ENDIF
C
      ELSEIF(NAMTYPE.EQ.3) THEN
        IF(NR.GT.1) THEN
          NAMOUT=LONAME(1:L)//DIND//SIND//'.txt'
        ELSE
          NAMOUT=LONAME(1:L)//SIND//'.txt'
        ENDIF
C
      ELSEIF(NAMTYPE.EQ.4) THEN
        IF(NR.GT.1) THEN
          NAMOUT=LONAME(1:L)//DIND//SIND//'.dat'
        ELSE
          NAMOUT=LONAME(1:L)//SIND//'.dat'
        ENDIF
C
      ELSEIF(NAMTYPE.EQ.5) THEN
        IF(NR.GT.1) THEN
          NAMOUT=LONAME(1:L)//DIND//'-radii.txt'
        ELSE
          NAMOUT=LONAME(1:L)//'-radii.txt'
        ENDIF
C
      ELSEIF(NAMTYPE.EQ.6) THEN
          NAMOUT= 'Stn'//SIND
C
      ELSEIF(NAMTYPE.EQ.9) THEN
        IF(NR.GT.1) THEN
          NAMOUT=LONAME(1:L)//DIND//'-ESBLADE.txt'
        ELSE
          NAMOUT=LONAME(1:L)//'-ESBLADE.txt'
        ENDIF
C
      ELSE
         WRITE(*,*) 'Name type index is out of range (GETLNAMES)'
         NAMOUT = 'ERROR'
      ENDIF
C
      RETURN
      END  ! GETLNAMES
C
C----------------------------------------------------------------------




      SUBROUTINE PRINTLOFT(LU,NDSK)
C--------------------------------------------------------------------
C    Writes loft data to terminal or disk
C--------------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      CHARACTER*80 NAMOUT,OUTSET,TTYPE,TIPTYPE,COORDSET,WINDSET
      LOGICAL ERROR, LXHUB, LXTIP
      CHARACTER*2 HUBTIP
C
      IF(.NOT.LCALC) THEN
         CALL LOFTGEOM(NDSK,ERROR)
         IF(ERROR) RETURN
      ENDIF
C
C---- header
C
      IF(LROUND) THEN
        TIPTYPE= ' Round tip mode'
      ELSE
        TIPTYPE= 'Square tip mode'
      ENDIF
C
      WRITE(LU,*)
C      WRITE(LU,1000)
      WRITE(LU,1010) NAME,TIPTYPE
      WRITE(LU,1000) 
C
C      IF(LTERROR) WRITE(LU,1015)
C
C---- airfoil data
C
      WRITE(LU,1035) SNAME,NPP2,PAF
C
C      DO I=1,NAF
C        YAFMM = YAF(I)*1000.
C        YAFIN = YAF(I)*MTI
C        WRITE(LU,1040) I,ENAME(I),TOCE(I),TETH(I),ALFZ(I),YAFMM,YAFIN
C      ENDDO
C
C      WRITE(LU,1030) NPP2,PAF
C
C---- paneling and thickness data
C
      IF(ITTYPE.EQ.1) THEN
         TTYPE = 'Linear_t/c'
      ELSEIF(ITTYPE.EQ.2) THEN
         TTYPE = 'Parabolic_t/c'
      ELSEIF(ITTYPE.EQ.3) THEN
         TTYPE = 'Splined_t/c'
      ELSEIF(ITTYPE.EQ.4) THEN
         TTYPE = 'Linear_t'
      ELSEIF(ITTYPE.EQ.5) THEN
         TTYPE = 'Parabolic_t'
      ELSEIF(ITTYPE.EQ.6) THEN
         TTYPE = 'Splined_t'
      ELSE
         TTYPE = 'Error'
      ENDIF
C
      WRITE(LU,1000)
      IF(LROUND) THEN
        TCUTMM = TCUTW*(1.-XI0)*RAD*1000.
        WRITE(LU,1106)NLOFT,PLOFT,
     &                TSLOC,TSFAC,AZTIP,
     &                TKHUB,OSHUB,AXHUB,
     &               TKTIP,TCUTMM,AXTIP
      ELSE
        WRITE(LU,1104)NLOFT,PLOFT,AZTIP,
     &                TKHUB,OSHUB,AXHUB,
     &                TKTIP,OSTIP,AXTIP
      ENDIF
C
      IF(ITTYPE.EQ.2 .OR. ITTYPE.EQ.5) THEN
         WRITE(LU,1120) TTYPE,PAXIS,APARA
      ELSE
         WRITE(LU,1110) TTYPE
      ENDIF
C
C---- circular root blend parameters
C
      IF(LCIRC) THEN
        WRITE(LU,1000)
        WRITE(LU,1260)CIRLEN,CIRRAD*1000.,CIRCHD,CIRTHK,CIRBET
      ENDIF
C
C---- station data
C
      WRITE(LU,1000)
      WRITE(LU,1200)
C
      DO I=1,NLOFTX
        YCM  = YLOFT(I) *100.
        YIN  = YLOFT(I) *MTI
        CDCM = CDLOFT(I)*100.
        CDIN = CDLOFT(I)*MTI
        THMM = THLOFT(I)*1000.
        THIN = THLOFT(I)*MTI
C
        BLDEG= BELOFT(I)/DTRX
        BCDEG= BECORR(I)/DTRX
C
        IF(I.EQ.IHUB) THEN
           HUBTIP='H '
        ELSEIF(I.EQ.ITIPX) THEN
           HUBTIP='T '
        ELSE
           HUBTIP='  '
        ENDIF
C
        IF(LROUND .AND. I.EQ.NLOFTX .AND. TCUTW.NE.0.0)THEN
          WRITE(LU,1225)NLOFTX,HUBTIP,YCM,YIN,CDCM,CDIN   
        ELSE
          WRITE(LU,1220)I,HUBTIP,YCM,YIN,CDCM,CDIN,BLDEG,
     &    BCDEG,AZXROT(I),AZLOFT(I),THMM,THIN,TCLOFT(I)
        ENDIF
C
      ENDDO
C
C---- output settings
C
      IF(LL2D) THEN
         IF(OUTFAC.EQ.1.) THEN
           OUTSET='      Output: 2D meters'
         ELSEIF(OUTFAC.EQ.100.) THEN
           OUTSET=' Output: 2D centimeters'
         ELSEIF(OUTFAC.EQ.1000.) THEN
           OUTSET=' Output: 2D millimeters'
         ELSEIF(OUTFAC.EQ.MTI) THEN
           OUTSET='      Output: 2D inches' 
         ELSE
           OUTSET='      Output units ****'
         ENDIF
       ELSE
         IF(OUTFAC.EQ.1.) THEN
           OUTSET='      Output: 3D meters'
         ELSEIF(OUTFAC.EQ.100.) THEN
           OUTSET=' Output: 3D centimeters'
         ELSEIF(OUTFAC.EQ.1000.) THEN
           OUTSET=' Output: 3D millimeters'
         ELSEIF(OUTFAC.EQ.MTI) THEN
           OUTSET='      Output: 3D inches' 
         ELSE
           OUTSET='      Output units ****'
         ENDIF
       ENDIF
C
      IF(WIND) THEN
        WINDSET = 'Windmill mode '
      ELSE
        WINDSET = 'Propeller mode'
      ENDIF
C
      WRITE(LU,1000)
      WRITE(LU,1250) WINDSET,OUTSET
      WRITE(LU,1000)
C
C---- check for extrapololations and warn
C
      LXHUB = .FALSE.
      LXTIP = .FALSE.
      DO I=IHUB,ITIP
        DTCHUB = TCLOFT(I)-TOCE(1)
        DTCTIP = TOCE(NAF)-TCLOFT(I)
        IF(DTCHUB.GT.0.0001) LXHUB=.TRUE.
        IF(DTCTIP.GT.0.0001) LXTIP=.TRUE.
      ENDDO
C
      CDRAT=CDLOFT(ITIPX)/RAD
C
      IF(LXHUB.AND..NOT.LCIRC) THEN
        WRITE(LU,*)
     & 'Warning: sections extrapolated beyond thickest parent'
      ENDIF
C
      IF(LXTIP) THEN
        WRITE(LU,*)
     & 'Warning: sections extrapolated beyond thinnest parent'
      ENDIF
C
      IF(CDRAT.LT.RTCRIT .AND. ITTYPE.GT.3) THEN
        WRITE(LU,*)
     & 'Warning: use t/c distributions for round tips'
      ENDIF
C
C
 1000 FORMAT(1X,83('-'))
 1010 FORMAT( ' Lofted Blade: ',A32, 21X, A16)
 1035 FORMAT( 
     &    ' Parents: ',A21,          'Points per side:',I4,9X,
     &    'Density -le/te  :',F6.2)
C
 1040 FORMAT(1X,I2,3X,A28,F7.4,5X,F7.4,4X,F7.3,6X,F7.2,5X,F7.3)
C
 1104 FORMAT(
     &    ' StationSpec    :',I4,10X, 'Density-hub/tip:',F6.2,7X,
     &    'Dihedral -tip mm: ',F6.2,
     &  /,' ThickSpecHub-mm:',F6.2,8X,'OverShootHub-mm:',F6.2,7X,
     &    'PitchAxisHub-x/c: ',F6.3,
     &  /,' ThickSpecTip   :',F6.2,8X,'OverShootTip   :',F6.2,7X,
     &    'PitchAxisTip    : ',F6.3)
C
 1106 FORMAT(
     &    ' StationSpec    :',I4,10X, 'Density-hub/tip:',F6.2,
     &  /,' TipRegion -blds:',F6.2,8X,'TipRefinement  :',F6.2,7X,
     &    'Dihedral -tip mm: ',F6.2,
     &  /,' ThickSpecHub-mm:',F6.2,8X,'OverShootHub-mm:',F6.2,7X,
     &    'PitchAxisHub-x/c: ',F6.3,
     &  /,' ThickSpecTip   :',F6.2,8X,'TipCut      -mm:',F6.2,7X,
     &    'PitchAxisTip    : ',F6.3)
C
 1110 FORMAT(' Distribution   : ',A15)
C
 1120 FORMAT(
     &       ' Distribution: ',A14,2X, 'AxisLocatn-blds:',F7.3,
     &       '      Coefficient:',G13.4)
C
 1200 FORMAT(  ' Stn  Rad_cm  Rad_in Chd_cm Chd_in  Bxrot',
C               XIIXXFFFFFFFXFFFFFFFXFFFFFFXFFFFFFXXFFFFF
     &    '  Bloft  A0aero A0loft  t_mm   t_in    t/c')
C          XXFFFFFXXFFFFFFXFFFFFFXFFFFFFXFFFFFFXXFFFFFF
 1220 FORMAT(1X,I2,A2,F7.2,1X,F7.2,1X,F6.2,1X,F6.2,2X,F5.2,
     &    2X,F5.2,1X,F6.2,1X,F6.2,1X,F6.2,1X,F6.3,2X,F6.4)
C
 1225 FORMAT(1X,I2,A2,F7.2,1X,F7.2,1X,F6.2,1X,F6.2)
C
 1230 FORMAT(A50,20X,A20,/)
C
 1240 FORMAT(A50,/)
 1250 FORMAT(1X,A14, 45X, A23)
C
 1260 FORMAT(' Circular Root Blend     Length/bladelength : ',F6.3,
     &      3X,' Blade root radius: ',F6.2,' mm',
     &       /,' Chord parameter:',F5.1,
     &      2X,' Thickness parameter: ',F5.1,
     &      4X,' Root beta relax  : ',F5.2,' deg')
C
       RETURN
       END  ! PRINTLOFT
C
C------------------------------------------------------------------------



      SUBROUTINE AFBLEND(NDSK,ERROR)
C--------------------------------------------------------------------
C    Interpolates airfoils to specified thickness/chord
C--------------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      DIMENSION XTEMP(NPX),YTEMP(NPX),STEMP(NPX),XPTMP(NPX),
     &          YPTMP(NPX)
      LOGICAL ERROR
C
      ERROR=.FALSE.
C
      IF(.NOT.LCALC) THEN
         CALL LOFTGEOM(NDSK,ERROR)
         IF(ERROR) RETURN
      ENDIF
C
      IF(LCIRC) THEN
        NAFT = NAF+1
      ELSE
        NAFT = NAF
      ENDIF
C
      NL = NLOFT2
      DO 200 L=1,NL
        DO I=1,NAFT
          IF(TOCE(I).EQ.TCLOFT(L)) THEN
             DO J=1,NPP
               BLXX(J,L)= XXE(J,I)
               BLYY(J,L)= YYE(J,I)
             ENDDO
             GO TO 200
          ENDIF
        ENDDO
C
        IF(LCIRC.AND.TCLOFT(L).GT.TOCE(1)) THEN
          I1 = NAF+1
          I2 = 1
        ELSE
          DO I = 1,NAF
            IF(TOCE(I).LT.TCLOFT(L)) THEN
              IF(I.EQ.1) THEN
                I1 = 1
                I2 = 2
              ELSE
                I1 = I-1
                I2 = I
              ENDIF
              GO TO 150
            ELSEIF(I.EQ.NAF) THEN
              I1 = NAF-1
              I2 = NAF
            ENDIF
          ENDDO
        ENDIF
C
 150    FAC = (TOCE(I1)-TCLOFT(L))/(TOCE(I1)-TOCE(I2))
        DO J=1,NPP
          BLXX(J,L) = XXE(J,I1) - FAC*(XXE(J,I1)-XXE(J,I2))
          BLYY(J,L) = YYE(J,I1) - FAC*(YYE(J,I1)-YYE(J,I2))
        ENDDO
C
 200  CONTINUE
C
C---- load geometry data arrays
C
      DO 300 I=1,NL
         DO J=1,NPP
           XTEMP(J) = BLXX(J,I)
           YTEMP(J) = BLYY(J,I)
         ENDDO
C        
         CALL SCALC (XTEMP,YTEMP,STEMP,NPP)
         CALL SEGSPL(XTEMP,XPTMP,STEMP,NPP)
         CALL SEGSPL(YTEMP,YPTMP,STEMP,NPP)
C
         CALL GEOPARX(XTEMP,XPTMP,YTEMP,YPTMP,STEMP,NPP,WXX1,
     &               SBLEX,CHORDBX,AREABX,RADBLEX,ANGBTEX,
     &               EI11BA,EI22BA,APX1BA,APX2BA,
     &               EI11BT,EI22BT,APX1BT,APX2BT,
     &               THICKBX,CAMBRBX,XTHICKBX,XCAMBRBX)
C
         BLENDATA(1,I)= AREABX
         BLENDATA(2,I)= RADBLEX
         BLENDATA(3,I)= ANGBTEX
         BLENDATA(4,I)= THICKBX
         BLENDATA(5,I)= XTHICKBX
         BLENDATA(6,I)= CAMBRBX
         BLENDATA(7,I)= XCAMBRBX
         BLENDATA(8,I)= YTEMP(1)-YTEMP(NPP)
C
 300  CONTINUE
C
C---- make t/c correction for circular root blends
C---- error due to signifant difference in t_max x/c
C
      IF(LCIRC) THEN
        DO 400 I= 2,NBRK
          YTFAC = TCLOFT(I) / BLENDATA(4,I)
          DO J=1,NPP
            BLYY(J,I) = BLYY(J,I) * YTFAC
            XTEMP(J) = BLXX(J,I)
            YTEMP(J) = BLYY(J,I)
          ENDDO
C        
          CALL SCALC (XTEMP,YTEMP,STEMP,NPP)
          CALL SEGSPL(XTEMP,XPTMP,STEMP,NPP)
          CALL SEGSPL(YTEMP,YPTMP,STEMP,NPP)
C
          CALL GEOPARX(XTEMP,XPTMP,YTEMP,YPTMP,STEMP,NPP,WXX1,
     &               SBLEX,CHORDBX,AREABX,RADBLEX,ANGBTEX,
     &               EI11BA,EI22BA,APX1BA,APX2BA,
     &               EI11BT,EI22BT,APX1BT,APX2BT,
     &               THICKBX,CAMBRBX,XTHICKBX,XCAMBRBX)
C
          BLENDATA(1,I)= AREABX
          BLENDATA(2,I)= RADBLEX
          BLENDATA(3,I)= ANGBTEX
          BLENDATA(4,I)= THICKBX
          BLENDATA(5,I)= XTHICKBX
          BLENDATA(6,I)= CAMBRBX
          BLENDATA(7,I)= XCAMBRBX
          BLENDATA(8,I)= YTEMP(1)-YTEMP(NPP)
 400    CONTINUE
C
      ENDIF
C
C---- all done
C
      LBLEN= .TRUE.
      LTRAN= .FALSE.
C
      RETURN
      END   ! AFBLEND
C
C------------------------------------------------------------------------



      SUBROUTINE AFTRANS(NDSK,ERROR)
C-----------------------------------------------------------------------
C     Transforms blended sections to spec chd, chd axis, beta, dih, wind
C-----------------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      LOGICAL ERROR
      DIMENSION XXT(NPX),YYT(NPX),SST(NPX),XXP(NPX),YYP(NPX)
      DIMENSION XTEMP(NPX),YTEMP(NPX)
C
      ERROR=.FALSE.
C
      IF(.NOT.LCALC) THEN
         CALL LOFTGEOM(NDSK,ERROR)
         IF(ERROR) RETURN
      ENDIF
C
      IF(.NOT.LBLEN) THEN
         CALL AFBLEND(NDSK,ERROR)
         IF(ERROR) RETURN
      ENDIF
C
      IF(LROTATE) THEN
        ROTFAC1=  1.0
        ROTFAC2= -1.0
      ELSE
        ROTFAC1= -1.0
        ROTFAC2=  1.0
      ENDIF
C
      IF(WIND) THEN
        WFAC = -1.0
      ELSE
        WFAC =  1.0
      ENDIF
C
C---- for each loft station & blended airfoil...
C
      NL = NLOFT2
      DO 100 L=1,NL
C
C---- find thickness at pitch axis, upper and lower surfaces
C
        DO I = 1,NPP
          XXT(I) = BLXX(I,L)
          YYT(I) = BLYY(I,L)
        ENDDO
C
        CALL SCALC (XXT,YYT,SST,NPP)
        CALL SEGSPL(XXT,XXP,SST,NPP)
        CALL SEGSPL(YYT,YYP,SST,NPP)
C
        SFAC = 1. + TCLOFT(L)/1.8
        CDAXIS= AXLOFT(L)
        STMP = SFAC * (1. - CDAXIS)
        CALL SINVRT(STMP,CDAXIS,XXT,XXP,SST,NPP)
        YAXU = SEVAL(STMP,YYT,YYP,SST,NPP)
C
        STMP = SFAC * (1. + CDAXIS)
        CALL SINVRT(STMP,CDAXIS,XXT,XXP,SST,NPP)
        YAXL = SEVAL(STMP,YYT,YYP,SST,NPP)
C
        IF(YAXU.EQ.YAXL) THEN
          WRITE(*,*)
          WRITE(*,*)'AFTRANS: Camberline calculation failed'
        ENDIF
C
C-----------------------------------
c        CDAXIS= AXLOFT(L)
c        DO J=2,NPP2
c	  IF(BLXX(J,L).LT.CDAXIS) THEN
c	     NT2 = J
c	     NT1 = J-1
c	     YAXU = BLYY(NT1,L) + (BLXX(NT1,L)-CDAXIS) * 
c     &         (BLYY(NT2,L)-BLYY(NT1,L))/(BLXX(NT1,L)-BLXX(NT2,L))
c	     GOTO 20
c          ENDIF
c        ENDDO
C
c        WRITE(*,*)'Blade thickness calculation failed'
c
c 20	DO J=NPP-1,NPP2+2, -1
c	  IF(BLXX(J,L).LT.CDAXIS) THEN
c	     NT2 = J
c             NT1 = J+1
c	     YAXL = BLYY(NT1,L) + (BLXX(NT1,L)-CDAXIS) * 
c     &         (BLYY(NT2,L)-BLYY(NT1,L))/(BLXX(NT1,L)-BLXX(NT2,L))
c	     GOTO 30
c          ENDIF
c        ENDDO
c
c        WRITE(*,*)'Blade thickness calculation failed'
C
C
 30     CONTINUE
C
C---- calculate axis, translate and scale section
C
	AXY  = (YAXU+YAXL)/2.0 
        DO J=1,NPP
          XTEMP(J) = (BLXX(J,L)-CDAXIS)*CDLOFT(L)
          YTEMP(J) = (BLYY(J,L)-AXY)   *CDLOFT(L) * WFAC
        ENDDO
C
C---- rotate to corrected beta
C
        SBETA = SIN(BECORR(L))
        CBETA = COS(BECORR(L))
        DO J=1,NPP
          TRXX(J,L)= (CBETA*XTEMP(J) + SBETA*YTEMP(J))*ROTFAC1
          TRYY(J,L)=  CBETA*YTEMP(J) - SBETA*XTEMP(J)
        ENDDO
C
C---- translate if dihedral is not zero
C
	IF(AZTIP.NE.0.0) THEN
          AZTIPM = AZTIP/1000.
          YRAD= ((YLOFT(ITIP)-YLOFT(IHUB))**2 + AZTIPM**2) /
     &           (AZTIPM * 2.0)
          ZTHETA = ASIN((YLOFT(L)-YLOFT(IHUB))/YRAD)
          CZAXIS = YRAD*(1.-COS(ZTHETA))   
C
	  DO J=1,NPP
	    TRYY(J,L) = TRYY(J,L) + CZAXIS*WFAC
          ENDDO    
        ENDIF
C
 100  CONTINUE
C
      RETURN
      END   !  AFTRANS
C
C----------------------------------------------------------------------
        

      SUBROUTINE SAVRAD(LU,NDSK,LAUTOS,LOVERW)
      INCLUDE 'XROTOR.INC'
      LOGICAL LAUTOS,LOPEN,LOVERW
      CHARACTER ANS*1,FNAM*80,LNAMT*80,STNT*1
      CHARACTER*30 OUTUN,OUTROT,OUTDIM,OUTRND
C
      LOPEN=.FALSE.
      IF(LU.EQ.6) GO TO 100
C
      IF(.NOT.LAUTOS) THEN
        WRITE(*,1010) LONAME
        CALL ASKS(' Enter loft radii filename^',FNAM)
        IF(FNAM.EQ.'A' .OR. FNAM.EQ.'a') RETURN
      ENDIF
C
      IF(FNAM(1:1).EQ.' ' .OR.LAUTOS) THEN
        CALL GETLNAMES(LONAME,NROTOR,NDSK,1,5,FNAM)
      ENDIF
C
C---- open file
C
      OPEN(LU,FILE=FNAM,STATUS='OLD',ERR=5)
      IF(LOVERW) GO TO 6
C
      WRITE(*,*)
      WRITE(*,*) 'Output file exists. Overwrite?  Y/n'
      READ (*,1000) ANS
      IF(INDEX('Nn',ANS).EQ.0) GO TO 6
C
      CLOSE(LU)
      WRITE(*,*) 'Loft radii file not saved'
      RETURN
C
 5    OPEN(LU,FILE=FNAM,STATUS='NEW',ERR=190)
 6    REWIND(LU)
      LOPEN = .TRUE.
C
C---- write to terminal or file
C
 100  CONTINUE
      CALL GETLNAMES(LONAME,NROTOR,NDSK,1,1,LNAMT)
      TCUTMM = TCUTW*(1.-XI0)*RAD*1000.
C
      IF(OUTFAC.EQ.MTI) THEN
         OUTUN = 'Units: inches'
      ELSEIF(OUTFAC.EQ.1000.) THEN
         OUTUN = 'Units: millimeters'
      ELSEIF(OUTFAC.EQ.100.) THEN
         OUTUN = 'Units: centimeters'
      ELSE
         OUTUN = 'Units: meters'
      ENDIF
C
      IF(LROUND) THEN
        OUTRND = 'Round tip mode'
      ELSE
        OUTRND = 'Square tip mode'
      ENDIF
C
      IF(LROTATE) THEN
         OUTROT = 'Left handed rotor'
      ELSE
         OUTROT = 'Right handed rotor'
      ENDIF
C
      IF(LL2D) THEN
         OUTDIM= '2D points files'
      ELSE
         OUTDIM= '3D points files'
      ENDIF
C
      WRITE(LU,1030)
      WRITE(LU,1020)
      WRITE(LU,1025) LNAMT,OUTROT,OUTRND
      IF(LROUND) WRITE(LU,1070) TCUTMM
      WRITE(LU,1035) OUTDIM,OUTUN
      WRITE(LU,1020)
      WRITE(LU,1040)
C
      DO I=1,NLOFTX
         IF(I.EQ.IHUB) THEN
            STNT = 'H'
         ELSEIF(I.EQ.ITIPX) THEN
            STNT = 'T'
         ELSE
            STNT = ' '
         ENDIF
         RADUN = YLOFT(I)*OUTFAC
         WRITE(LU,1050) I,STNT,RADUN
      ENDDO
C
      WRITE(LU,1020)
      WRITE(LU,*)
C
      IF(LOPEN) THEN
        CLOSE(LU)
        WRITE(*,1060) FNAM
      ENDIF
      RETURN
C
 190  WRITE(*,*) 'Bad filename'
      WRITE(*,*) 'Loft radii file not saved'
      RETURN
C
 1000 FORMAT(A)
 1010 FORMAT(/,' Current loft output base name: ',A30,
     &       /,' <enter> to use base name with std suffix',
     &       /,' <a>  to abort')
C
 1020 FORMAT(1X,29('-'))
 1030 FORMAT(/,'  ESLOFT Lofted Section Radii')
 1025 FORMAT(2X,A40,/, 2X,A20, /, 2X,A20) 
 1035 FORMAT(2X,A20, /, 2X,A20)
C
 1040 FORMAT(1X,' Station            Radius')
C
c 1050 FORMAT(3X,I3,A1,10X,G16.7)   ! exponential output
 1050 FORMAT(3X,I3,A1,7X,F15.7)  ! decimal output
C
 1060 FORMAT(/,' Station radii written to disk: ',A30)
C
 1070 FORMAT(1X,' Tip cut:',F5.2,' mm')
C
      END   ! SAVRAD
C
C----------------------------------------------------------------------




      SUBROUTINE SAVLOFT(NDSK,NPFSAVE,IPFSAVE,IPFTYPE)
C------------------------------------------------------------
C     Saves arbitrary station points-files to disk
C     IPFTYPE = 1   Blended normalized sections saved
C     IPFTYPE = 2   Transformed sections saved
C------------------------------------------------------------
      INCLUDE 'XROTOR.INC'
      DIMENSION IPFSAVE(NLSX)
      CHARACTER ANS*1,FNAM*80,PFNAME*80
      LOGICAL LAUTOS,LOVERW
C
      LU = 19
      LAUTOS = .FALSE.
      LOVERW= .FALSE.
C
      IF(IPFTYPE.EQ.1) THEN
         NAMTYPE = 4
      ELSE
         NAMTYPE = 3
      ENDIF
C
      DO 100 I= 1,NPFSAVE
        IPF = IPFSAVE(I)
C
        IF(.NOT.LAUTOS) THEN
          WRITE(*,1010) IPF, LONAME
          CALL ASKS(' Enter points-file filename^',FNAM)
          IF(FNAM.EQ.'A' .OR. FNAM.EQ.'a') RETURN
          IF(FNAM(1:1).EQ.' ') LAUTOS = .TRUE.
        ENDIF
C
        IF(LAUTOS) THEN
          CALL GETLNAMES(LONAME,NROTOR,NDSK,IPF,NAMTYPE,FNAM)
        ENDIF
C
C---- open file
C
        OPEN(LU,FILE=FNAM,STATUS='OLD',ERR=5)
        IF(LOVERW) GO TO 6
C
        WRITE(*,1015)
        READ (*,1000) ANS
C
        IF(ANS.EQ.'N' .OR. ANS.EQ.'n') THEN
          CLOSE(LU)
          WRITE(*,*) 'Points file not saved'
          RETURN
        ELSEIF(ANS.EQ.'Y' .OR. ANS.EQ.'y') THEN
          GO TO 6
        ELSEIF(ANS.EQ.' ') THEN
          LOVERW = .TRUE.
          GO TO 6
        ENDIF
C
 5      OPEN(LU,FILE=FNAM,STATUS='NEW',ERR=190)
 6      REWIND(LU)
C
C---- write data
C
        IF(IPFTYPE.EQ.1) THEN
          CALL GETLNAMES(LONAME,NROTOR,NDSK,IPF,2,PFNAME)
          WRITE(LU,1040) PFNAME
          DO J=1,NPP
            WRITE(LU,1045) BLXX(J,IPF),BLYY(J,IPF)
          ENDDO
        ELSE
          ZTEMP = YLOFT(IPF)* OUTFAC
          DO J=1,NPP
            XTEMP= TRXX(J,IPF)* OUTFAC
            YTEMP= TRYY(J,IPF)* OUTFAC
            IF(LL2D) THEN
              WRITE(LU,1050) XTEMP,YTEMP
            ELSE
              WRITE(LU,1060) XTEMP,YTEMP,ZTEMP
            ENDIF
          ENDDO
        ENDIF
C
        CLOSE(LU)
        WRITE(*,1080) IPF,FNAM
C
 100  CONTINUE
C
C---- save station radii file if all stations are saved
C
      IF(NPFSAVE.EQ.NLOFT2 .AND. IPFTYPE.EQ.2) THEN
        CALL SAVRAD(LU,NDSK,LAUTOS,LOVERW)
      ENDIF
C
      RETURN
C
 190  WRITE(*,*) 'Bad filename'
      WRITE(*,*) 'Points file not saved'
      RETURN
C
C
C---- formats
C
 1000 FORMAT(A)
 1010 FORMAT(/,' Loft station ',I2,
     &       /,' Current loft output base name: ',A30,
     &       /,' <enter> to use base name with std suffixes',
     &       /,' <a>  to abort')
C
 1015 FORMAT(/,' Output file exists. Overwrite?  y/n',
     &       /,' <enter> to overwrite all')
C
 1040 FORMAT(1X,A40)
 1045 FORMAT(2(F12.6))
C
 1050 FORMAT(2(F14.7))
 1060 FORMAT(3(F14.7))
C 1050 FORMAT(2(G16.7))
C 1060 FORMAT(3(G16.7))
C
 1080 FORMAT(' Station ',I2,' written to disk: ',A32)
C
      END  !  SAVLOFT
C


      SUBROUTINE MODTC
      INCLUDE 'XROTOR.INC'
C------------------------------------------------
C     Takes user cursor input to modify 
C     current thickness/chord array.
C     Modified from MODCH
C     PJC, Esotec Developements, Sept 2011
C------------------------------------------------
      DIMENSION YLIMS(2)
      EXTERNAL PLCHAR,PLMATH
C
      PLFAC = 0.95
      PLPAR = 1.20*PAR
      SH = 0.3*CSIZE
      NSPLT = 20
C
C---- work with temporary arrays
      TCMAX = TCLOFT(1)
      DO I=1, NLOFT2
        W1(I) = YLOFT(I)/YLOFT(ITIP)
        W2(I) = TCLOFT(I)
        TCMAX = MAX(TCLOFT(I),TCMAX)
      ENDDO
      CALL SPLINE(W2,W3,W1,NLOFT2)
C
      DY = 0.01
      YLIMS(1) = 0.
      YLIMS(2) = 1.1*TCMAX
      CALL PLTMODL(NLOFT2,W1,W2,DY,YLIMS,
     &            PLFAC,PLPAR,NSPLT,XOFF,XSF,YOFF,YSF)
C
      XPLT = -5.0*CSIZE
      YPLT = PLPAR - 0.5*DY*YSF - 0.7*CSIZE
c      YPLT = 1.5*DY*YSF - 0.7*CSIZE
      CALL PLCHAR(XPLT,YPLT,1.4*CSIZE,'t/c',0.0,3)
C
C---- get new W2 array
      CALL CRSMOD(NLOFT2,W1,W2,W3,
     &            XOFF,XSF,YOFF,YSF, SH, NSPLT,
     &            LSLOPE, IMOD1,IMOD2 )
C
C---- store as thickness/chords
      DO I = IMOD1, IMOD2
        TCLOFT(I) = W2(I)
      ENDDO
C
      RETURN
      END   ! MODTC



      SUBROUTINE MODTH
      INCLUDE 'XROTOR.INC'
C------------------------------------------------
C     Takes user cursor input to modify 
C     current thickness array.
C     Modified from MODCH
C     PJC, Esotec Developements, Sept 2011
C------------------------------------------------
      DIMENSION YLIMS(2)
      EXTERNAL PLCHAR,PLMATH
C
      PLFAC = 0.95
      PLPAR = 1.20*PAR
      SH = 0.3*CSIZE
      NSPLT = 20
C
C---- work with temporary arrays
      THMAX = THLOFT(1)*1000.
      DO I=1, NLOFT2
        W1(I) = YLOFT(I)/YLOFT(ITIP)
        W2(I) = THLOFT(I)*1000.
        THMAX = MAX(THLOFT(I)*1000.,THMAX)
      ENDDO
      CALL SPLINE(W2,W3,W1,NLOFT2)
C
      DY = 1.0
      YLIMS(1) = 0.
      YLIMS(2) = 1.1*THMAX
      CALL PLTMODL(NLOFT2,W1,W2,DY,YLIMS,
     &            PLFAC,PLPAR,NSPLT,XOFF,XSF,YOFF,YSF)
C
      XPLT = -6.0*CSIZE
      YPLT = PLPAR - 0.5*DY*YSF - 0.7*CSIZE
c      YPLT = 1.5*DY*YSF - 0.7*CSIZE
      CALL PLCHAR(XPLT,YPLT,1.4*CSIZE,'t mm',0.0,4)
C
C---- get new W2 array
      CALL CRSMOD(NLOFT2,W1,W2,W3,
     &            XOFF,XSF,YOFF,YSF, SH, NSPLT,
     &            LSLOPE, IMOD1,IMOD2 )
C
C---- store as thickness
      DO I = IMOD1, IMOD2
        THLOFT(I) = W2(I)/1000.
      ENDDO
C
      RETURN
      END   ! MODTH



      SUBROUTINE PLTMODL(N,X,Y,DYMIN,YLIMS,
     &                  PLFAC,PLPAR,NSPLT,XOFF,XSF,YOFF,YSF)
      INCLUDE 'XROTOR.INC'
      DIMENSION X(N),Y(N)
      DIMENSION YLIMS(2)
C-------------------------------------------------------------
C     Plots Y(X) array with grid overlay, assumes 0<=X<=1
C
C     user can optionally specify Y limits(min,max) or autoscaling 
C     in arrays YLIMS(1) = ymin (999. for autoscale on ymin)
C               YLIMS(2) = ymax (999. for autoscale on ymax)
C
C     Returns the scaling factors and offsets.
C
C     Intended for use with user-driven cursor interaction.
C-------------------------------------------------------------
C     Modified for use by ESLOFT
C     PJC, Esotec Developments, Sept 2011
C-------------------------------------------------------------
      DATA LMASK1, LMASK2, LMASK3 / -32640, -30584, -21846 /
C
      YMIN = Y(1)
      YMAX = Y(1)
      DO I=2, NLOFT2
        YMIN = MIN(YMIN,Y(I))
        YMAX = MAX(YMAX,Y(I))
      ENDDO
      IF(YLIMS(1).NE.999.) YMIN = YLIMS(1) 
      IF(YLIMS(2).NE.999.) YMAX = YLIMS(2) 
C
      IF(ABS(YMAX-YMIN).EQ.0.) THEN
        IF(YMAX.NE.0.) THEN
          YMIN = 0.5*YMAX
          YMAX = 1.5*YMAX
         ELSE
          YMAX = YMIN + 1.0
        ENDIF
      ENDIF
C
      CALL SCALIT(1,YMAX,YMIN,YFAC)
C
      YDEL = MAX( 1.0/(5.0*YFAC) , DYMIN )
      YMIN = YDEL*(AINT(YMIN/YDEL + 1000.8) - 1001.0)
      YMAX = YDEL*(AINT(YMAX/YDEL + 1001.2) - 1000.0)
C
      IF(YMIN.LT.0.0) YMIN = 0.
C
C---- increase y range if it is too narrow for useful editing
c      IF((YMAX-YMIN) .LT. 0.5*(YMAX+YMIN)) THEN
c        YMAX = YMAX + YDEL
c        YMIN = YMIN - YDEL
c      ENDIF
c      IF((YMAX-YMIN) .LT. 0.5*(YMAX+YMIN)) THEN
c        YMAX = YMAX + YDEL
c        YMIN = YMIN - YDEL
c      ENDIF
C
      DYMIN = YDEL
C
      YSF = PLPAR / (YMAX-YMIN)
      YOFF = YMIN
C
      XSF = 1./1.1
      XOFF = 0.0
C
C
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC*SIZE,LPLOT,LLAND)
      CALL PLOTABS(1.0,1.0,-3)
C
      CALL GETCOLOR(ICOL0)
C
      CS = 1.2*CSIZE
      CALL NEWPEN(2)
C
      CALL XAXIS(0.0,0.0,1.0,0.2*XSF, 0.0,0.2, CSIZE,1)
      XPLT = 0.5*XSF - 1.2*CS
      YPLT =     - 2.5*CS
      CALL PLCHAR(XPLT,YPLT,CS,'r/R',0.0,3)
C
      CALL YAXIS(0.0,0.0,PLPAR,YDEL*YSF, YMIN,YDEL, CSIZE,-2)
      IF(LGRID) THEN
       CALL NEWPEN(1)
       CALL NEWCOLORNAME('cyan')
       NXG = 11
       NYG = INT( (YMAX-YMIN)/(0.5*YDEL) + 0.01 )
       CALL PLGRID(0.0,0.0, NXG,0.1*XSF, NYG,0.5*YDEL*YSF, LMASK2 )
      ENDIF
C
      SH = 0.4*CSIZE
      CALL NEWCOLORNAME('blue')
      CALL XYSYMB(NLOFT2,X,Y,XOFF,XSF,YOFF,YSF,SH,1)
C
      CALL NEWCOLOR(ICOL0)
      CALL NEWPEN(2)
      CALL XYLINE(NLOFT2,X,Y,XOFF,XSF,YOFF,YSF,1)
C
      CALL PLFLUSH
C
      RETURN
      END   ! PLOTMODL
C-------------------------------------------------------------


      SUBROUTINE TCLOCATE
      INCLUDE 'XROTOR.INC'
      DIMENSION TOCS(NLSX)
      LOGICAL LHI,SERROR
C
      NYLOC = 100     ! number of intervals calculated on y
      YTX = 0.10      ! searched bladelengths outbd of tip
      IF(LCIRC) THEN
        YRX = 0.0     ! searched bladelengths inbd of root
      ELSE
        YRX = 0.1
      ENDIF
C
      BLENGTH = YLOFT(ITIP)-YLOFT(IHUB)
      YTC1 = YLOFT(IHUB) - YRX*BLENGTH
      YTC2 = YLOFT(ITIP) + YTX*BLENGTH
      YINT = (YTC2-YTC1)/FLOAT(NYLOC)
C
      DO I=1,NAF
        NTC(I) = 0
      ENDDO
C
      CALL SEGSPL(TCLOFT,TOCS,YLOFT,NLOFT2)
C
      DO 200 J=1,NAF
        TCAF = TOCE(J)
        YLOC = YTC1 - YINT
        TCT = 0.0
        DO 100 I=1,NYLOC+1
          YLOC = YLOC + YINT
          TCTL=TCT
          TCT = SEVAL(YLOC,TCLOFT,TOCS,YLOFT,NLOFT2)
C
          IF(I.GE.2 .AND. TCT.EQ.TCTL) GOTO 100
C 
          IF(I.EQ.1) THEN
            TCT2=SEVAL(2.*YLOC,TCLOFT,TOCS,YLOFT,NLOFT2) 
            IF(TCT.LT.TCAF) THEN
              LHI = .FALSE.
            ELSEIF(TCT.GT.TCAF) THEN
              LHI = .TRUE.
            ELSE
              IF(TCT2.LE.TCT) THEN
                LHI = .TRUE.
              ELSE
                LHI = .FALSE.
              ENDIF
            ENDIF
          ENDIF
C
          IF(TCT.EQ.TCAF) THEN
            NTC(J)=NTC(J)+1
            K = NTC(J)
            TCLOC(J,K) = YLOC
            LHI = .NOT.LHI
          ENDIF
C
          IF(TCT.GT.TCAF.AND..NOT.LHI .OR. TCT.LT.TCAF.AND.LHI)THEN
            YAF = YLOC
            CALL SINVRTER(YAF,TCAF,TCLOFT,TOCS,YLOFT,NLOFT2,SERROR)
            IF(.NOT.SERROR .AND. NTC(J)+1.LE.NTCX) THEN
              NTC(J) = NTC(J)+1
              K = NTC(J)
              TCLOC(J,K) = YAF
            ENDIF
            LHI = .NOT.LHI
          ENDIF
C
 100    CONTINUE
 200  CONTINUE
C
      RETURN
      END   ! TCLOCATE
C
C---------------------------------------------------------------------


      SUBROUTINE ROOTBLEND
      INCLUDE 'XROTOR.INC'
C
      DIMENSION C1 (NLSX),C2 (NLSX),C3 (NLSX),TCS(NLSX),
     &          C1S(NLSX),C2S(NLSX),C3S(NLSX),CDS(NLSX),
     &          CLE (3),CTE (3),TBLEN (3),YBLENC(3),
     &          CLES(3),CTES(3),TBLENS(3),YBLENT(3),
     &          XT(NPX,NAFX),YT(NPX,NAFX),NT(NPX),
     &          CDLOFTO(NLSX)
C
C---- spline chordlines in meters
C
      DO I=1,NLOFT2
        CDAXIS= AXLOFT(I)
        C1(I) =  CDAXIS      * CDLOFT(I)
        C2(I) = (CDAXIS-1.0) * CDLOFT(I)
        C3(I) = THLOFT(I)
      ENDDO
C
      CALL SEGSPL(C1,C1S,YLOFT,NLOFT2)
      CALL SEGSPL(C2,C2S,YLOFT,NLOFT2)
      CALL SEGSPL(C3,C3S,YLOFT,NLOFT2)
C
C---- calculate x and derivatives at break chordlines and thickness
C
      YBRK = YLOFT(IHUB) + CIRLEN*(YLOFT(ITIP)-YLOFT(IHUB))
      DO I = 1,NLOFT2
        IF(YLOFT(I).LT.YBRK) NBRK = I
      ENDDO
C
      C1BRK = SEVAL(YBRK,C1,C1S,YLOFT,NLOFT2)
      C2BRK = SEVAL(YBRK,C2,C2S,YLOFT,NLOFT2)
      TBRK  = SEVAL(YBRK,C3,C3S,YLOFT,NLOFT2)
C
      DC1BRK= DEVAL(YBRK,C1,C1S,YLOFT,NLOFT2)
      DC2BRK= DEVAL(YBRK,C2,C2S,YLOFT,NLOFT2)
      DTBRK = DEVAL(YBRK,C3,C3S,YLOFT,NLOFT2)
C
C---- calculate spline points for chordline and thickness blends
C
      YP2C= YBRK - (YBRK-YLOFT(IHUB))* CIRCHD/100.
      YP2T= YLOFT(IHUB) + (YBRK-YLOFT(IHUB))*CIRTHK/100.
C
      YFAC= (YP2C-YLOFT(IHUB))/(YBRK-YLOFT(IHUB))
      CLE(1) = CIRRAD
      CLE(3) = C1BRK
      CLE(2) = CLE(1) + (CLE(3)-CLE(1)) * YFAC       
C
      CTE(1) = -CIRRAD
      CTE(3) = C2BRK
      CTE(2) = CTE(1) + (CTE(3)-CTE(1)) * YFAC
C
      YFAC= (YP2T-YLOFT(IHUB))/(YBRK-YLOFT(IHUB))
      TBLEN(1) = CIRRAD*2.0
      TBLEN(3) = TBRK
      TBLEN(2) = TBLEN(1) + (TBLEN(3)-TBLEN(1)) * YFAC
C
      YBLENC(1) = YLOFT(IHUB)
      YBLENC(2) = YP2C
      YBLENC(3) = YBRK
C
      YBLENT(1) = YLOFT(IHUB)
      YBLENT(2) = YP2T
      YBLENT(3) = YBRK
C
C---- spline chord, t, t/c and pitch axes to loft stations
C
      CALL SPLIND(CLE,  CLES,  YBLENC,3,0.0,DC1BRK)
      CALL SPLIND(CTE,  CTES,  YBLENC,3,0.0,DC2BRK)
      CALL SPLIND(TBLEN,TBLENS,YBLENT,3,0.0,DTBRK)
C
      DO I = 1,NBRK
        THLOFT(I) = SEVAL(YLOFT(I),TBLEN,TBLENS,YBLENT,3)
        TCDLE     = SEVAL(YLOFT(I),CLE,CLES,YBLENC,3)
        TCDTE     = SEVAL(YLOFT(I),CTE,CTES,YBLENC,3)
        CDLOFTO(I) = CDLOFT(I)  ! save chord for beta relaxation
        CDLOFT(I) = TCDLE - TCDTE
        AXLOFT(I) = TCDLE / (TCDLE-TCDTE)
        TCLOFT(I) = THLOFT(I)/CDLOFT(I)
      ENDDO
C
      CALL TCLOCATE
C
C---- relax beta according to relative chord change
C
      IF(WIND) THEN
        WFAC = -1.0
      ELSE
        WFAC = 1.0
      ENDIF
C
C      CDBRK = C1BRK - C2BRK
      FAC1 = (CDLOFTO(1)-CDLOFT(1))/CDLOFTO(1)
      DO I = 1,NBRK
c        BDEL = CIRBET*DTRX * (YBRK-YLOFT(I))/(YBRK-YLOFT(IHUB))
C        BDEL = CIRBET*DTRX*(CDBRK-CDLOFT(I))/(CDBRK-CDLOFT(IHUB))
        FACI = (CDLOFTO(I)-CDLOFT(I))/CDLOFTO(I)
        BDEL = CIRBET*DTRX * FACI/FAC1
        BELOFT(I) = BELOFT(I) - BDEL*WFAC
      ENDDO
C
C---- CIRCLE
C
      DO I = 1,NLOFT2
        IF(TCLOFT(I).GT.TOCE(1)) NTE=I
      ENDDO
      YTE = YLOFT(NTE)
C
      CALL SEGSPL(TCLOFT,TCS,YLOFT,NLOFT2)
      CALL SEGSPL(CDLOFT,CDS,YLOFT,NLOFT2)
      CALL SINVRT(YTE,TOCE(1),TCLOFT,TCS,YLOFT,NLOFT2)
C
      TEFAC = 1.1  ! This is a fudge - scales circle te
C
      TET = SEVAL(YTE,CDLOFT,CDS,YLOFT,NLOFT2)*TETH(1)
      TETC= TEFAC * TET/(2.0 * CIRRAD)
C
      NPPS = 40
      IAF = NAF+1
      TANG = ATAN(TETC)
      DANG = (PI-TANG)/FLOAT(2*NPPS-1)
      DO I = 1,NPPS
        PANG = DANG * (1.+FLOAT(2*(I-1)))
        IT = NPPS+1-I
        IB = NPPS+I
        XT(IT,IAF) = (1.0 - COS(PANG))/2.
        YT(IT,IAF) = SIN(PANG)/2.
        XT(IB,IAF) = XT(IT,IAF)
        YT(IB,IAF) =-YT(IT,IAF)
      ENDDO
C
C---- panel circle
C
      NT(IAF) = 2*NPPS
      CALL AFPANEL(NPX,NAFX,IAF,NT,XT,YT,NPP2,XXE,YYE,PAF)
      TOCE(IAF) = 1.0
      ALFZ(IAF) = 0.0
C
      RETURN
      END  ! ROOTBLEND
C
C---------------------------------------------------------------------


      SUBROUTINE EXPGEOM
      INCLUDE 'XROTOR.INC'
      DIMENSION BLS(NLSX),CLS(NLSX)
C
      CALL SEGSPL(BELOFT,BLS,YLOFT,NLOFT2)
      CALL SEGSPL(CDLOFT,CLS,YLOFT,NLOFT2)
C
      DO I = 1,II
        YXR = XI(I) * RAD
        CH(I)  = SEVAL(YXR,CDLOFT,CLS,YLOFT,NLOFT2) / RAD
        BETA(I)= SEVAL(YXR,BELOFT,BLS,YLOFT,NLOFT2)
      ENDDO
C
      RETURN
      END  ! EXPGEOM
C
C-----------------------------------------------------------------------


      SUBROUTINE LOFTINIT1
      INCLUDE 'XROTOR.INC'
C
      SNAME = 'Unnamed Airfoil Set'  
      NAF   = 0        ! no airfoils loaded
      NPP2  = 60       ! points per side for paneled airfoils
      PAF   = 8.0      ! airfoil point spacing parameter (TE/LE)
      OUTFAC= 1.0      ! lofted points file units are meters
      LL2D  =.FALSE.   ! lofted points files are 3D
      LROTATE =.TRUE.  ! left hand rotation selected
      RTCRIT = 0.025   ! round tip criterion - tip_chord/radius
C
C--- plotting stuff
C
      LGGRIDX =.TRUE.  ! xfoil grid plotting is on
      LGTICKX =.TRUE.  ! xfoil ticks are on
      LGPARMX =.TRUE.  ! airfoil geo parameters will be plotted
C
      RETURN
      END  ! LOFTINIT1
C
C--------------------------------------------------------------------


      SUBROUTINE LOFTINIT2
      INCLUDE 'XROTOR.INC'
C
      TKHUB  = 0.0    ! root thickness defined by thickest airfoil
      TKTIP  = 0.0    ! tip thickness defined by thinnest airfoil
      AXHUB  = 0.35   ! pitch axis at root (x/c)
      AXTIP  = 0.35   ! pitch axis at tip 
      AZTIP  = 0.0    ! dihedral z at tip (mm)
      NLOFT  = 16     ! number of loft stations (not including overshoot)
      PLOFT  = 1.0    ! loft station density factor (hub/tip)
      TSFAC  = 5.0    ! tip station refinement factor
      TSLOC  = 0.2    ! tip station region/bladelength
      OSHUB  = 0.0    ! no overshoot at hub (mm)
      OSTIP  = 0.0    ! no overshoot at tip (mm)
      TCUTD  = 0.01   ! default tip stn inset/bladelength for round tips
      TCUTW  = TCUTD  ! working tip cut (for resetting defaults)
      ITTYPE = 1      ! thickness defined as linear thickness/chord
      PAXIS  = 0.3    ! parabolic axis (units of bladelengths inbd of hub)
      GTICKX = 0.0005 ! tick length on xfoil plots, fraction of arc
      LTDEF  =.FALSE. ! t undefined -initial splined t & t/c are linear
C
      LCIRC  =.FALSE. ! no circular root blend
      CIRRAT = 0.17   ! initial blade root_radius/root_chord
      CIRLEN = 0.2    ! initial blend_length/blade_length
      CIRCHD = 50.    ! chd contour parameter: 1 (thin) --> 99 (thick)
      CIRTHK = 50.    ! t   contour parameter:           "
      CIRBET = 5.0    ! root beta relaxation (degrees)
C
      RETURN
      END  !  LOFTINIT2
C
C---------------------------------------------------------------------


      SUBROUTINE SAVEBLADE
      INCLUDE 'XROTOR.INC'
C------------------------------------------------------------
C     Saves ESBLADE loft data file to disk
C------------------------------------------------------------
      CHARACTER ANS*1,FNAM*80
C
      LU = 19
      NAMTYPE = 9
C
      WRITE(*,1010) LONAME
      CALL ASKS(' Enter ESBLADE filename^',FNAM)
      IF(FNAM.EQ.'A' .OR. FNAM.EQ.'a') RETURN
      IF(FNAM(1:1).EQ.' ') THEN
        CALL GETLNAMES(LONAME,1,1,1,NAMTYPE,FNAM)
      ENDIF
C
C---- open file
C
      OPEN(LU,FILE=FNAM,STATUS='OLD',ERR=5)
      WRITE(*,1015)
      READ (*,1000) ANS
C
      IF(ANS.EQ.'N' .OR. ANS.EQ.'n') THEN
        CLOSE(LU)
        WRITE(*,*) 'File not saved'
        RETURN
      ELSEIF(ANS.EQ.'y' .OR. ANS.EQ.' ') THEN
        GO TO 6
      ENDIF
C
 5    OPEN(LU,FILE=FNAM,STATUS='NEW',ERR=190)
 6    REWIND(LU)
C
C---- write header data
C
      IF(LCIRC) THEN
        ICIRC = 1
      ELSE
        ICIRC = 0
      ENDIF
C
      WRITE(LU,1100) LONAME
      WRITE(LU,1120) ICIRC,NLOFT2,NPP
C
C---- write coordinates
C
      DO IPF = 1,NLOFT2
        WRITE(LU,1130) YLOFT(IPF)
        DO J=1,NPP
          WRITE(LU,1140) TRXX(J,IPF), TRYY(J,IPF)
        ENDDO
      ENDDO
C
      WRITE(LU,*)
      CLOSE(LU)
C
      WRITE(*,1150) FNAM
      RETURN
C
 190  WRITE(*,*) 'Bad filename'
      WRITE(*,*) 'File not saved'
      RETURN
C
C
C---- formats
C
 1000 FORMAT(A)
 1010 FORMAT(/,' Current loft base name: ',A30,
     &       /,' <enter> to use base name with std suffix',
     &       /,' <a>  to abort')
 1015 FORMAT(/,' Output file exists. Overwrite?  Y/n')
C
 1100 FORMAT('ESBLADE Loft Data File generated by ESLOFT',
     &     /,'Loft name: ',A32)
 1120 FORMAT(3(I6))
 1130 FORMAT(F14.7)
 1140 FORMAT(3(F14.7))
 1150 FORMAT(/,' ESBLADE file written to disk: ', A40)
C
      END  !SAVEBLADE



