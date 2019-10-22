C**********************************************************************
C    Module crotor.f
C    
C    Based on XOPER with acknowledgments to Mark Drela and Harold Youngren
C
C    Esotec code Copyright (C) 2011 Philip Carter, Esotec Developments
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
C
      SUBROUTINE CROTOR
      INCLUDE 'XROTOR.INC'
      CHARACTER*4 COMAND, ANS, CONAND
      CHARACTER*132 COMARG, ANSARG, CONARG
      CHARACTER*1 CHKEY, VWTMODE
      CHARACTER*32 DNAME, TNAME
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR
c
C--------------------------------------------------------------
C     Counter-rotation extension for XROTOR
C     Philip Carter, Esotec Developments: May 2008 - Aug 2011
C     Version 0.85
C--------------------------------------------------------------
C
      PLFAC1 = 0.7
      PLFAC2 = 0.8
      PLFACD = 0.6
      XORG = 0.15
      YORG = 0.10
C
      GREEK  = .FALSE.
      LFILER = .FALSE.
C
C--------------------------------------------------------------
C---  Import current rotor geometry (if still current)
C
      LCON = .FALSE.     ! CR system is not converged
C
      IF(LROTF .AND. LFWD) THEN
         IF(NAME .EQ. NAMECR(1)) THEN
            CALL STORCR(1) 
            WRITE(*,*) 'FWD rotor geometry updated'
            IF (FNAMEF.NE.FNAME) THEN
               WRITE(*,*) 'FWD rotor filename updated'
               FNAMEF = FNAME
            ENDIF
         ELSE
            WRITE(*,*) 'FWD rotor not updated'
         ENDIF
      ENDIF
c
      IF (LROTA .AND. .NOT.LFWD) THEN
         IF(NAME .EQ. NAMECR(2)) THEN
            CALL STORCR(2)
            WRITE(*,*) 'AFT rotor geometry updated'
            IF (FNAMEA.NE.FNAME) THEN
               WRITE(*,*) 'AFT rotor filename updated'
               FNAMEA = FNAME
            ENDIF
         ELSE
            WRITE(*,*) 'AFT rotor not updated'
         ENDIF
      ENDIF
C
      VEL = VELCR
C
C--------------------------------------------------------------
C---  Display current CR configuration
C
      IF(LDEF) THEN 
         CALL DISPIN(LUWRIT)
         IF(LFWD .AND. LROTF) THEN
           WRITE(*,*) 'FWD rotor loaded'
         ELSEIF(.NOT.LFWD .AND. LROTA) THEN
           WRITE(*,*) 'AFT rotor loaded'
         ELSE
           WRITE(*,*) 'Rotors not defined'
         ENDIF
      ENDIF
C
C--------------------------------------------------------------
C
 900  CONTINUE
      LFILER = .FALSE.
C
      CALL ASKC('.CROT^',COMAND,COMARG)
C
      DO I=1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
      ENDDO
      NINPUT = 0
      CALL GETINT(COMARG,IINPUT,NINPUT,ERROR)
      NINPUT = 0
      CALL GETFLT(COMARG,RINPUT,NINPUT,ERROR)
C
C--- tidy up for exit
C
      IF(COMAND.EQ.'    ') THEN
         WRITE(*,*)
         IF(LFWD .AND. LROTF) THEN
           WRITE(*,4300) NAMECR(1)
           FNAME = FNAMEF
         ELSEIF(.NOT.LFWD .AND. LROTA) THEN
           WRITE(*,4400) NAMECR(2)
           FNAME = FNAMEA
         ENDIF
         CALL CLRZOOM
         RETURN
      ENDIF
C
      IF(COMAND.EQ.'?   ') WRITE(*,1100)
      IF(COMAND.EQ.'?   ') GO TO 900
      IF(COMAND.EQ.'VRTX') GO TO 1
      IF(COMAND.EQ.'FORM') GO TO 2
      IF(COMAND.EQ.'WAKE') GO TO 3
      IF(COMAND.EQ.'TERS') GO TO 4
      IF(COMAND.EQ.'HARD') GO TO 6
      IF(COMAND.EQ.'SIZE') GO TO 8
      IF(COMAND.EQ.'ANNO') GO TO 9
      IF(COMAND.EQ.'DISP') GO TO 10
      IF(COMAND.EQ.'NAME') GO TO 15
      IF(COMAND.EQ.'NFWD'.OR.COMAND.EQ.'NF  ') GO TO 16
      IF(COMAND.EQ.'NAFT'.OR.COMAND.EQ.'NA  ') GO TO 17
      IF(COMAND.EQ.'WRIT') GO TO 20
      IF(COMAND.EQ.'DUCT') GO TO 22
      IF(COMAND.EQ.'VRAT') GO TO 24
      IF(COMAND.EQ.'PLOT') GO TO 30
      IF(COMAND.EQ.'ATMO') GO TO 35
      IF(COMAND.EQ.'VELO') GO TO 38
      IF(COMAND.EQ.'ANGL') GO TO 50
      IF(COMAND.EQ.'SFWD'.OR.COMAND.EQ.'SF  ') GO TO 60
      IF(COMAND.EQ.'SAFT'.OR.COMAND.EQ.'SA  ') GO TO 65
C
      IF(COMAND.EQ.'ITER') GO TO 75
      IF(COMAND.EQ.'INIT') GO TO 76
      IF(COMAND.EQ.'REIN') GO TO 78
c
      IF(COMAND.EQ.'INPU'.OR.COMAND.EQ.'IN  ') GO TO 100
      IF(COMAND.EQ.'LFWD'.OR.COMAND.EQ.'LF  ') GO TO 120
      IF(COMAND.EQ.'LAFT'.OR.COMAND.EQ.'LA  ') GO TO 130
C
      IF(COMAND.EQ.'PFWD'.OR.COMAND.EQ.'PF  ') GO TO 140
      IF(COMAND.EQ.'PAFT'.OR.COMAND.EQ.'PA  ') GO TO 150
      IF(COMAND.EQ.'RFWD'.OR.COMAND.EQ.'RF  ') GO TO 160
      IF(COMAND.EQ.'RAFT'.OR.COMAND.EQ.'RA  ') GO TO 170
      IF(COMAND.EQ.'VELW') GO TO 180
      IF(COMAND.EQ.'FIX ') GO TO 190
C
      IF(COMAND.EQ.'DISI') GO TO 200
      IF(COMAND.EQ.'DISS') GO TO 205
      IF(COMAND.EQ.'DISV') GO TO 210
      IF(COMAND.EQ.'WRIV') GO TO 220
      IF(COMAND.EQ.'CRIT') GO TO 240
      IF(COMAND.EQ.'CCON') GO TO 250
      IF(COMAND.EQ.'PLIN') GO TO 260
      IF(COMAND.EQ.'VCLR') GO TO 270
      IF(COMAND.EQ.'SYNC') GO TO 275
      IF(COMAND.EQ.'CLCR') GO TO 280
      IF(COMAND.EQ.'BLEN') GO TO 285
C
      IF(COMAND.EQ.'FWD ') GO TO 300
      IF(COMAND.EQ.'AFT ') GO TO 320
      IF(COMAND.EQ.'POWE') GO TO 400
      IF(COMAND.EQ.'RPM ') GO TO 400
C
      IF(COMAND.EQ.'DFWD'.OR.COMAND.EQ.'DF  ') THEN
	 LFORWARD = .TRUE.
	 GO TO 330
      ENDIF
c
      IF(COMAND.EQ.'DAFT'.OR.COMAND.EQ.'DA  ') THEN
	 LFORWARD = .FALSE.
	 GO TO 330
      ENDIF
c
      IF(COMAND.EQ.'Z   ') THEN
       CALL USETZOOM(.TRUE.,.TRUE.)
       CALL REPLOT(IDEV)
       GO TO 900
      ENDIF
      IF(COMAND.EQ.'U   ') THEN
       CALL CLRZOOM
       CALL REPLOT(IDEV)
       GO TO 900
      ENDIF
C
      WRITE(*,1050) COMAND
      GO TO 900
C
C
C---------------------------------------------------------------------
    1 VRTX = .NOT.VRTX
      IF(.NOT.VRTX) WRITE(*,*)'Discrete Vortex Formulation deselected'
      IF(VRTX)      WRITE(*,*)'Discrete Vortex Formulation selected'
      GO TO 900
C
C---------------------------------------------------------------------
    2 FAST = .NOT.FAST
      IF(FAST)      WRITE(*,*)'Graded Momentum Formulation selected'
      IF(.NOT.FAST) WRITE(*,*)'Potential Formulation selected'
      GO TO 900
C
C---------------------------------------------------------------------
    3 FREE = .NOT.FREE
      IF(FREE)      WRITE(*,*)'Self-deforming wake selected'
      IF(.NOT.FREE) WRITE(*,*)'Rigid wake selected'
      GO TO 900
C
C---------------------------------------------------------------------
    4 TERSE = .NOT.TERSE
      IF(TERSE)      WRITE(*,*)'Terse output selected'
      IF(.NOT.TERSE) WRITE(*,*)'Verbose output selected'
      GO TO 900
C
C---------------------------------------------------------------------
C--- Hardcopy current plot
    6 IF(LPLOT) THEN
       CALL PLEND
       CALL REPLOT(IDEVRP)
      ELSE
       WRITE(*,*) 'No current plot'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change plot size
    8 IF(NINPUT.GE.1) THEN
       SIZE = RINPUT(1)
      ELSE
       WRITE(*,*) 'Current plot size =', SIZE
       CALL ASKR('Enter new plot size^',SIZE)
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Annotate plot
    9 IF(LPLOT) THEN
       CALL ANNOT(1.2*CSIZE)
      ELSE
       WRITE(*,*) 'No current plot'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Display current prop operating point data 
C
   10 CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor'
      ELSE
         WRITE(*,*) 'AFT rotor'
      ENDIF
C
      IF(LCON) THEN
         WRITE(*,*) 'CR system converged'
      ELSE
         WRITE(*,*) 'CR system not converged'
      ENDIF
C
      CALL OUTPUT(LUWRIT)
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change CR case name
C
   15 CRNAME = COMARG
      IF(CRNAME(1:1).EQ.' ')
     &  CALL ASKS('Enter CR case name^',CRNAME)
      GO TO 900
C---------------------------------------------------------------------
C--- Change FWD rotor name
C
   16 TNAME = COMARG
      IF(TNAME(1:1).EQ.' ')
     &  CALL ASKS('Enter FWD rotor name^',TNAME)
      NAMECR(1) = TNAME
C
      IF(LFWD .AND. LROTF) NAME = NAMECR(1)
C
      GO TO 900
C---------------------------------------------------------------------
C--- Change AFT rotor name
C
   17 TNAME = COMARG
      IF(TNAME(1:1).EQ.' ')
     &  CALL ASKS('Enter AFT rotor name^',TNAME)
      NAMECR(2) = TNAME
C
      IF(.NOT.LFWD .AND. LROTA) NAME = NAMECR(2)
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- Write current prop operating point data to file
C
   20 IF(COMARG(1:1).NE.' ') SAVFIL = COMARG
      CALL OPFILE(LUSAVE,SAVFIL)      
      CALL DISPIN(LUSAVE)
C
      IF(LFWD) THEN
         WRITE(LUSAVE,*) 'FWD rotor'
      ELSE
         WRITE(LUSAVE,*) 'AFT rotor'
      ENDIF
C
      CALL OUTPUT(LUSAVE)
      CLOSE(LUSAVE)
      GO TO 900
C
C--------------------------------------------------------------
 22   DUCT = .NOT.DUCT
      IF(DUCT) THEN
       WRITE(*,*) 'Duct option selected'
       IF(NINPUT.GE.1) THEN
        URDUCT = RINPUT(1)
       ELSE
        CALL ASKR('Enter Aexit/Aprop for duct^',URDUCT)
       ENDIF
      ELSE
       WRITE(*,*) 'Free-tip option selected'
       URDUCT = 1.0
      ENDIF
      GO TO 900
C
C--------------------------------------------------------------
 24   IF(DUCT) THEN
       IF(NINPUT.GE.1) THEN
        URDUCT = RINPUT(1)
       ELSE
        CALL ASKR('Enter Aexit/Aprop for duct^',URDUCT)
       ENDIF
      ELSE
       WRITE(*,*) '*** Select duct option first'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Plot stuff
C--- Borrowed from XOPER
C
   30 IF(NINPUT.GE.1) THEN
       NPLOT = IINPUT(1)
      ELSE
       WRITE(*,2000)
       NPLOT = 3
       CALL ASKI('select plot number^',NPLOT)
      ENDIF
C
      IF(NPLOT.EQ.0) THEN
       GO TO 900
C--- 3 view geometry plot of single blade
      ELSE IF(NPLOT.EQ.1) THEN
       CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC1*SIZE,LPLOT,LLAND)
       CALL PLOT(XORG,YORG,-3)
       CALL GEOPLT('ALUE')
C--- Geometry of all blades, axial view
      ELSE IF(NPLOT.EQ.2) THEN
        CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFACD*SIZE,LPLOT,.NOT.LLAND)
        CALL PLOT(0.175,0.175,-3)
        CALL PRPPLT
C--- Plot of operating point (Gam, CL, M, eff) + data
      ELSE IF(NPLOT.EQ.3) THEN
       CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC2*SIZE,LPLOT,LLAND)
       CALL PLOT(XORG,YORG,-3)
       CALL CLPLT
C--- Combined geometry and operating point
      ELSEIF(NPLOT.EQ.4) THEN
       CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFACD*SIZE,LPLOT,.NOT.LLAND)
       CALL PLOT(0.175,0.075,-3)
       CALL GEOPLT('AL')
       CALL PLOTABS(0.0,0.0,-3)
       CALL PLOT(0.175,0.875,-3)
       CALL CLPLT
C--- Data for stored cases (vs r/R)
      ELSE IF(NPLOT.EQ.5) THEN
       CALL ACLPLT
C--- Case sequence parameters
      ELSE IF(NPLOT.EQ.6) THEN
       CALL CASPLT
C--- Induced velocities on blade
      ELSE IF(NPLOT.EQ.7) THEN
       CALL UVIPLT
C--- Induced velocities immediately downstream of rotor
      ELSE IF(NPLOT.EQ.8) THEN
c       CALL UVIPLT2
       CALL UVIPLT3
C--- Velocity triangles
      ELSE IF(NPLOT.EQ.9) THEN
       CALL TRIPLT
C--- Imposed external slipstream velocities
      ELSE IF(NPLOT.EQ.10) THEN
       IF(NADD.LT.2) THEN
        WRITE(*,*) 'No slipstream profiles present'
        GO TO 900
       ENDIF
       CALL VELPLT
C--- Plot reference x,y data
      ELSE IF(NPLOT.EQ.11) THEN
       FNAME = ' '
       CALL REFPLT(FNAME, XYOFF(1),XYOFF(2),XYFAC(1),XYFAC(2),
     &             0.5*CSIZE, 1)
C--- Plot blade parameters vs r/R
      ELSE IF(NPLOT.EQ.12) THEN
        CALL PLOT_DATA(NAME)
C
C
      ELSE IF(NPLOT.EQ.13) THEN
        IF(LCON) THEN
          CALL CRPLIN
        ELSE
           WRITE(*,*) 'CR System is not converged'
        ENDIF
C
      ELSE
       NINPUT = 0
       GO TO 30
C
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change altitude
   35 IF(NINPUT.GE.1) THEN
       ALT = RINPUT(1)
      ELSE
       CALL ASKR('flight altitude (km)^',ALT)
      ENDIF
      CALL ATMO(ALT,VSO,RHO,RMU)
      CALL FLOSHO(LUWRIT, VSO, RHO, RMU)
      CONV = .FALSE.
      LCON = .FALSE.
      GO TO 900
C
C---------------------------------------------------------------------
C--- Change flight velocity
   38 VELOLD = VEL
      IF(NINPUT.GE.1) THEN
       VEL = RINPUT(1)
      ELSE
       CALL ASKR('flight speed (m/s)^',VEL)
      ENDIF
C--- Change CT,CQ,CP to give same thrust,torque,power
      THR  = TTOT *(RHO*VELOLD**2*RAD**2)
      TTOT = THR / (RHO*VEL**2*RAD**2)
      TRQ  = QTOT *(RHO*VELOLD**2*RAD**3)
      QTOT = TRQ / (RHO*VEL**2*RAD**3)
      PWR  = PTOT *(RHO*VELOLD**3*RAD**2)
      PTOT = PWR / (RHO*VEL**3*RAD**2)
C
      VELCR = VEL
      CONV  = .FALSE.
      LCON  = .FALSE.
      GO TO 900
C
C
C---------------------------------------------------------------------
C--- Change blade angle - current rotor
C
 50   IF(NINPUT.GE.1) THEN
       DELB = RINPUT(1)
      ELSE
         IF(LFWD) THEN
           CALL ASKR('Enter FWD rotor angle change (deg)^',DELB)
         ELSE
           CALL ASKR('Enter AFT rotor angle change (deg)^',DELB)
         ENDIF
      ENDIF
C
      DO I=1, II
        BETA(I)  = BETA(I)  + DELB*PI/180.
        BETA0(I) = BETA0(I) + DELB*PI/180.
      ENDDO
C
      CONV = .FALSE.
      LCON = .FALSE.
      GO TO 900      
C
C---------------------------------------------------------------------
C--- Save FWD rotor to disk
C
 60   IF(.NOT.LROTF) THEN
        WRITE(*,*)'FWD rotor does not exist'
        GO TO 900
      ENDIF
C
      CALL LOADCR(1)

      IF(COMARG(1:1) .EQ. ' ') THEN
        IF(FNAMEF .NE. 'none') COMARG = FNAMEF
      ENDIF
C
      CALL SAVE(COMARG)
C
      IF(.NOT.LFILER) THEN
        FNAMEF = FNAME
        WRITE(*,61) FNAMEF
      ENDIF
      GO TO 900
C
 61   FORMAT(' FWD rotor saved to disk',
     &     /,' Filename: ',A32)
C
C---------------------------------------------------------------------
C--- Save AFT rotor to disk
C
 65   IF(.NOT.LROTA) THEN
        WRITE(*,*)'AFT rotor does not exist'
        GO TO 900
      ENDIF
C
      CALL LOADCR(2)

      IF(COMARG(1:1) .EQ. ' ') THEN
        IF(FNAMEA .NE. 'none') COMARG = FNAMEA
      ENDIF
C
      CALL SAVE(COMARG)
C
      IF(.NOT.LFILER) THEN
        FNAMEA = FNAME
        WRITE(*,66) FNAMEA
      ENDIF
      GO TO 900
C
 66   FORMAT(' AFT rotor saved to disk',
     &     /,' Filename: ',A32)
C
C---------------------------------------------------------------------
C--- Set max number or iterations for nonlinear solution
 75   IF(NINPUT.GE.1) THEN
       NITERA = IINPUT(1)
      ELSE
       CALL ASKI('Max number of iterations^',NITERA)
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Toggle initialization flag 
 76   LOPRINI = .NOT.LOPRINI
      IF(LOPRINI) THEN
       WRITE(*,*) 'Analysis case will be initialized'
      ELSE
       WRITE(*,*) 'Analysis case will not be initialized'
      ENDIF
      GO TO 900
C
C---------------------------------------------------------------------
C--- Reinitialize operating point
 78   CALL REINIT
      GO TO 900
C
C---------------------------------------------------------------------
C
C---------------------------------------------------------------------
C--- INPU - input CR specifications
c
C---  Specify Power
C
 100  CALL ASKR('Enter total power input (W)^',POWERT)
      POWERF = POWERT/2.0
      POWERA = POWERF
C
C---  Specify RPM
C
      IF(LSRPM) THEN
         IF(RRAT.EQ.1.0) THEN
           CALL ASKR('Enter rpm - both rotors^',RPMF)
         ELSE
           CALL ASKR('Enter rpm - FWD rotor^',RPMF)
         ENDIF
         RPMA = RPMF * RRAT
      ELSE
         CALL ASKR('Enter rpm - FWD rotor^',RPMF)
         CALL ASKR('Enter rpm - AFT rotor^',RPMA)
      ENDIF
C
C--- Specify flight speed
C
      VELOLD = VEL
      CALL ASKR('Enter flight speed - m/s^',VELCR)
      VEL = VELCR
C
C--- Change CT,CQ,CP to give same thrust,torque,power
      THR  = TTOT *(RHO*VELOLD**2*RAD**2)
      TTOT = THR / (RHO*VEL**2*RAD**2)
      TRQ  = QTOT *(RHO*VELOLD**2*RAD**3)
      QTOT = TRQ / (RHO*VEL**2*RAD**3)
      PWR  = PTOT *(RHO*VELOLD**3*RAD**2)
      PTOT = PWR / (RHO*VEL**3*RAD**2)
C
C---  End input
C
      LCON = .FALSE.
      LDEF = .TRUE.
C
      CALL DISPIN(LUWRIT)
      IF(LFWD .AND. LROTF) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSEIF (.NOT.LFWD .AND. LROTA) THEN
         WRITE(*,*) 'AFT rotor loaded'
      ELSE
         WRITE(*,*) 'Rotors not defined'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- LFWD - Load forward rotor
C
 120  IF(COMARG(1:1).NE.' ') THEN
        DNAME = COMARG
        CALL LOADF(DNAME)
        IF(LFILER) GOTO 900
      ELSE
        WRITE(*,122) NAMECR(1),NAME
        CALL ASKS('Enter FWD rotor filename^',DNAME)
C
        IF(DNAME.EQ.'A' .OR. DNAME.EQ.'a') THEN
           GO TO 900
        ELSEIF(DNAME(1:1).EQ.' ') THEN
           IF(LROTOR) THEN
             LFWD  = .TRUE.
             CALL STORCR(1)
             FNAMEF = 'none'
             LCON = .FALSE.
           ELSE
             WRITE(*,*) 'There is no loaded rotor'
             GOTO 900
           ENDIF
        ELSE
           CALL LOADF(DNAME)
           IF(LFILER) GOTO 900
        ENDIF
      ENDIF
C
      CALL DISPIN(LUWRIT)
      WRITE(*,*) 'FWD rotor loaded'
      LROTF = .TRUE.
      GO TO 900
C
 122  FORMAT(' Replacing FWD rotor: ',A32,
     &     /,' <a> to abort',
     &     /,' <ret> to store current rotor: ',A32)
C
C---------------------------------------------------------------------
C--- LAFT - Load aft propeller
C
 130  IF(COMARG(1:1).NE.' ') THEN
        DNAME = COMARG
        CALL LOADA(DNAME)
        IF(LFILER) GOTO 900
      ELSE
        WRITE(*,132) NAMECR(2),NAME
        CALL ASKS('Enter AFT rotor filename^',DNAME)
C
        IF(DNAME.EQ.'A' .OR. DNAME.EQ.'a') THEN
           GO TO 900
        ELSEIF(DNAME(1:1).EQ.' ') THEN
           IF(LROTOR) THEN
             LFWD  = .FALSE.
             CALL STORCR(2)
             FNAMEA = 'none'
             LCON = .FALSE.
           ELSE
             WRITE(*,*) 'There is no loaded rotor'
             GOTO 900
           ENDIF
        ELSE
           CALL LOADA(DNAME)
           IF(LFILER) GOTO 900
        ENDIF
      ENDIF       
C
      CALL DISPIN(LUWRIT)
      WRITE(*,*) 'AFT rotor loaded'
      LROTA = .TRUE.
      GO TO 900
C
 132  FORMAT(' Replacing AFT rotor: ',A32,
     &     /,' <a> to abort',
     &     /,' <ret> to store current rotor: ',A32)
C
C---------------------------------------------------------------------
C--- PFWD - change power, forward propeller
C
 140  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        POWERF = RINPUT(1)
      ELSE
        CALL ASKR('Enter power (W) - FWD rotor^',POWERF)
      ENDIF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD .AND. LROTF) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSEIF (.NOT.LFWD .AND. LROTA) THEN
         WRITE(*,*) 'AFT rotor loaded'
      ELSE
         WRITE(*,*) 'Rotors not defined'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- PAFT - change power, aft propeller
C
 150  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        POWERA = RINPUT(1)
      ELSE
        CALL ASKR('Enter power (W) - AFT rotor^',POWERA)
      ENDIF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD .AND. LROTF) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSEIF (.NOT.LFWD .AND. LROTA) THEN
         WRITE(*,*) 'AFT rotor loaded'
      ELSE
         WRITE(*,*) 'Rotors not defined'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- RFWD - change rpm, forward propeller
C
 160  IF(.NOT.LDEF) THEN
        WRITE(*,*) 'Use INPUT command first'
        GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        RPMF = RINPUT(1)
      ELSE
        CALL ASKR('Enter rpm - FWD rotor^',RPMF)
      ENDIF
C
      IF(LSRPM) RPMA = RPMF*RRAT
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD .AND. LROTF) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSEIF (.NOT.LFWD .AND. LROTA) THEN
         WRITE(*,*) 'AFT rotor loaded'
      ELSE
         WRITE(*,*) 'Rotors not defined'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C---  RAFT - change rpm, aft propeller
C
 170  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF
C
      IF (NINPUT.GE.1) THEN
        RPMA = RINPUT(1)
      ELSE
        CALL ASKR('Enter rpm - AFT rotor^',RPMA)
      ENDIF
C
      IF(LSRPM) RPMF = RPMA/RRAT
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD .AND. LROTF) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSEIF (.NOT.LFWD .AND. LROTA) THEN
         WRITE(*,*) 'AFT rotor loaded'
      ELSE
         WRITE(*,*) 'Rotors not defined'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- VELW - specify velocity weights
C
 180  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      WRITE(*,6000)
 6000 FORMAT(     ' D for default velocity weights',
     &        /,  ' S to specify',
     &        /,  '<ret> to leave unchanged')
C
      CALL ASKS('Select velocity weight mode^', VWTMODE)
c
      IF(VWTMODE .EQ. ' ') THEN
          GOTO 900
	ELSEIF(VWTMODE .EQ. 'D' .OR. VWTMODE .EQ. 'd') THEN
          UWTF = 0.5
          UWTA = 0.5
          VWTF = 0.0
          VWTA = -1.0
        ELSEIF(VWTMODE .EQ. 'S' .OR. VWTMODE .EQ. 's') THEN    
          CALL ASKR('Enter FWD axial velocity weight (0 ===> 1)^',UWTF)
          CALL ASKR('Enter AFT axial velocity weight (0 ===> 1)^',UWTA)
          CALL ASKR('Enter FWD tang. velocity weight (0 , +/-1)^',VWTF)
          CALL ASKR('Enter AFT tang. velocity weight (0 , +/-1)^',VWTA)
        ELSE	  
          GOTO 180
      ENDIF
C
      LCON = .FALSE.
      CONV = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD .AND. LROTF) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSEIF(.NOT.LFWD .AND. LROTA) THEN
         WRITE(*,*) 'AFT rotor loaded'
      ELSE
         WRITE(*,*) 'Rotors not defined'
      ENDIF
C
      GO TO 900
C
C
C---------------------------------------------------------------------
C--- FIX - fixed pitch/rpm toggle
C
 190  LPITCH = .NOT.LPITCH
C
      IF(LPITCH) THEN
         WRITE(*,*) 'Blade pitch is fixed, rpm is variable'
      ELSE
         WRITE(*,*) 'Rpm is fixed, pitch is variable'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- DISI - display CR specifications
C
 200  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      END IF  
C
      CALL DISPIN(LUWRIT)
C
      IF(LFWD .AND. LROTF) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSEIF(.NOT.LFWD .AND. LROTA) THEN
         WRITE(*,*) 'AFT rotor loaded'
      ELSE
         WRITE(*,*) 'Rotors not defined'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- DISS - display external slipstreams
C
 205  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      ENDIF  
C
      IF(.NOT.LROTF .AND. .NOT.LROTA) THEN
	WRITE(*,*) 'Load rotors first'
        GO TO 900
      ENDIF
C     
      IF(NADDF.EQ.0 .AND. NADDA.EQ.0) THEN
         WRITE(*,*) 'There are no external slipstreams to display'
         GO TO 900
      ENDIF
C
      IF(RADCR(1).EQ.RADCR(2) .AND. XI0CR(1).EQ.XI0CR(2) .AND.
     &    NADDF.EQ.NADDA) THEN
C
        WRITE(*,3900)
        DO I=1,NADDF
          WRITE(*,4000) RADDF(I),UADDF(I),VADDF(I),UADDA(I),VADDA(I)
        ENDDO
        WRITE(*,4100)
      ELSE
        WRITE(*,3600)
        WRITE(*,3800) (RADDF(I),UADDF(I),VADDF(I), I=1, NADDF)
        WRITE(*,3700)
        WRITE(*,3800) (RADDA(I),UADDA(I),VADDA(I), I=1, NADDA)
        WRITE(*,4200)
      ENDIF
C
      IF(LCON) THEN
         WRITE(*,*) 'Slipstream converged'
      ELSE
         WRITE(*,*) 'Slipstream not converged'
      ENDIF
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF   
CC      
      GO TO 900
C
C---------------------------------------------------------------------
C--- DISV - display induced velocity profiles
C
 210  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      ENDIF  
C
      IF(.NOT.LROTF .AND. .NOT.LROTA) THEN
	WRITE(*,*) 'Load rotors first'
        GO TO 900
      ENDIF
C
      CALL OUTVEL(LUWRIT)
C     
      IF(LCON) THEN
        WRITE(*,*) 'Slipstream converged'  
      ELSE
        WRITE(*,*) 'Slipstream not converged'
      ENDIF
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF   
C
      GO TO 900
C
CC---------------------------------------------------------------------
C--- WRIV - write velocity profiles to disk
C
 220  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      ENDIF  
C
      IF(.NOT.LROTF .AND. .NOT.LROTA) THEN
	WRITE(*,*) 'Load rotors first'
        GO TO 900
      ENDIF
C
      IF(COMARG(1:1).NE.' ') SAVFIL = COMARG
C
      CALL OPFILE(LUSAVE,SAVFIL)      
      CALL DISPIN(LUSAVE)
      CALL OUTVEL(LUSAVE)
C
      IF(LCON) THEN
        WRITE(*,*) 'Slipstream converged'  
      ELSE
        WRITE(*,*) 'Slipstream not converged'
      ENDIF
C
      CLOSE(LUSAVE)
      GO TO 900
C
C---------------------------------------------------------------------
C--- CRIT - set CR iteration limit
C
 240  IF(NINPUT.GE.1) THEN
        ICRITL = IINPUT(1)
      ELSE
        CALL ASKI('Enter max number of CR iterations^',ICRITL)
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- CCON - set CR convergence delta
C
 250  IF(NINPUT.GE.1) THEN
        CRCON = RINPUT(1)
      ELSE
        CALL ASKR('Enter new CR convergence delta^',CRCON)
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- PLIN - plot combined CR induced velocities
C
 260  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      ENDIF  
C
      IF(.NOT.LROTF .OR. .NOT.LROTA) THEN
	WRITE(*,*) 'Load rotors first'
        GO TO 900
      ENDIF
C
      IF(.NOT.LCON) WRITE(*,*) 'Slipstream is not converged'
C
      CALL CRPLIN
      GO TO 900
C
C---------------------------------------------------------------------
C--- VCLR - initialize slipstream velocities
C
 270  NADD  = 0
      NADDF = 0
      NADDA = 0
C
      WRITE(*,*) 'Current and stored slipstreams zeroed'
      LCON = .FALSE.
      CONV = .FALSE.
      LVCLR= .TRUE.
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- SYNC - synchronize rpms toggle
C
 275  LSRPM = .NOT.LSRPM
C
      WRITE(*,*)
      IF(LSRPM) THEN
         WRITE(*,*) 'Rotor speeds will be synchronized'
         CALL ASKR('Enter speed ratio (Rpm_AFT / Rpm_FWD)^',RRAT)         
         RPMA = RPMF * RRAT
      ELSE
         WRITE(*,*) 'Rotor speeds will not be synchronized'
      ENDIF
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- CLCR - change MIL design lift coefficients
C
 280  IF(NINPUT.GE.1) THEN
        CRCLR = RINPUT(1)
        CRCLT = CRCLR
      ELSE
        WRITE(*,*)
        WRITE(*,4500) CRCLR,CRCLT
        CALL ASKR('Enter design CL at blade root^',CRCLR)
        CALL ASKR('Enter design CL at blade tip^',CRCLT)
      ENDIF
C
      GO TO 900
C
C
C---------------------------------------------------------------------
C--- BLEN - blend between loaded rotors
C
 285  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GO TO 900
      ENDIF  
C
      IF(.NOT.LROTF .OR. .NOT.LROTA) THEN
	WRITE(*,*) 'Load rotors first'
        GO TO 900
      ENDIF
C
      WRITE(*,*)
      IF(LFWD) THEN
        WRITE(*,*) 'Replacing FWD rotor: ', NAMECR(1)
      ELSE
        WRITE(*,*) 'Replacing AFT rotor: ', NAMECR(2)
      ENDIF
C
      WRITE(*,*)'<a> to abort  <enter> to leave name unchanged'
C
      CALL ASKS('Enter new rotor name^',BNAME)
C
      IF(BNAME.EQ.'A' .OR. BNAME.EQ.'a') GO TO 900
      IF(BNAME(1:1) .NE. ' ') NAME = BNAME
C
      WRITE(*,*)
      WRITE(*,*)   '       FWD   0 <------> 1   AFT'
C
      CALL ASKR('Enter rotor interpolation factor^',BLENDF)
C
      LBA = .FALSE.
      LBC = .FALSE.
C
 288  CALL ASKS('Blend angles, chords, or both? (a,c,b)^',CHKEY)
C
      IF (CHKEY.EQ.' ') THEN
         WRITE(*,*) 'Interpolation aborted - geometry unchanged'
         GO TO 900
      ENDIF
C         
      IF    (CHKEY.EQ.'A' .OR. CHKEY.EQ.'a') THEN
         LBA = .TRUE.
      ELSEIF(CHKEY.EQ.'C' .OR. CHKEY.EQ.'c') THEN
         LBC = .TRUE.
      ELSEIF(CHKEY.EQ.'B' .OR. CHKEY.EQ.'b') THEN
         LBA = .TRUE.
         LBC = .TRUE.
      ELSE
         GO TO 288
      ENDIF
C                  
      CALL CRBLEND
C
      GO TO 900
C
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C--- FWD - Run forward propeller in current slipstream
C
 300  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'INPUT default parameters first'
         GO TO 900
      ELSEIF(.NOT.LROTF) THEN
         WRITE(*,*) 'Load rotor first'
         GO TO 900
      END IF  
C
C--- load rotor and slipstream
C
      CALL LOADCR(1)
C
      RPMINF = RPMF
      IF(NINPUT.GE.1) RPMINF = RINPUT(1)
C
      RPM = RPMINF
      ADV = VEL / (RAD*RPM*PI/30.)
      CONV = .FALSE.
      CALL APER(4,2,LOPRINI)
C
      WRITE(*,3400) CRNAME
      CALL OUTPUT(LUWRIT)
      WRITE(*,*)
C
      IF(CONV) THEN
         IF(LCON) THEN
            IF(RPMINF.EQ.RCONF) THEN
               WRITE(*,*) 'FWD rotor at converged CR state'
            ELSE
               WRITE(*,*) 'FWD rotor off converged CR state'
            ENDIF
         ELSE
            WRITE(*,*) 'FWD rotor - CR system not converged'
         ENDIF
      ELSE
         WRITE(*,*) 'FWD rotor not converged'
      ENDIF
C
      IF(RPMINF.NE.RPMF) WRITE(*,2310) RPMINF
C
C      CALL DISPIN(LUWRIT)
C
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC2*SIZE,LPLOT,LLAND)
      CALL PLOT(XORG,YORG,-3)
      CALL CLPLT
C
C--- store output
C
      CALL CRDATA(1)
      IF(CONV) CALL PUTVEL
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- AFT - Run aft  propeller in current slipstream
C
 320  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'INPUT default parameters first'
         GO TO 900
      ELSEIF(.NOT.LROTA) THEN
         WRITE(*,*) 'Load rotor first'
         GO TO 900
      END IF  
C
      RPMINA = RPMA
      IF(NINPUT.GE.1) RPMINA = RINPUT(1)
C
      CALL LOADCR(2)
C
      RPM = RPMINA
      ADV = VEL / (RAD*RPM*PI/30.)
      CONV = .FALSE.
      CALL APER(4,2,LOPRINI)
C
      WRITE(*,3420) CRNAME
      CALL OUTPUT(LUWRIT)
      WRITE(*,*)
C
      IF(CONV) THEN
         IF(LCON) THEN
            IF(RPMINA.EQ.RCONA) THEN
               WRITE(*,*) 'AFT rotor at converged CR state'
            ELSE
               WRITE(*,*) 'AFT rotor off converged CR state'
            ENDIF
         ELSE
            WRITE(*,*) 'AFT rotor - CR system not converged'
         ENDIF
      ELSE
         WRITE(*,*) 'AFT rotor not converged'
      ENDIF
C
      IF(RPMINA.NE.RPMA) WRITE(*,2320) RPMINA
C
C      CALL DISPIN(LUWRIT)
C
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFAC2*SIZE,LPLOT,LLAND)
      CALL PLOT(XORG,YORG,-3)
      CALL CLPLT
C
      CALL CRDATA(2)
      IF(CONV) CALL PUTVEL
C
      GO TO 900
C
C---------------------------------------------------------------------
C--- DFWD or DAFT - design MIL prop
C
 330  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'INPUt parameters first'
         GO TO 900
      END IF  
C
C      IF(.NOT.LCON) THEN
C         WRITE(*,*) 'WARNING: slipstream is not converged'
C      ENDIF
C
      CALL DESCR
C
      GO TO 900
C
C---------------------------------------------------------------------
C---------------------------------------------------------------------
C--- Initialize CR Iteration loop
C
 400  IF(.NOT.LDEF) THEN
         WRITE(*,*) 'Use INPUT command first'
         GOTO 900
      ENDIF
c
      IF(.NOT.LROTF .OR. .NOT.LROTA) THEN
         WRITE(*,*) 'Load rotors first'
         GO TO 900
      END IF  
C
      LCONF  = .FALSE.
      LCONA  = .FALSE.
      LCON   = .FALSE.
      LNCON  = .FALSE.
      LSWIT  = .FALSE.
      TTOTF  = 0.0
      TTOTA  = 0.0
      ICRITR = 0
C
C---- process input
C
      POWINF = POWERF
      POWINA = POWERA
      POWINT = POWERF+POWERA
      POWIN  = 0.0
      RPMINF = RPMF
      RPMINA = RPMA
      RPMIN  = 0.0
C
      IF(NINPUT.GE.1) THEN
        IF(COMAND.EQ.'RPM ') THEN
          RPMIN  = RINPUT(1)
          RPMINF = RPMIN
          IF(LSRPM) THEN
            RPMINA = RPMINF*RRAT
          ELSE
            RPMINA = RPMINF
          ENDIF
        ELSE
          POWIN  = RINPUT(1)
          POWINF = POWIN/2.0
          POWINA = POWIN/2.0
        ENDIF
      ENDIF
C
C
C------------------------------------------------------------------------
C---- Iteration loop
C
      LFORWARD=.FALSE.
 410  ICRITR = ICRITR + 1
      LFORWARD = .NOT.LFORWARD
      LAUTO  = .FALSE.
C
      IF(LFORWARD) THEN
         CALL LOADCR(1)
      ELSE 
         CALL LOADCR(2)
      ENDIF
C
      IF(COMAND.EQ.'RPM ') GO TO 440
C
C-------------------------------------------------------------------------
C--- POWE -  specified power
C
 430  IF(LFWD) THEN
         PSPEC = POWINF
         RPM   = RPMINF
      ELSE
         PSPEC = POWINA
         RPM   = RPMINA
      ENDIF
C
      CONV = .FALSE.
      IF(.NOT.LPITCH) THEN
         ADV = VEL/(RAD*RPM*PI/30.0)
         CALL APER(3,1,LOPRINI)
      ELSE
         IF(.NOT.LSRPM) THEN
           CALL APER(3,2,LOPRINI)
C
           IF(LFWD) THEN
             RPMFWD = VEL/(RAD*ADV*PI/30.)
           ELSE
             RPMAFT = VEL/(RAD*ADV*PI/30.)
           ENDIF
C
         ELSE
           IF((LCONA.OR.LCONF) .AND. .NOT.LSWIT) THEN
             RPMTF = (RPMFWD+RPMAFT)/(1.+RRAT)
             RPMTA =  RPMTF*RRAT
             LSWIT = .TRUE.
             LCONF = .FALSE.
             LCONA = .FALSE.
             LCON  = .FALSE.
           ENDIF
C
           IF(LSWIT) THEN
             IF(LFWD)THEN
               RPM = RPMTF
             ELSE
               RPM = RPMTA
             ENDIF
             ADV = VEL / (RAD*RPM*PI/30.)
             CONV = .FALSE.
             CALL APER(4,2,LOPRINI)
C
             IF(LFWD) THEN
               POWFWD = PTOT*RHO*VEL**3*RAD**2
             ELSE
               POWAFT = PTOT*RHO*VEL**3*RAD**2
             ENDIF
           ELSE
             CALL APER(3,2,LOPRINI)
C
             IF(LFWD) THEN
               RPMFWD = VEL/(RAD*ADV*PI/30.)
             ELSE
               RPMAFT = VEL/(RAD*ADV*PI/30.)
             ENDIF
C
           ENDIF
        ENDIF
      ENDIF
C
C----- converged: check for invalid pitch change and neg. thrust
C  
      IF(CONV) THEN
        IF(.NOT.LPITCH) THEN
          IF(ABS(DBETA).GT.PI/15.0 .AND. TTOT.LT.0.0) THEN
            WRITE(*,*)
            WRITE(*,*) 'Neg. thrust and excessive blade angle change'
            GO TO 590
          ENDIF
        ENDIF
        GO TO 445
      ENDIF
C
C----- auto loop or not converged
C
      IF(LAUTO) GO TO 620      ! not converged, auto loop
      GO TO 590                ! not converged, first time
C
C
C---------------------------------------------------------------------
C--- RPM - constant pitch and rpm
C
 440  IF(LFWD)THEN
         RPM = RPMINF
      ELSE
         RPM = RPMINA
      ENDIF
C
      ADV = VEL / (RAD*RPM*PI/30.)
      CONV = .FALSE.
      CALL APER(4,2,LOPRINI)
C
      IF(LFWD) THEN
         POWFWD = PTOT*RHO*VEL**3*RAD**2
      ELSE
         POWAFT = PTOT*RHO*VEL**3*RAD**2
      ENDIF
C
      IF(.NOT.CONV) GO TO 590
C
C---------------------------------------------------------------------
C--- Converged CR iteration exists
C--- Process slipstream velocities
C
 445  CALL PUTVEL
C
C--- Check slipstream convergence--------------------------------------
C
 460  IF(LFWD) THEN
         TDIFF = ABS((TTOT-TTOTF)/TTOT)
         TTOTF = TTOT
      ELSE
         TDIFF = ABS((TTOT-TTOTA)/TTOT)
         TTOTA = TTOT
      END IF
C 
      IF(TDIFF .LE. CRCON) THEN
         IF (LFWD) THEN
            LCONF = .TRUE.
         ELSE
            LCONA = .TRUE.
         END IF
      END IF
C
      IF(LCONF .AND. LCONA) THEN
        IF(LPITCH.AND.LSRPM.AND..NOT.LSWIT.AND.(COMAND.EQ.'POWE'))THEN
          GO TO 410
        ELSE
          GO TO 500
        ENDIF
      ENDIF
C
      IF(ICRITR .GE. ICRITL) THEN
         WRITE(*,*) 'CR iteration limit exceeded.'
         GO TO 510
      END IF
C
      GO TO 410
C
C--- System is converged----------------------------------------
C
 500  IF (.NOT.LFWD .AND. .NOT.LCON)  GO TO 410
      LCON  = .TRUE.
C
      GO TO 520
C
C--- System is not converged------------------------------------
C      
 510  IF (.NOT.LFWD .AND. .NOT.LNCON) GO TO 410
      LNCON = .TRUE.
C
C--- Output-----------------------------------------------------
C
 520  IF(LFWD) THEN
         IF(LCON) THEN
            WRITE(*,2600) CRNAME
         ELSE
            WRITE(*,2800) CRNAME
         ENDIF
      ELSE
         IF(LCON) THEN
            WRITE(*,3000) CRNAME
         ELSE
            WRITE(*,3200) CRNAME
         ENDIF
      ENDIF
C
      CALL OUTPUT(LUWRIT)
C
      IF(LFWD) THEN
        WRITE(*,3500)
        CALL CRDATA(1)
        GO TO 410
      ENDIF
C
      CALL CRDATA(2)
      TCRTOT = TCR(1) + TCR(2)
      PCRTOT = PCR(1) + PCR(2)
      ECRTOT = TCRTOT*VEL/PCRTOT
      TCRLB  = TCRTOT/4.4482216
      PCRHP  = PCRTOT/745.69987
      VKNOT  = VEL*1.943844
C
      WRITE(*,2500)TCRTOT/1000.,PCRTOT/1000.,ECRTOT,TCRLB,PCRHP,VKNOT
C
C      CALL DISPIN(LUWRIT)
C
C---- update default input
C
      POWERF = POWINF
      POWERA = POWINA
      RPMF   = RPMINF
      RPMA   = RPMINA
C
C---  store data for converged slipstream 
C
      PCONF = POWINF
      PCONA = POWINA
      RCONF = RPMINF
      RCONA = RPMINA    
C
      IF(LSWIT .OR. (COMAND.EQ.'RPM ')) THEN
        PCONF = POWFWD
        PCONA = POWAFT
      ENDIF
C
      IF(LPITCH .AND..NOT.LSRPM.AND.(COMAND.EQ.'POWE'))THEN
        RCONF = RPMFWD
        RCONA = RPMAFT
      ENDIF
C
C--- reporting
C
      IF(COMAND.EQ.'RPM ') THEN
        WRITE(*,*) 'Specified rpm, fixed pitch'
        IF(RPMIN.NE.0.0) THEN
           IF(.NOT.LSRPM .OR. RRAT.EQ.1.0) THEN
              WRITE(*,2300) RPMINF
           ELSE
              WRITE(*,2310) RPMINF
           ENDIF
        ENDIF
      ELSE 
        IF(LPITCH) THEN
          IF(LSRPM) THEN
           WRITE(*,*)'(Near) specified power, fixed pitch, synched rpm'
          ELSE
           WRITE(*,*) 'Specified power, fixed pitch'
          ENDIF
        ELSE
          WRITE(*,*) 'Specified power and rpm, variable pitch'
        ENDIF
C
        IF(POWIN.NE.0.0) WRITE(*,2400) POWIN
        IF(POWIN.EQ.0.0 .AND. LSWIT) WRITE(*,2400) POWINT
C
      ENDIF
C
      IF(LCON) THEN
        WRITE(*,2100) ICRITR
      ELSE
        WRITE(*,2200) ICRITR
      ENDIF
C
      IF(LFWD) THEN
         WRITE(*,*) 'FWD rotor loaded'
      ELSE
         WRITE(*,*) 'AFT rotor loaded'
      ENDIF
C
      CALL CRPLIN
C
      GO TO 900
C
C
C-------------------------------------------------------------------
C-------------------------------------------------------------------
C----- convergence failed: return blade angles and display output
C
 590  IF(COMAND .EQ. 'POWE') THEN
        DO I=1, II
          BETA(I)  = BETA(I)  - DBETA
          BETA0(I) = BETA0(I) - DBETA
        ENDDO
      ENDIF
C
      CALL OUTPUT(LUWRIT)
C
      IF(COMAND .EQ. 'POWE') THEN
        WRITE(*,*) 'Initial blade angles restored'
      ENDIF
C
      WRITE(*,*)
      IF(LFWD) THEN
        WRITE(*,4800) ICRITR
      ELSE
        WRITE(*,4900) ICRITR
      ENDIF
C
C---- Convergence prompt-----------------------------------------------
C
 600  CALL ASKC('..CONV^',CONAND,CONARG)
C
      DO I=1, 20
        IINPUT(I) = 0
        RINPUT(I) = 0.0
      ENDDO
      NINPUT = 0
      CALL GETINT(CONARG,IINPUT,NINPUT,ERROR)
      NINPUT = 0
      CALL GETFLT(CONARG,RINPUT,NINPUT,ERROR)
C
C---- convergence commands
C
      IF(CONAND.EQ.'    ') THEN
         IF(COMAND.EQ.'POWE') GO TO 430
         IF(COMAND.EQ.'RPM ') GO TO 440
      ENDIF
C
      IF(CONAND.EQ.'?   ') THEN
        WRITE(*,5000)
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'STOP') THEN
         WRITE(*,*) ' CR iteration aborted'
         LCON = .FALSE.
         GO TO 900
      END IF
C
      IF(CONAND.EQ.'AUTO' .OR. CONAND.EQ.'A   ') THEN
         IF(LPITCH .OR. COMAND.EQ.'RPM ') THEN
            WRITE(*,*) 'AUTO command not available for fixed pitch'
            GO TO 600
         ENDIF
         LAUTO = .TRUE.
         IAUTO = 0
         GO TO 620
      ENDIF
C
      IF(CONAND.EQ.'ASET') THEN
        CALL ASKI('Max number of convergence attempts^',NAUTO)
        CALL ASKR('Max deviation from velocity vector (deg)^',CRANGE)
 610    WRITE(*,*)
        WRITE(*,*) 'root 0 <---------> 1 tip'
        CALL ASKR( 'Sample blade location^',CLOC)
        IF(CLOC.LT.0.0 .OR. CLOC .GT. 1.0) GO TO 610
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'VRTX') THEN
        VRTX = .NOT.VRTX
        IF(.NOT.VRTX) WRITE(*,*)'Discrete Vortex Formulation deselected'
        IF(VRTX)      WRITE(*,*)'Discrete Vortex Formulation selected'
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'FORM') THEN
        FAST = .NOT.FAST
        IF(FAST)      WRITE(*,*)'Graded Momentum Formulation selected'
        IF(.NOT.FAST) WRITE(*,*)'Potential Formulation selected'
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'WAKE') THEN
        FREE = .NOT.FREE
        IF(FREE)      WRITE(*,*)'Self-deforming wake selected'
        IF(.NOT.FREE) WRITE(*,*)'Rigid wake selected'
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'INIT') THEN
        LOPRINI = .NOT.LOPRINI
        IF(LOPRINI) THEN
          WRITE(*,*) 'Analysis case will be initialized'
        ELSE
          WRITE(*,*) 'Analysis case will not be initialized'
        ENDIF
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'REIN') THEN
        CALL REINIT
        GO TO 600
      ENDIF
C
      IF(CONAND.EQ.'ANGL') THEN
        IF(NINPUT.GE.1) THEN
          DELB = RINPUT(1)
        ELSE      
          CALL ASKR('Enter blade angle change (deg)^',DELB)
        ENDIF
C
        DO I=1, II
           BETA(I)  = BETA(I)  + DELB*PI/180.
           BETA0(I) = BETA0(I) + DELB*PI/180.
        ENDDO
C
        GO TO 600
      ENDIF
C
      WRITE(*,1050) CONAND  !  command not recognized
      GO TO 600
C
C
C---- Auto-converge loop-------------------------------------------
C
  620 IAUTO = IAUTO + 1
C
      IF(IAUTO .GT. NAUTO) THEN
         LAUTO = .FALSE.
         WRITE(*,*) 'AUTO command failed'
         GO TO 600
      ENDIF
C
      CALL VVECT(IAUTO)
      GO TO 430
C
C
C---------------------------------------------------------------------
c---------------------------------------------------------------------
c
c
 1100 FORMAT(  
     &  /'   INPU    Input default operating parameters'
     &  /'   LFWD    Load and store forward rotor'
     &  /'   LAFT    Load and store aft rotor'
     &  /'   DFWD    Design forward rotor'
     &  /'   DAFT    Design aft rotor'
     &  /'   SFWD f  Save forward rotor to disk'
     &  /'   SAFT f  Save aft rotor to disk'
C
     & //'   PFWD r  Change power forward rotor'
     &  /'   PAFT r  Change power aft rotor'
     &  /'   RFWD r  Change rpm forward rotor'
     &  /'   RAFT r  Change rpm aft rotor'
     &  /'   VELO r  Change flight speed'
     &  /'   VELW    Change slipstream velocity weights' 
     &  /'   VCLR    Initialize slipstream velocity profiles' 
C    
     & //'   POWE r  Run CR analysis (specified power)'
     &  /'   RPM  r  Run CR analysis (specified rpm, fixed pitch)'
     &  /'   FWD  r  Run forward rotor in current slipstream'
     &  /'   AFT  r  Run aft rotor in current slipstream'

     & //'   FIX     Fixed pitch/rpm toggle'
     &  /'   SYNC    Synchronize rpms toggle'   
     &  /'   ANGL r  Change blade angles current rotor'    
     &  /'   CLCR r  Change lift coefficients for MIL design'  
     &  /'   ATMO r  Set fluid properties from standard atmosphere'
     &  /'   BLEN    Interpolate between stored rotor geometries' 
     &  /'   NAME s  Change CR case name'
     &  /'   NFWD s  Change fwd rotor name'
     &  /'   NAFT s  Change aft rotor name'
c
     & //'   CRIT i  Change CR iteration limit'
     &  /'   CCON r  Change CR convergence delta (del_thrust/thrust)'
     &  /'   FORM    Toggle between Graded Mom. and Potential Form'
     &  /'   VRTX    Toggle between Graded Mom. and Vortex Form'
     &  /'   WAKE    Toggle between rigid and self-deforming wake'
     &  /'   INIT    Initialize next analysis case'
     &  /'   REIN    Re-initialize rotor to known operating state'
c
     & //'   DISI    Display current CR Default Input and status'
     &  /'   DISS    Display current CR external slipstreams'
     &  /'   DISV    Display current CR average induced velocities'
     &  /'   DISP    Display current rotor operating state'
     &  /'   WRIT f  Write current rotor operating state to disk'
     &  /'   WRIV f  Write current induced velocities to disk'
     &  /'   TERS    Toggle between terse and verbose output'
c
     & //'   PLOT i  Plot various rotor parameters'
     &  /'   ANNO    Annotate plot'
     &  /'   HARD    Hardcopy current plot'
     &  /'   SIZE r  Change plot-object size')
c
 1000 FORMAT(A)
 1050 FORMAT(1X,A4,' command not recognized.' //
     &             '  Type "?" for list, <Enter> to exit menu.')
C
 2000 FORMAT(/'  0   CANCEL'
     &       /'  1   Geometry'
     &       /'  2   Axial Geometry (all blades)'
     &       /'  3   Radial distributions for current case'
     &       /'  4   Radial distributions plus geometry'
     &       /'  5   Radial distributions for all cases'
     &       /'  6   Case sequence parameters'
     &       /'  7   Induced velocities on blade vs r/R'
     &       /'  8   Induced velocities in slipstream vs r/R'
     &       /'  9   Velocity triangles'
     &       /' 10   External slipstream velocity profiles'
     &       /' 11   Reference x,y data'
     &       /' 12   Plot blade data (Gam,CL,CD,etc) vs r/R'
     &       /' 13   Plot CR System average induced velocities')
C
C
 2100 FORMAT(1X, 'Converged after',I3,' iterations')
C
 2200 FORMAT(1X, 'Not converged after',I3,' iterations')
C
 2300 FORMAT(1X, 'Specified rpm - both rotors: ',F6.0)
C
 2310 FORMAT(1X, 'Specified rpm - forward rotor: ',F6.0)
C
 2320 FORMAT(1X, 'Specified rpm - aft rotor: ',F6.0)
C
 2400 FORMAT(1X, 'Specified total power (W): ',F7.0)
C
 2500 FORMAT(/1X,75('-')
     & /' System Thrust kN: ',F8.4, 6X,'Power input kW: ',F8.3,
     & 4X,'  Effy: ',F6.4,
     & /'              lbf: ',F8.2, 6X,'            hp: ',F8.2,
     & 4X,' V(kt): ',F6.2,
     & /1X,75('-'))
C
C
 2600 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor at converged CR state: ',A32)
C
 2700 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor off converged CR state: ',A32)
CC
 2800 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor - CR system not converged: ',A32)
C
 2900 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor not converged: ',A32)
C
C
 3000 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor at converged CR state: ',A32)
C
 3100 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor off converged CR state: ',A32)
C
 3200 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor - CR system not converged: ',A32)

 3300 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor not converged: ',A32)
C
 3400 FORMAT(/1X,75('-')
     &       //1X,' FWD Rotor: ',A32)
C
 3420 FORMAT(/1X,75('-')
     &       //1X,' AFT Rotor: ',A32)
C
 3500 FORMAT(/1X,75('-'))
C
C
 3600 FORMAT(/'        Slipstream Velocity Profiles (m/s)'
     &       /' -------------------------------------------------'
     &       /'                 At FWD Rotor'
     &       /'      r (m)          Vaxi            Vtan')
C
 3700 FORMAT(/' -------------------------------------------------'
     &       /'                 At AFT Rotor'
     &       /'      r (m)          Vaxi            Vtan')
C
 3800 FORMAT( F12.4,4X,F12.4,4X,F12.4)
C
 3900 FORMAT(/'                Slipstream Velocity Profiles (m/s)'
     &       /1X,63('-'),
     &       /'                     At FWD Rotor',
     &        '               At AFT Rotor'
     &       /'   r(m)            Vaxi        Vtan',
     &        '           Vaxi        Vtan')

 4000 FORMAT(1X, F8.4,7X,F8.4,4x,F8.4,7X,F8.4,4X,F8.4)
C
 4100 FORMAT(1X, 63('-'))
C
 4200 FORMAT(1X, 48('-'))
C
 4300 FORMAT(1X, 'FWD Rotor loaded: ',A32)
C
 4400 FORMAT(1X, 'AFT Rotor loaded: ',A32)
C
 4500 FORMAT(/1X,'Current design CLs:',F5.2,' (root) -',
     &           F5.2,' (tip)')
C
 4600 FORMAT(1X, 'Name: ',A32)
C
 4800 FORMAT(/ ' FWD Rotor, CR Iteration # ', I2
     &       / ' Subroutine APER convergence failed')    
C   
 4900 FORMAT(/ ' AFT Rotor, CR Iteration # ', I2
     &       / ' Subroutine APER convergence failed')    
C
 5000 FORMAT(
     &  /'   <ret>   Continue'
     &  /'   AUTO<a> Auto-initialize blade angles and continue'
     &  /'   ANGL r  Change blade angles manually'   
     &  /'   FORM    Toggle between Graded Mom.and Potential Form'
     &  /'   VRTX    Toggle between Graded Mom.and Vortex Form'
     &  /'   WAKE    Toggle between rigid and self-deforming wake'
     &  /'   INIT    Initialize next analysis case'
     &  /'   REIN    Re-initialize rotor to known operating state'
     &  /'   ASET    Change AUTO settings'
     &  /'   STOP    Abort and return to CROTOR prompt')
C
      END ! CROTOR
C
C------------------------------------------------------------------------



      SUBROUTINE STORCR(ICR)
      INCLUDE 'XROTOR.INC'
C--------------------------------------------------
C     Stores blade geometry for future CR iteration
C--------------------------------------------------
C
      J = ICR
C
      RADCR(J)  = RAD
      RAKECR(J) = RAKE
      XI0CR(J)  = XI0
      XW0CR(J)  = XW0
      IICR(J)   = II
      NBLDCR(J) = NBLDS
      NAMECR(J) = NAME
C
      DO I=1, II
        XICR(J,I)    = XI(I)
        CHCR(J,I)    = CH(I)
        BETACR(J,I)  = BETA(I)
        BETA0CR(J,I) = BETA0(I)
        UBODYCR(J,I) = UBODY(I)
      END DO
C
      RETURN
      END  ! STORCR
C
C-------------------------------------------------------------------------



      SUBROUTINE LOADCR(ICR)
      INCLUDE 'XROTOR.INC'
C---------------------------------------------------
C     Loads geometry and slipstream for CR iteration
C---------------------------------------------------
C
      J = ICR
C
      IF(J .EQ. 1) THEN
         K = 2
      ELSE
         K = 1
      ENDIF
C
C--- Store blade angles of outgoing rotor
C
      IF(LFWD) THEN
	IF(J.NE.1) THEN
          DO I=1,II
            BETACR(K,I) = BETA(I)
            BETA0CR(K,I)= BETA0(I)
          ENDDO
        ELSE
          DO I=1,II
            BETACR(J,I) = BETA(I)
            BETA0CR(J,I)= BETA0(I)
          ENDDO
        ENDIF
      ELSE
	IF(J.NE.2) THEN
          DO I=1,II
            BETACR(K,I) = BETA(I)
            BETA0CR(K,I)= BETA0(I)
          ENDDO  
        ELSE
          DO I=1,II
            BETACR(J,I) = BETA(I)
            BETA0CR(J,I)= BETA0(I)
          ENDDO
        ENDIF
      ENDIF
C
C--- Read in new geometry
C      
      RAD   = RADCR(J)
      RAKE  = RAKECR(J)
      XI0   = XI0CR(J)
      XW0   = XW0CR(J)
      II    = IICR(J)
      NBLDS = NBLDCR(J)
      NAME  = NAMECR(J)
C
      DO I=1, II
        XI(I)    = XICR(J,I)
        CH(I)    = CHCR(J,I)
        BETA(I)  = BETACR(J,I)
        BETA0(I) = BETA0CR(J,I)
        UBODY(I) = UBODYCR(J,I)
      END DO
C
C--- load imposed velocity profiles
C
 50   IF(J .EQ. 1) THEN
         LFWD = .TRUE.
         NADD = NADDF
         DO I=1,NADD
           RADD(I)  = RADDF(I)
           UADD(I)  = UADDF(I)
           VADD(I)  = VADDF(I)
           UADDR(I) = UADDRF(I)
           VADDR(I) = VADDRF(I)
         ENDDO
      ELSE
         LFWD = .FALSE.
         NADD = NADDA
         DO I=1,NADD
           RADD(I)  = RADDA(I)
           UADD(I)  = UADDA(I)
           VADD(I)  = VADDA(I)
           UADDR(I) = UADDRA(I)
           VADDR(I) = VADDRA(I)
         ENDDO
      ENDIF
C
C      CALL XWINIT
C
      IINF = II + II/2
C
      CALL SETIAERO

      RETURN    
      END   ! LOADCR
C
C-------------------------------------------------------------



      SUBROUTINE PUTVEL
      INCLUDE 'XROTOR.INC'
C--------------------------------------------------------------
C     Automates SAVVEL and GETVEL
C     Gathers, applies weights and stores slipstream velocities
C--------------------------------------------------------------
C
      NADD=II
      NADDA=NADD
      NADDF=NADD
C
      BLDS = FLOAT(NBLDS)
      DO 10 I=1, II
C------ use circumferentially averaged induced velocity 
        VT = BLDS*GAM(I)/(4.0*PI*XI(I))
        VA = VT*XI(I)/ADW
C
C------ include duct effect on freestream and induced axial velocity
        UDUCT     = 0.0
        VADUCT_VA = 1.0
        IF(DUCT) THEN
          UDUCT = URDUCT-1.0
          VADUCT_VA = 2.0*URDUCT
        ENDIF
C
        VA = VA * VADUCT_VA
        UTOT = 1.0 + UDUCT + UBODY(I)
        CALL UVADD(XI(I),WA,WT)
C
        CI = XI(I)/ADV - WT  -  VT
        SI = UTOT      + WA  +  VA
C
        RDIM = XI(I)*RAD
        UDIM =  2.0*(SI - (UTOT      + WA))*VEL
        VDIM = -2.0*(CI - (XI(I)/ADV - WT))*VEL
C
C---- store the weighted data
C
        IF(LFWD) THEN 
          RADDA(I)=RDIM
          UADDA(I)=UDIM * UWTA
          VADDA(I)=VDIM * VWTA
        ELSE
          RADDF(I)=RDIM
          UADDF(I)=UDIM * UWTF
          VADDF(I)=VDIM * VWTF
       ENDIF
C
 10   CONTINUE
C
C--- make the data available
C
      IF(LFWD) THEN 
        CALL SPLINE(UADDA,UADDRA,RADDA,NADDA)
        CALL SPLINE(VADDA,VADDRA,RADDA,NADDA)
      ELSE
        CALL SPLINE(UADDF,UADDRF,RADDF,NADDF)
        CALL SPLINE(VADDF,VADDRF,RADDF,NADDF)
      ENDIF
C
      LVCLR = .FALSE.
      RETURN
      END  ! PUTVEL
C
C-------------------------------------------------------------



      SUBROUTINE PUTVELD
      INCLUDE 'XROTOR.INC'
C--------------------------------------------------------------
C     Automates SAVVEL and GETVEL
C     Gathers, applies weights and stores slipstream velocities
C     Modified from PUTVEL for design purposes...
C--------------------------------------------------------------
C
      NADD=II
      NADDA=NADD
      NADDF=NADD
C
      BLDS = FLOAT(NBLDS)
      DO 10 I=1, II
C------ use circumferentially averaged induced velocity 
        VT = BLDS*GAM(I)/(4.0*PI*XI(I))
        VA = VT*XI(I)/ADW
C
C------ include duct effect on freestream and induced axial velocity
        UDUCT     = 0.0
        VADUCT_VA = 1.0
        IF(DUCT) THEN
          UDUCT = URDUCT-1.0
          VADUCT_VA = 2.0*URDUCT
        ENDIF
C
        VA = VA * VADUCT_VA
        UTOT = 1.0 + UDUCT + UBODY(I)
        CALL UVADD(XI(I),WA,WT)
C
        CI = XI(I)/ADV - WT  -  VT
        SI = UTOT      + WA  +  VA
C
        RDIM = XI(I)*RAD
        UDIM =  2.0*(SI - (UTOT      + WA))*VEL
        VDIM = -2.0*(CI - (XI(I)/ADV - WT))*VEL
C
C---- load the weighted data from previous solution
C
        IF(LFWD) THEN
          RADD(I) =RDIM
          UADD(I) =UDIM * UWTF
          VADD(I) =VDIM * VWTF
        ELSE
          RADD(I) =RDIM
          UADD(I) =UDIM * UWTA
          VADD(I) =VDIM * VWTA
        ENDIF
C
 10   CONTINUE
C
C--- store the data
C
      CALL SPLINE(UADD,UADDR,RADD,NADD)
      CALL SPLINE(VADD,VADDR,RADD,NADD)
C
      IF(LFWD) THEN
         NADDF = NADD
         DO I=1,NADDF
            RADDF(I) = RADD(I)
            UADDF(I) = UADD(I)
            VADDF(I) = VADD(I)
            UADDRF(I)= UADDR(I)
            VADDRF(I)= VADDR(I)
         ENDDO
      ELSE
         NADDA = NADD
         DO I=1,NADDA
            RADDA(I) = RADD(I)
            UADDA(I) = UADD(I)
            VADDA(I) = VADD(I)
            UADDRA(I)= UADDR(I)
            VADDRA(I)= VADDR(I)
         ENDDO
      ENDIF
C
      LVCLR = .FALSE.
      RETURN
      END  ! END PUTVELD
C
C-----------------------------------------------------------------------------      



      SUBROUTINE LOADVEL
      INCLUDE 'XROTOR.INC'
C
C--- Loads current weighted slipstream data into current rotor
C
      IF(LFWD) THEN
         NADD = NADDF
         DO I=1,NADD
            RADD(I) = RADDF(I)
            UADD(I) = UADDF(I)
            VADD(I) = VADDF(I)
            UADDR(I)= UADDRF(I)
            VADDR(I)= VADDRF(I)
         ENDDO
      ELSE
         NADD = NADDA
         DO I=1,NADD
            RADD(I) = RADDA(I)
            UADD(I) = UADDA(I)
            VADD(I) = VADDA(I)
            UADDR(I)= UADDRA(I)
            VADDR(I)= VADDRA(I)
         ENDDO
      ENDIF
C
      RETURN
      END  ! LOADVEL
C
C------------------------------------------------------------------------------------
C

      SUBROUTINE DISPIN(LU)
      INCLUDE 'XROTOR.INC'
C
      WRITE(LU,1200) CRNAME,NAMECR(1),NAMECR(2),FNAMEF,FNAMEA,
     &               POWERF,POWERA
C
      IF(LSRPM) THEN
        WRITE(LU,1220) RRAT,RPMF,RPMA
      ELSE
        WRITE(LU,1210) RPMF,RPMA
      ENDIF
C
      WRITE(LU,1250) UWTF,UWTA,VWTF,VWTA
C
      IF(LCON) THEN
         WRITE(LU,1300) ALT, VEL
      ELSE
         WRITE(LU,1400) ALT, VEL
      ENDIF
C
      RETURN
C
 1200 FORMAT(
     & /' ------------------------------------------------------------'
     & /'                   CR SYSTEM DEFAULT INPUT'
     & /1X,A25,               'Fwd Rotor           Aft Rotor'
     & /' ------------------------------------------------------------'
     & /' Rotor name               ',A20,A20
     & /' Filename                 ',A20,A20
     & /' Power(W)                 ',F8.1,12X,F8.1)
C
 1210 FORMAT(
     &  ' Rpm (no ratio)             ',F6.1,14X,F6.1)
C
 1220 FORMAT(
     &  ' Rpm (ratio',F6.3,')          ',F6.1,14X,F6.1)
C
 1250 FORMAT(
     &  ' Axial Vel. Wt.             ',F6.3,14X,F6.3
     & /' Tang. Vel. Wt.             ',F6.3,14X,F6.3
     & /' ------------------------------------------------------------')
C
 1300 FORMAT(
     &   ' Slipstream converged         Alt(km):',F5.2,
     &   '    V(m/s): ',F6.2)
C
 1400 FORMAT(
     &   ' Slipstream not converged     Alt(km):',F5.2,
     &   '    V(m/s): ',F6.2)
C
      END  ! DISPIN


C---------------------------------------------------------------------

      SUBROUTINE OUTVEL(LU)
      INCLUDE 'XROTOR.INC'
C---------------------------------------------------
C     Output induced velocities
C---------------------------------------------------
      REAL Z1(II),Z2(II),Z3(II),Z4(II),Z5(II),Z6(II)
C
C---- extract velocity data
C
      DO I=1,II
        Z1(I) =  CVEL3(1,I)   !  FWD rotor axial
        Z2(I) =  CVEL4(1,I)   !  FWD rotor tangential
        Z3(I) =  CVEL3(2,I)   !  AFT rotor axial
        Z4(I) = -CVEL4(2,I)   !  AFT rotor tangential
        Z5(I) =  Z1(I)+Z3(I)  !  Total axial
        Z6(I) =  Z2(I)+Z4(I)  !  Total tangential
      ENDDO
C
      IF(RADCR(1).EQ.RADCR(2) .AND. XI0CR(1).EQ.XI0CR(2) .AND.
     &         IICR(1).EQ.IICR(2) .AND. LCON) THEN
C
        WRITE(LU,1500)
        DO I=1,II
           RVEL = RAD*XI(I)
           WRITE(LU,1600) RVEL, Z1(I), Z2(I), Z3(I), Z4(I),
     &                          Z5(I), Z6(I)
        ENDDO
        WRITE(*,*)
C        WRITE(LU,1800)
      ELSE
        IF(LROTF) THEN
          WRITE(LU,1000)
          J = IICR(1)
          DO I=1, J
            RVEL = RADCR(1)*XICR(1,I)
            WRITE(LU,1400) RVEL, Z1(I), Z2(I)
          ENDDO
        ENDIF
C
        IF(LROTA) THEN
          WRITE(LU,1200)
          J = IICR(2)
          DO I=1, J
            RVEL = RADCR(2)*XICR(2,I)
            WRITE(LU,1400) RVEL, Z3(I), Z4(I)
          ENDDO
        ENDIF
C        WRITE(*,*)
C        WRITE(LU,1700)
      ENDIF
C    
      RETURN
C
C
C
 1000 FORMAT(/'          Induced Velocity Profiles (m/s)'
     &       /' ------------------------------------------------'
     &       /'                 From FWD Rotor'
     &       /'      r (m)           Vaxi          Vtan')
C
 1200 FORMAT(/' ------------------------------------------------'
     &       /'                 From AFT Rotor'
     &       /'      r (m)           Vaxi          Vtan')

 1400 FORMAT(F12.4,4X,F12.4,4X,F12.4)
C
C
 1500 FORMAT(/ 19X, 'Average Induced Velocity Profiles (m/s)'
     &       /1X,74('-'),
     &       /'                From FWD Rotor        From AFT Rotor',
     &        '            System'
     &       /'   r(m)         Vaxi      Vtan        Vaxi',
     &        '      Vtan        Vaxi      Vtan')

 1600 FORMAT(1X,F8.4,4X,F8.4,2x,F8.4,4X,F8.4,2X,F8.4,4X,F8.4,2X,F8.4)
C
 1700 FORMAT(1X, 48('-'))
C
 1800 FORMAT(1X, 74('-'))
C
C
      END   !  OUTVEL
C


C---------------------------------------------------------------------

      SUBROUTINE LOADF(DNAME)
      INCLUDE 'XROTOR.INC'
      CHARACTER*32 DNAME
C---------------------------------------------------
C     Load forward rotor
C---------------------------------------------------
C
      CALL LOAD(DNAME)
      VEL = VELCR
C
      IF(LFILER) THEN
         WRITE(*,*) 'FWD rotor not loaded'
         RETURN
      ENDIF
C
      NADDF = NADD
      DO 100 I=1,NADD
         RADDF(I)  = RADD(I)
         UADDF(I)  = UADD(I)
         VADDF(I)  = VADD(I)
         UADDRF(I) = UADDR(I)
         VADDRF(I) = VADDR(I)
 100  CONTINUE
C
      CALL STORCR(1)
      LFWD = .TRUE.
      LCON = .FALSE.
      FNAMEF = DNAME
C
      RETURN
      END
C
C---------------------------------------------------------------------

      SUBROUTINE LOADA(DNAME)
      INCLUDE 'XROTOR.INC'
      CHARACTER*32 DNAME
C---------------------------------------------------
C     Load aft rotor
C---------------------------------------------------
C
      CALL LOAD(DNAME)
      VEL = VELCR
C
      IF(LFILER) THEN
         WRITE(*,*) 'AFT rotor not loaded'
         RETURN
      ENDIF
C
      NADDA = NADD
      DO 100 I=1,NADD
         RADDA(I)  = RADD(I)
         UADDA(I)  = UADD(I)
         VADDA(I)  = VADD(I)
         UADDRA(I) = UADDR(I)
         VADDRA(I) = VADDR(I)
 100  CONTINUE
C
      CALL STORCR(2)
      LFWD = .FALSE.
      LCON = .FALSE.
      FNAMEA = DNAME
C
      RETURN
      END
C
C-------------------------------------------------------------------------



      SUBROUTINE CRDATA(ICR)
      INCLUDE 'XROTOR.INC'
C----------------------------------------------------
C     Stores output data from converged system
C----------------------------------------------------
C
      J = ICR
C
C----Calculate and store induced velocities
      BLDS = FLOAT(NBLDS)
C
      DO I = 1, II
C---- induced velocity on blade 
        VT = VIND(3,I)
        VA = VIND(1,I)
C------ include duct effect on freestream and induced axial velocity
        UDUCT     = 0.0
        VADUCT_VA = 1.0
        IF(DUCT) THEN
          UDUCT = URDUCT-1.0
          VADUCT_VA = 2.0*URDUCT
        ENDIF
        VA = VA * VADUCT_VA
        W1(I) = VA
        W2(I) = VT
C---- velocity from circulation
        VT = 0.5*BLDS*GAM(I) / (2.0*PI*XI(I))
        VA = 0.5*BLDS*GAM(I) / (2.0*PI*ADW  )
C------ include duct effect on axial induced velocity
        VA = VA * VADUCT_VA
        W3(I) = VA
        W4(I) = VT
C---- dimensional induced velocities
        W1(I) = W1(I)*VEL
        W2(I) = W2(I)*VEL
        W3(I) = W3(I)*VEL
        W4(I) = W4(I)*VEL
C
        CVEL1(J,I) = W1(I)
        CVEL2(J,I) = W2(I)
        CVEL3(J,I) = W3(I)
        CVEL4(J,I) = W4(I)
C
      ENDDO
c
C--- details for reporting...
C
      ADVCR(J) = PI*ADV
      RPM = VEL/(RAD*ADV*PI/30.)
      RPMCR(J) = RPM
      TCR(J)   = TTOT*RHO*VEL**2*RAD**2
      ECR(J)   = TTOT/PTOT
      PCR(J)   = PTOT*RHO*VEL**3*RAD**2
C
      RETURN
      END  ! CRDATA
C
C----------------------------------------------------------------


      SUBROUTINE CRPLIN
      INCLUDE 'XROTOR.INC'
C--------------------------------------
C     Plots CR induced velocities vs r/R
C     Based on UVIPLT
C--------------------------------------
      REAL Z1(II),Z2(II),Z3(II),Z4(II),Z5(II),Z6(II)
      CHARACTER*20 EVSN
C
      EXTERNAL PLCHAR,PLMATH
      DATA LMASK1, LMASK2, LMASK3 / -32640, -30584, -21846 /
C
C---- limitations
C
      IF(RADCR(1).NE.RADCR(2) .OR. XI0CR(1).NE.XI0CR(2) .OR.
     &         IICR(1).NE.IICR(2)) THEN
         WRITE(*,*) 
     &  'Plotting requires identical radii and stations'
         RETURN
      ENDIF
C
C---- plot aspect ratio
      PLPAR = 1.15
C
C---- character size for axis numbers, labels
      CS  = CSIZE
      CSL = CSIZE*1.4
C
      CSCALE = 1.8*CS   !  axial/tang. text
      CSLAB  = 1.1*CS   !  Label text
C
C---- find radial stations closest to 1/2 and 3/4 radius for labels
C
      XIB = 0.75
      XIA = 0.50
      XIBX = 1.0
      XIAX = 1.0
C
      DO I = 1, II
        IF(ABS(XI(I)-XIA).LT.XIAX) THEN
          XIAX = ABS(XI(I)-XIA)
          IA = I
        ENDIF
        IF(ABS(XI(I)-XIB).LT.XIBX) THEN
          XIBX = ABS(XI(I)-XIB)
          IB = I
        ENDIF
      ENDDO
C
C---- extract velocity data
C
      DO I=1,II
        Z1(I) =  CVEL3(1,I)   !  FWD rotor axial
        Z2(I) =  CVEL4(1,I)   !  FWD rotor tangential
        Z3(I) =  CVEL3(2,I)   !  AFT rotor axial
        Z4(I) = -CVEL4(2,I)   !  AFT rotor tangential
        Z5(I) =  Z1(I)+Z3(I)  !  Total axial
        Z6(I) =  Z2(I)+Z4(I)  !  Total tangential
      ENDDO
C
      WMIN = 0.
      WMAX = 0.
        DO I = 3, II-II/5
          WMIN =MIN(WMIN,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I))
          WMAX =MAX(WMAX,Z1(I),Z2(I),Z3(I),Z4(I),Z5(I),Z6(I))
        ENDDO
C
      CALL SCALIT(1,WMIN,0.0,WMNFAC)
      CALL SCALIT(1,WMAX,0.0,WMXFAC)
C
      WFAC = MIN( WMNFAC , WMXFAC )
      WDEL = 0.5 / (5.0*WFAC)
C
      IF(WMIN .LT. 0.0) WMIN = -WDEL * AINT( -WMIN/WDEL + 0.99 )
      IF(WMAX .GT. 0.0) WMAX =  WDEL * AINT(  WMAX/WDEL + 0.99 )
C
      WFAC = PLPAR / (WMAX - WMIN)
C
C
      CALL PLTINI(SCRNFR,IPSLU,IDEV,7.0,LPLOT,.FALSE.)
      CALL PLOTABS(0.9,0.8,-3) !  left and bottom margins
C
      CALL GETCOLOR(ICOL0)
C
      CALL PLOT(0.0,-WFAC*WMIN,-3)
C
C---- case title
C
C      CALL NEWPEN(4)
C      XT = 0.0
C      YT = WFAC*WMAX + 2.0*CSL
C      CALL PLCHAR(XT,YT,CSL,CRNAME,0.0,-1)
C
      CALL NEWPEN(1)
      CALL PLOT(0.0,0.0,3)
      CALL PLOT(1.0,0.0,2)
C
C---- label axes
C
      CALL NEWPEN(2)
      CALL XAXIS(0.0,WFAC*WMIN, 1.0,0.2    , 0.0,0.2, CS,1)
      CALL PLCHAR(0.5-1.5*CSL,WFAC*WMIN-3.0*CSL,CSL,'r/R',0.0,3)
C
      CALL NEWCOLORNAME('cyan')
      IF(LGRID) THEN
       CALL NEWPEN(1)
       NXG = 5
       NYG = INT( (WMAX-WMIN)/WDEL + 0.0001 )
       CALL PLGRID(0.0,WFAC*WMIN, NXG,0.2, NYG,WFAC*WDEL, LMASK2 )
      ENDIF
C
      CALL NEWCOLORNAME('black')
      CALL NEWPEN(2)
      CALL YAXIS(0.0,WFAC*WMIN,WFAC*(WMAX-WMIN),WFAC*WDEL,
     &            WMIN,WDEL, CS,-2)
C
      XL = -5.0*CSL
      YL = WFAC*(WMAX-3.5*WDEL) + 0.1*CSL
      CALL PLCHAR(XL,YL,CSL,'Vind'  ,0.0,4)
      CALL PLCHAR(XL+1.5*CSL,YL-1.2*CSL,0.8*CSL,'m/s'  ,0.0,3)
C
C---- axial/tangential
C
      XL = XI(IA) + 1.5*CS
      YL  = WFAC*(MAX(Z1(IA),Z3(IA)) + Z5(IA))/2.0 - 0.5*CS
C
      IF(Z5(IA) .LT. 0.35*Z5(II)) THEN
         YL = WFAC*(Z5(IA)+0.35*(Z5(II)-Z5(IA)))
      ENDIF
C
      IF(WIND) YL = -YL
C
      CALL PLCHAR(XL,YL+0.6*CS,CSCALE,'v'  ,0.0,1)
      CALL PLSUBS(XL,YL+0.6*CS,CSCALE,'axi',0.0,3,PLCHAR)
C
      XL = XI(IB) + 1.5*CS
      YL = WFAC*Z4(IB) - 4.0*CS
C
      CALL PLCHAR(XL,YL+0.6*CS,CSCALE,'v'  ,0.0,1)
      CALL PLSUBS(XL,YL+0.6*CS,CSCALE,'tan',0.0,3,PLCHAR)
C     
C---- Forward Rotor
C
      CALL NEWPEN(4)
      CALL NEWCOLORNAME('blue')
C
      IF(WIND) THEN
        CALL XYLINE(II,XI,Z1,0.0,1.0,0.0,-WFAC,1)
        CALL XYLINE(II,XI,Z2,0.0,1.0,0.0,-WFAC,1)
      ELSE
        CALL XYLINE(II,XI,Z1,0.0,1.0,0.0, WFAC,1)
        CALL XYLINE(II,XI,Z2,0.0,1.0,0.0, WFAC,1)
      ENDIF
C
C---- Aft Rotor
C
      CALL NEWCOLORNAME('green')
C     
      IF(WIND) THEN
       CALL XYLINE(II,XI,Z3,0.0,1.0,0.0,-WFAC,1)
       CALL XYLINE(II,XI,Z4,0.0,1.0,0.0,-WFAC,1)
      ELSE
       CALL XYLINE(II,XI,Z3,0.0,1.0,0.0, WFAC,1)
       CALL XYLINE(II,XI,Z4,0.0,1.0,0.0, WFAC,1)
      ENDIF
C
C---- Totals
C
      CALL NEWCOLORNAME('red')
C     
      IF(WIND) THEN
       CALL XYLINE(II,XI,Z5,0.0,1.0,0.0,-WFAC,1)
       CALL XYLINE(II,XI,Z6,0.0,1.0,0.0,-WFAC,1)
      ELSE
       CALL XYLINE(II,XI,Z5,0.0,1.0,0.0, WFAC,1)
       CALL XYLINE(II,XI,Z6,0.0,1.0,0.0, WFAC,1)
      ENDIF
C
C-----------------------------------------------------------------------
C--- Numerical Legend
C
C    ADVCR(J) Advance Ratio
C    RPMCR(J) RPM
C    TCR(J)   Thrust
C    ECR(J)   Efficiency
C    PCR(J)   Power
C    TCRTOT   Thrust CR total
C    PCRTOT   Power CR total
C    ECRTOT   Efficiency system
C    TCRLB    Thrust in lbf
C    PCRHP    Power in horsepower
C    VKNOT    Speed in knots
C--------------------------------
C
C---- character size for version
C
      CSV = 0.85*CSIZE
C
C---  placing legend line labels...
C 
      XLLINE = 3.5*CS   ! legend line length
      YLLINE = 0.4*CSL  ! distance above the baseline
C
C---- set label locations
C     
      YL  = WFAC*WMAX + 10.0*CSL !  sets top margin
      XL1 = 0.0                  !  legend lines begin
      XL2 = XL1 +  4.0*CS        !  labels begin
      XL3 = XL2 +  5.0*CS        !  names begin
      XL4 = XL3 + 17.0*CS        !  thrust
      XL5 = XL4 + 10.0*CS        !  power
      XL6 = XL5 + 10.0*CS        !  effy
      XL7 = XL6 +  9.0*CS        !  rpm
      XL8 = XL7 + 10.0*CS        !  J
C
C--- plot title
C
      CALL STRIP(EVERSION,NV)
      ZV = 8.0 + FLOAT(NV)
C
      CALL GETCOLOR(ICOL0)
      CALL NEWCOLORNAME('black')
      CALL NEWPEN(2)
      CALL PLCHAR(XL1,YL,1.1*CS,'Converged CR System',
     & 0.0,19)
      CALL NEWCOLORNAME('cyan')
      CALL PLCHAR(1.0-ZV*CSV,YL,CSV,'CROTOR v',0.0,8)
      CALL PLCHAR(999.,YL,CSV,EVERSION,0.0,-1)
C
      YL = YL - 1.0*CS
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('black')
      CALL PLOT(0.0,YL,3)
      CALL PLOT(1.0,YL,2)
C
C--- legend titles
C
      YL = YL - 2.5*CS
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('orange')
      CALL PLCHAR(XL3-3.0*CS,YL,CS,'   Roter    ',0.0,12)
      CALL PLCHAR(XL4-3.0*CS,YL,CS,' Power kW ',0.0,10)
      CALL PLCHAR(XL5-3.0*CS,YL,CS,' Thrust kN ',0.0,11)
      CALL PLCHAR(XL6-3.0*CS,YL,CS,'   Effy   ',0.0,10)
      CALL PLCHAR(XL7-3.0*CS,YL,CS,'   Rpm    ',0.0,10)
      CALL PLCHAR(XL8-3.0*CS,YL,CS,'     J    ',0.0,10)
C
C--- FWD data 
C
      YL = YL - 2.5*CS
C
      CALL NEWCOLORNAME('blue')
      CALL PLOT(XL1,YL+YLLINE,3)
      CALL PLOT(XL1+XLLINE,YL+YLLINE,2)
      CALL NEWCOLORNAME('black')
C
      CALL PLCHAR(XL2,YL,CS,'FWD',0.0,3)
      CALL PLCHAR(XL3,YL,CS,NAMECR(1),0.0,15)
      CALL PLNUMB(XL4,YL,CS,PCR(1)/1000. ,0.0,3)
      CALL PLNUMB(XL5,YL,CS,TCR(1)/1000. ,0.0,4)
      CALL PLNUMB(XL6,YL,CS,ECR(1) ,0.0,3)
      CALL PLNUMB(XL7,YL,CS,RPMCR(1),0.0,0)
      CALL PLNUMB(XL8,YL,CS,ADVCR(1),0.0,4)
C
C--- AFT data 
C
      YL = YL - 2.5*CS
C
      CALL NEWCOLORNAME('green')
      CALL PLOT(XL1,YL+YLLINE,3)
      CALL PLOT(XL1+XLLINE,YL+YLLINE,2)
      CALL NEWCOLORNAME('black')
C
      CALL PLCHAR(XL2,YL,CS,'AFT',0.0,3)
      CALL PLCHAR(XL3,YL,CS,NAMECR(2),0.0,15)
      CALL PLNUMB(XL4,YL,CS,PCR(2)/1000. ,0.0,3)
      CALL PLNUMB(XL5,YL,CS,TCR(2)/1000. ,0.0,4)
      CALL PLNUMB(XL6,YL,CS,ECR(2) ,0.0,3)
      CALL PLNUMB(XL7,YL,CS,RPMCR(2) ,0.0,0)
      CALL PLNUMB(XL8,YL,CS,ADVCR(2) ,0.0,4)
C
C--- SYS data 
C
      YL = YL - 2.5*CS
C
      CALL NEWCOLORNAME('red')
      CALL PLOT(XL1,YL+YLLINE,3)
      CALL PLOT(XL1+XLLINE,YL+YLLINE,2)
      CALL NEWCOLORNAME('black')
C
      CALL PLCHAR(XL2,YL,CS,'SYS',0.0,3)
      CALL PLCHAR(XL3,YL,CS,CRNAME,0.0,15)
      CALL PLNUMB(XL4,YL,CS,PCRTOT/1000. ,0.0,3)
      CALL PLNUMB(XL5,YL,CS,TCRTOT/1000. ,0.0,4)
      CALL PLNUMB(XL6,YL,CS,ECRTOT ,0.0,3)
C
      CALL PLCHAR(999.,YL,CS,'  Vm/s:',0.0,7)
      CALL PLNUMB(999.,YL,CS,VEL,0.0,1)
      CALL PLCHAR(999.,YL,CS,'  hkm:',0.0,6)
      CALL PLNUMB(999.,YL,CS,ALT,0.0,1)
C
C      YL = YL - 1.0*CS
C
C      CALL NEWPEN(1)
C      CALL PLOT(0.0,YL,3)
C      CALL PLOT(1.0,YL,2)
C
C-------------------------------------
      CALL PLFLUSH
C
      RETURN
      END !   CRPLIN
C
C----------------------------------------------------------------



      SUBROUTINE DESCR
      INCLUDE 'XROTOR.INC'
C--------------------------------------
C---- Design MIL prop within CROTOR
C--------------------------------------
C
      CHARACTER*32 DNAME
C
      PLFACD = 0.6
      PLFAC1 = 0.7
      PLFAC2 = 0.85
      XORG = 0.15
      YORG = 0.10
C
      IF(LFORWARD) THEN
        DNAME = NAMECR(1)
        WRITE(*,*)'Replacing FWD rotor: ', DNAME
      ELSE
        DNAME = NAMECR(2)
        WRITE(*,*)'Replacing AFT rotor: ', DNAME
      ENDIF
C
      WRITE(*,1000) CRCLR,CRCLT,VELCR
C
      WRITE(*,2000)
 2000 FORMAT( ' <a> to abort',
     &      / ' <ret> to use current rotor parameters')
C
      CALL ASKS('Enter new rotor name^',DNAME)
C
      IF(DNAME.EQ.'A' .OR. DNAME.EQ.'a') RETURN
      IF(DNAME(1:1) .NE. ' ') THEN
        NAME = DNAME
      ELSE IF(LROTOR) THEN
        RADDES = RAD
        R0DES  = XI0*RAD
        RWDES  = XW0*RAD
        NAME   = 'Designed Rotor'
        GOTO 100
      ELSE
        NAME = 'Designed Rotor'
        WRITE(*,*)'There is no loaded rotor'
        WRITE(*,*)'Rotor name is undefined'
      ENDIF
C
      CALL ASKI('Enter number of blades ^',NBLDS)
      CALL ASKR('Enter tip radius (m)   ^',RADDES)
      CALL ASKR('Enter hub radius (m)   ^',R0DES)
      CALL ASKR('Hub wake displacement body radius (m) ^',RWDES)
C
  100 VELDES = VELCR
      ADVDES = 0.0
      TDDES  = 0.0
      DEST = .FALSE.
      DESP = .TRUE.
C
      IF(LFORWARD) THEN
        RPMDES = RPMF
        PDDES  = POWERF
      ELSE
        RPMDES = RPMA
        PDDES  = POWERA
      ENDIF
C
      CLROOT = CRCLR
      CLTIP  = CRCLT
      CALL SETCLD(CLROOT,CLTIP)
      CLDES0 = 0.5*(CLROOT+CLTIP)
C
C--- use imposed velocity profiles from last solution
C
      IF(.NOT.LVCLR .AND. LROTOR) CALL PUTVELD
C
      LROTOR = .FALSE.
C
C--- design it
C
      LDESINI = .TRUE.
      CALL DESGEN
      CALL PLTINI(SCRNFR,IPSLU,IDEV,PLFACD*SIZE,LPLOT,.NOT.LLAND)
      CALL PLOT(0.175,0.075,-3)
      CALL GEOPLT('AL')
      CALL PLOTABS(0.0,0.0,-3)
      CALL PLOT(0.175,0.875,-3)
      CALL CLPLT
C
      LFWD = LFORWARD 
c
      IF(LFWD) THEN
        CALL STORCR(1)
        CALL CRDATA(1)
        FNAMEF = 'none'
        LROTF = .TRUE.
      ELSE
        CALL STORCR(2)
        CALL CRDATA(2)
        FNAMEA = 'none'
        LROTA = .TRUE.
      ENDIF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
        WRITE(*,*) 'Designed FWD rotor loaded'
      ELSE
        WRITE(*,*) 'Designed AFT rotor loaded'
      ENDIF
C
C--- store weighted slipstream for other rotor
C
      CALL PUTVEL
C
      RETURN
C
C
 1000 FORMAT(' Design CLs (root-tip): ',F4.2,'-',F4.2,
     &     /,' Flight speed (m/s): ', F5.1)
C
      END   ! DESCR
C
C----------------------------------------------------------------



      SUBROUTINE CRBLEND
      INCLUDE 'XROTOR.INC'
C--------------------------------------
C     Blends between loaded rotors
C--------------------------------------
C
      BF = BLENDF
      IF(.NOT.LROTF .OR. .NOT.LROTA) THEN
	WRITE(*,*) 'Rotors not defined'
	RETURN
      ENDIF
C
C---- limitations
C
      IF(RADCR(1).NE.RADCR(2) .OR. XI0CR(1).NE.XI0CR(2) .OR.
     &         IICR(1).NE.IICR(2)) THEN
         WRITE(*,*) 
     &  'Blending requires identical radii and stations'
         RETURN
      ENDIF
C
      DO I=1,II
       IF(LBC)CH(I)   =CHCR(1,I)   +BF*(CHCR(2,I)   -CHCR(1,I))
       IF(LBA)BETA(I) =BETACR(1,I) +BF*(BETACR(2,I) -BETACR(1,I))
       IF(LBA)BETA0(I)=BETA0CR(1,I)+BF*(BETA0CR(2,I)-BETA0CR(1,I))
      ENDDO
C
      IF(LFWD) THEN
         CALL STORCR(1)
         FNAMEF = 'none'
      ELSE
         CALL STORCR(2)
         FNAMEA = 'none'
      END IF
C
      LCON = .FALSE.
      CALL DISPIN(LUWRIT)
C
      IF(LFWD) THEN
        WRITE(*,*) 'Blended FWD rotor loaded'
      ELSE
        WRITE(*,*) 'Blended AFT rotor loaded'
      ENDIF
C
      RETURN
      END
C
C----------------------------------------------------------------



      SUBROUTINE VVECT(IAUTO)
      INCLUDE 'XROTOR.INC'
C-------------------------------------------------------------
C---- calculates velocity vector at sample blade station and
C---- adjusts blade angle incrementally around velocity vector
C-------------------------------------------------------------
C
C---  Find sample blade station
C
      DO I = 1,II
        BLOC = (XI(I) - XI0)/(1 - XI0)   
        IF(BLOC.GE.CLOC) GO TO 10
      ENDDO 
C
 10   IF(I.GT.II) I=II
C
C---  increments
C
      RAUTO = FLOAT(NAUTO)
      BINC = 2. * CRANGE/(RAUTO - 1.)
C
C---  deviation from velocity vector
C         
      BSIGN = (-1.0) ** IAUTO
C
      IDEL = IAUTO/2
      RDEL = FLOAT(IDEL)
C      
      BDEL = (BSIGN * BINC * RDEL) * PI/180.
C
C---  calculate velocity vector
C
      VT = VIND(3,I)
      VA = VIND(1,I)
C
      UDUCT     = 0.0
      VADUCT_VA = 1.0
      IF(DUCT) THEN
        UDUCT = URDUCT-1.0
        VADUCT_VA = 2.0*URDUCT
      ENDIF
      VA = VA * VADUCT_VA
      UTOT = 1.0 + UDUCT + UBODY(I)
      CALL UVADD(XI(I),WA,WT)
C
      VTANG  = XI(I)/ADV - WT  -  VT
      VAXIAL = UTOT      + WA  +  VA
C
      VV = ATAN(VAXIAL/VTANG)
C
C---  move blades
C
      BMOVE  = VV + BDEL - BETA(I)
C
      DO J=1, II
        BETA(J)  = BETA(J)  + BMOVE
        BETA0(J) = BETA0(J) + BMOVE
      ENDDO
C
      IF(IAUTO.EQ.1) WRITE(*,1000)
C
      BDELDG = BDEL * 180./PI
      VVDG = VV * 180./PI
      BETADG = BETA(I) * 180./PI
C
      WRITE(*,1100) IAUTO, I, BINC, BDELDG, VVDG, BETADG
C
      RETURN
C
 1000 FORMAT(
     &  / ' Iter   Stn     Incr      Dev       VV       Beta')
C
 1100 FORMAT(2X, I2, 4X, I2,4X,F6.2,4X,F6.2,4X,F6.2,4X,F6.2)
C
      END


C---------------------------------------------------------------
      SUBROUTINE CRINIT
      INCLUDE 'XROTOR.INC'
C----------------------------------------------------------
C     CROTOR Initialization + ESPARA
C----------------------------------------------------------
C
      LROTF  = .FALSE.  ! forward rotor geometry not loaded
      LROTA  = .FALSE.  ! aft rotor geometry not loaded
      LDEF   = .FALSE.  ! default menu not specified
      LPITCH = .FALSE.  ! pitch is not fixed
      LSRPM  = .TRUE.   ! rpms are synchronized
      LFWD   = .TRUE.   ! forward rotor is active
      LVCLR  = .FALSE.  ! slipstreams have not been initialized
      LESPARA= .FALSE.  ! PARA analysis reporting is not active
C
      FNAMEF = 'none'
      FNAMEA = 'none'
      CRNAME = 'CR System'
      NAMECR(1)= 'undefined'
      NAMECR(2)= 'undefined'
C
C     Standard CR velocity weights
      UWTF = 0.5
      UWTA = 0.5
      VWTF = 0.0
      VWTA = -1.0
C
      ICRITL = 20     ! CR iteration limit
      CRCON  = 0.001  ! convergence flag delta (thrust)
C
      CRCLR  = 0.6     ! design CL - root
      CRCLT  = 0.6     ! design CL - tip
      BLENDF = 0.5     ! rotor interpolation factor
      RRAT   = 1.0     ! rpm ratio (rpms synchronized)
C
      NAUTO  = 5       ! auto converge - max attempts
      CRANGE = 10.0    ! auto converge - max deviation (deg)
      CLOC   = 0.7     ! auto converge - VV blade location
C
      PLOC   = 0.5     ! ESPARA beta sample location (bladelengths)
C
      NADDF = 0        ! no slipstream at fwd rotor
      NADDA = 0        ! no slipstream at aft rotor
C
      RETURN
      END   ! CRINIT


C-----------------------------------------------------------------


