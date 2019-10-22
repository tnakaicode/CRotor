C**********************************************************************
C    Module espara.f
C    
C    Acknowledgments to Mark Drela and Harold Youngren
C
C    Esotec code Copyright (C) 2001-2011 Philip Carter, Esotec Developments
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

      SUBROUTINE ESPARA
      INCLUDE 'XROTOR.INC'
      INCLUDE 'ESPARA.INC'
C-------------------------------------------------------------------------
C     Multi-Rotor Parametric Analysis in XROTOR
C     Version 0.8  September 2011
C-------------------------------------------------------------------------
      CHARACTER*32 EFILET,NAMRD
      CHARACTER*4 COMAND,CONAND
      CHARACTER*132 COMARG,ANSARG,PROMPT,CONARG,LINE
      CHARACTER*1 CHKEY, ANS
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR
C
C----Check for loaded rotor
C
      IF(.NOT.LROTOR) THEN
        WRITE(*,*)'Subroutine ESPARA requires a loaded rotor'
        RETURN
      ENDIF
C
C----Initialize Arrays
C
      CALL INITDB
      CALL INITPROP(1,NPROPA)
      CONV = .FALSE.
C
C----Database File IO Unit
C
      ESUNT=71   
      GREEK = .FALSE.
C
C----Analysis reporting curtailed
C
      LESPARA = .TRUE.
C
C----Reference blade station for beta output
C
      DO I = 1,II
        BLOC = (XI(I) - XI0)/(1 - XI0)   
        IF(BLOC .GE. PLOC) THEN    ! PLOC set in CRINIT
           BETLOC = I
           GO TO 5
        ENDIF
      ENDDO
      BETLOC = II
C
 5    CONTINUE

C
C----PARA Menu----------------------------------------------------------
C
      WRITE(*,6100)
C
 1000 CONTINUE
      CALL ASKC('.PARA^',COMAND,COMARG)
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
      IF(COMAND.EQ.'    ') GO TO 10
      IF(COMAND.EQ.'?   ') WRITE(*,6100)
      IF(COMAND.EQ.'?   ') GO TO 1000
      IF(COMAND.EQ.'OPEN') GO TO 100
      IF(COMAND.EQ.'SAVE'.OR.COMAND.EQ.'S   ') GOTO 800
      IF(COMAND.EQ.'SAVA') GO TO 700
C
      IF(COMAND.EQ.'DBAS'.OR.COMAND.EQ.'D   ') GOTO 185
      IF(COMAND.EQ.'NEW ') GO TO 160 
      IF(COMAND.EQ.'VEL ') GO TO 20 
      IF(COMAND.EQ.'RPM ') GO TO 20  
      IF(COMAND.EQ.'POW ') GO TO 20     
      IF(COMAND.EQ.'ALT ') GO TO 20
C
      IF(COMAND.EQ.'PROP'.OR.COMAND.EQ.'P   ') GOTO 200
      IF(COMAND.EQ.'DELP') GO TO 130
      IF(COMAND.EQ.'NAMP') GO TO 150 
      IF(COMAND.EQ.'BLOC') GO TO 60 
C
      IF(COMAND.EQ.'BOUN'.OR.COMAND.EQ.'B   ') GOTO 350
      IF(COMAND.EQ.'BCLR') GO TO 360
      IF(COMAND.EQ.'BVEL') GO TO 50
      IF(COMAND.EQ.'BRPM') GO TO 50
      IF(COMAND.EQ.'BPOW') GO TO 50 
      IF(COMAND.EQ.'BALT') GO TO 50
C
      IF(COMAND.EQ.'RUN '.OR.COMAND.EQ.'R   ') GOTO 400           
C
      WRITE(*,1050) COMAND
      GO TO 1000
C
C
 6100 FORMAT(
     &  /'   OPEN f  Load Esprop database from disk'
     &  /'   SAVE<s> Save data to current Esprop file'
     &  /'   SAVA f  Save data to NEW Esprop file (Save As)'
C
     & //'   DBAS<d> Display database parameters'
     &  /'   NEW     Enter   parameters for new database'
     &  /'   VEL     Modify  Velocity parameters'
     &  /'   RPM     Modify  RPM          "     '
     &  /'   POW     Modify  Power        "     '
     &  /'   ALT     Modify  Altitude     "     '
C
     & //'   PROP<p> Display propeller statistics'
     &  /'   DELP i  Delete  propeller from database'
     &  /'   NAMP i  Modify  propeller name'
     &  /'   BLOC r  Modify  beta sample location (0->1)'
C
     & //'   BOUN<b> Display analysis bounds'
     &  /'   BCLR    Clear   analysis bounds (full sweep)'
     &  /'   BVEL    Modify  Velocity  bounds'
     &  /'   BRPM    Modify  RPM         "   '
     &  /'   BPOW    Modify  Power       "   '
     &  /'   BALT    Modify  Altitude    "   '
C
     & //'   RUN  i  Run parametric analysis sweep'
     &  /'  <ret>    Return to top level (unsaved data is lost)')
C
C----SAVE warning before quitting
C
 10   IF(SAVASK) THEN
         WRITE(*,*) 'Unsaved data will be lost'
         WRITE(*,*) 'Exit PARA anyway? (Y/n)'
         READ(*,7000) ANS
         IF(INDEX('Nn',ANS).NE.0) GO TO 1000
      END IF
      LESPARA = .FALSE.
      RETURN
C
C----Modify Database Parameters------------------------
C
 20   IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(NPROP.NE.0) THEN
         WRITE(*,*)'Database contains propeller data' 
         WRITE(*,*)'Cannot change database parameters'
         GO TO 1000
      END IF
C
      IF(COMAND.EQ.'VEL ') GO TO 165 
      IF(COMAND.EQ.'RPM ') GO TO 170  
      IF(COMAND.EQ.'POW ') GO TO 175     
      IF(COMAND.EQ.'ALT ') GO TO 180
C
C---Modify Analysis Bounds--------------------------------
C
 50   IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(COMAND .EQ. 'BVEL') THEN
         PROMPT=' Enter Velocity analysis bounds (0 for full sweep)^'
         CALL PARAM(PROMPT,NVEL,LVELU,LVELL)
         GO TO 350
      END IF
C
      IF(COMAND .EQ. 'BRPM') THEN
         PROMPT=' Enter RPM analysis bounds (0 for full sweep)^'
         CALL PARAM(PROMPT,NRPM,LRPMU,LRPML)
         GO TO 350
      END IF
C
      IF(COMAND .EQ. 'BPOW') THEN
         PROMPT=' Enter Power analysis bounds (0 for full sweep)^'
         CALL PARAM(PROMPT,NPWR,LPWRU,LPWRL)
         GO TO 350
      END IF
C
      IF(COMAND .EQ. 'BALT') THEN
         PROMPT=' Enter Altitude analysis bounds (0 for full sweep)^'
         CALL PARAM(PROMPT,NALT,LALTU,LALTL)
         GO TO 350
      END IF
C
C----------------------------------------------------------------------
C----Change blade station for Beta output
C
 60   IF(NINPUT.GE.1) THEN
        PLOCT = RINPUT(1)
      ELSE
        PLOCT = PLOC
        WRITE(*,*)'root 0 <---> 1 tip'
        CALL ASKR('Enter beta output blade location^',PLOCT)
      ENDIF
C
      IF(PLOCT.LT.0.0 .OR. PLOCT .GT. 1.0) THEN
        NINPUT = 0
        GO TO 60
      ENDIF
C
      PLOC = PLOCT
C
      DO I = 1,II
        BLOC = (XI(I) - XI0)/(1. - XI0)   
        IF(BLOC .GE. PLOC) THEN
           BETLOC = I
           GO TO 65
        ENDIF
      ENDDO  
C
      BETLOC = II
 65   BRAD   = XI(BETLOC)*RAD   ! Beta Stn radius
C
      WRITE(*,7050)BETLOC, XI(BETLOC), BRAD, BLOC
      GO TO 1000
C
C-----------------------------------------------------------------------
C----Open EXISTING ESPROP File
C
 100  IF(SAVASK) THEN
         WRITE(*,*) 'Unsaved data will be lost'
         WRITE(*,*) 'Overwrite current database? (Y/n)'
         READ(*,7000) ANS
         IF(INDEX('Nn',ANS).NE.0) GO TO 1000
      END IF
C
      EFILET = COMARG
      IF(EFILET(1:1) .EQ. ' ')
     &CALL ASKS(' Enter ESPROP filename (A to abort)^',EFILET)
C
      IF(EFILET.EQ.'A' .OR. EFILET.EQ.'a') GOTO 1000
C
C----Open File and check first line
C
      EFILE = EFILET
      OPEN(UNIT=ESUNT,FILE=EFILE,STATUS='OLD',IOSTAT=KODE)
C
      IF(KODE.NE.0) THEN
         WRITE(*,*)'Cannot open file ',EFILE
         GO TO 1000
      END IF
C
      CALL RDLINE2(ESUNT,LINE,ICNT)
      IF(LINE.EQ.'END' .OR. LINE.EQ.'ERR') GO TO 1000
C
      IF(LINE(1:6).NE.'ESPROP') THEN
        WRITE(*,*) 'Not an ESPROP database file'
        CLOSE(ESUNT)
        GO TO 1000
      ENDIF
C
C----Initialize if needed and read data into arrays
C
      CALL INITPROP(1,NPROP)
      IF(PAREAD) CALL INITDB
C
      READ(ESUNT,7010,ERR=900)NPROP,NALT,NPWR,NRPM,NVEL
C
      IF(NPROP .GT. NPROPA .OR. NALT .GT. NALTA .OR. NPWR .GT. NPWRA
     &   .OR. NRPM .GT. NRPMA .OR. NVEL .GT. NVELA) THEN
         WRITE(*,*)'Too many operating points - enlarge arrays'
         WRITE(*,*)'File read aborted'
         CLOSE(ESUNT)
         GO TO 1000
      END IF
C
      DO I=1,NPROP
         READ(ESUNT,7020,ERR=900) EPROP(I)
      ENDDO
C
      DO I=1,NALT
         READ(ESUNT,7030,ERR=900) EALT(I)
      ENDDO
C
      DO I=1,NPWR
         READ(ESUNT,7030,ERR=900) EPWR(I)
      ENDDO
C
      DO I=1,NRPM
         READ(ESUNT,7030,ERR=900) ERPM(I)
      ENDDO
C
      DO I=1,NVEL
         READ(ESUNT,7030,ERR=900) EVEL(I)
      ENDDO
C 
      DO I=1,NPROP
         READ(ESUNT,7035,ERR=900)BLDNUM(I),BLDRAD(I),
     &       BETRAD(I),BETSTN(I),NPANAL(I),NPNOTC(I)
      ENDDO
C
      DO 111 IPROP=1,NPROP
         DO 111 IALT=1,NALT
            DO 111 IPWR=1,NPWR
               DO 111 IRPM=1,NRPM
                  DO 111 IVEL=1,NVEL
                     READ(ESUNT,7040,ERR=900)
     &               EANG  (IPROP,IALT,IPWR,IRPM,IVEL),
     &               EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL),
     &               EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL),
     &               EEFFID(IPROP,IALT,IPWR,IRPM,IVEL),
     &               ETHRST(IPROP,IALT,IPWR,IRPM,IVEL),
     &               EADV  (IPROP,IALT,IPWR,IRPM,IVEL),
     &               EMACH (IPROP,IALT,IPWR,IRPM,IVEL),
     &               ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)
C
 111  CONTINUE
C
      CLOSE(ESUNT)
C
C----Initialize analysis bounds for full sweep
C
      LVELL=1
      LPWRL=1
      LRPML=1
      LALTL=1
      LVELU=NVEL
      LPWRU=NPWR
      LRPMU=NRPM
      LALTU=NALT
C
      PAREAD = .TRUE.
      FILYES = .TRUE.
      SAVASK = .FALSE.
C
      WRITE(*,*)'ESPROP Database Loaded: ',EFILE
      GO TO 190
C
C-------------------------------------------------------------------------
C----Delete propeller
C
 130  IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(NPROP.EQ.0)THEN
         WRITE(*,*)'No propeller data exists in the database'
         GOTO 1000
      ENDIF
C
      IF(NINPUT.GE.1) THEN
        IPROPT = IINPUT(1)
      ELSE 
        IPROPT = NPROP
        CALL ASKI(' Enter index of propeller being deleted^',IPROPT)
      ENDIF
C
      IF(IPROPT.LT.1 .OR. IPROPT.GT.NPROP) THEN
        WRITE(*,*)'Index out of range'
        IPROPT = NPROP
        GOTO 130
      ENDIF
C
      CALL DELPROP(IPROPT)
      SAVASK = .TRUE.
      GO TO 200
C
C----Change Propeller Names---------------------------
C
 150  IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(NPROP.EQ.0)THEN
         WRITE(*,*)'No propeller data exists in the database'
         GOTO 1000
      ENDIF
C
      IF(NINPUT.GE.1) THEN
        NUMBLD = IINPUT(1)
      ELSE 
        NUMBLD=1
        CALL ASKI(' Enter index of propeller being named^',NUMBLD)
      ENDIF
C
      IF(NUMBLD.LT.1 .OR. NUMBLD.GT.NPROP) THEN
        WRITE(*,*)'Index out of range'
        GOTO 150
      ENDIF
C
      WRITE(*,7175) NUMBLD, EPROP(NUMBLD)
      CALL ASKS(' Enter propeller name <ret takes default>^', NAMRD)
      IF (NAMRD(1:1) .EQ. ' ') GO TO 1000
C
      EPROP(NUMBLD) = NAMRD
      SAVASK=.TRUE.
      GO TO 200
C
C-------------------------------------------------------------------------
C----Enter Database Parameters
C
 160  CONTINUE
C
      IF(NPROP.NE.0)THEN
        WRITE(*,*)'Propeller data exists'
        WRITE(*,*)'Overwrite existing data and parameters? (Y/n)'
        READ(*,7000) ANS
        IF(INDEX('Nn',ANS).EQ.0) THEN
          CALL INITPROP(1,NPROP)
          CALL INITDB
        ELSE
          GOTO 1000
        ENDIF
      ELSEIF(PAREAD)THEN
        WRITE(*,*)'Database parameters exist'
        WRITE(*,*)'Overwrite existing parameters? (Y/n)'
        READ(*,7000) ANS
        IF(INDEX('Nn',ANS).EQ.0) THEN
          CALL INITDB
        ELSE
          GOTO 1000
        ENDIF
      ENDIF
C
      PAREAD = .FALSE.
C
C----Show array limits
C
      WRITE(*,7160) NPROPA,NVELA,NPWRA,NRPMA,NALTA
C
C----Velocity Parameters
C
 165  WRITE(*,*)
      PROMPT= ' Enter Velocity parameters (knots)^'
      CALL ESPVAL(PROMPT,NVALUE,NVEL,NVELA,EVEL)
      LVELL=1
      LVELU=NVEL
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----RPM Parameters
C
 170  WRITE(*,*)
      PROMPT= ' Enter RPM parameters^'
      CALL ESPVAL(PROMPT,NVALUE,NRPM,NRPMA,ERPM)
      LRPML=1
      LRPMU=NRPM
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----Power Parameters
C
 175  WRITE(*,*)
      PROMPT= ' Enter Power parameters (hp)^'
      CALL ESPVAL(PROMPT,NVALUE,NPWR,NPWRA,EPWR)
      LPWRL=1
      LPWRU=NPWR
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----Altitude Parameters
C
 180  WRITE(*,*)
      PROMPT= ' Enter Altitude parameters (ft)^'
      CALL ESPVAL(PROMPT,NVALUE,NALT,NALTA,EALT)
      LALTL=1
      LALTU=NALT
      SAVASK=.TRUE.
      IF(PAREAD) GO TO 1000
C
C----Parameters read in - write to terminal
C
      PAREAD = .TRUE.
      GO TO 185
C
C--------------------------------------------------------------------------
C----Write Parameter List to Terminal
C
 185  CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load an ESPROP file'
         GO TO 1000
      END IF
C
 190  CONTINUE
C
C----Accumulated totals
C
      NPANAC=0
      NPNCAC=0
      DO I=1,NPROP
         NPANAC=NPANAC+NPANAL(I)
         NPNCAC=NPNCAC+NPNOTC(I)
      ENDDO
C
      NPPRO =NALT*NPWR*NRPM*NVEL
      NPNTOT=NPROP*NPPRO
      NPCONV=NPANAC-NPNCAC
C
C----Write database parameters
C
      WRITE(*,7100)EFILE,NPROP,NVEL,NRPM,NPWR,NALT,
     &NPPRO,NPNTOT,NPANAC,NPCONV,NPNCAC
C
      WRITE(*,7110)
      DO I=1,NPROP
         WRITE(*,7140)I,EPROP(I)
      ENDDO
C
      WRITE(*,7130)
      DO I=1,NVEL
         WRITE(*,7150)I,EVEL(I)
      ENDDO
C
      WRITE(*,7125)
      DO I=1,NRPM
         WRITE(*,7150)I,ERPM(I)
      ENDDO
C
      WRITE(*,7120)
      DO I=1,NPWR
         WRITE(*,7150)I,EPWR(I)
      ENDDO
C
      WRITE(*,7115)
      DO I=1,NALT
         WRITE(*,7150)I,EALT(I)
      ENDDO
C
      WRITE(*,7135)
C
      GO TO 1000
C
C------------------------------------------------------------------------
C----Display propeller statistics
C
 200  CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(NPROP.EQ.0) THEN
         WRITE(*,*)'No propellers have been analyzed'
         GOTO 1000
      ENDIF
C
      WRITE(*,7400) EFILE
      DO I=1,NPROP
         WRITE(*,7420)I,EPROP(I),BLDNUM(I),BLDRAD(I),
     &       BETSTN(I),BETRAD(I),NPANAL(I),NPNOTC(I)
      ENDDO
      WRITE(*,7430)
C
      GO TO 1000
C
C----------------------------------------------------------------------
C----Display analysis parameter bounds
C
 350  IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      NOVEL=LVELU-LVELL+1
      NORPM=LRPMU-LRPML+1
      NOPWR=LPWRU-LPWRL+1
      NOALT=LALTU-LALTL+1
C
      NAOUT = NOVEL*NOPWR*NORPM*NOALT
C
      WRITE(*,7200)
C
      IF(LVELL.EQ.1 .AND. LVELU.EQ.NVEL .AND.
     &   LRPML.EQ.1 .AND. LRPMU.EQ.NRPM .AND.
     &   LPWRL.EQ.1 .AND. LPWRU.EQ.NPWR .AND.
     &   LALTL.EQ.1 .AND. LALTU.EQ.NALT) THEN
         WRITE(*,7210)
      ELSE
         WRITE(*,7220)
      ENDIF
C
      WRITE(*,7230)
     &   LVELL,LVELU,EVEL(LVELL),EVEL(LVELU),NOVEL,
     &   LRPML,LRPMU,ERPM(LRPML),ERPM(LRPMU),NORPM,
     &   LPWRL,LPWRU,EPWR(LPWRL),EPWR(LPWRU),NOPWR,
     &   LALTL,LALTU,EALT(LALTL),EALT(LALTU),NOALT,
     &   NAOUT
C
      GO TO 1000
C
C-----------------------------------------------------------------------
C----Clear analysis parameter bounds
C
 360  IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      LVELL = 1
      LRPML = 1
      LPWRL = 1
      LALTL = 1
      LVELU = NVEL
      LRPMU = NRPM
      LPWRU = NPWR
      LALTU = NALT
C
      GO TO 350
C
C-----------------------------------------------------------------------
C----Analysis Routines
C-----------------------------------------------------------------------
C
 400  CONTINUE
C
      IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      IF(NINPUT.GE.1) THEN
        LPROPT = IINPUT(1)
        IF(LPROPT.GT.0 .AND. LPROPT.LE.NPROP) THEN
          NPCONT=NPANAL(LPROPT)-NPNOTC(LPROPT)
          WRITE(*,2010)LPROPT,EPROP(LPROPT),NPANAL(LPROPT),NPCONT,LPROPT
          READ(*,7000) ANS
          IF(INDEX('Nn',ANS).NE.0) GOTO 1000
          LPROP=LPROPT
        ELSEIF(LPROPT.EQ.(NPROP+1)) THEN
          LPROP=NPROP+1
        ELSE
          WRITE(*,*)'Index outside current range'
          WRITE(*,*)'First available index used'
          LPROP=NPROP+1
        ENDIF
      ELSE
        LPROP = NPROP+1
      ENDIF
C
 2010 FORMAT(
     &     /,' Propeller index ',I2,' belongs to ',A32,
     &     /,' Analyzed  operating points:',I5,
     &     /,' Converged operating points:',I5,
     &     /,' Overwrite propeller ',I2' ? (Y/n)')  
C
 2020 FORMAT(
     &     /,' Database index ',I2,'  Propeller name: ',A32,
     &     /,' Total points to be analysed:',I5,
     &     /,' <a> to abort',
     &     /,' <ret> to run analysis sweep') 
C
C----Present analysis data and provide opportunity to abort
C
      NOVEL=LVELU-LVELL+1
      NORPM=LRPMU-LRPML+1
      NOPWR=LPWRU-LPWRL+1
      NOALT=LALTU-LALTL+1
C
      NAOUT = NOVEL*NORPM*NOPWR*NOALT
C
      WRITE(*,2020)LPROP,NAME,NAOUT
      READ(*,7000) ANS
      IF(INDEX('Aa',ANS).NE.0) GOTO 1000
C
C----Set propeller index, initialize overwrite, record blade data
C
      IPROP = LPROP
      IF(IPROP.GT.NPROP) THEN
        NPROP=NPROP+1
      ELSE
        CALL INITPROP(IPROP,IPROP)
      ENDIF
C
      EPROP (IPROP) = NAME  
      BLDNUM(IPROP) = NBLDS
      BLDRAD(IPROP) = RAD
      BETRAD(IPROP) = XI(BETLOC)*RAD
      BETSTN(IPROP) = BETLOC
      SAVASK=.TRUE. 
C
C----Loop through operating points
C
      DO 699 IVEL=LVELL,LVELU       
        VEL = FLOAT(EVEL(IVEL))*VELCON
C
        DO 699 IRPM=LRPML,LRPMU
          RPM = FLOAT(ERPM(IRPM))
          ADV = VEL / (RAD*RPM*PI/30.) 
C
          DO 699 IPWR=LPWRL,LPWRU
            PSPEC = FLOAT(EPWR(IPWR))*PWRCON  
C
            DO 699 IALT = LALTL, LALTU
              ALT = FLOAT(EALT(IALT))*ALTCON
              CALL ATMO(ALT,VSO,RHO,RMU)
C
              NPANAL(IPROP)=NPANAL(IPROP)+1  ! Increment counter   
              LPAUTO = .FALSE.                
C
C----Initialize blade angle
C
              IF(IPWR.EQ.LPWRL.AND.IALT.EQ.LALTL) CALL VVECT(1)
C
C----Analyse operating point
C
 450          CONV = .FALSE.
              CALL APER(3,1,LOPRINI)
C
C----If converged, trap negative convergence
C
              IF(CONV) THEN
                IF(TTOT.LT.0.) THEN
                  WRITE(*,7520)
                  GO TO 480
                END IF
C
C----Write to arrays and to terminal and continue
C
                CALL ESOPUT (LUWRIT,TDIM,EFFTOT,EFFIND,EIDEAL,
     &                       REFDEG,MACH,STALPC,BETLOC)
C
                TDIMKG=TDIM/9.81
                ADVJ=ADV*PI
C
                EANG  (IPROP,IALT,IPWR,IRPM,IVEL) = REFDEG
                EADV  (IPROP,IALT,IPWR,IRPM,IVEL) = ADVJ
                EMACH (IPROP,IALT,IPWR,IRPM,IVEL) = MACH
                EEFFID(IPROP,IALT,IPWR,IRPM,IVEL) = EIDEAL
                EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL) = EFFIND
                EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL) = EFFTOT
                ETHRST(IPROP,IALT,IPWR,IRPM,IVEL) = TDIMKG
                ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) = STALPC
C
C                WRITE(*,7500) NPANAL(IPROP),NAOUT,
C     &          IPROP,EVEL(IVEL),ERPM(IRPM),EPWR(IPWR),EALT(IALT),
C     &          REFDEG,ADVJ,MACH,TDIMKG,STALPC,EIDEAL,EFFIND,EFFTOT
C
                WRITE(*,7510)NPANAL(IPROP),NAOUT,EVEL(IVEL),ERPM(IRPM),
     &                      EPWR(IPWR),EALT(IALT)
                GO TO 699
              END IF
C
C----------------------------------------------------------------------
C----Convergence Failed or Negative 
C----Write unconverged output and failed parameters to terminal
C
C----Restore blade angles
C
 480        IF(LPAUTO) GO TO 620
C
            DO I=1, II
              BETA(I)  = BETA(I)  - DBETA
              BETA0(I) = BETA0(I) - DBETA
            ENDDO
C
            CALL OUTPUT(LUWRIT)
            WRITE(*,*) 'Initial blade angles restored'
C
            ESMETV=FLOAT(EVEL(IVEL))*VELCON
            ESMETA=FLOAT(EALT(IALT))*ALTCON
            ESMETP=FLOAT(EPWR(IPWR))*PWRCON
C
            WRITE(*,7550) NPANAL(IPROP),NAOUT,IPROP,
     &      EVEL(IVEL),ERPM(IRPM),EPWR(IPWR),EALT(IALT),ESMETV,ESMETP 
C
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
 5000 FORMAT(
     &  /'   <ret>   Continue'
     &  /'   AUTO<a> Auto-initialize blade angles and continue'
     &  /'   ANGL r  Change blade angles manually'  
     &  /'   RPM     Analyse at current blade angle' 
     &  /'   FORM    Toggle between Graded Mom.and Potential Form'
     &  /'   VRTX    Toggle between Graded Mom.and Vortex Form'
     &  /'   WAKE    Toggle between rigid and self-deforming wake'
     &  /'   INIT    Initialize next analysis case'
     &  /'   REIN    Re-initialize rotor to known operating state'
     &  /'   ASET    Change AUTO settings'
     &  /'   SKIP    Give up trying to converge this point'
     &  /'   STOP    Abort this run and return to .PARA prompt')
C
C---------------------------------------------
      IF(CONAND.EQ.'    ') GO TO 450
C---------------------------------------------
      IF(CONAND.EQ.'?   ') THEN
        WRITE(*,5000)
        GO TO 600
      ENDIF
C---------------------------------------------
      IF(CONAND.EQ.'STOP') THEN
        WRITE(*,7580)
        WRITE(*,7300) IPROP,EPROP(IPROP),
     &  LVELL,LVELU,EVEL(LVELL),EVEL(LVELU),NOVEL,
     &  LRPML,LRPMU,ERPM(LRPML),ERPM(LRPMU),NORPM,
     &  LPWRL,LPWRU,EPWR(LPWRL),EPWR(LPWRU),NOPWR,
     &  LALTL,LALTU,EALT(LALTL),EALT(LALTU),NOALT,
     &  NAOUT
C
        ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)= -99.9 ! Non-conv. flag
        NPNOTC(IPROP)=NPNOTC(IPROP)+1            ! Non-conv. counter
C
        WRITE(*,7600)IVEL,IRPM,IPWR,IALT,NPANAL(IPROP),
     &               NPNOTC(IPROP)
        GO TO 1000 
      END IF
C--------------------------------------------
      IF(CONAND.EQ.'AUTO' .OR. CONAND.EQ.'A   ') THEN
         LPAUTO = .TRUE.
         IAUTO = 0
         GO TO 620
      ENDIF
C-------------------------------------------
      IF(CONAND.EQ.'ASET') THEN
        CALL ASKI('Max number of convergence attempts^',NAUTO)
        CALL ASKR('Max deviation from velocity vector (deg)^',CRANGE)
 610    WRITE(*,*)
        WRITE(*,*) 'root 0 <---------> 1 tip'
        CALL ASKR( 'Sample blade location^',CLOC)
        IF(CLOC.LT.0.0 .OR. CLOC .GT. 1.0) GO TO 610
        GO TO 600
      ENDIF
C-----------------------------------------
      IF(CONAND.EQ.'VRTX') THEN
        VRTX = .NOT.VRTX
        IF(.NOT.VRTX) WRITE(*,*)'Discrete Vortex Formulation deselected'
        IF(VRTX)      WRITE(*,*)'Discrete Vortex Formulation selected'
        GO TO 600
      ENDIF
C------------------------------------------
      IF(CONAND.EQ.'FORM') THEN
        FAST = .NOT.FAST
        IF(FAST)      WRITE(*,*)'Graded Momentum Formulation selected'
        IF(.NOT.FAST) WRITE(*,*)'Potential Formulation selected'
        GO TO 600
      ENDIF
C----------------------------------------
      IF(CONAND.EQ.'WAKE') THEN
        FREE = .NOT.FREE
        IF(FREE)      WRITE(*,*)'Self-deforming wake selected'
        IF(.NOT.FREE) WRITE(*,*)'Rigid wake selected'
        GO TO 600
      ENDIF
C---------------------------------------
      IF(CONAND.EQ.'INIT') THEN
        LOPRINI = .NOT.LOPRINI
        IF(LOPRINI) THEN
          WRITE(*,*) 'Analysis case will be initialized'
        ELSE
          WRITE(*,*) 'Analysis case will not be initialized'
        ENDIF
        GO TO 600
      ENDIF
C-------------------------------------
      IF(CONAND.EQ.'REIN') THEN
        CALL REINIT
        GO TO 600
      ENDIF
C--------------------------------------
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
C-------------------------------------
      IF(CONAND.EQ.'RPM ') THEN
        CONV = .FALSE.
        CALL APER(4,2,LOPRINI)
        CALL OUTPUT(LUWRIT)
C
        IF(CONV .AND. TTOT .GE. 0.) THEN
           GO TO 600
        ELSE IF(CONV .AND. TTOT .LT. 0.) THEN
           WRITE(*,7520)
        ELSE IF(TTOT .LT. 0.) THEN
           WRITE(*,7530)
        ENDIF
C
        WRITE(*,7550) NPANAL(IPROP),NAOUT,IPROP,EVEL(IVEL),
     &  ERPM(IRPM),EPWR(IPWR),EALT(IALT),ESMETV,ESMETP 
C
        GO TO 600
      ENDIF
C-----------------------------------------
      IF(CONAND.EQ.'SKIP')THEN
        ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)= -99.9 ! Non-conv. flag
        NPNOTC(IPROP)=NPNOTC(IPROP)+1            ! Non-conv. counter
        GO TO 699
      ENDIF
C-----------------------------------------
      WRITE(*,1050) CONAND  !  command not recognized
      GO TO 600
C
C
C---- Auto-converge loop-------------------------------------------
C
  620 IAUTO = IAUTO + 1
C
      IF(IAUTO .GT. NAUTO) THEN
         LPAUTO = .FALSE.
         WRITE(*,*) 'AUTO command failed'
         GO TO 600
      ENDIF
C
      CALL VVECT(IAUTO)
      GO TO 450
C
C---------------------------------------------------------------------
C----End Parameter Loop
C---------------------------------------------------------------------
C
 699  CONTINUE   ! Analyze next point
C
C----ALL DONE! Display totals and return to PARA menu
C
      WRITE(*,7570) NPANAL(IPROP),NPNOTC(IPROP)
      GO TO 1000 
C
C
C-----------------------------------------------------------------------
C----Open NEW ESPROP file
C
 700  IF(.NOT.PAREAD) THEN
         WRITE(*,*)'Database Parameters not defined'
         WRITE(*,*)'Execute NEW or load ESPROP file'
         GO TO 1000
      END IF
C
      EFILET = COMARG
      IF(EFILET(1:1) .EQ. ' ')
     &CALL ASKS(' Enter new database filename (A to abort)^',EFILET)
C
      IF(EFILET.EQ.'A'.OR.EFILET.EQ.'a') GOTO 1000
C
      EFILE = EFILET
      OPEN(UNIT=ESUNT,FILE=EFILE,STATUS='NEW',IOSTAT=KODE)
C
      IF(KODE.NE.0) THEN
         WRITE(*,*)'Error opening file ',EFILE
         GO TO 1000
      END IF
C
      FILYES = .TRUE.
      GO TO 805
C
C-----------------------------------------------------------------------
C----Write Arrays to ESPROP File
C
 800  CONTINUE
C
      IF(.NOT.FILYES) GO TO 700
C
      OPEN(UNIT=ESUNT,FILE=EFILE,STATUS='OLD',IOSTAT=KODE)
C
      IF(KODE.NE.0) THEN
         WRITE(*,*)'Error Opening File ',EFILE
         GO TO 1000
      END IF
C
 805  REWIND(ESUNT)
C
      WRITE(ESUNT,7005,ERR=910)
      WRITE(ESUNT,7010,ERR=910)NPROP,NALT,NPWR,NRPM,NVEL
C
      DO I=1,NPROP
         WRITE(ESUNT,7020,ERR=910)EPROP(I)
      ENDDO
C
      DO I=1,NALT
         WRITE(ESUNT,7030,ERR=910)EALT(I)
      ENDDO
C
      DO I=1,NPWR
         WRITE(ESUNT,7030,ERR=910)EPWR(I)
      ENDDO
C
      DO I=1,NRPM
         WRITE(ESUNT,7030,ERR=910)ERPM(I)
      ENDDO
C
      DO I=1,NVEL
         WRITE(ESUNT,7030,ERR=910)EVEL(I)        
      ENDDO
C
      DO I=1,NPROP
         WRITE(ESUNT,7035,ERR=910)BLDNUM(I),BLDRAD(I),
     &        BETRAD(I),BETSTN(I),NPANAL(I),NPNOTC(I)
      ENDDO
C
      DO 811 IPROP=1,NPROP
         DO 811 IALT=1,NALT
            DO 811 IPWR=1,NPWR
               DO 811 IRPM=1,NRPM
                  DO 811 IVEL=1,NVEL
                      WRITE(ESUNT,7040,ERR=910)
     &                EANG  (IPROP,IALT,IPWR,IRPM,IVEL),
     &                EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL),
     &                EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL),
     &                EEFFID(IPROP,IALT,IPWR,IRPM,IVEL),
     &                ETHRST(IPROP,IALT,IPWR,IRPM,IVEL),
     &                EADV  (IPROP,IALT,IPWR,IRPM,IVEL),
     &                EMACH (IPROP,IALT,IPWR,IRPM,IVEL),
     &                ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)
C
 811  CONTINUE
C
      CLOSE(ESUNT)
      WRITE(*,*)'Data written to file: ',EFILE
      SAVASK = .FALSE.
C
      GO TO 1000     
C
C----End of File Print Routine------------------------------------------
C
C----End Game
C
 900  WRITE(*,*)' ****Database File Read Error. Aborted****'
      CLOSE(ESUNT)
      GO TO 1000
C
 910  WRITE(*,*)' ****Disk File Write Error. Aborted****'
      CLOSE (ESUNT)
      GO TO 1000
C
C
C----Format Statements----------------------------------------------------
C
C
 1050 FORMAT(1X,A4,' command not recognized.' //
     &             '  Type "?" for list, <Return> to exit menu.')
C
 7000 FORMAT(A)
C
 7005 FORMAT('ESPROP Parametric Database')
 7010 FORMAT(1X,I4,I4,I4,I4,I4)
 7020 FORMAT(1X,A30)
 7030 FORMAT(1X,I6)
 7035 FORMAT(1X,I4,F9.5,F9.5,I6,I6,I6)
 7040 FORMAT(1X,F7.3,F7.4,F7.4,F7.4,F7.2,F7.3,F7.3,F7.2)
C
 7050 FORMAT(' Beta output blade station: ',I2,/,
     &       ' Blade station r/R:        ',F6.3,/,
     &       ' Station radius (m):       ',F6.3,/,
     &       ' Blade location (0 - 1):   ',F6.3)
C
 7100 FORMAT(/,6X,'ESPROP Database Parameters',/,
     &4X, 30('='),/,
     &6X,'Filename: ',A30,/,
     &4X, 30('-'),/,
     &7X,I2,' Propellers',/,
     &7X,I2,' Velocity Points',/,
     &7X,I2,' RPM Points',/,
     &7X,I2,' Power Points',/,
     &7X,I2,' Altitude Points',/,
     &4X, 30('-'),/,
     &5X,' Points per rotor    ',I6,/,
     &5X,' Total Points        ',I6,/,
     &5X,' Analysed Points     ',I6,/,
     &5X,' Converged Solutions ',I6,/,
     &5X,' Failed Points       ',I6)
C
 7110 FORMAT(4X,30('-'),/,6X,' Propeller')
 7115 FORMAT(4X,30('-'),/,6X,' Altitude       (ft)')
 7120 FORMAT(4X,30('-'),/,6X,'   Power         (hp)')
 7125 FORMAT(4X,30('-'),/,6X,'    RPM        (r/min)')
 7130 FORMAT(4X,30('-'),/,6X,' Velocity      (knots)')
 7135 FORMAT(4X,30('='),/)
C
 7140 FORMAT(10X,I2,5X,A30)
 7150 FORMAT(10X,I2,8X,I6)
C
 7160 FORMAT(/,' Current Array Limits',/,
     &         ' --------------------',/,
     &        3X,I2,'  Propellers',/,
     &        3X,I2,'  Velocities',/,
     &        3X,I2,'  Powers',/,
     &        3X,I2,'  RPMs',/,
     &        3X,I2,'  Altitudes',/,
     &         ' --------------------')
C
C
 7175 FORMAT(/,' Propeller ',I2,': current name is ',A32)
C
 7180 FORMAT(/,' WARNING: Propeller ',I2,' contains data',/,
     &       ' Continue with analysis? <y/n>')
C
 7185 FORMAT(/,' Propeller ',I2,' data initialized to zero')
C
 7200 FORMAT(/,'                 Analysis Bounds',/,2X, 46('-'))
 7210 FORMAT(2X,'Full parameter sweep')
 7220 FORMAT(2X,'Partial parameter sweep')
 7230 FORMAT(
     &'  Velocity ',I2,' to ',I2,3X,I5,' to ',I5,' kts  ',I2,' pts'/,
     &'  RPM      ',I2,' to ',I2,3X,I5,' to ',I5,' rpm  ',I2,' pts'/,
     &'  Power    ',I2,' to ',I2,3X,I5,' to ',I5,' hp   ',I2,' pts'/,
     &'  Altitude ',I2,' to ',I2,3X,I5,' to ',I5,' ft   ',I2,' pts'/,
     &   2X, 46('-'),/,
     &'  Total Operating Points = ',I5,/)
C
 7300 FORMAT(/,
     &'                 Analysis Bounds',/,
     &'                 ---------------',/,
     &'  Propeller ',I2,'            Name: ',A16,/,
     &   2X, 46('-'),/,
     &'  Velocity ',I2,' to ',I2,3X,I5,' to ',I5,' kts  ',I2,' pts'/,
     &'  RPM      ',I2,' to ',I2,3X,I5,' to ',I5,' rpm  ',I2,' pts'/,
     &'  Power    ',I2,' to ',I2,3X,I5,' to ',I5,' hp   ',I2,' pts'/,
     &'  Altitude ',I2,' to ',I2,3X,I5,' to ',I5,' ft   ',I2,' pts'/,
     &2X, 46('-'),/,
     &'  Total Operating Points = ',I5,/)
C
 7400 FORMAT(/,
     &'                        Propeller Statistics',/,
     &'                        --------------------',/,
     &26X,A32,/,
     &' ---------------------------------------------------',
     &'----------------',/,
     &' Prop    Prop        Blade  Radius   Beta  BetaRad  ',
     &'Analysed  Failed',/,
     &' Index   Name        Number   (m)    Stn     (m)    ',
     &' Points   Points',/,
     &' ---------------------------------------------------',
     &'----------------')
C
 7420 FORMAT(2X,I2,2X,A15,2X,I1,4X,F5.3,3X,I4,4X,F5.3,5X,I5,4X,I4)
 7430 FORMAT(1X,67('-'))
C
C
 7500 FORMAT(/,1X,'Analysis Point ',I4,' of ',I4,/,
     &1X,'Prop ',I2,8X,'Vel:',I4,' kts',3X,'RPM:',I5,5X,
     &'Pwr:',I4,' hp',4X,'Alt:',I6,' ft',/,
     &1X,'BAng=',F4.1,' deg',2X,'AdvR=',F5.3,5X,'Mach=',F4.2,5X,
     &'Thrst=',F5.1,' kg',/,
     &1X,'Stall%=',F6.1,2X,'EffIdl=',F5.3,3X,'EffInd=',F5.3,2X,
     &'Effy=',F5.3,/)
C
 7510 FORMAT(I5,' of',I5,' points:  Vel=',I4,'  Rpm=',I5,'  Power=',I5,
     &'  Alt=',I5)
C
 7520 FORMAT(/22X,'*** NEGATIVE CONVERGENCE ***')
 7530 FORMAT(/24X,'*** NEGATIVE THRUST ***')
C
 7550 FORMAT(/,22X,'UNCONVERGED OPERATING POINT',/,
     &22X,'Analysis Point',I5,' of',I5,/,
     &1X,'Prop: ',I2,3X,'Velocity: ',I3,' kts',3X,'RPM: ',I4,4X,
     &'Pwr: ',I4,' hp',4X,'Alt: ',I5,' ft',/,
     &20X,F5.1,' m/s',17X,F8.1,' W ',/)
C
 7570 FORMAT(/,
     &6X,'==== Parametric Analysis Run Complete ====',/,
     &13X, I4,' points analysed',/,
     &13X, I4,' points unconverged',/,
     &13X, 'SAVE <s> to write data to disk')
C
 7580 FORMAT(/,10X,'*** Analysis Run Abandoned ***')
C
 7600 FORMAT(
     &9X,'       Run Abandoned at:',/,
     &9X,'         Velocity = ',I2,/,
     &9X,'         RPM      = ',I2,/,
     &9X,'         Power    = ',I2,/,  
     &9X,'         Altitude = ',I2,//,
     &13X, I4,' points analysed',/,
     &13X, I4,' unconverged points',/)
C
C
      END    !ESPARA
C
C-------------------------------------------------------------------------
C----ESOPUT - Based on OUTPUT by Drela
C    
      SUBROUTINE ESOPUT
     &        (LU,TDIM,EFFTOT,EFFIND,EIDEAL,REFDEG,MACH,STALPC,BETLOC)
C
      INCLUDE 'XROTOR.INC'
      INTEGER BETLOC
C
      TDIM = TTOT*RHO*VEL**2*RAD**2
      QDIM = QTOT*RHO*VEL**2*RAD**3
      PDIM = PTOT *RHO*VEL**3*RAD**2
      EFFTOT = TTOT/PTOT
      RPM = VEL/(RAD*ADV*PI/30.)
      DIA = 2.0*RAD
C
      TNACEL = (TWAK-TINV)*RHO*VEL**2*RAD**2
C
C---- standard coefficients based on forward speed
      TC = TDIM/(0.5*RHO*VEL**2 * PI*RAD**2)
      PC = PDIM/(0.5*RHO*VEL**3 * PI*RAD**2)
C
C---- standard coefficients based on rotational speed
      EN = RPM/60.0
      CT = TDIM/(RHO*EN**2*DIA**4)
      CP = PDIM/(RHO*EN**3*DIA**5)
C
C---- induced efficiency (including nacelle thrust effect)
      EFFIND = TWAK/PWAK
C
C---- ideal (actuator disk) efficiency
      TCLIM = MAX( -1.0 , TC )
      EIDEAL = 2.0 / (1.0 + SQRT(TCLIM + 1.0))
C
C----Reference blade angle
C
      REFDEG = BETA(BETLOC)*180./PI
C
C----Tip Mach Calculation
C
      CALL UVADD(XI(II),WA,WT)
C
        VT = VIND(3,II)
        VA = VIND(1,II)
        UTOT = URDUCT + UBODY(II)
C
        CI = XI(II)/ADV - WT  -  VT
        SI = UTOT       + WA  +  VA
        MACH = SQRT(SI*SI + CI*CI) * VEL/VSO
C
C---- find stalled region-percentage of blade
C
      DO I=1, II
        IF(STALL(I)) THEN
          ISTIN=I
          GO TO 12
        END IF
      ENDDO
C
      STALPC=0.0
      GO TO 30
C
 12   DO I=ISTIN+1, II
        IF(.NOT.STALL(I)) THEN
          ISTOUT = I-1
          GO TO 15
        END IF
      ENDDO
C
      STALPC=100.*(1.- XI(ISTIN))/(1.- XI0)
      GO TO 20
C
 15   STALPC=100.*(((XI(ISTOUT)+XI(ISTOUT+1))/2.)-XI(ISTIN))/
     & (1.- XI0)
C
 20   IF(CL(ISTIN) .LE. 0.) THEN
         STALPC = -1.0 * STALPC
      END IF
C
 30   RETURN
      END  !ESOPUT
C---------------------------------------------------------------------------


C-----------------------------------------
C     Subroutine ESIMP
C     Reads in ESIMP format blade geometry file
C     Based on ARBI by Drela
C-----------------------------------------

      SUBROUTINE ESIMP
      INCLUDE 'XROTOR.INC'
C
      LOGICAL LCOR
      CHARACTER ESNAME*32
      REAL RADMET(30),CHDMET(30)
      INTEGER ESUNIT
C
      ESUNIT=72     ! ESOTEC File IO unit
C
      GREEK = .FALSE.
      CONV = .FALSE.
C
 100  CALL ASKS(' Enter ESIMP geometry filename (A to abort)^',ESNAME)
C
      IF(ESNAME .EQ. 'A' .OR. ESNAME .EQ. 'a') GO TO 900
C
      OPEN(UNIT=ESUNIT,FILE=ESNAME,STATUS='OLD',IOSTAT=KODE)
C
         IF(KODE.NE.0) THEN
            WRITE(*,*)'File cannot be opened'
            GO TO 100
         END IF
C
C----Read in geometry data
C
      READ(ESUNIT,2200,ERR=800) NAME
      READ(ESUNIT,*,ERR=800) RAD,ROOT,NST
C
      IF(NST.LT.3) THEN
         WRITE(*,*) 'Must have 3 or more stations. File not read'
         CLOSE(UNIT=ESUNIT)
         GO TO 100
      END IF
C
      IF(NST.GT.30) THEN
         WRITE(*,*) 'Maximum of 30 stations. File not read'
         CLOSE(UNIT=ESUNIT)
         GO TO 100
      END IF
C
      IF(RAD.LE.ROOT) THEN
         WRITE(*,*) 'Tip radius smaller than hub. File not read'
         CLOSE(UNIT=ESUNIT)
         GO TO 100
      END IF
C
      XI0 = ROOT/RAD
      XW0 = ROOT/RAD
      CALL SETX
      CALL XWINIT
C
      DO 20 N=1, NST 
C  
        READ (ESUNIT,*,ERR=800) RADMET(N),CHDMET(N),W3(N)
C
        IF(RADMET(N).LT.ROOT .OR. RADMET(N).GT.RAD) THEN
         WRITE(*,*) 'Radii must be bounded by hub and tip. Not read'
         GO TO 100
        ENDIF
C
        IF(N.GT.1 .AND. RADMET(N).LE.RADMET(N-1)) THEN
         WRITE(*,*) 'Radii must monotonically increase. File not read'
         GO TO 100
        ENDIF
C
        W1(N)=RADMET(N)/RAD
        W2(N)=CHDMET(N)/RAD
C
   20 CONTINUE
C
      CLOSE(UNIT=ESUNIT)
C
      CALL ASKI(' Enter number of blades ^', NBLDS)
      CALL ASKR(' Enter flight speed (m/s) ^', VEL)
C
C----End of data input
C
   60 DO 70 N=1, NST
        W3(N) = W3(N)*PI/180.
        W1(N) = TINVRT(W1(N))
   70 CONTINUE
C
      CALL SPLINE(W2,W4,W1,NST)
      CALL SPLINE(W3,W5,W1,NST)
C
      DO 90 I=1, II
        CH(I)   = SEVAL(T(I),W2,W4,W1,NST)
        BETA(I) = SEVAL(T(I),W3,W5,W1,NST)
        BETA0(I) = BETA(I)
        CH(I) = ABS(CH(I))                   ! negative chords are a no-no
        UBODY(I) = 0.
   90 CONTINUE
C
C---- estimate reasonable advance ratio to start iterative routines
C
      IS = II/2 + 1
C
C---HHY had to set A0 to 0.0 as A0 is now section property
      A0  = 0.0 
      ANG = BETA(IS) - A0
      ADV = XI(IS)*SIN(ANG)/COS(ANG)
      ADV = MAX(0.1,ADV)
      ADW = ADV
C
      CALL SETIAERO
C
      WRITE(*,2500) NAME
      GO TO 900
C
 800  WRITE(*,*)'File read error. Aborted'
      CLOSE(UNIT=ESUNIT)
C
 900  RETURN
C
C----Format Statements-----------------------------------------------
C
 2200 FORMAT(A30)
 2500 FORMAT(/,
     &1X,43('='),/,
     &' New rotor geometry created from ESIMP file.',/,
     &' Current rotor name is: ',A30,/,
     &1X,43('='))
C
      END ! ESIMP



C------------------------------------------------------------------------
C    SUBROUTINE PARAM
C    Prompt for parameter bounds
C    Read string from keyboard and convert to integers
C    Confirm integers are bounded by 1 and NPAR
C    Confirm second integer .GE. first integer
C    Single integer sets both bounds
C    0 returns integers 1  and NPAR
C    Returns lower bound in IPARL
C    Returns upper bound in IPARU
C------------------------------------------------------------------------
C
      SUBROUTINE PARAM(PROMPT,NPAR,IPARU,IPARL)
C
      CHARACTER PROMPT*132
      CHARACTER ASKARG*132
      LOGICAL ERROR
      DIMENSION IINPUT(20)

 210  CALL ASKS(PROMPT,ASKARG)
C
      DO I=1, 20
        IINPUT(I) = 0
      ENDDO
      NINPUT = 2
      CALL GETINT(ASKARG,IINPUT,NINPUT,ERROR)
C
      IF(ERROR .OR. NINPUT .EQ. 0) THEN
         WRITE(*,*)'Data input error'
         GO TO 210
      END IF
C
      IF(IINPUT(1) .EQ. 0) THEN
         IPARL=1
         IPARU=NPAR
         GO TO 220
      END IF
C
      IF(NINPUT .EQ. 1) THEN
         IPARL=IINPUT(1)
         IPARU=IINPUT(1)
         GO TO 215
      ELSE
         IPARL=IINPUT(1)
         IPARU=IINPUT(2)
         GO TO 215
      END IF
C
 215  IF(IPARL .LT. 1 .OR. IPARU .GT. NPAR) THEN
         WRITE(*,*)'Range outside database parameters'
         GO TO 210
      END IF
C
      IF(IPARU .LT. IPARL) THEN
         WRITE(*,*)'Upper bound smaller than lower bound'
         GO TO 210
      END IF
C
 220  RETURN
      END   ! PARAM



C----------------------------------------------------------------------
C----Subroutine ESPVAL
C----Prompt for and read in parameter values
C----------------------------------------------------------------------
      SUBROUTINE ESPVAL(PROMPT,NVALUE,NDIM,NARRAY,IVALUE)
C
      CHARACTER PROMPT*132
      CHARACTER ASKARG*132
      LOGICAL ERROR
      DIMENSION IVALIN(NVALUE)
      DIMENSION IVALUE(NARRAY)
C
 210  CALL ASKS(PROMPT,ASKARG)
C
      DO I=1, NVALUE
        IVALIN(I) = 0
      ENDDO
      NINPUT = NVALUE
C
      CALL GETINT(ASKARG,IVALIN,NINPUT,ERROR)
C
      IF(ERROR) THEN
         WRITE(*,*)'Data input error'
         GO TO 210
      END IF
C
      IF(NINPUT .GT. NARRAY) THEN
         WRITE(*,*)'Too many values: max = ',NARRAY
         GO TO 210
      END IF
C
      IF(NINPUT .LT. 2) THEN
         WRITE(*,*)'At least 2 parameter values required'
         GO TO 210
      END IF
C
      DO I=2,NINPUT
         IF(IVALIN(I) .LE. IVALIN(I-1)) THEN
           WRITE(*,*)'Values must increase monotonically'
           GO TO 210
         END IF
      ENDDO
C
      NDIM=NINPUT
C
      DO I=1,NDIM
         IVALUE(I)=IVALIN(I)
      ENDDO
C
      RETURN
      END    !ESPVAL
C
C----------------------------------------------------------------------


      SUBROUTINE INITPROP(IPL,IPH)
      INCLUDE 'ESPARA.INC'
C
      DO 2 IPROP=IPL,IPH
         EPROP (IPROP)= 'undefined'
         NPANAL(IPROP)= 0
         NPNOTC(IPROP)= 0
         BLDNUM(IPROP)= 0
         BLDRAD(IPROP)= 0.0 
         BETRAD(IPROP)= 0.0
         BETSTN(IPROP)= 0
C
         DO 2 IALT=1,NALTA
            DO 2 IPWR=1,NPWRA
               DO 2 IRPM=1,NRPMA
                  DO 2 IVEL=1,NVELA
          EANG  (IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EADV  (IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EMACH (IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EEFFID(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          ETHRST(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
          ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)=0.0
 2    CONTINUE
C
      RETURN
      END  !INITPROP
C
C------------------------------------------------------------------------------


      SUBROUTINE INITDB
      INCLUDE 'ESPARA.INC'
C
      DO I=1,NALTA
         EALT(I)=0
      ENDDO
C
      DO I=1,NPWRA
         EPWR(I)=0
      ENDDO
C
      DO I=1,NRPMA
         ERPM(I)=0
      ENDDO
C
      DO I=1,NVELA
         EVEL(I)=0
      ENDDO
C
C----Initialize Accumulated Totals
C
      NPANAC=0    ! Accumulated Analysed Points
      NPNCAC=0    ! Accumulated Unconverged Points
      NPROP =0    ! No propellers initially loaded
C
C----Default analysis bounds
C
      LVELL=1
      LPWRL=1
      LRPML=1
      LALTL=1
      LVELU=1
      LPWRU=1
      LRPMU=1
      LALTU=1
      LPROP=0
C
      EFILE='file not saved' ! Initial ESPROP filename
      PAREAD=.FALSE.  ! True when file parameters have been read in
      FILYES=.FALSE.  ! True when file is available for writing to
      SAVASK=.FALSE.  ! True if data or parameters have not been saved
C
      RETURN
      END  !INITDB
C
C------------------------------------------------------------------------------

      SUBROUTINE DELPROP(IDEL)
      INCLUDE 'ESPARA.INC'
C
      DO 2 IPROP=IDEL,NPROP-1
         EPROP (IPROP)= EPROP (IPROP+1)
         NPANAL(IPROP)= NPANAL(IPROP+1)
         NPNOTC(IPROP)= NPNOTC(IPROP+1)
         BLDNUM(IPROP)= BLDNUM(IPROP+1)
         BLDRAD(IPROP)= BLDRAD(IPROP+1)
         BETRAD(IPROP)= BETRAD(IPROP+1)
         BETSTN(IPROP)= BETSTN(IPROP+1)
C
         DO 2 IALT=1,NALT
            DO 2 IPWR=1,NPWR
               DO 2 IRPM=1,NRPM
                  DO 2 IVEL=1,NVEL
          EANG  (IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    EANG  (IPROP+1,IALT,IPWR,IRPM,IVEL)
          EADV  (IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    EADV  (IPROP+1,IALT,IPWR,IRPM,IVEL)
          EMACH (IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    EMACH (IPROP+1,IALT,IPWR,IRPM,IVEL)
          EEFFID(IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    EEFFID(IPROP+1,IALT,IPWR,IRPM,IVEL)
          EEFFIN(IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    EEFFIN(IPROP+1,IALT,IPWR,IRPM,IVEL)
          EEFFNT(IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    EEFFNT(IPROP+1,IALT,IPWR,IRPM,IVEL)
          ETHRST(IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    ETHRST(IPROP+1,IALT,IPWR,IRPM,IVEL)
          ESTALL(IPROP,  IALT,IPWR,IRPM,IVEL)=
     &    ESTALL(IPROP+1,IALT,IPWR,IRPM,IVEL)
 2    CONTINUE
C
      CALL INITPROP(NPROP,NPROP)
      NPROP = NPROP-1
C
      RETURN
      END  !DELPROP
C
C--------------------------------------------------------------------------



