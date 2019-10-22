C**********************************************************************
C    ESPROP
C    Program for display of numeric and plotted data from ESPROP Databases.
C    Copyright (C) 2001-2011 Philip Carter, Esotec Developments
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
C
      PROGRAM ESPROP
C-------------------------------------------------------------
C     Version 0.8
C     PJC, Esotec Developments, October 2011
C-------------------------------------------------------------
C     NPROPA = Propeller array size
C     NALTA  = Altitude array size
C     NPWRA  = Power array size
C     NRPMA  = RPM array size
C     NVELA  = Velocity array size
C-------------------------------------------------------------
      PARAMETER (NPROPA=12,NALTA=6,NPWRA=12,NRPMA=10,NVELA=15)
C  ***NVELA cannot be smaller than NALTA, NPWRA, or NRPMA!***
C            ***Modify ESPARA arrays accordingly***
C
      PARAMETER (NPLMEN=8)  ! No. of plot types implemented
C
      INTEGER PROPAR(NPROPA),SORTOP(NPROPA)
C
      CHARACTER*32 EPROP(NPROPA),SPROP(NPROPA),PPROP(8)
      CHARACTER*32 EFILE,PAFILE,SVFILE,STFILE
      CHARACTER*8 COMPAR,VERSN
C
      CHARACTER*4 COMAND
      CHARACTER*132 COMARG, PROMPT, ASKARG, LINE
      CHARACTER*1 CHKEY,ANS
C
      DIMENSION IINPUT(20)
      DIMENSION RINPUT(20)
      LOGICAL ERROR
C
      INTEGER EALT(NALTA),EPWR(NPWRA),ERPM(NRPMA),EVEL(NVELA)
      INTEGER SALT(NALTA),SPWR(NPWRA),SRPM(NRPMA),SVEL(NVELA)
      INTEGER ESUNT,OUTLIM,OUTLIX,PLTLIM,PLTLIX,PLTYPE(NPLMEN)
C
      REAL EANG  (NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EADV  (NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EMACH (NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EEFFID(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EEFFIN(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL EEFFNT(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL ETHRST(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
      REAL ESTALL(NPROPA,NALTA,NPWRA,NRPMA,NVELA)
C
      REAL SANG(NPROPA),SSTALL(NPROPA),SMACH(NPROPA),
     $     SADV(NPROPA),SEFFID(NPROPA),SEFFIN(NPROPA),
     $     SEFFNT(NPROPA),STHRST(NPROPA)
C
      REAL PEFFNT(8,NVELA),PTHRST(8,NVELA),PSTALL(8,NVELA),
     $     PVEL(NVELA),PPWR(NPWRA),PRPM(NRPMA),PALT(NALTA)
C
      INTEGER NPANAL(NPROPA),NPNOTC(NPROPA),BLDNUM(NPROPA),
     &        BETSTN(NPROPA)
      REAL BLDRAD(NPROPA),BETRAD(NPROPA)
C
      LOGICAL DBREAD   ! True if database is loaded
      LOGICAL OUTYES   ! True if output parameters are defined
      LOGICAL CHPAR    ! True if output parameters are being changed
      LOGICAL FLSAVE   ! True if output to be saved to disk
      LOGICAL TMWRIT   ! True if output to be written to terminal
      LOGICAL PAPROM   ! True if parameter prompts to be displayed 
      LOGICAL SORT     ! True if listings sorted by efficiency
      LOGICAL PLPROM   ! True to display plotting parameter prompts
      LOGICAL BATCH    ! True if batch plotting mode
      LOGICAL PSFILE   ! True if Postscript saved to disk
C
C----Logical Unit Numbers
C
      LUREAD = 5    ! terminal read
      LUWRIT = 6    ! terminal write
      LUTEMP = 3    ! general-use disk I/O unit
      LUSAVE = 4    ! save file 
      ESUNT  = 72   ! Database File IO Unit
C
C----Conversion Factors
C
      ALTCON= 1./3280.84    ! Feet to Kilometers
      PWRCON= 746.0         ! HP to Watts
      VELCON= 0.51444       ! Knots to m/s
C
C----Initialize
C
      VERSN  = '0.8'        ! Program ESPROP version number
      OUTLIM = 100          ! Limit for lower output warning
      OUTLIX = 1000         ! Limit for higher output warning
      PLTLIM = 25           ! Limit for lower plot warning
      PLTLIX = 100          ! Limit for higher plot warning
      NPTYPE=1              ! Default numher of plot types
      PLTYPE(1)=1           ! Default plot type array
C
      DBREAD = .FALSE.      ! Initialize logical
      OUTYES = .FALSE.
      CHPAR  = .FALSE.
      FLSAVE = .FALSE.
      TMWRIT = .TRUE.
      PAPROM = .TRUE.
      SORT   = .FALSE.
      PLPROM = .FALSE.
      BATCH  = .FALSE.
      PSFILE = .FALSE.
C
      NVELU  = 0            ! Initialize parameter ranges
      NVELL  = 0
      NRPMU  = 0
      NRPML  = 0
      NPWRU  = 0
      NPWRL  = 0
      NALTU  = 0
      NALTL  = 0
      NPRPAR = 0
C
      LU     = LUWRIT       ! Default output to terminal
      RELSIZ = 0.66         ! Relative screen size default
      IDEV   = 1            ! Standard plot output to Xwindow
      IDEVP  = 4            ! Standard plot hardcopy is color
C
      CALL PLINITIALIZE
C
C------------------------------------------------------------------------
C----Display welcome screen and main menu
C
      WRITE(*,6000) VERSN
      WRITE(*,6100)
C
 6100 FORMAT(
     $  /'   OPEN f  Load Esprop database file'
     $  /'   DBAS<d> Display database parameters'
     $  /'   SAVD    Save    database parameters to disk'
     $  /'   PROP<p> Display propeller statistics'
     $  /'   SAVP    Save    propeller statistics to disk'
C
     $ //'   PENT    Enter output parameters (all)'
     $  /'   STAT<s> Display numeric output status'
C
     $ //'   EPRO    Enter new propeller list'
     $  /'   CPRO    Add/remove propellers'
     $  /'   CVEL    Change Velocity points'
     $  /'   CPOW    Change Power      "  '
     $  /'   CRPM    Change Rpm        "  '
     $  /'   CALT    Change Altitude   "  '
C
     $ //'   LPRO    List Propellers at active points'
     $  /'   LVEL    List Velocities       "  '
     $  /'   LPOW    List Powers           "  '
     $  /'   LRPM    List Rpms             "  '
     $  /'   LALT    List Altitudes        "  '
C
     $ //'  .PLOT    Plotting facility'
C
     $ //'   SORT    Sort listings by efficiency toggle'
     $  /'   SAVE    Save listings to disk toggle'
     $  /'   TERM    Write listings to terminal toggle'
     $  /'   PROM    Parameter prompt toggle'
C
     $ //'   QUIT    Exit ESPROP')
C
C----Main Menu
C
 1000 CONTINUE
C
      CALL ASKC(' ESPROP^',COMAND,COMARG)
C---------------------------------------
      IF(COMAND.EQ.'?   ') WRITE(*,6100)
      IF(COMAND.EQ.'?   ') GO TO 1000
C---------------------------------------
      IF(COMAND.EQ.'OPEN') THEN
         EFILE=COMARG
         LU=LUWRIT
         IF(EFILE(1:1).EQ.' ') THEN
            GO TO 100
         ELSE
            GO TO 110
         END IF
      END IF
C--------------------------------------------------
      IF(COMAND.EQ.'DBAS'.OR.COMAND.EQ.'D   ') THEN
         LU=LUWRIT
         GO TO 185
      END IF
C--------------------------------------------------
      IF(COMAND.EQ.'SAVD') THEN
         LU=LUTEMP
         GO TO 185
      END IF
C--------------------------------------------------
      IF(COMAND.EQ.'PROP'.OR.COMAND.EQ.'P   ') THEN
         LU=LUWRIT
         GO TO 150
      END IF
C--------------------------------------------------
      IF(COMAND.EQ.'SAVP') THEN
         LU=LUTEMP
         GO TO 150
      END IF
C--------------------------------------------------
      IF(COMAND.EQ.'PLOT') THEN
         IF(.NOT.DBREAD) THEN
            WRITE(*,6550)
            GO TO 1000
         END IF
         GO TO 400
      END IF
C--------------------------------------------------
      IF(COMAND.EQ.'PENT') THEN
         CHPAR = .FALSE.
         GO TO 200
      END IF
C
      IF(COMAND.EQ.'STAT'.OR.COMAND.EQ.'S   ') THEN
         GO TO 250
      ENDIF
C--------------------------------------------------
      IF(COMAND.EQ.'EPRO') THEN
         CHPAR = .TRUE.
         GO TO 200      
      END IF
C
      IF(COMAND.EQ.'CPRO') THEN
         CHPAR = .TRUE.
         GO TO 200      
      END IF
C
      IF(COMAND.EQ.'CVEL') THEN
         CHPAR = .TRUE.
         GO TO 200      
      END IF
C
      IF(COMAND.EQ.'CRPM') THEN
         CHPAR = .TRUE.
         GO TO 200      
      END IF
C
      IF(COMAND.EQ.'CPOW') THEN
         CHPAR = .TRUE.
         GO TO 200      
      END IF
C
      IF(COMAND.EQ.'CALT') THEN
         CHPAR = .TRUE.
         GO TO 200      
      END IF
C--------------------------------------
      IF(COMAND.EQ.'LPRO') THEN
         COMPAR='PRO'
         GO TO 260      
      END IF
C
      IF(COMAND.EQ.'LVEL') THEN
         COMPAR='VEL'
         GO TO 260
      END IF
C
      IF(COMAND.EQ.'LRPM') THEN
         COMPAR='RPM'
         GO TO 260
      END IF
C
      IF(COMAND.EQ.'LPOW') THEN
         COMPAR='PWR'
         GO TO 260
      END IF
C
      IF(COMAND.EQ.'LALT') THEN
         COMPAR='ALT'
         GO TO 260
      END IF
C----------------------------------
      IF(COMAND.EQ.'SORT') GO TO 10
      IF(COMAND.EQ.'SAVE') GO TO 40
      IF(COMAND.EQ.'TERM') GO TO 50
      IF(COMAND.EQ.'PROM') GO TO 20
C----------------------------------
      IF(COMAND.EQ.'QUIT') THEN
C
         IF(FLSAVE) THEN
            CLOSE(UNIT=LUSAVE)
            WRITE(*,6555) SVFILE
         END IF
C
         CALL PLCLOSE
C
         IF(PSFILE) WRITE(*,6560)
C
         STOP
      END IF
C-----------------------------------------------------------------------
      WRITE(*,*)' Command not recognized - type ? for menu'
      GO TO 1000
C-----------------------------------------------------------------------
C----END COMMANDS
C
C----Toggle Sort
C
 10   CONTINUE
C
      IF(SORT) THEN
         SORT = .FALSE.
         WRITE(*,*)' Output lists will not be sorted'
      ELSE
         SORT = .TRUE.
         WRITE(*,*)' Output lists will be sorted by efficiency'
      END IF
C
      GO TO 1000
C
C----Toggle Parameter Output-------------------------------------------- 
C
 20   CONTINUE
C
      IF(PAPROM) THEN
         PAPROM = .FALSE.
         WRITE(*,2075)
      ELSE
         PAPROM = .TRUE.
         WRITE(*,2080)
      END IF
C
      GO TO 1000
C
C----Toggle File Output-------------------------------------------------
C
 40   CONTINUE
C
      IF(.NOT. FLSAVE) THEN   
 45      CALL ASKS(' Enter output filename <A to abort>^',SVFILE)
C
         IF (SVFILE .EQ. 'A' .OR. SVFILE .EQ. 'a') GO TO 1000
C
         OPEN(UNIT=LUSAVE,FILE=SVFILE,STATUS='NEW',IOSTAT=KODE)
C
         IF(KODE.NE.0) THEN
            WRITE(*,*)' Cannot open file ',SVFILE
            GO TO 45
         END IF
C
         FLSAVE = .TRUE.
         WRITE(*,6540) SVFILE
C
      ELSE
C
         CLOSE(UNIT=LUSAVE)
         FLSAVE = .FALSE.
         WRITE(*,6545) SVFILE
C
         IF(.NOT. TMWRIT)THEN
            TMWRIT = .TRUE.
            WRITE(*,*)' Terminal output activated'
         END IF
C
         WRITE(*,6520)
C
      END IF
C
      GO TO 1000
C
C----Toggle Terminal Output-------------------------------------------
C
 50   CONTINUE
C
      IF(TMWRIT .AND. .NOT. FLSAVE) THEN
         WRITE(*,6510)
         GO TO 1000
      END IF
C
      IF(TMWRIT .AND. FLSAVE) THEN
         TMWRIT = .FALSE.
         WRITE(*,6530) SVFILE
      ELSE
         TMWRIT = .TRUE.
         WRITE(*,6540) SVFILE
      END IF
C
      GO TO 1000   
C
C
C------------------------------------------------------------------------
C----Load ESPROP Database File
C-------------------------------------------------------------------------
C
 100  CONTINUE
C
      CALL ASKS(' Enter ESPROP database filename <A to abort> ^',EFILE)
      IF (EFILE .EQ. 'A' .OR. EFILE .EQ. 'a') GO TO 1000
C
 110  OPEN(UNIT=ESUNT,FILE=EFILE,STATUS='OLD',IOSTAT=KODE)
C
      IF(KODE.NE.0) THEN
         WRITE(*,*) ' Cannot open file ',EFILE
         GO TO 100
      END IF
C
C----Read data into arrays
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
      READ(ESUNT,7010,ERR=900)NPROP,NALT,NPWR,NRPM,NVEL
C
      IF(NPROP .GT. NPROPA .OR. NALT .GT. NALTA .OR. NPWR .GT. NPWRA
     $ .OR. NRPM .GT. NRPMA .OR. NVEL .GT. NVELA) THEN
         WRITE(*,*)' Too many operating points -enlarge arrays'
         WRITE(*,*)' File read aborted'
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
         READ(ESUNT,7035,ERR=900)
     $ BLDNUM(I),BLDRAD(I),BETRAD(I),BETSTN(I),NPANAL(I),NPNOTC(I)
      ENDDO
C 
      DO 111 IPROP=1,NPROP
         DO 111 IALT=1,NALT
            DO 111 IPWR=1,NPWR
               DO 111 IRPM=1,NRPM
                  DO 111 IVEL=1,NVEL
                     READ(ESUNT,7040,ERR=900)
     $               EANG  (IPROP,IALT,IPWR,IRPM,IVEL),
     $               EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL),
     $               EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL),
     $               EEFFID(IPROP,IALT,IPWR,IRPM,IVEL),
     $               ETHRST(IPROP,IALT,IPWR,IRPM,IVEL),
     $               EADV  (IPROP,IALT,IPWR,IRPM,IVEL),
     $               EMACH (IPROP,IALT,IPWR,IRPM,IVEL),
     $               ESTALL(IPROP,IALT,IPWR,IRPM,IVEL)
C
 111  CONTINUE
C
      CLOSE(ESUNT)
C
      DBREAD = .TRUE.
      OUTYES = .FALSE.
C
C----Calculate totals
C
      NPANAC=0    ! No. of analysed points accumulated
      NPNCAC=0    ! No. of unconverged points accumulated
      DO I=1,NPROP
         NPANAC=NPANAC+NPANAL(I)
         NPNCAC=NPNCAC+NPNOTC(I)
      ENDDO
C
      NPPRO =NALT*NPWR*NRPM*NVEL
      NPNTOT=NPROP*NPPRO          ! Total points in database
      NPCONV=NPANAC-NPNCAC        ! Total converged points in database
C
C----Display Database Parameters
C
      GO TO 195
C
C--------------------------------------------------------------------------
C----Write Propeller Summary to terminal or disk
C
 150  IF(.NOT. DBREAD) THEN
         WRITE(*,*)' Database not loaded'
         GO TO 1000
      END IF
C
C----Open file if save to disk
C
      IF(LU .EQ. LUTEMP) THEN
C
 155     CALL ASKS(' Enter summary filename <A to abort>^',STFILE)
C  
         IF (STFILE .EQ. 'A' .OR. STFILE .EQ. 'a') THEN
            GO TO 1000
         END IF
C
         OPEN(UNIT=LU,FILE=STFILE,STATUS='NEW',IOSTAT=KODE)
C
         IF(KODE.NE.0) THEN
            WRITE(*,*)' Cannot open file ',STFILE
            GO TO 155
         END IF
      END IF 
C
C----Write stats
C
      WRITE(LU,7400) EFILE
      DO I=1,NPROP
         WRITE(LU,7420)I,EPROP(I),BLDNUM(I),BLDRAD(I),
     &        BETSTN(I),BETRAD(I),NPANAL(I),NPNOTC(I)
      ENDDO
      WRITE(LU,7430)
C
      IF(LU .EQ. LUTEMP) THEN
         CLOSE(UNIT=LU)
         WRITE(*,*)' Summary saved to file: ',STFILE
         LU = LUWRIT
      END IF     
C
      GO TO 1000
C
C--------------------------------------------------------------------------
C----Write Database Parameters to terminal or disk
C
 185  IF(.NOT. DBREAD) THEN
         WRITE(*,*)' Database not loaded'
         GO TO 1000
      END IF
C
C----Open file if save to disk
C
      IF(LU .EQ. LUTEMP) THEN
C
 190     CALL ASKS(' Enter parameter filename <A to abort>^',PAFILE)
C  
         IF (PAFILE .EQ. 'A' .OR. PAFILE .EQ. 'a') THEN
            GO TO 1000
         END IF
C
         OPEN(UNIT=LU,FILE=PAFILE,STATUS='NEW',IOSTAT=KODE)
C
         IF(KODE.NE.0) THEN
            WRITE(*,*)' Cannot open file ',PAFILE
            GO TO 190
         END IF
      END IF 
C
C----Write out database parameters
C
 195  CONTINUE
C
      WRITE(LU,7100,ERR=910)EFILE,NPROP,NVEL,NPWR,NRPM,NALT,
     $NPPRO,NPNTOT,NPANAC,NPCONV,NPNCAC
C
      WRITE(LU,7110,ERR=910)
      DO I=1,NPROP
         WRITE(LU,7140,ERR=910)I,EPROP(I)
      ENDDO
C
      WRITE(LU,7130,ERR=910)
      DO I=1,NVEL
         WRITE(LU,7150,ERR=910)I,EVEL(I)
      ENDDO
C
      WRITE(LU,7120,ERR=910)
      DO I=1,NPWR
         WRITE(LU,7150,ERR=910)I,EPWR(I)
      ENDDO
C
      WRITE(LU,7125,ERR=910)
      DO I=1,NRPM
         WRITE(LU,7150,ERR=910)I,ERPM(I)
      ENDDO
C
      WRITE(LU,7115,ERR=910)
      DO I=1,NALT
         WRITE(LU,7150,ERR=910)I,EALT(I)
      ENDDO
C
      WRITE(LU,7135)
C
      IF(LU .EQ. LUTEMP) THEN
         CLOSE(UNIT=LU)
         WRITE(*,*)' Database Parameters saved to file: ',PAFILE
         LU = LUWRIT
      END IF     
C
      GO TO 1000
C
C-------------------------------------------------------------------------
C----Enter or change Output Parameters
C
 200  CONTINUE
C
      IF(.NOT.DBREAD) THEN
         WRITE(*,*)' Database not loaded'
         GO TO 1000
      END IF
C
      IF(CHPAR .AND. .NOT.OUTYES) THEN
         WRITE(*,*)' Output Parameters not entered (use PENT)'
         GO TO 1000
      END IF
C
      IF(COMAND.EQ.'EPRO') GO TO 205
      IF(COMAND.EQ.'CVEL') GO TO 210
      IF(COMAND.EQ.'CRPM') GO TO 215
      IF(COMAND.EQ.'CPOW') GO TO 220
      IF(COMAND.EQ.'CALT') GO TO 225
      IF(COMAND.EQ.'CPRO') GO TO 230
C
C
 205  CALL PROPEN(NPROPA,NPROP,EPROP,PAPROM,PLPROM,PROPAR,NPRPAR)      
      IF(CHPAR) GO TO 250
C
 210  CALL VELEN(NVELA,NVEL,EVEL,PAPROM,NVELU,NVELL)
      IF(CHPAR) GO TO 250
C
 215  CALL RPMEN(NRPMA,NRPM,ERPM,PAPROM,NRPMU,NRPML)
      IF(CHPAR) GO TO 250
C
 220  CALL PWREN(NPWRA,NPWR,EPWR,PAPROM,NPWRU,NPWRL)
      IF(CHPAR) GO TO 250
C
 225  CALL ALTEN(NALTA,NALT,EALT,PAPROM,NALTU,NALTL)
      IF(CHPAR) GO TO 250
C
      OUTYES = .TRUE.
      GO TO 250
C
 230  CALL PROPCH(NPROPA,NPROP,EPROP,PAPROM,PLPROM,PROPAR,NPRPAR)
      GO TO 250
C
C
C----Display Output Parameters--------------------------------------------
C
 250  CONTINUE
C
      IF(.NOT.DBREAD) THEN
         WRITE(*,*)' Database not loaded'
         GO TO 1000
      END IF
C
      IF(.NOT.OUTYES) THEN
         WRITE(*,*)' Output Parameters not defined (use PENT)'
         GO TO 1000
      END IF
C
      NOVEL=NVELU-NVELL+1
      NOPWR=NPWRU-NPWRL+1
      NORPM=NRPMU-NRPML+1
      NOALT=NALTU-NALTL+1
C
      NOUT = NPRPAR*NOVEL*NOPWR*NORPM*NOALT
C
      WRITE(*,7300)
C
      DO I=1,NPRPAR
         WRITE(*,7310)PROPAR(I),EPROP(PROPAR(I))
      END DO
C
      WRITE(*,7320)NVELL,NVELU, EVEL(NVELL),EVEL(NVELU),NOVEL,
     $             NPWRL,NPWRU, EPWR(NPWRL),EPWR(NPWRU),NOPWR,
     $             NRPML,NRPMU, ERPM(NRPML),ERPM(NRPMU),NORPM,
     $             NALTL,NALTU, EALT(NALTL),EALT(NALTU),NOALT,
     $             NOUT
C
      IF(TMWRIT .AND. .NOT. FLSAVE) THEN
         WRITE(*,*)' Listings will be written to terminal only'
      ELSE IF(FLSAVE .AND. .NOT. TMWRIT) THEN
         WRITE(*,*)' Listings will be written to disk file only'
      ELSE IF(FLSAVE .AND. TMWRIT) THEN
         WRITE(*,*)' Listings will be written to both terminal and disk'
      END IF
C
      IF(SORT) THEN
         WRITE(*,*)' Listings will be sorted'
      ELSE
         WRITE(*,*)' Listings will not be sorted'
      END IF
C
      GO TO 1000
C
C-------------------------------------------------------------------------
C----Output Management and Filters
C
 260  CONTINUE
C
      IF(.NOT.DBREAD) THEN
         WRITE(*,*)' Database not loaded'
         GO TO 1000
      END IF
C
      IF(.NOT.OUTYES) THEN
         WRITE(*,*)' Output Parameters not defined (use PENT)'
         GO TO 1000
      END IF
C
C----Lower limit warning
C
      IF(NOUT .GT. OUTLIM .AND. NOUT .LT. OUTLIX) THEN
         IF(TMWRIT .AND. .NOT. FLSAVE) THEN
            WRITE(*,2010) NOUT
            GO TO 265
         ELSE IF(FLSAVE .AND. .NOT. TMWRIT) THEN
            WRITE(*,2020) NOUT
            GO TO 265
         ELSE IF(FLSAVE .AND. TMWRIT) THEN
            WRITE(*,2030) NOUT
            GO TO 265
         END IF         
      END IF
C
C----Higher Limit Warning
C
      IF(NOUT .GE. OUTLIX) THEN
         IF(TMWRIT .AND. .NOT. FLSAVE) THEN
            WRITE(*,2040) NOUT
            GO TO 265
         ELSE IF(FLSAVE .AND. .NOT. TMWRIT) THEN
            WRITE(*,2050) NOUT
            GO TO 265
         ELSE IF(FLSAVE .AND. TMWRIT) THEN
            WRITE(*,2060) NOUT
            GO TO 265
         END IF         
      END IF
C
      GO TO 270      ! NOUT is under output limits
C
C----Read Warning Response
C   
 265  READ(*,7000) ANS
      IF(INDEX('Nn',ANS).NE.0) THEN
         WRITE(*,*)' Numeric Output Aborted'
         GO TO 1000
      END IF
C
C
C----Manage Logical Units for Terminal Write and File Write--------------
C
 270  CONTINUE
      K=0         ! Initialize counter for write loops
C
 275  CONTINUE    ! Output Routines return here
      K=K+1       ! Increment counter
C
      IF(TMWRIT .AND. .NOT. FLSAVE) THEN
         IF(K .EQ. 1) THEN
            LU = LUWRIT
            GO TO 280
         ELSE
            GO TO 1000
         END IF
      END IF
C
      IF(FLSAVE .AND. .NOT. TMWRIT) THEN
         IF(K .EQ. 1) THEN
            LU=LUSAVE
            GO TO 280
         ELSE
            LU=LUWRIT
            WRITE(*,2070) SVFILE
            GO TO 1000
         END IF
      END IF
C
      IF(FLSAVE .AND. TMWRIT) THEN
         IF(K .EQ. 1) THEN
            LU = LUWRIT
            GO TO 280
         ELSE IF(K .EQ. 2) THEN
            LU = LUSAVE
            GO TO 280
         ELSE
            LU = LUWRIT
            WRITE(*,2070) SVFILE
            GO TO 1000
         END IF
      END IF
C
C----Direct to output routine-----------------------------------
C
 280  CONTINUE 
C
      IF(COMPAR .EQ. 'PRO')GO TO 310
      IF(COMPAR .EQ. 'VEL')GO TO 320
      IF(COMPAR .EQ. 'PWR')GO TO 330
      IF(COMPAR .EQ. 'RPM')GO TO 340
      IF(COMPAR .EQ. 'ALT')GO TO 350
C
C
C------------------------------------------------------------------------
C----Numeric Output Routines
C------------------------------------------------------------------------
C----Compare Propellers
C
 310  CONTINUE
C
      DO 319 IVEL=NVELL,NVELU
         DO 319 IPWR=NPWRL,NPWRU
            DO 319 IRPM=NRPML,NRPMU
               DO 319 IALT=NALTL,NALTU
C
      DO 315 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
C
         SPROP (JPROP) = EPROP (IPROP)
         SANG  (JPROP) = EANG  (IPROP,IALT,IPWR,IRPM,IVEL)
         SSTALL(JPROP) = ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) 
         SMACH (JPROP) = EMACH (IPROP,IALT,IPWR,IRPM,IVEL) 
         SADV  (JPROP) = EADV  (IPROP,IALT,IPWR,IRPM,IVEL)
         SEFFID(JPROP) = EEFFID(IPROP,IALT,IPWR,IRPM,IVEL)
         SEFFIN(JPROP) = EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL)
         SEFFNT(JPROP) = EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL)
         STHRST(JPROP) = ETHRST(IPROP,IALT,IPWR,IRPM,IVEL)
C
 315  CONTINUE
C
C----Sort list according to effy (or not)
C
      IF (SORT) THEN
         CALL ESORT(NPROPA,SEFFNT,NPRPAR,SORTOP)
      ELSE
         DO I=1,NPRPAR
            SORTOP(I)=I
         ENDDO
      END IF
C
C----Write Header
C
      IF(SORT)      WRITE(LU,3001,ERR=910)
      IF(.NOT.SORT) WRITE(LU,3002,ERR=910)
C
      WRITE(LU,3010,ERR=910)EVEL(IVEL),EPWR(IPWR),
     $                      ERPM(IRPM),EALT(IALT)
C   
C----Write listings in order of SORTOP
C        
      DO 317 I=1,NPRPAR
         N=SORTOP(I) 
C
         IF(SSTALL(N) .EQ. -99.9) THEN
            WRITE(LU,3012,ERR=910)SPROP(N)
            GO TO 317
         END IF
C
         WRITE(LU,3014,ERR=910)SPROP(N),SANG(N),SSTALL(N),SMACH(N),
     $             SADV(N),SEFFID(N),SEFFIN(N),SEFFNT(N),STHRST(N)
C
 317  CONTINUE
 319  CONTINUE   
      GO TO 275
C
C----Compare Velocities-------------------------------------------------
C
 320  CONTINUE
C
      DO 329 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
         DO 329 IPWR=NPWRL,NPWRU
            DO 329 IRPM=NRPML,NRPMU
               DO 329 IALT=NALTL,NALTU
C
      DO 325 IVEL=NVELL,NVELU
             JVEL=IVEL-NVELL+1
C
         SVEL  (JVEL) =  EVEL  (IVEL)
         SANG  (JVEL) =  EANG  (IPROP,IALT,IPWR,IRPM,IVEL)
         SSTALL(JVEL) =  ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) 
         SMACH (JVEL) =  EMACH (IPROP,IALT,IPWR,IRPM,IVEL) 
         SADV  (JVEL) =  EADV  (IPROP,IALT,IPWR,IRPM,IVEL) 
         SEFFID(JVEL) =  EEFFID(IPROP,IALT,IPWR,IRPM,IVEL)
         SEFFIN(JVEL) =  EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL)
         SEFFNT(JVEL) =  EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL)
         STHRST(JVEL) =  ETHRST(IPROP,IALT,IPWR,IRPM,IVEL)
C
 325  CONTINUE
C
C----Sort list according to effy (or not)
C
      IF (SORT) THEN
         CALL ESORT(NPROPA,SEFFNT,NOVEL,SORTOP)
      ELSE
         DO I=1,NOVEL
            SORTOP(I)=I
         ENDDO
      END IF
C
C----Write Header
C
      IF(SORT)      WRITE(LU,3001,ERR=910)
      IF(.NOT.SORT) WRITE(LU,3002,ERR=910)
C
      WRITE(LU,3020,ERR=910)EPROP(IPROP),EPWR(IPWR),
     $                      ERPM(IRPM),EALT(IALT)
C
C----Write listings in order of SORTOP
C
      DO 327 I=1,NOVEL
         N=SORTOP(I)
C
         IF(SSTALL(N) .EQ. -99.9) THEN
            WRITE(LU,3022,ERR=910) SVEL(N)
            GO TO 327
         END IF

         WRITE(LU,3024,ERR=910)SVEL(N),SANG(N),SSTALL(N),SMACH(N),
     $              SADV(N),SEFFID(N),SEFFIN(N),SEFFNT(N),STHRST(N)
C
 327  CONTINUE
 329  CONTINUE
      GO TO 275
C
C----Compare Power Settings---------------------------------------------
C
 330  CONTINUE
C
      DO 339 JPROP=1,NPRPAR
             IPROP=PROPAR(JPROP)
         DO 339 IVEL=NVELL,NVELU
            DO 339 IRPM=NRPML,NRPMU
               DO 339 IALT=NALTL,NALTU
C
      DO 335 IPWR=NPWRL,NPWRU
         JPWR=IPWR-NPWRL+1
C
         SPWR  (JPWR)  =  EPWR  (IPWR)
         SANG  (JPWR)  =  EANG  (IPROP,IALT,IPWR,IRPM,IVEL)
         SSTALL(JPWR)  =  ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) 
         SMACH (JPWR)  =  EMACH (IPROP,IALT,IPWR,IRPM,IVEL) 
         SADV  (JPWR)  =  EADV  (IPROP,IALT,IPWR,IRPM,IVEL) 
         SEFFID(JPWR)  =  EEFFID(IPROP,IALT,IPWR,IRPM,IVEL)
         SEFFIN(JPWR)  =  EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL)
         SEFFNT(JPWR)  =  EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL)
         STHRST(JPWR)  =  ETHRST(IPROP,IALT,IPWR,IRPM,IVEL)
C
 335     CONTINUE
C
C----Sort list according to effy
C
      IF (SORT) THEN
         CALL ESORT(NPROPA,SEFFNT,NOPWR,SORTOP)
      ELSE
         DO I=1,NOPWR
            SORTOP(I)=I
         ENDDO
      END IF
C
C----Write Header
C
      IF(SORT)      WRITE(LU,3001,ERR=910)
      IF(.NOT.SORT) WRITE(LU,3002,ERR=910)
C
      WRITE(LU,3030,ERR=910)EPROP(IPROP),EVEL(IVEL),
     $                      ERPM(IRPM),  EALT(IALT)
C
C----Write listings in order of SORTOP
C
      DO 337 I=1,NOPWR
         N=SORTOP(I)

      IF(SSTALL(N) .EQ. -99.9) THEN
         WRITE(LU,3032,ERR=910) SPWR(N)
         GO TO 337
      END IF

      WRITE(LU,3034,ERR=910)SPWR(N),SANG(N),SSTALL(N),SMACH(N),
     $         SADV(N),SEFFID(N),SEFFIN(N),SEFFNT(N),STHRST(N)
C
 337  CONTINUE
 339  CONTINUE
      GO TO 275
C
C
C----Compare RPM Settings---------------------------------------------
C
 340  CONTINUE
C
      DO 349 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
         DO 349 IVEL=NVELL,NVELU
            DO 349 IPWR=NPWRL,NPWRU
               DO 349 IALT=NALTL,NALTU
C
         DO 345 IRPM=NRPML,NRPMU
           JRPM=IRPM-NRPML+1
C
           SRPM  (JRPM) =  ERPM  (IRPM)
           SANG  (JRPM) =  EANG  (IPROP,IALT,IPWR,IRPM,IVEL)
           SSTALL(JRPM) =  ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) 
           SMACH (JRPM) =  EMACH (IPROP,IALT,IPWR,IRPM,IVEL) 
           SADV  (JRPM) =  EADV  (IPROP,IALT,IPWR,IRPM,IVEL) 
           SEFFID(JRPM) =  EEFFID(IPROP,IALT,IPWR,IRPM,IVEL)
           SEFFIN(JRPM) =  EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL)
           SEFFNT(JRPM) =  EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL)
           STHRST(JRPM) =  ETHRST(IPROP,IALT,IPWR,IRPM,IVEL)
C
 345  CONTINUE
C
C----Sort list according to effy
C
      IF (SORT) THEN
         CALL ESORT(NPROPA,SEFFNT,NORPM,SORTOP)
      ELSE
         DO I=1,NORPM
            SORTOP(I)=I
         ENDDO
      END IF
C
C----Write Header
C
      IF(SORT)      WRITE(LU,3001,ERR=910)
      IF(.NOT.SORT) WRITE(LU,3002,ERR=910)
C
      WRITE(LU,3040,ERR=910)EPROP(IPROP),EVEL(IVEL),
     $                      EPWR(IPWR),  EALT(IALT) 
C
C----Write listings in order of SORTOP
C
      DO 347 I=1,NORPM
         N=SORTOP(I)
C
         IF(SSTALL(N) .EQ. -99.9) THEN
            WRITE(LU,3042,ERR=910) SRPM(N)
            GO TO 349
         END IF

         WRITE(LU,3044,ERR=910)SRPM(N),SANG(N),SSTALL(N),SMACH(N),
     $            SADV(N),SEFFID(N),SEFFIN(N),SEFFNT(N),STHRST(N)

 347  CONTINUE
 349  CONTINUE
      GO TO 275
C
C----Compare ALTITUDES-------------------------------------------------
C
 350  CONTINUE
C
      DO 359 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
         DO 359 IVEL=NVELL,NVELU
            DO 359 IPWR=NPWRL,NPWRU
               DO 359 IRPM=NRPML,NRPMU

         DO 355 IALT=NALTL,NALTU
            JALT=IALT-NALTL+1
C
            SALT  (JALT) =  EALT  (IALT)
            SANG  (JALT) =  EANG  (IPROP,IALT,IPWR,IRPM,IVEL)
            SSTALL(JALT) =  ESTALL(IPROP,IALT,IPWR,IRPM,IVEL) 
            SMACH (JALT) =  EMACH (IPROP,IALT,IPWR,IRPM,IVEL) 
            SADV  (JALT) =  EADV  (IPROP,IALT,IPWR,IRPM,IVEL) 
            SEFFID(JALT) =  EEFFID(IPROP,IALT,IPWR,IRPM,IVEL)
            SEFFIN(JALT) =  EEFFIN(IPROP,IALT,IPWR,IRPM,IVEL)
            SEFFNT(JALT) =  EEFFNT(IPROP,IALT,IPWR,IRPM,IVEL)
            STHRST(JALT) =  ETHRST(IPROP,IALT,IPWR,IRPM,IVEL)
C
 355  CONTINUE
C
C----Sort list according to effy
C
      IF (SORT) THEN
         CALL ESORT(NPROPA,SEFFNT,NOALT,SORTOP)
      ELSE
         DO I=1,NOALT
            SORTOP(I)=I
         ENDDO
      END IF
C
C----Write Header
C
      IF(SORT)      WRITE(LU,3001,ERR=910)
      IF(.NOT.SORT) WRITE(LU,3002,ERR=910)
C
      WRITE(LU,3050,ERR=910)EPROP(IPROP),EVEL(IVEL),
     $                      EPWR(IPWR),  ERPM(IRPM)
C
C----Write listings in order of SORTOP
C
      DO 357 I=1,NOALT
         N=SORTOP(I)
C
         IF(SSTALL(N) .EQ. -99.9) THEN
            WRITE(LU,3052,ERR=910) SALT(N)
            GO TO 359
         END IF

         WRITE(LU,3054,ERR=910)SALT(N),SANG(N),SSTALL(N),SMACH(N),
     $            SADV(N),SEFFID(N),SEFFIN(N),SEFFNT(N),STHRST(N)

 357  CONTINUE
 359  CONTINUE
      GO TO 275
C
C------------------------------------------------------------------------
C----End of numeric output routines
C----PLOT Routines begin
C------------------------------------------------------------------------
C----Convert Parameter list and prompting
C
 400  CONTINUE    
C
      PLPROM = .TRUE.
C
      IF(OUTYES) THEN
         IF(NPRPAR .GT. 8) THEN
            WRITE(*,4250)
            DO I=9,NPRPAR
               WRITE(*,4260) EPROP(PROPAR(I))
            ENDDO
            NPRPAR=8
         END IF
      END IF
C
      IF(OUTYES) GO TO 430
C
C----Return to plot menu here--------------------------------------------
C
 410  CONTINUE
C
      CALL ASKC('.PLOT ^',COMAND,COMARG)
C----------------------------------------
      IF(COMAND.EQ.'?   ') WRITE(*,4100)
      IF(COMAND.EQ.'?   ') GO TO 410
C----------------------------------------
      IF(COMAND.EQ.'    ') THEN
         PLPROM = .FALSE.
         GO TO 1000
      END IF
C----------------------------------------
      IF(COMAND.EQ.'PENT') THEN
         CHPAR = .FALSE.
         GO TO 420
      END IF
C
      IF(COMAND.EQ.'STAT') GO TO 430
C----------------------------------------   
      IF(COMAND.EQ.'EPRO') THEN
         CHPAR = .TRUE.
         GO TO 420
      END IF
C
      IF(COMAND.EQ.'CPRO') THEN
         CHPAR = .TRUE.
         GO TO 420
      END IF
C     
      IF(COMAND.EQ.'CVEL') THEN
         CHPAR = .TRUE.
         GO TO 420
      END IF
C     
      IF(COMAND.EQ.'CPOW') THEN
         CHPAR = .TRUE.
         GO TO 420
      END IF
C
      IF(COMAND.EQ.'CRPM') THEN
         CHPAR = .TRUE.
         GO TO 420
      END IF
C
      IF(COMAND.EQ.'CALT') THEN
         CHPAR = .TRUE.
         GO TO 420
      END IF
C---------------------------------------- 
      IF(COMAND.EQ.'SELP') GO TO 440
      IF(COMAND.EQ.'EXEC') GO TO 450
C----------------------------------------
      IF(COMAND.EQ.'BATC') GO TO 412
      IF(COMAND.EQ.'COLO') GO TO 416
      IF(COMAND.EQ.'SIZE') GO TO 418      
C----------------------------------------
C
      WRITE(*,*)' Type ? for menu - <ret> to exit plotting'
      GO TO 410
C
C-----------------------------------------------------------------------
C----Toggle batch mode
C
 412  CONTINUE
      IF(BATCH) THEN
         BATCH = .FALSE.
         IDEV  = 1
         WRITE(*,*)' Interactive Plotting selected'
      ELSE
         BATCH = .TRUE.
         IDEV  = IDEVP
         WRITE(*,*)' Batch Plotting selected'
      END IF
C
      GO TO 410
C
C-----------------------------------------------------------------------
C----Plot color toggle
C
 416  CONTINUE
      IF(IDEVP .EQ. 4) THEN
         IDEVP = 2
         WRITE(*,*)' Black & White hardcopy selected'
      ELSE
         IDEVP = 4
         WRITE(*,*)' Color hardcopy selected'
      END IF
C
      IF(BATCH) IDEV = IDEVP
C
      GO TO 410
C
C-----------------------------------------------------------------------
C----Plot screen size
C
 418  CONTINUE
      WRITE(*,4270) RELSIZ
      CALL ASKR(' Enter new plot window size ^',RELSIZ)
      GO TO 410
C
C-----------------------------------------------------------------------
C----Enter or change Plot Parameters
C
 420  CONTINUE
C
      IF(CHPAR .AND. .NOT.OUTYES)THEN
         WRITE(*,*)' Plot Parameters not defined (use PENT)'
         GO TO 410
      END IF
C
      IF(COMAND.EQ.'EPRO') GO TO 422
      IF(COMAND.EQ.'CVEL') GO TO 423
      IF(COMAND.EQ.'CPOW') GO TO 424
      IF(COMAND.EQ.'CRPM') GO TO 425
      IF(COMAND.EQ.'CALT') GO TO 426
      IF(COMAND.EQ.'CPRO') GO TO 427
C
C
 422  CALL PROPEN(NPROPA,NPROP,EPROP,PAPROM,PLPROM,PROPAR,NPRPAR)      
      IF(CHPAR) GO TO 430
C
 423  CALL VELEN(NVELA,NVEL,EVEL,PAPROM,NVELU,NVELL)
      IF(CHPAR) GO TO 430
C
 424  CALL PWREN(NPWRA,NPWR,EPWR,PAPROM,NPWRU,NPWRL)
      IF(CHPAR) GO TO 430
C
 425  CALL RPMEN(NRPMA,NRPM,ERPM,PAPROM,NRPMU,NRPML)
      IF(CHPAR) GO TO 430
C
 426  CALL ALTEN(NALTA,NALT,EALT,PAPROM,NALTU,NALTL)
      IF(CHPAR) GO TO 430
C
      OUTYES = .TRUE.
      GO TO 430
C
 427  CALL PROPCH(NPROPA,NPROP,EPROP,PAPROM,PLPROM,PROPAR,NPRPAR) 
      GO TO 430
C
C-----------------------------------------------------------------------
C----Write plotting parameters to terminal
C
 430  CONTINUE
C
      IF(.NOT. OUTYES) THEN
         WRITE(*,*)' Plot Parameters not defined (use PENT)'
         GO TO 410
      END IF
C
C----Calculate total points to be plotted
C
      NOVEL=NVELU-NVELL+1
      NOPWR=NPWRU-NPWRL+1
      NORPM=NRPMU-NRPML+1
      NOALT=NALTU-NALTL+1
C
      NOUT = NPRPAR*NOVEL*NOPWR*NORPM*NOALT
C
C----Calculate total plots to be plotted
C
      NPLEV=0
      NPLEP=0
      NPLER=0
      NPLEA=0
      NPLTV=0
      NPLTP=0
      NPLTR=0
      NPLTA=0
C
      DO I=1,NPTYPE
         IF(PLTYPE(I).EQ.1) NPLEV = NOPWR*NORPM*NOALT
         IF(PLTYPE(I).EQ.2) NPLEP = NOVEL*NORPM*NOALT
         IF(PLTYPE(I).EQ.3) NPLER = NOVEL*NOPWR*NOALT
         IF(PLTYPE(I).EQ.4) NPLEA = NOVEL*NOPWR*NORPM
C
         IF(PLTYPE(I).EQ.5) NPLTV = NOPWR*NORPM*NOALT
         IF(PLTYPE(I).EQ.6) NPLTP = NOVEL*NORPM*NOALT
         IF(PLTYPE(I).EQ.7) NPLTR = NOVEL*NOPWR*NOALT
         IF(PLTYPE(I).EQ.8) NPLTA = NOVEL*NOPWR*NORPM
      ENDDO
C
      NPLTOT = NPLEV+NPLEP+NPLER+NPLEA+NPLTV+NPLTP+NPLTR+NPLTA
C
C----Write Plot Output Status
C
      WRITE(*,4300)
C
      DO I=1,NPRPAR
         WRITE(*,4310)PROPAR(I),EPROP(PROPAR(I))
      END DO
C
      WRITE(*,4320)NVELL,NVELU, EVEL(NVELL),EVEL(NVELU),NOVEL,
     $             NPWRL,NPWRU, EPWR(NPWRL),EPWR(NPWRU),NOPWR,
     $             NRPML,NRPMU, ERPM(NRPML),ERPM(NRPMU),NORPM,
     $             NALTL,NALTU, EALT(NALTL),EALT(NALTU),NOALT,
     $             NOUT
C
      DO I=1,NPTYPE
         IF(PLTYPE(I).EQ.1) WRITE(*,4331) NPLEV
         IF(PLTYPE(I).EQ.2) WRITE(*,4332) NPLEP
         IF(PLTYPE(I).EQ.3) WRITE(*,4333) NPLER
         IF(PLTYPE(I).EQ.4) WRITE(*,4334) NPLEA
         IF(PLTYPE(I).EQ.5) WRITE(*,4335) NPLTV
         IF(PLTYPE(I).EQ.6) WRITE(*,4336) NPLTP
         IF(PLTYPE(I).EQ.7) WRITE(*,4337) NPLTR
         IF(PLTYPE(I).EQ.8) WRITE(*,4338) NPLTA
      ENDDO
C
      WRITE(*,4340) NPLTOT
C
      IF(BATCH) THEN
         WRITE(*,4351)
      ELSE
         WRITE(*,4352)
      END IF
C
      IF(IDEVP .EQ. 4) THEN
         WRITE(*,4353)
      ELSE
         WRITE(*,4354)
      END IF
C
      GO TO 410
C
C-----------------------------------------------------------------------
C----Select plot types
C
 440  IF(.NOT. OUTYES) THEN
         WRITE(*,*)' Plot Parameters not defined (use PENT)'
         GO TO 410
      END IF

      IF(PAPROM) THEN
         WRITE(*,4280)
      END IF

 442  CALL ASKS(' Enter plot types <0 for all> ^',ASKARG)
C
      DO I=1, 20
        IINPUT(I) = 0
      ENDDO
      NINPUT = 20
C
      CALL GETINT(ASKARG,IINPUT,NINPUT,ERROR)
C
      IF(ERROR) THEN
         WRITE(*,*)' Data input error'
         GO TO 442
      END IF
C
      IF(NINPUT .EQ. 0) GO TO 410
C
      IF(IINPUT(1) .EQ. 0) THEN
         NPTYPE=NPLMEN
         DO J=1,NPTYPE
            PLTYPE(J)=J
         ENDDO
         GO TO 430
      END IF
C
      IF(NINPUT .GT. NPLMEN) NINPUT = NPLMEN
C
      DO J=1,NINPUT
         IF(IINPUT(J).GT.NPLMEN .OR. IINPUT(J).LT.1) THEN
            WRITE(*,*)' Invalid entry'
            GO TO 442
         END IF
      ENDDO
C
      NPTYPE=NINPUT
C
      DO J=1,NPTYPE
         PLTYPE(J)=IINPUT(J)
      ENDDO
C
C----Remove duplicates
C
      NPLDEL=0
      DO 448 J=NPTYPE,2,-1
         DO K=1,J-1
            IF(PLTYPE(K).EQ.PLTYPE(J)) THEN
               DO L=K,NPTYPE-NPLDEL-1
                  PLTYPE(L)=PLTYPE(L+1)
               ENDDO
               NPLDEL=NPLDEL+1
               GO TO 448
            ENDIF
         ENDDO
 448  CONTINUE
C
      NPTYPE=NPTYPE-NPLDEL
      GO TO 430
C
C-----------------------------------------------------------------------
C----Plotting Management and Filters
C
 450  CONTINUE
C
      IF(.NOT.OUTYES) THEN
         WRITE(*,*)' Plot Parameters not defined (use PENT)'
         GO TO 410
      END IF
C
C----Lower plot limit warning
C
      IF(NPLTOT .GT. PLTLIM .AND. NPLTOT .LT. PLTLIX) THEN
         IF(BATCH) THEN
            WRITE(*,4365) NPLTOT
         ELSE 
            WRITE(*,4360) NPLTOT
         END IF     
         GO TO 470    
      END IF
C
C----Higher Plot Limit Warning
C
      IF(NPLTOT .GE. PLTLIX) THEN
         IF(BATCH) THEN
            WRITE(*,4375) NPLTOT
         ELSE 
            WRITE(*,4370) NPLTOT
         END IF     
         GO TO 470    
      END IF
C
      GO TO 500      ! NPLTOT is under output limits - proceed
C
C----Read Warning Response
C   
 470  READ(*,7000) ANS
      IF(INDEX('Nn',ANS).NE.0) THEN
         WRITE(*,*)' Plot Run Aborted'
         GO TO 410
      END IF
C
C-------------------------------------------------------------------------
C----Loop through plot parameters
C
 500  CONTINUE
      IF(BATCH) PSFILE=.TRUE.     ! plot.ps file will be created
      N=0                         ! Initialize plot counter
      NPRPLT=NPRPAR               ! Initialize number of propeller curves
C
C  
      DO 700 K=1,NPTYPE
C
        DO 650 IVEL=NVELL,NVELU
           KVEL=EVEL(IVEL)
C
          DO 650 IPWR=NPWRL,NPWRU
             KPWR=EPWR(IPWR)
C
             DO 650 IRPM=NRPML,NRPMU
                 KRPM=ERPM(IRPM)
C               
                DO 650 IALT=NALTL,NALTU
                   KALT=EALT(IALT)
C
C----Run plot type
C                  
                   IF(PLTYPE(K).EQ.1) GO TO 510 ! Efficiency/Velocity
                   IF(PLTYPE(K).EQ.2) GO TO 520 ! Efficiency/Power
                   IF(PLTYPE(K).EQ.3) GO TO 530 ! Efficiency/RPM
                   IF(PLTYPE(K).EQ.4) GO TO 540 ! Efficiency/Altitude
C
                   IF(PLTYPE(K).EQ.5) GO TO 510 ! Thrust/Velocity
                   IF(PLTYPE(K).EQ.6) GO TO 520 ! Thrust/Power
                   IF(PLTYPE(K).EQ.7) GO TO 530 ! Thrust/RPM
                   IF(PLTYPE(K).EQ.8) GO TO 540 ! Thrust/Altitude
C
C
C-------------------------------------------------------------------------
C----Plot Velocities
C
 510  CONTINUE
C
      IF(IVEL.GT.NVELL) GO TO 650  ! Don't repeat the plot
C
      IF(NVEL.LT.2) THEN
         WRITE(*,*)' Insufficient points for Velocity plot'
         GO TO 650
      END IF
C
      DO I=1,NVEL
        PVEL(I)=EVEL(I)   ! Load Real velocities into PVEL
      ENDDO
C
      DO 514 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
         PPROP(JPROP) = EPROP(IPROP)
C
         DO 512 IV=1,NVEL
            PEFFNT(JPROP,IV) =  EEFFNT(IPROP,IALT,IPWR,IRPM,IV)
            PTHRST(JPROP,IV) =  ETHRST(IPROP,IALT,IPWR,IRPM,IV)
            PSTALL(JPROP,IV) =  ESTALL(IPROP,IALT,IPWR,IRPM,IV)
C
 512  CONTINUE   
 514  CONTINUE
C
C----Deal with any unconverged points
C
      DO 518 J=NPRPAR,1,-1
         DO 516 I=1,NVEL
C
            IF(PSTALL(J,I).EQ.-99.9 .OR. PTHRST(J,I).EQ.0.0) THEN
               WRITE(*,4380)PROPAR(J),EPROP(PROPAR(J)),EVEL(I),
     $                      KPWR,KRPM,KALT,N+1
C
               DO IP=J,NPRPLT-1
                  PPROP(IP)=PPROP(IP+1)
                  DO IV=1,NVEL
                     PEFFNT(IP,IV)=PEFFNT(IP+1,IV)
                     PTHRST(IP,IV)=PTHRST(IP+1,IV)
                  ENDDO
               ENDDO
C
               NPRPLT=NPRPLT-1
               GO TO 518
            ENDIF
C
 516  CONTINUE      
 518  CONTINUE
C
C----Abandon plot if no plottable curves
C
      IF(NPRPLT .EQ.0) THEN
         WRITE(*,4390) N+1
         GO TO 650
      END IF
C
C----Plot Efficiency
C
      IF(PLTYPE(K) .EQ.1) THEN
            CALL PEFVEL (RELSIZ,IDEV,NVELA,NPRPLT,
     $      NVEL,PVEL,PEFFNT,PPROP,KPWR,KRPM,KALT)
            N=N+1
      END IF
C
C----Plot Thrust
C
      IF(PLTYPE(K) .EQ.5) THEN
            CALL PTHVEL (RELSIZ,IDEV,NVELA,NPRPLT,
     $      NVEL,PVEL,PTHRST,PPROP,KPWR,KRPM,KALT)
            N=N+1
      END IF
C
      WRITE(*,4440)N,NPLTOT     ! Write plot number just plotted
      IF(BATCH) GO TO 650       ! Jump plot menu in BATCH mode  
      GO TO 600
C
C----End Plot Velocities
C---------------------------------------------------------------------------
C----Plot Powers
C
 520  CONTINUE
C
      IF(IPWR.GT.NPWRL) GO TO 650   ! Don't repeat the plot
C
      IF(NPWR.LT.2) THEN
         WRITE(*,*)' Insufficient points for Power plot'
         GO TO 650
      END IF
C  
      DO I=1,NPWR
         PPWR(I)=EPWR(I) 
      ENDDO
C
      DO 524 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
         PPROP(JPROP) = EPROP(IPROP)
C
         DO 522 IP=1,NPWR
            PEFFNT(JPROP,IP) =  EEFFNT(IPROP,IALT,IP,IRPM,IVEL)
            PTHRST(JPROP,IP) =  ETHRST(IPROP,IALT,IP,IRPM,IVEL)
            PSTALL(JPROP,IP) =  ESTALL(IPROP,IALT,IP,IRPM,IVEL)
C
 522  CONTINUE   
 524  CONTINUE
C
C----Deal with any unconverged points
C
      DO 528 J=NPRPAR,1,-1
         DO 526 I=1,NPWR
C
            IF(PSTALL(J,I).EQ.-99.9 .OR. PTHRST(J,I).EQ.0.0) THEN
               WRITE(*,4380)PROPAR(J),EPROP(PROPAR(J)),KVEL,
     $                      EPWR(I),KRPM,KALT,N+1
C
               DO IP=J,NPRPLT-1
                  PPROP(IP)=PPROP(IP+1)
                  DO IV=1,NPWR
                     PEFFNT(IP,IV)=PEFFNT(IP+1,IV)
                     PTHRST(IP,IV)=PTHRST(IP+1,IV)
                  ENDDO
               ENDDO
C
               NPRPLT=NPRPLT-1
               GO TO 528
            ENDIF
C
 526  CONTINUE      
 528  CONTINUE
C
C----Abandon plot if no plottable curves
C
      IF(NPRPLT .EQ.0) THEN
         WRITE(*,4390) N+1
         GO TO 650
      END IF
C
C----Plot Efficiency vs power
C
      IF(PLTYPE(K) .EQ.2) THEN
            CALL PEFPWR (RELSIZ,IDEV,NPWRA,NPRPLT,
     $      NPWR,PPWR,PEFFNT,PPROP,KVEL,KRPM,KALT)
            N=N+1
      END IF
C
C----Plot Thrust vs power
C
      IF(PLTYPE(K) .EQ.6) THEN
            CALL PTHPWR (RELSIZ,IDEV,NPWRA,NPRPLT,
     $      NPWR,PPWR,PTHRST,PPROP,KVEL,KRPM,KALT)
            N=N+1
      END IF
C
      WRITE(*,4440)N,NPLTOT      ! Write plot number just plotted
      IF(BATCH) GO TO 650        ! Jump plot menu in BATCH mode 
      GO TO 600
C     
C----End Plot Powers
C---------------------------------------------------------------------------
C----Plot RPMs
C
 530  CONTINUE
C  
      IF(IRPM.GT.NRPML) GO TO 650   ! Don't repeat the plot
C
      IF(NRPM.LT.2) THEN
         WRITE(*,*)' Insufficient points for Rpm plot'
         GO TO 650
      END IF
C
      DO I=1,NRPM
         PRPM(I)=ERPM(I) 
      ENDDO
C
      DO 534 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
         PPROP(JPROP) = EPROP(IPROP)
C
         DO 532 IR=1,NRPM
            PEFFNT(JPROP,IR) =  EEFFNT(IPROP,IALT,IPWR,IR,IVEL)
            PTHRST(JPROP,IR) =  ETHRST(IPROP,IALT,IPWR,IR,IVEL)
            PSTALL(JPROP,IR) =  ESTALL(IPROP,IALT,IPWR,IR,IVEL)
C
 532  CONTINUE   
 534  CONTINUE
C
C----Deal with any unconverged points
C
      DO 538 J=NPRPAR,1,-1
         DO 536 I=1,NRPM
C
            IF(PSTALL(J,I).EQ.-99.9 .OR. PTHRST(J,I).EQ.0.0) THEN
               WRITE(*,4380)PROPAR(J),EPROP(PROPAR(J)),KVEL,
     $                      KPWR,ERPM(I),KALT,N+1
C
               DO IP=J,NPRPLT-1
                  PPROP(IP)=PPROP(IP+1)
                  DO IV=1,NRPM
                     PEFFNT(IP,IV)=PEFFNT(IP+1,IV)
                     PTHRST(IP,IV)=PTHRST(IP+1,IV)
                  ENDDO
               ENDDO
C
               NPRPLT=NPRPLT-1
               GO TO 538
            ENDIF
C
 536  CONTINUE      
 538  CONTINUE
C
C----Abandon plot if no plottable curves
C
      IF(NPRPLT .EQ.0) THEN
         WRITE(*,4390) N+1
         GO TO 650
      END IF
C
C----Plot Efficiency
C
      IF(PLTYPE(K) .EQ.3) THEN
            CALL PEFRPM (RELSIZ,IDEV,NRPMA,NPRPLT,
     $      NRPM,PRPM,PEFFNT,PPROP,KVEL,KPWR,KALT)
            N=N+1
      END IF
C
C----Plot Thrust
C
      IF(PLTYPE(K) .EQ.7) THEN
            CALL PTHRPM (RELSIZ,IDEV,NRPMA,NPRPLT,
     $      NRPM,PRPM,PTHRST,PPROP,KVEL,KPWR,KALT)
            N=N+1
      END IF
C
      WRITE(*,4440)N,NPLTOT      ! Write plot number just plotted
      IF(BATCH) GO TO 650        ! Jump plot menu in BATCH mode 
      GO TO 600

C     
C----End Plot RPMS
C---------------------------------------------------------------------------
C----Plot Altitudes
C
 540  CONTINUE
C
      IF(IALT.GT.NALTL) GO TO 650   ! Don't repeat the plot
C
      IF(NALT.LT.2) THEN
         WRITE(*,*)' Insufficient points for Altitude plot'
         GO TO 650
      END IF
C
      DO I=1,NALT
         PALT(I)=EALT(I) 
      ENDDO
C
      DO 544 JPROP=1,NPRPAR
         IPROP=PROPAR(JPROP)
         PPROP(JPROP) = EPROP(IPROP)
C
         DO 542 IA=1,NALT
            PEFFNT(JPROP,IA) =  EEFFNT(IPROP,IA,IPWR,IRPM,IVEL)
            PTHRST(JPROP,IA) =  ETHRST(IPROP,IA,IPWR,IRPM,IVEL)
            PSTALL(JPROP,IA) =  ESTALL(IPROP,IA,IPWR,IRPM,IVEL)
C
 542  CONTINUE   
 544  CONTINUE
C
C----Deal with any unconverged points
C
      DO 548 J=NPRPAR,1,-1
         DO 546 I=1,NALT
C
            IF(PSTALL(J,I).EQ.-99.9 .OR. PTHRST(J,I).EQ.0.0) THEN
               WRITE(*,4380)PROPAR(J),EPROP(PROPAR(J)),KVEL,
     $                      KPWR,KRPM,EALT(I),N+1
C
               DO IP=J,NPRPLT-1
                  PPROP(IP)=PPROP(IP+1)
                  DO IV=1,NALT
                     PEFFNT(IP,IV)=PEFFNT(IP+1,IV)
                     PTHRST(IP,IV)=PTHRST(IP+1,IV)
                  ENDDO
               ENDDO
C
               NPRPLT=NPRPLT-1
               GO TO 548
            ENDIF
C
 546  CONTINUE      
 548  CONTINUE
C
C----Abandon plot if no plottable curves
C
      IF(NPRPLT .EQ.0) THEN
         WRITE(*,4390) N+1
         GO TO 650
      END IF
C
C----Plot Efficiency
C
      IF(PLTYPE(K) .EQ.4) THEN
            CALL PEFALT (RELSIZ,IDEV,NALTA,NPRPLT,
     $      NALT,PALT,PEFFNT,PPROP,KVEL,KPWR,KRPM)
            N=N+1
      END IF
C
C----Plot Thrust
C
      IF(PLTYPE(K) .EQ.8) THEN
            CALL PTHALT (RELSIZ,IDEV,NALTA,NPRPLT,
     $      NALT,PALT,PTHRST,PPROP,KVEL,KPWR,KRPM)
            N=N+1
      END IF
C
      WRITE(*,4440)N,NPLTOT      ! Write plot number just plotted
      IF(BATCH) GO TO 650        ! Jump plot menu in BATCH mode 
      GO TO 600
C
C
C-------------------------------------------------------------------------
C----Plot control menu
C
 600  CALL ASKC('..PLOTC ^',COMAND,COMARG)
C---------------------------------------------------------
      IF(COMAND.EQ.'?   ') WRITE(*,4200)
      IF(COMAND.EQ.'?   ') GO TO 600 
C---------------------------------------------------------
      IF(COMAND.EQ.'    ') GO TO 650 
C---------------------------------------------------------
      IF(COMAND.EQ.'REPL' .OR. COMAND.EQ.'R   ') THEN
         CALL REPLOT(1)
         GO TO 600
      END IF
C---------------------------------------------------------
      IF(COMAND.EQ.'HARD' .OR. COMAND.EQ.'H   ') THEN
         CALL REPLOT(IDEVP)
         PSFILE = .TRUE.
         GO TO 600
      END IF
C---------------------------------------------------------
      IF(COMAND.EQ.'STOP' .OR. COMAND.EQ.'S   ') THEN
         WRITE(*,4400) N,NPLTOT
         GO TO 410
      END IF
C---------------------------------------------------------
      WRITE(*,*)' Command not recognized - type ? for menu'
      GO TO 600  
C------------------------------------------------------------------------
C
C----Get another operating point
C
 650  CONTINUE
C
C----Process another plot type
C
 700  CONTINUE
C
C----All done. Report and return to ESPLOT menu
C
      WRITE(*,4500)
      GO TO 410
C
C------------------------------------------------------------------------
C----Read/Write Error Handling
C
 900  WRITE(UNIT=*,FMT=*)
     $   ' ****Incompatible Database File Format. Aborted****'
      CLOSE(UNIT=ESUNT)
      GO TO 1000
C
 910  WRITE(UNIT=*,FMT=*) ' ****File Write Error. Aborted****'
      IF(LU .EQ. LUTEMP .OR. LU .EQ. LUSAVE) CLOSE (UNIT=LU)
      GO TO 1000
C
C
C------------------------------------------------------------------------
C-----Numeric Output Format Statements
C------------------------------------------------------------------------
C
 6000 FORMAT(
     $ //'   ==============================================='
     $  /'                 ESPROP version ',A8
     $  /'          Display, sorting, and plotting of'
     $  /'        ESPROP parametric propeller databases'
     $  /
     $  /'   This software comes with absolutely no warranty'
     $  /'       Copyright (C) 2001-2011, Philip Carter'
     $  /'          Subject to the GNU Public License'
     $  /'   ===============================================')
C
 6510 FORMAT(/,'  Open SAVE file before suppressing Terminal Output',/,
     $         '       *** Terminal Output remains active ***')
C
 6520 FORMAT(  '  Output will be directed to Terminal only')
 6530 FORMAT(/,'  Output will be directed to Disk only',/,
     $         '  Filename: ',A30)
 6540 FORMAT(/,'  Output will be directed to both Terminal and Disk',/,
     $         '  Filename: ',A30)
 6545 FORMAT(/,'  Save file closed - Filename: ',A30)
C
 6550 FORMAT(  '  Load Database before entering plot menu')
C
 6555 FORMAT(/,'  Save file closed and available: ',A30)
C
 6560 FORMAT(/,'  Postscript hardcopy available in file "plot.ps"',/,
     $         '  Move or rename file before saving further plots',/)
C
 7000 FORMAT(A)
 7010 FORMAT(1X,I4,I4,I4,I4,I4)
 7020 FORMAT(1X,A30)
 7030 FORMAT(1X,I6)
 7035 FORMAT(1X,I4,F9.5,F9.5,I6,I6,I6)
 7040 FORMAT(1X,F7.3,F7.4,F7.4,F7.4,F7.2,F7.3,F7.3,F7.2)
C
C
 7100 FORMAT(/,6X,'ESPROP Database Parameters',/,
     $4X, 30('='),/,
     $6X,'Filename: ',A30,/,
     $4X, 30('-'),/,
     $7X,I2,' Propellers',/,
     $7X,I2,' Velocity Points',/,
     $7X,I2,' Power Points',/,
     $7X,I2,' Rpm Points',/,
     $7X,I2,' Altitude Points',/,
     $4X, 30('-'),/,
     $5X,' Points per propeller ',I6,/,
     $5X,' Total Points         ',I6,/,
     $5X,' Analysed Points      ',I6,/,
     $5X,' Converged Solutions  ',I6,/,
     $5X,' Failed Points        ',I6)
C
 7110 FORMAT(4X,30('-'),/,6X,' Propeller')
 7115 FORMAT(4X,30('-'),/,6X,' Altitude       (ft)')
 7120 FORMAT(4X,30('-'),/,6X,'   Power         (hp)')
 7125 FORMAT(4X,30('-'),/,6X,'    Rpm        (r/min)')
 7130 FORMAT(4X,30('-'),/,6X,' Velocity      (knots)')
 7135 FORMAT(4X,30('='),/)
C
 7140 FORMAT(10X,I2,5X,A30)
 7150 FORMAT(10X,I2,8X,I6)
C
C
 7210 FORMAT(/,6X,' Propeller',/,4X,30('-'))
 7220 FORMAT(/,6X,' Altitude       (ft)',/,4X,30('-'))
 7230 FORMAT(/,6X,'   Power         (hp)',/,4X,30('-'))
 7240 FORMAT(/,6X,'    Rpm        (r/min)',/,4X,30('-'))
 7250 FORMAT(/,6X,' Velocity      (knots)',/,4X,30('-'))
C
C
 7270 FORMAT(4X,30('-'),/,
     $4X,'One integer for single value',/,
     $4X,'Two integers for range of values',/,
     $4X,'0 for all values')
C
C
 7300 FORMAT(/,
     $'             Numeric Output Status',/,
     $'             ---------------------'  )
 7310 FORMAT(13X,I2,2X,A20)
 7320 FORMAT(2X, 46('-'),/,
     $'  Velocity ',I2,' to ',I2,3X,I5,' to ',I5,' kts   ',I2,' pt'/,
     $'  Power    ',I2,' to ',I2,3X,I5,' to ',I5,' hp    ',I2,' pt'/,
     $'  Rpm      ',I2,' to ',I2,3X,I5,' to ',I5,' rpm   ',I2,' pt'/,
     $'  Altitude ',I2,' to ',I2,3X,I5,' to ',I5,' ft    ',I2,' pt'/,
     $2X, 46('-'),/,
     $'  Total active operating points:        ',I5,/,
     $2X, 46('-'))
C
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
 2010 FORMAT(1X,' You are about to write ',I3,' operating points',/,
     $       1X,' to terminal. Continue?? <y/n>')
 2020 FORMAT(1X,' You are about to write ',I3,' operating points',/,
     $       1X,' to disk. Continue?? <y/n>')
 2030 FORMAT(1X,' You are about to write ',I3,' operating points',/,
     $       1X,' to both terminal and disk. Continue?? <y/n>')
C
 2040 FORMAT(1X,' You are about to write ',I5,' operating points',
     $       1X,'to terminal.',/,
     $       1X,' Are you REALLY SURE you want to do this??? <y/n>')
c
 2050 FORMAT(1X,' You are about to write ',I5,' operating points',
     $       1X,'to disk.',/,
     $       1X,' Are you REALLY SURE you want to do this??? <y/n>')
C
 2060 FORMAT(1X,' You are about to write ',I5,' operating points',
     $       1X,'to both terminal and disk.',/,
     $       1X,' Are you REALLY SURE you want to do this??? <y/n>')
C
C
 2070 FORMAT(/,'  Output saved to file: ',A30)
C
 2075 FORMAT(/,'  Parameter prompts will not be displayed')
 2080 FORMAT(/,'  Parameter prompts will be displayed')
C
C
 3001 FORMAT(/,1X,'Sorted')
 3002 FORMAT(/,1X,'Not sorted')
C
C---Propellers
C
 3010 FORMAT(1X,77('-'),/,
     $22X, I3,' KNOTS',7X, I3,' HP',7X, I4,' RPM',7X, I5,' FT',/, 
     $         1X,77('-'),/,
     $1X,'PROPELLER',7X,'BldAng',2X,'Stall%',2X,'TpMach',2X,
     $'  J  ',2X,'IdlEff',2X,'IndEff',2X,' Effy ',2X,'Thr-kg')
C
 3012 FORMAT(1X,A14,14X,'-----solution did not converge-----')
C
 3014 FORMAT(1X,A14,2X,F5.1,3X,F5.1,3X,F5.3,3X,F5.3,3X,
     $       F5.3,3X,F5.3,3X,F5.3,3X,F5.1)
C
C---Velocities
C
 3020 FORMAT(1X,77('-'),/,
     $1X,A16,  21X,  I3,' HP',7X,  I4,' RPM',7X,  I5,' FT',/,  
     $1X,         77('-'),/,
     $2X,'VELOCITY-kts',3X,'BldAng',2X,'Stall%',2X,'TpMach',2X,
     $'  J  ',2X,'IdlEff',2X,'IndEff',2X,' Effy ',2X,'Thr-kg')
C
 3022 FORMAT(6X,I3,20X,'-----solution did not converge-----')
C
 3024 FORMAT(6X,I3,8X,F5.1,3X,F5.1,3X,F5.3,3X,F5.3,3X,
     $       F5.3,3X,F5.3,3X,F5.3,3X,F5.1)
C
C---Powers
C
 3030 FORMAT(1X,77('-'),/,
     $1X,A16,5X, I3,' KNOTS',20X, I4,' RPM',7X, I5,' FT',/,  
     $1X,         77('-'),/,
     $4X,'POWER-hp',5X,'BldAng',2X,'Stall%',2X,'TpMach',2X,
     $'  J  ',2X,'IdlEff',2X,'IndEff',2X,' Effy ',2X,'Thr-kg')
C
 3032 FORMAT(6X,I3,20X,'-----solution did not converge-----')
C
 3034 FORMAT(6X,I3,8X,F5.1,3X,F5.1,3X,F5.3,3X,F5.3,3X,
     $       F5.3,3X,F5.3,3X,F5.3,3X,F5.1)
C
C---RPMs
C
 3040 FORMAT(1X,77('-'),/,
     $1X,A16,5X, I3,' KNOTS',7X, I3,' HP',22X, I5,' FT',/,  
     $1X,         77('-'),/,
     $7X,'RPM',7X,'BldAng',2X,'Stall%',2X,'TpMach',2X,
     $'  J  ',2X,'IdlEff',2X,'IndEff',2X,' Effy ',2X,'Thr-kg')
C
 3042 FORMAT(6X,I4,19X,'-----solution did not converge-----')
C
 3044 FORMAT(6X,I4,7X,F5.1,3X,F5.1,3X,F5.3,3X,F5.3,3X,
     $       F5.3,3X,F5.3,3X,F5.3,3X,F5.1)
C
C---Altitudes
C
 3050 FORMAT(1X,77('-'),/,
     $1X,A16,5X, I3,' KNOTS',7X, I3,' HP',7X, I4,' RPM',/,  
     $1X,         77('-'),/,
     $3X,'ALTITUDE-ft',3X,'BldAng',2X,'Stall%',2X,'TpMach',2X,
     $'  J  ',2X,'IdlEff',2X,'IndEff',2X,' Effy ',2X,'Thr-kg')
C
 3052 FORMAT(5X,I5,19X,'-----solution did not converge-----')
C
 3054 FORMAT(5X,I5,7X,F5.1,3X,F5.1,3X,F5.3,3X,F5.3,3X,
     $       F5.3,3X,F5.3,3X,F5.3,3X,F5.1)
C
C------------------------------------------------------------------------
C----Plot output format statements
C------------------------------------------------------------------------
C
 4100 FORMAT(
     $  /'   PENT    Enter plot output parameters'
     $  /'   STAT    Display plot output status'
C
     $ //'   EPRO    Enter new propeller list'
     $  /'   CPRO    Add/remove propellers'
     $  /'   CVEL    Change Velocity points'
     $  /'   CPOW    Change Power      "'
     $  /'   CRPM    Change Rpm        "'
     $  /'   CALT    Change Altitude   "'
C
     $ //'   SELP    Select plot types'
     $  /'   EXEC    Execute plots'
C
     $ //'   BATC    Batch plot toggle'
     $  /'   COLO    Color hardcopy toggle'
     $  /'   SIZE    Change plot window size'
C
     $ //'  <ret>    Return to ESPROP prompt (numeric output)')
C
C
 4200 FORMAT(
     $  /'   Plot Control Menu:'
     $  /'  <ret>    Plot next operating point'
     $  /'   REPL    Replot current plot'
     $  /'   HARD    Save current plot to file plot.ps'
     $  /'   STOP    Abandon this plot run')
C
 4250 FORMAT(/,
     $1X,' Propeller parameter list reduced to 8',/,
     $1X,' Removed from list:')
 4260 FORMAT(5X,A30)
C
 4270 FORMAT(/,1X,'Current relative plot window size is ',F4.2)
C
 4280 FORMAT(/,
     $' ----------------------',/,
     $'  1  Effy  vs Velocity',/,
     $'  2  Effy  vs Power',/,
     $'  3  Effy  vs Rpm',/,
     $'  4  Effy  vs Altitude',//,
C
     $'  5  Thrst vs Velocity',/,
     $'  6  Thrst vs Power',/,
     $'  7  Thrst vs Rpm',/,
     $'  8  Thrst vs Altitude'/,
     $' ----------------------')    
C
 4300 FORMAT(/,
     $'               Plot Output Status',/,
     $'               ------------------'  )
 4310 FORMAT(15X,I2,2X,A20)
 4320 FORMAT(2X, 46('-'),/,
     $'  Velocity ',I2,' to ',I2,3X,I5,' to ',I5,' kts   ',I2,' pt'/,
     $'  Power    ',I2,' to ',I2,3X,I5,' to ',I5,' hp    ',I2,' pt'/,
     $'  Rpm      ',I2,' to ',I2,3X,I5,' to ',I5,' rpm   ',I2,' pt'/,
     $'  Altitude ',I2,' to ',I2,3X,I5,' to ',I5,' ft    ',I2,' pt'/,
     $2X, 46('-'),/,
     $'  Total active operating points:        ',I5,/,
     $2X, 46('-'),/,
     $11X,       'Active Plot Types       Qty')
C
 4331 FORMAT(11X,'1  Effy  vs Velocity   ',I3)
 4332 FORMAT(11X,'2  Effy  vs Power      ',I3)
 4333 FORMAT(11X,'3  Effy  vs Rpm        ',I3)
 4334 FORMAT(11X,'4  Effy  vs Altitude   ',I3)
C
 4335 FORMAT(11X,'5  Thrst vs Velocity   ',I3)
 4336 FORMAT(11X,'6  Thrst vs Power      ',I3)
 4337 FORMAT(11X,'7  Thrst vs Rpm        ',I3)
 4338 FORMAT(11X,'8  Thrst vs Altitude   ',I3)
 4340 FORMAT(11X,'           Plot Total:',I4,/,
     $       11X,'--------------------------')
C
 4351 FORMAT(11X,'Batch plotting')
 4352 FORMAT(11X,'Interactive plotting')
 4353 FORMAT(11X,'Color hardcopy')
 4354 FORMAT(11X,'B&W hardcopy')
C
 4360 FORMAT(1X,' You are about to interactively plot ',I3,' plots',/,
     $       1X,' Continue?? <y/n>')
C
 4365 FORMAT(1X,' You are about to batch plot ',I3,' plots',/,
     $       1X,' Continue?? <y/n>')
C
 4370 FORMAT(1X,' You are about to interactively plot ',I4,' plots',/,
     $       1X,' Are you REALLY SURE you want to do this?? <y/n>')
C
 4375 FORMAT(1X,' You are about to batch plot ',I4,' plots',/,
     $       1X,' Are you REALLY SURE you want to do this?? <y/n>')
C
 4380 FORMAT(/,
     $'  Propeller ',I2,' is missing a converged solution',/,
     $'  Name = ',A18,' Velocity = ',I3,' kts'/,
     $'  Power = ',I4,' hp',2X,'Rpm = ',I5,2X,'Alt = ',I5,' ft',/,
     $'  This curve has been removed from plot ',I3)
C
 4390 FORMAT(/,' No fully converged curves on plot ',I3,/,
     $         ' This plot has not been plotted')
C
 4400 FORMAT(/,
     $'    ===== Plot Run Abandoned ===== ',/,
     $'     ',I3,' of ',I3,' plots completed',/)
C
 4440 FORMAT(/,' Plot ',I3, ' of ',I3)
C
 4500 FORMAT(/,' ===== Plot Run Complete =====')
C
C-----------------------------------------------------------------------
      END       ! Esprop
C-----------------------------------------------------------------------



C-----------------------------------------------------------------------
C                          SUBROUTINES
C------------------------------------------------------------------------
C    SUBROUTINE PARPRO
C    Prompt for series of integers
C    Read string from keyboard and convert to integers
C    Confirm integers are bounded by 1 and NPROP
C    0 returns all integers 1 to NPROP
C    Returns integers in PROPAR(20)
C    Returns number of integers in NPRPAR
C------------------------------------------------------------------------
C
      SUBROUTINE PARPRO(PROMPT,NPROPA,NPROP,PROPAR,NPRPAR)
C
      CHARACTER PROMPT*132
      CHARACTER ASKARG*132
      LOGICAL ERROR
      DIMENSION IINPUT(20)
      INTEGER PROPAR(NPROPA)
C
 210  CALL ASKS(PROMPT,ASKARG)
C
      DO I=1, 20
        IINPUT(I) = 0
      ENDDO
      NINPUT = 20
C
      CALL GETINT(ASKARG,IINPUT,NINPUT,ERROR)
C
      IF(ERROR) THEN
         WRITE(*,*)' Data input error'
         GO TO 210
      END IF
C
      IF(NINPUT .EQ. 0) THEN
         IF(NPRPAR .EQ. 0) THEN
            WRITE(*,*)' Must complete first entry'
            GO TO 210
         ELSE
            WRITE(*,*)' No changes'
            GO TO 299
         END IF
      END IF
C
      IF(IINPUT(1) .EQ. 0) THEN
         NPRPAR=NPROP
         DO I=1,NPRPAR
            PROPAR(I)=I
         ENDDO
         GO TO 299
      END IF
C
C----Catch duplicates
C
      DO 215 I=1,NINPUT
         NDUP=0
C
         DO J=1,NINPUT
            IF(IINPUT(I) .EQ. IINPUT(J)) THEN
               NDUP=NDUP+1
            END IF
         END DO
C
         IF(NDUP .GT. 1) THEN
            WRITE(*,*)' Duplicate entries not permitted'
            GO TO 210
         END IF
C
 215  CONTINUE
C
C----Check ranges
C
      IF(NINPUT .GT. 20 .OR. NINPUT .GT. NPROP) THEN
         WRITE(*,*)' Too many parameters - type 0 for all'
         GO TO 210
      END IF
C
      DO I=1,NINPUT
         IF(IINPUT(I) .LT. 1 .OR. IINPUT(I) .GT. NPROP)THEN
            WRITE(*,*)' Propeller parameters outside range'
            GO TO 210
         END IF
      ENDDO
C
C----Write input to NPRPAR and PROPAR
C
      NPRPAR=NINPUT
      DO I=1,NPRPAR
         PROPAR(I)=IINPUT(I)
      ENDDO
C
 299  RETURN
      END    !Parpro
C
C-------------------------------------------------------------------------


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
      CHARACTER ASKARG*32
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
      IF(ERROR) THEN
         WRITE(*,*)' Data input error'
         GO TO 210
      END IF
C
      IF(NINPUT .EQ. 0) THEN
         IF(IPARU .EQ. 0) THEN
            WRITE(*,*) ' Must complete first entry'
            GO TO 210
         ELSE
            WRITE(*,*) ' No changes'
            GO TO 220
         END IF
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
         WRITE(*,*)' Range outside database parameters'
         GO TO 210
      END IF
C
      IF(IPARU .LT. IPARL) THEN
         WRITE(*,*)' Upper bound smaller than lower bound'
         GO TO 210
      END IF
C
 220  RETURN
      END   ! Param


C-----------------------------------------------------------------------
C      SORT BY EFFICIENCY
C
C      EFFY - Efficiency values in standard order
C      SORTOP - Sequence indexing highest to lowest values in EFFY
C      NPAR - Number of operating points being sorted
C      NARRAY - Array dimension for EFFY and SORTOP
C-----------------------------------------------------------------------
C
      SUBROUTINE ESORT(NARRAY,EFFY,NOPS,SORTOP)
C
      REAL EFFY(NARRAY)
      INTEGER SORTOP(NARRAY)
C      
      DO I=1,NARRAY
         SORTOP(I)=0
      ENDDO
C
      DO 199 I=1,NOPS
C
         NHIGH=1
         DO J= 1,NOPS
            IF(EFFY(I) .LT. EFFY(J)) NHIGH=NHIGH+1
         ENDDO
C
         DO K=NHIGH,NOPS
            IF(SORTOP(K) .EQ.0)THEN
               SORTOP(K)=I
               GO TO 199
            END IF
         ENDDO
C
 199  CONTINUE
C
      RETURN
      END


C-----------------------------------------------------------------------
C     Parameter Entry and Modification subroutines
C-----------------------------------------------------------------------
C     ENTER PROPELLER INDICES
C-----------------------------------------------------------------------
C
      SUBROUTINE PROPEN(NPROPA,NPROP,EPROP,PAPROM,PLPROM,PROPAR,NPRPAR)
C
      CHARACTER EPROP(NPROPA)*32
      LOGICAL PAPROM
      LOGICAL PLPROM
      CHARACTER*132 PROMPT
C
      INTEGER PROPAR(NPROPA)
C
      IF(PAPROM) THEN
         WRITE(*,7210)
         DO I=1,NPROP
            WRITE(*,7140)I,EPROP(I)
         ENDDO
C
         IF(.NOT.PLPROM) WRITE(*,7260)
         IF(PLPROM) WRITE(*,7270)
C         
      END IF
C
      PROMPT=' Enter Propeller indices^'
 10   CALL PARPRO(PROMPT,NPROPA,NPROP,PROPAR,NPRPAR)
C
      IF(PLPROM .AND. NPRPAR .GT.8) THEN
         WRITE(*,*)' Maximum of 8 propellers per plot'
         GO TO 10
      END IF
C
      IF(NPRPAR .EQ. 1) THEN
         WRITE(*,7300)
      ELSE
         WRITE(*,7310)NPRPAR
      END IF
C
      RETURN
C      
 7210 FORMAT(/,6X,' Propeller',/,4X,30('-'))
 7140 FORMAT(10X,I2,5X,A30)
 7260 FORMAT(4X,30('-'),/,
     $4X,'Integer values in any order',/,
     $4X,'0 for all values')
 7270 FORMAT(4X,30('-'),/,
     $4X,'Integer values in any order',/,
     $4X,'Maximum of 8 props for plotting')
 7300 FORMAT(/,4X,'1 propeller selected')
 7310 FORMAT(/,4X,I2,' propellers selected')
C
      END  ! Propen
C
C--------------------------------------------------------------------- 
C
      SUBROUTINE VELEN(NVELA,NVEL,EVEL,PAPROM,NVELU,NVELL)
C
      INTEGER EVEL(NVELA)
      LOGICAL PAPROM
      CHARACTER*132 PROMPT
C
      IF(PAPROM) THEN
         WRITE(*,7250)
         DO I=1,NVEL
            WRITE(*,7150)I,EVEL(I)
         ENDDO
         WRITE(*,7270)         
      END IF
C
      PROMPT=' Enter Velocity index range^'
      CALL PARAM(PROMPT,NVEL,NVELU,NVELL)
C
      NOVEL=NVELU-NVELL+1
C
      IF(NOVEL .EQ. 1) THEN
         WRITE(*,7300) EVEL(NVELL)
      ELSE
         WRITE(*,7310) NOVEL,EVEL(NVELL),EVEL(NVELU)
      END IF
C
      RETURN
C
 7250 FORMAT(/,6X,' Velocity      (knots)',/,4X,30('-'))
 7150 FORMAT(10X,I2,8X,I6)
 7270 FORMAT(4X,30('-'),/,
     $4X,'One integer for single value',/,
     $4X,'Two integers for range of values',/,
     $4X,'0 for all values')
 7300 FORMAT(/,4X,'1 airspeed selected',/,
     $         4X,I3,' knots')
 7310 FORMAT(/,4X,I2,' airspeeds selected',/,
     $         4X,I3,' to ',I3,' knots')
C
      END !VELEN
C
C---------------------------------------------------------------------
C
      SUBROUTINE RPMEN(NRPMA,NRPM,ERPM,PAPROM,NRPMU,NRPML)
C
      INTEGER ERPM(NRPMA)
      LOGICAL PAPROM
      CHARACTER*132 PROMPT
C
      IF(PAPROM) THEN
         WRITE(*,7240)
         DO I=1,NRPM
            WRITE(*,7150)I,ERPM(I)
         ENDDO
         WRITE(*,7270)         
      END IF
C
      PROMPT=' Enter Rpm index range^'
      CALL PARAM(PROMPT,NRPM,NRPMU,NRPML)
C
      NORPM=NRPMU-NRPML+1
C
      IF(NORPM .EQ. 1) THEN
         WRITE(*,7300) ERPM(NRPML)
      ELSE
         WRITE(*,7310) NORPM,ERPM(NRPML),ERPM(NRPMU)
      END IF
C
      RETURN
C
 7240 FORMAT(/,6X,'    Rpm        (r/min)',/,4X,30('-'))
 7150 FORMAT(10X,I2,8X,I6)
 7270 FORMAT(4X,30('-'),/,
     $4X,'One integer for single value',/,
     $4X,'Two integers for range of values',/,
     $4X,'0 for all values')
 7300 FORMAT(/,4X,'1 Rpm selected',/,
     $         4X,I4,' rpm')
 7310 FORMAT(/,4X,I2,' Rpms selected',/,
     $         4X,I4,' to ',I4,' rpm')
C
      END ! RPMEN
C
C----------------------------------------------------------------------
C
      SUBROUTINE PWREN(NPWRA,NPWR,EPWR,PAPROM,NPWRU,NPWRL)
C
      INTEGER EPWR(NPWRA)
      LOGICAL PAPROM
      CHARACTER*132 PROMPT
C     
      IF(PAPROM) THEN
         WRITE(*,7230)
         DO I=1,NPWR
            WRITE(*,7150)I,EPWR(I)
         ENDDO
         WRITE(*,7270)         
      END IF
C
      PROMPT=' Enter Power index range^'
      CALL PARAM(PROMPT,NPWR,NPWRU,NPWRL)
C
      NOPWR=NPWRU-NPWRL+1
C
      IF(NOPWR .EQ. 1) THEN
         WRITE(*,7300) EPWR(NPWRL)
      ELSE
         WRITE(*,7310) NOPWR,EPWR(NPWRL),EPWR(NPWRU)
      END IF
C
      RETURN
C
 7230 FORMAT(/,6X,'   Power         (hp)',/,4X,30('-'))
 7150 FORMAT(10X,I2,8X,I6)
 7270 FORMAT(4X,30('-'),/,
     $4X,'One integer for single value',/,
     $4X,'Two integers for range of values',/,
     $4X,'0 for all values')
 7300 FORMAT(/,4X,'1 power selected',/,
     $         4X,I4,' hp')
 7310 FORMAT(/,4X,I2,' powers selected',/,
     $         4X,I4,' to',I4,' hp')
C
      END ! PWREN
C
C----------------------------------------------------------------------
C
      SUBROUTINE ALTEN(NALTA,NALT,EALT,PAPROM,NALTU,NALTL)
C
      INTEGER EALT(NALTA)
      LOGICAL PAPROM
      CHARACTER*132 PROMPT
C
      IF(PAPROM) THEN
         WRITE(*,7220)
         DO I=1,NALT
            WRITE(*,7150)I,EALT(I)
         ENDDO
         WRITE(*,7270)         
      END IF 
C
      PROMPT=' Enter Altitude index range^'
      CALL PARAM(PROMPT,NALT,NALTU,NALTL)
C
      NOALT=NALTU-NALTL+1
C
      IF(NOALT .EQ. 1) THEN
         WRITE(*,7300) EALT(NALTL)
      ELSE
         WRITE(*,7310) NOALT,EALT(NALTL),EALT(NALTU)
      END IF
C
      RETURN
C
 7220 FORMAT(/,6X,' Altitude       (ft)',/,4X,30('-'))
 7150 FORMAT(10X,I2,8X,I6)
 7270 FORMAT(4X,30('-'),/,
     $4X,'One integer for single value',/,
     $4X,'Two integers for range of values',/,
     $4X,'0 for all values')
 7300 FORMAT(/,4X,'1 altitude selected',/,
     $         4X,I5,' ft')
 7310 FORMAT(/,4X,I2,' altitudes selected',/,
     $         4X,I5,' to',I5,' ft')
C
      END ! ALTEN
C
C-------------------------------------------------------------------------

C------------------------------------------------------------------------
C    SUBROUTINE ADRPRO  - Add or remove parameters from PROPAR
C    Prompt for series of integers
C    Read string from keyboard and convert to integers
C    etc
C------------------------------------------------------------------------
C
      SUBROUTINE ADRPRO(PROMPT,NPROPA,NPROP,PROPAR,NPRPAR)
C
      CHARACTER PROMPT*132
      CHARACTER ASKARG*132
      LOGICAL ERROR
      DIMENSION IINPUT(20)
      INTEGER PROPAR(NPROPA),ROPAR(NPROPA)
      INTEGER DELPRO(NPROPA),DELST(NPROPA)
C
C
 210  CALL ASKS(PROMPT,ASKARG)
C
      DO I=1, 20
        IINPUT(I) = 0
      ENDDO
      NINPUT = 20
C
      CALL GETINT(ASKARG,IINPUT,NINPUT,ERROR)
C
      IF(ERROR) THEN
         WRITE(*,*)' Data input error'
         GO TO 210
      END IF
C
      IF(NINPUT .EQ. 0) THEN
         IF(NPRPAR .EQ. 0) THEN
            WRITE(*,*)' Must complete first entry'
            GO TO 210
         ELSE
            WRITE(*,*)' No changes'
            GO TO 299
         END IF
      END IF
C
C----Catch duplicates
C
      DO 215 I=1,NINPUT
         NDUP=0
C
         DO J=1,NINPUT
            IF(IINPUT(I) .EQ. IINPUT(J)) THEN
               NDUP=NDUP+1
            END IF
         END DO
C
         IF(NDUP .GT. 1) THEN
            WRITE(*,*)' Duplicate entries not permitted'
            GO TO 210
         END IF
C
 215  CONTINUE
C
C----Initialize dummies
C
      NPAR=NPRPAR
      DO I=1,NPAR
         ROPAR(I)=PROPAR(I)
      ENDDO
C
C----Identify PROPAR indices to be deleted
C
      NDEL=0
      DO 220 I=1,NINPUT
         IF(IINPUT(I) .LT. 0) THEN
            DO J=1,NPAR
               IF(-IINPUT(I) .EQ. ROPAR(J)) THEN
                  NDEL=NDEL+1
                  DELPRO(NDEL)=J
               END IF
            END DO
         END IF
 220  CONTINUE
C
C----If none to be deleted jump to additions
C
      IF(NDEL .EQ. 0) GO TO 230
C
C----Sort Delpro--------------------------------------------------------
C      
      DO 225  I=1,NDEL
         INDX=1
C
         DO J=1,NDEL
            IF(DELPRO(J) .LT. DELPRO(I)) THEN
               INDX=INDX+1
            END IF
         ENDDO
C
         DELST(INDX)=DELPRO(I)
 225  CONTINUE
         
C
C----Shuffle ROPAR indices to delete items in DELPRO
C
      DO 227 I=NDEL,1,-1
         J=DELST(I)
C
         DO K=J,NPAR-1
            ROPAR(K)=ROPAR(K+1)
         ENDDO
         NPAR=NPAR-1
C
 227  CONTINUE   
C
C----Add on additions to end of ROPAR-----------------------------------
C
 230  CONTINUE
C
      DO 240 I=1,NINPUT
         IF(IINPUT(I).GT.0) THEN
C
            IF(IINPUT(I) .GT. NPROP) THEN
               WRITE(*,*)' Parameter outside range'
               GO TO 210
            END IF
C
            DO J=1,NPAR
               IF(IINPUT(I) .EQ. ROPAR(J)) THEN
                  WRITE(*,*)' Duplicate prop omitted'
                  GO TO 240
               END IF
            END DO
C
            NPAR=NPAR+1
            ROPAR(NPAR)=IINPUT(I)
C
         END IF
 240  CONTINUE
C
C----Check that we still have a propeller
C
      IF(NPAR .EQ.0) THEN
         WRITE(*,*)' Must specify at least one propeller'
         GO TO 210
      END IF
C
C----Write from Dummies and out of here
C
      NPRPAR=NPAR
      DO I=1,NPRPAR
         PROPAR(I)=ROPAR(I)
      END DO
C
 299  RETURN
      END    ! ADRPRO


C-----------------------------------------------------------------------
C     Add or remove Propeller Indices
C----------------------------------------------------------------------
C
      SUBROUTINE PROPCH(NPROPA,NPROP,EPROP,PAPROM,PLPROM,PROPAR,NPRPAR)
C
      CHARACTER EPROP(NPROPA)*32
      LOGICAL PAPROM
      LOGICAL PLPROM
      CHARACTER*132 PROMPT
C
      INTEGER PROPAR(NPROPA)
C
      IF(PAPROM) THEN
         WRITE(*,7210)
         DO I=1,NPROP
            WRITE(*,7140)I,EPROP(I)
         ENDDO
      END IF
C
 5    IF(PAPROM) THEN
         WRITE(*,7400)
         DO I=1,NPRPAR
            J=PROPAR(I)
            WRITE(*,7140) J, EPROP(J)
         ENDDO
C
         IF(PLPROM) THEN
            WRITE(*,7270)
         ELSE
            WRITE(*,7260)
         END IF
C         
      END IF
C
      PROMPT=' Add or Remove Propellers ^'
 10   CALL ADRPRO(PROMPT,NPROPA,NPROP,PROPAR,NPRPAR)
C
      IF(PLPROM .AND. NPRPAR .GT.8) THEN
         NOVER=NPRPAR-8
         WRITE(*,7410) NOVER
         GO TO 5
      END IF
C
      IF(NPRPAR .EQ. 1) THEN
         WRITE(*,7300)
      ELSE
         WRITE(*,7310)NPRPAR
      END IF
C
      RETURN
C      
 7210 FORMAT(/,6X,' Propeller',/,4X,30('-'))
 7140 FORMAT(10X,I2,5X,A30)
 7260 FORMAT(4X,30('-'),/,
     $4X,'Positive integers to add',/,
     $4X,'Negative integers to remove')
 7270 FORMAT(4X,30('-'),/,
     $4X,'Positive integers to add',/,
     $4X,'Negative integers to remove',/,
     $4X,'Maximum of 8 props for plotting')
 7300 FORMAT(/,2X,'1 propeller selected')
 7310 FORMAT(/,2X,I2,' propellers selected')
C
 7400 FORMAT(4X,30('-'),/,6X,' Current Propellers:')
 7410 FORMAT(/,2X,'8 propellers maximum per plot',/,
     $         2X,'Must remove at least',I2)
C
      END  ! Propch
C
C---------------------------------------------------------------------- 


C--- taken from DFDC userio-------------
      SUBROUTINE RDLINE2(LUN,LINE,ICNT)
C...Purpose  Read a non-comment line from the input file 
C
C...Input    Data read from unit LUN
C...Output   LINE  Character string with input line
C                  LINE is set to 'END' for end or errors
C
C   Comment lines are assumed to start with ! or #
C
      CHARACTER*(*) LINE
      LOGICAL LECHO
      DATA LECHO / .FALSE. /
C
 1000 FORMAT(A)
 1010 FORMAT(I4,1X,A)
C
   20 ICNT = ICNT + 1
      READ (LUN,1000,END=80,ERR=90) LINE
      IF(LECHO) WRITE(*,1010) ICNT,LINE(1:60)
C
C---- skip comment line
      IF(INDEX('!#',LINE(1:1)) .NE. 0) GO TO 20
C
C---- skip blank line
      IF(LINE.EQ.' ') GO TO 20
C
C---- normal return after significant line
      RETURN
C
   80 LINE = 'END '
      RETURN
C
   90 LINE = 'ERR '
      RETURN
      END

