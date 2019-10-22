C**********************************************************************
C    Module esplots.f 
C    Plot routines for ESPROP, a program for display of numeric and 
C    plotted data from ESPROP Databases.
C    Copyright (C) 2001-2011 Philip Carter, Esotec Developments 
C    philip (at) esotec (dot) org 
C    Acknowledgements to Harold Youngren
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


C-----------------------------------------------------------------------
C       PEFVEL - Plotting Efficiency vs Velocity in ESPROP
C-----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NVA    - Array size for velocities (same as NVELA)
C       NPRO   - No of curves (propellers) to be plotted (max 8)
C       NVEL   - No of coordinates points (velocities) per curve
C       PX     - Array of x-axis (velocity) points (NVEL)
C       EY     - Array of y-axis (efficiency values) (NPRO x NVEL)
C       PPROP  - Array of names of propellers (NPRO)
C       KPWR   - Power operating point
C       KRPM   - RPM operating point
C       KALT   - Altitude operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PEFVEL(RELSIZ,IDEV,NVA,NPRO,NVEL,PX,EY,
     $                  PPROP,KPWR,KRPM,KALT)
C
      REAL PX(NVA),PY(NVA),EY(8,NVA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/6.4,7.4/ ! End points of legend lines
C
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Y axis fixed at 0 to 1
C
      YRANGE = 1.0     ! Range of values on y-axis 
      ANNLOY = 0.0     ! First value on y axis 
      YOFSET = 0.0     ! No y-axis offset
C
      YSCALE = PLHIGH/YRANGE       ! Coordinates Y scale
      INVALY = 10                  ! No of intervals on Y-axis
C
      DYANN = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
      DELTY = YRANGE/FLOAT(INVALY) ! Delta annotations Y-axis
C
C
C----Set up X-axis------------------------------------------------------
C----Min and max velocities
C
      XMIN=PX(1)
      XMAX=PX(NVEL)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C
C-----Plot Routines----------------------------------------------------------
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX, 1)
C
C----Annotate axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLMATHABS(0.17,4.36, 0.35,'h',0.0,1)
      CALL PLCHARABS(4.7,0.3,CHTIT,'VELOCITY-knots',0.0,-1)
C
C----Operating point
C
      RPWR=KPWR    ! Convert to real
      RRPM=KRPM
      RALT=KALT
C
      IF(KPWR .LT.100) THEN
         PWRX = 1.0
      ELSE
         PWRX = 1.14
      END IF
C
      RPMX = 3.48
C
      IF(KALT .LT. 10) THEN
         ALTX = 5.56
      ELSE IF(KALT .LT.100) THEN
         ALTX = 5.7
      ELSE IF(KALT .LT.1000) THEN
         ALTX = 5.84
      ELSE IF(KALT .LT.10000) THEN
         ALTX = 5.98
      ELSE
         ALTX = 6.12
      END IF
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RPWR, 0.,-1)
      CALL PLCHAR(PWRX,6.6, CHOP,'HP', 0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RRPM, 0.,-1)
      CALL PLCHAR(RPMX,6.6, CHOP,'RPM',0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RALT, 0.,-1)
      CALL PLCHAR(ALTX,6.6, CHOP,'FT', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot efficiency curves----------------------------------------------
C
      DO 10 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NVEL
            PY(I)=EY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)=VERTLB+VERTLD*(NPRO-J)
         ENDDO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NVEL,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 10   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END     ! PEFVEL



C------------------------------------------------------------------------
C       PEFPWR - Plotting Efficiency vs Power in ESPROP
C-----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NPA    - Array size for powers (same as NPWRA)
C       NPRO   - No of curves (propellers) to be plotted (max 8)
C       NPWR   - No of coordinates points (powers) per curve
C       PX     - Array of x-axis (power) points (NPWR)
C       EY     - Array of y-axis (efficiency values) (NPRO x NPWR)
C       PPROP  - Array of names of propellers (NPRO)
C       KVEL   - Velocity operating point
C       KRPM   - RPM operating point
C       KALT   - Altitude operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PEFPWR(RELSIZ,IDEV,NPA,NPRO,NPWR,PX,EY,
     $                  PPROP,KVEL,KRPM,KALT)
C
      REAL PX(NPA),PY(NPA),EY(8,NPA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/0.4,1.4/ ! End points of legend lines
C
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Y axis fixed at 0 to 1
C
      YRANGE = 1.0     ! Range of values on y-axis 
      ANNLOY = 0.0     ! First value on y axis 
      YOFSET = 0.0     ! No y-axis offset
C
      YSCALE = PLHIGH/YRANGE       ! Coordinates Y scale
      INVALY = 10                  ! No of intervals on Y-axis
C
      DYANN = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
      DELTY = YRANGE/FLOAT(INVALY) ! Delta annotations Y-axis
C
C
C----Set up X-axis------------------------------------------------------
C----Min and max powers
C
      XMIN=PX(1)
      XMAX=PX(NPWR)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C----------------------------------------------------------------------
C----Plot Routines
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX, 1)
C
C----Annotate axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLMATHABS(0.17,4.36, 0.35,'h',0.0,1)
      CALL PLCHARABS(4.9,0.3,CHTIT,'POWER-hp',0.0,-1)
C
C----Operating point
C
      RVEL=KVEL    ! Convert to real
      RRPM=KRPM
      RALT=KALT
C
      IF(KVEL .LT.100) THEN
         VELX = 1.0
      ELSE
         VELX = 1.14
      END IF
C
      RPMX = 3.48
C
      IF(KALT .LT. 10) THEN
         ALTX = 5.56
      ELSE IF(KALT .LT.100) THEN
         ALTX = 5.7
      ELSE IF(KALT .LT.1000) THEN
         ALTX = 5.84
      ELSE IF(KALT .LT.10000) THEN
         ALTX = 5.98
      ELSE
         ALTX = 6.12
      END IF
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RVEL, 0.,-1)
      CALL PLCHAR(VELX,6.6, CHOP,'KTS',0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RRPM, 0.,-1)
      CALL PLCHAR(RPMX,6.6, CHOP,'RPM',0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RALT, 0.,-1)
      CALL PLCHAR(ALTX,6.6, CHOP,'FT', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot efficiency curves----------------------------------------------
C
      DO 10 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NPWR
            PY(I)=EY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)=VERTLB+VERTLD*(NPRO-J)
         ENDDO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NPWR,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 10   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END     ! PEFPWR



C------------------------------------------------------------------------
C       PEFRPM - Plotting Efficiency vs RPM in ESPROP
C-----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NRA    - Array size for RPMs (same as NRPMA)
C       NPRO   - No of curves (propellers) to be plotted (max 8)
C       NRPM   - No of coordinates points (RPMs) per curve
C       PX     - Array of x-axis (RPM) points (NRPM)
C       EY     - Array of y-axis (efficiency values) (NPRO x NRPM)
C       PPROP  - Array of names of propellers (NPRO)
C       KVEL   - Velocity operating point
C       KPWR   - Power operating point
C       KALT   - Altitude operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PEFRPM(RELSIZ,IDEV,NRA,NPRO,NRPM,PX,EY,
     $                  PPROP,KVEL,KPWR,KALT)
C
      REAL PX(NRA),PY(NRA),EY(8,NRA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/6.4,7.4/ ! End points of legend lines
C
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Y axis fixed at 0 to 1
C
      YRANGE = 1.0     ! Range of values on y-axis 
      ANNLOY = 0.0     ! First value on y axis 
      YOFSET = 0.0     ! No y-axis offset
C
      YSCALE = PLHIGH/YRANGE       ! Coordinates Y scale
      INVALY = 10                  ! No of intervals on Y-axis
C
      DYANN = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
      DELTY = YRANGE/FLOAT(INVALY) ! Delta annotations Y-axis
C
C
C----Set up X-axis------------------------------------------------------
C----Min and max RPMs
C
      XMIN=PX(1)
      XMAX=PX(NRPM)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C
C-----Plot Routines----------------------------------------------------------
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX, 1)
C
C----Annotate axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLMATHABS(0.17,4.36, 0.35,'h',0.0,1)
      CALL PLCHARABS(5.4,0.3,CHTIT,'RPM',0.0,-1)
C
C----Operating point
C
      RVEL=KVEL    ! Convert to real
      RPWR=KPWR
      RALT=KALT
C
      IF(KVEL .LT.100) THEN
         VELX = 1.0
      ELSE
         VELX = 1.14
      END IF
C
      IF(KPWR .LT.100) THEN
         PWRX = 3.2
      ELSE
         PWRX = 3.34
      END IF
C
      IF(KALT .LT. 10) THEN
         ALTX = 5.56
      ELSE IF(KALT .LT.100) THEN
         ALTX = 5.7
      ELSE IF(KALT .LT.1000) THEN
         ALTX = 5.84
      ELSE IF(KALT .LT.10000) THEN
         ALTX = 5.98
      ELSE
         ALTX = 6.12
      END IF
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RVEL, 0.,-1)
      CALL PLCHAR(VELX,6.6, CHOP,'KTS',0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RPWR, 0.,-1)
      CALL PLCHAR(PWRX,6.6, CHOP,'HP', 0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RALT, 0.,-1)
      CALL PLCHAR(ALTX,6.6, CHOP,'FT', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot efficiency curves----------------------------------------------
C
      DO 10 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NRPM
            PY(I)=EY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)=VERTLB+VERTLD*(NPRO-J)
         ENDDO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NRPM,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 10   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END     ! PEFRPM



C------------------------------------------------------------------------
C       PEFALT - Plotting Efficiency vs Altitude in ESPROP 
C-----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NAA    - Array size for altitudes (same as NALTA)
C       NPRO   - No of curves (propellers) to be plotted (max 8)
C       NALT   - No of coordinates points (altitudes) per curve
C       PX     - Array of x-axis (altitude) points (NALT)
C       EY     - Array of y-axis (efficiency values) (NPRO x NALT)
C       PPROP  - Array of names of propellers (NPRO)
C       KVEL   - Velocity operating point
C       KPWR   - Power operating point
C       KRPM   - RPM operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PEFALT(RELSIZ,IDEV,NAA,NPRO,NALT,PX,EY,
     $                  PPROP,KVEL,KPWR,KRPM)
C
      REAL PX(NAA),PY(NAA),EY(8,NAA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/0.4,1.4/ ! End points of legend lines
C
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Y axis fixed at 0 to 1
C
      YRANGE = 1.0     ! Range of values on y-axis 
      ANNLOY = 0.0     ! First value on y axis 
      YOFSET = 0.0     ! No y-axis offset
C
      YSCALE = PLHIGH/YRANGE       ! Coordinates Y scale
      INVALY = 10                  ! No of intervals on Y-axis
C
      DYANN = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
      DELTY = YRANGE/FLOAT(INVALY) ! Delta annotations Y-axis
C
C
C----Set up X-axis------------------------------------------------------
C----Min and max altitudes
C
      XMIN=PX(1)
      XMAX=PX(NALT)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C
C-----Plot Routines----------------------------------------------------------
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX, 1)
C
C----Annotate axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLMATHABS(0.17,4.36, 0.35,'h',0.0,1)
      CALL PLCHARABS(4.85,0.3,CHTIT,'ALTITUDE-ft',0.0,-1)
C
C----Operating point
C
      RVEL=KVEL    ! Convert to real
      RPWR=KPWR
      RRPM=KRPM
C
      IF(KVEL .LT.100) THEN
         VELX = 1.0
      ELSE
         VELX = 1.14
      END IF
C
      IF(KPWR .LT.100) THEN
         PWRX = 3.2
      ELSE
         PWRX = 3.34
      END IF

      RPMX = 5.98
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RVEL, 0.,-1)
      CALL PLCHAR(VELX,6.6, CHOP,'KTS', 0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RPWR, 0.,-1)
      CALL PLCHAR(PWRX,6.6, CHOP,'HP',0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RRPM, 0.,-1)
      CALL PLCHAR(RPMX,6.6, CHOP,'RPM', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot efficiency curves----------------------------------------------
C
      DO 10 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NALT
            PY(I)=EY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)=VERTLB+VERTLD*(NPRO-J)
         ENDDO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NALT,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 10   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END     ! PEFALT





C----THRUST PLOT ROUTINES
C
C---------------------------------------------------------------------
C       PTHVEL - Plotting Thrust vs Velocity from ESPROP
C----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NVA    - Array size for velocities (same as NVELA)
C       NPRO   - No of curves (propellers) to be plotted (1-8)
C       NVEL   - No of coordinates points (velocities) per curve
C       PX     - Array of x-axis (velocity) points (NVEL)
C       TY     - Array of y-axis (thrust values) (NPRO x NVEL)
C       PPROP  - Array of names of propellers (NPRO)
C       KPWR   - Power operating point
C       KRPM   - RPM operating point
C       KALT   - Altitude operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PTHVEL(RELSIZ,IDEV,NVA,NPRO,NVEL,PX,TY,
     $                  PPROP,KPWR,KRPM,KALT)
C
      REAL PX(NVA),PY(NVA),TY(8,NVA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/6.4,7.4/
C
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Set up X-axis------------------------------------------------------
C----Min and max velocities
C
      XMIN=PX(1)
      XMAX=PX(NVEL)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C
C----Set up Y-axis------------------------------------------------------
C----Find min and max thrusts
C
      YMAX=0.0
      YMIN=100000.0
C
      DO 40 I=1,NPRO
         DO 30 J=1,NVEL
            IF(TY(I,J) .GT. YMAX) YMAX = TY(I,J)
            IF(TY(I,J) .LT. YMIN) YMIN = TY(I,J)
 30      CONTINUE
 40   CONTINUE
C
      YRANGE=YMAX-YMIN
C
C----Specify DELTY
C
      CALL GDELTY(YRANGE,DELTY)
C
C----Calculate Y axis ranges
C
      ANNHIY = INT((YMAX+DELTY)/DELTY) * DELTY      ! High annotation Y
      ANNLOY = INT(YMIN/DELTY) * DELTY              ! Low annotation Y
      INVALY = INT((ANNHIY-ANNLOY)/DELTY+0.001)     ! No of intervals Y
C
      DYANN  = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
C
C----Scale and offset - Y axis
C
      YSCALE = PLHIGH/(ANNHIY-ANNLOY)
      YOFSET = ANNLOY
C
C
C----Plot routines------------------------------------------------------
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX,-1)
C
C----Annotate axes
C
      ROTATE = 90.0
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHARABS(0.35,3.48, CHTIT, 'THRUST-kgf',ROTATE, -1)
      CALL PLCHARABS(4.7,0.3, CHTIT, 'VELOCITY-knots',0.,-1)
C
C----Operating point
C
      RPWR=KPWR   ! Convert to real
      RRPM=KRPM
      RALT=KALT
C
      IF(KPWR .LT.100) THEN
         PWRX = 1.0
      ELSE
         PWRX = 1.14
      END IF
C
      RPMX = 3.48
C
      IF(KALT .LT. 10) THEN
         ALTX = 5.56
      ELSE IF(KALT .LT.100) THEN
         ALTX = 5.7
      ELSE IF(KALT .LT.1000) THEN
         ALTX = 5.84
      ELSE IF(KALT .LT.10000) THEN
         ALTX = 5.98
      ELSE
         ALTX = 6.12
      END IF
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RPWR, 0.,-1)
      CALL PLCHAR(PWRX,6.6, CHOP,'HP', 0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RRPM, 0.,-1)
      CALL PLCHAR(RPMX,6.6, CHOP,'RPM',0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RALT, 0.,-1)
      CALL PLCHAR(ALTX,6.6, CHOP,'FT', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot thrust curves------------------------------------------------
C
      DO 50 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NVEL
            PY(I)=TY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)= PLHIGH - VERTLT - VERTLD*(J-1)
         END DO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NVEL,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 50   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END      ! PTHVEL



C---------------------------------------------------------------------
C       PTHPWR - Plotting Thrust vs Power from ESPROP
C----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NPA    - Array size for powers (same as NPWRA)
C       NPRO   - No of curves (propellers) to be plotted (1-8)
C       NPWR   - No of coordinates points (powers) per curve
C       PX     - Array of x-axis (power) points (NPWR)
C       TY     - Array of y-axis (thrust values) (NPRO x NPWR)
C       PPROP  - Array of names of propellers (NPRO)
C       KVEL   - Velocity operating point
C       KRPM   - RPM operating point
C       KALT   - Altitude operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PTHPWR(RELSIZ,IDEV,NPA,NPRO,NPWR,PX,TY,
     $                  PPROP,KVEL,KRPM,KALT)
C
      REAL PX(NPA),PY(NPA),TY(8,NPA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/6.4,7.4/
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Set up X-axis------------------------------------------------------
C----Min and max powers
C
      XMIN=PX(1)
      XMAX=PX(NPWR)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C
C----Set up Y-axis------------------------------------------------------
C----Find min and max thrusts
C
      YMAX=0.0
      YMIN=100000.0
C
      DO 40 I=1,NPRO
         DO 30 J=1,NPWR
            IF(TY(I,J) .GT. YMAX) YMAX = TY(I,J)
            IF(TY(I,J) .LT. YMIN) YMIN = TY(I,J)
 30      CONTINUE
 40   CONTINUE
C
      YRANGE=YMAX-YMIN
C
C----Get DELTY
C
      CALL GDELTY(YRANGE,DELTY)
C
C----Calculate Y axis ranges
C
      ANNHIY = INT((YMAX+DELTY)/DELTY) * DELTY      ! High annotation Y
      ANNLOY = INT(YMIN/DELTY) * DELTY              ! Low annotation Y
      INVALY = INT((ANNHIY-ANNLOY)/DELTY+0.001)     ! No of intervals Y
C
      DYANN  = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
C
C----Scale and offset - Y axis
C
      YSCALE = PLHIGH/(ANNHIY-ANNLOY)
      YOFSET = ANNLOY
C
C
C----Plot routines------------------------------------------------------
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX,-1)
C
C----Annotate axes
C
      ROTATE = 90.0
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHARABS(0.35,3.48, CHTIT, 'THRUST-kgf',ROTATE, -1)
      CALL PLCHARABS(4.9,0.3, CHTIT, 'POWER-hp',0.,-1)
C
C----Operating point
C
      RVEL=KVEL    ! Convert to real
      RRPM=KRPM
      RALT=KALT
C
      IF(KVEL .LT.100) THEN
         VELX = 1.0
      ELSE
         VELX = 1.14
      END IF
C
      RPMX = 3.48
C
      IF(KALT .LT. 10) THEN
         ALTX = 5.56
      ELSE IF(KALT .LT.100) THEN
         ALTX = 5.7
      ELSE IF(KALT .LT.1000) THEN
         ALTX = 5.84
      ELSE IF(KALT .LT.10000) THEN
         ALTX = 5.98
      ELSE
         ALTX = 6.12
      END IF
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RVEL, 0.,-1)
      CALL PLCHAR(VELX,6.6, CHOP,'KTS',0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RRPM, 0.,-1)
      CALL PLCHAR(RPMX,6.6, CHOP,'RPM',0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RALT, 0.,-1)
      CALL PLCHAR(ALTX,6.6, CHOP,'FT', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot thrust curves------------------------------------------------
C
      DO 50 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NPWR
            PY(I)=TY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)=VERTLB+VERTLD*(NPRO-J)
         ENDDO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NPWR,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 50   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END      ! PTHPWR



C---------------------------------------------------------------------
C       PTHRPM - Plotting Thrust vs RPM from ESPROP
C----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NRA    - Array size for RPMs (same as NRPMA)
C       NPRO   - No of curves (propellers) to be plotted (1-8)
C       NRPM   - No of coordinates points (RPMs) per curve
C       PX     - Array of x-axis (RPM) points (NRPM)
C       TY     - Array of y-axis (thrust values) (NPRO x NRPM)
C       PPROP  - Array of names of propellers (NPRO)
C       KVEL   - Velocity operating point
C       KPWR   - Power operating point
C       KALT   - Altitude operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PTHRPM(RELSIZ,IDEV,NRA,NPRO,NRPM,PX,TY,
     $                  PPROP,KVEL,KPWR,KALT)
C
      REAL PX(NRA),PY(NRA),TY(8,NRA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/6.4,7.4/
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Set up X-axis------------------------------------------------------
C----Min and max RPMs
C
      XMIN=PX(1)
      XMAX=PX(NRPM)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C
C----Set up Y-axis------------------------------------------------------
C----Find min and max thrusts
C
      YMAX=0.0
      YMIN=100000.0
C
      DO 40 I=1,NPRO
         DO 30 J=1,NRPM
            IF(TY(I,J) .GT. YMAX) YMAX = TY(I,J)
            IF(TY(I,J) .LT. YMIN) YMIN = TY(I,J)
 30      CONTINUE
 40   CONTINUE
C
      YRANGE=YMAX-YMIN
C
C----Specify DELTY
C
      CALL GDELTY(YRANGE,DELTY)
C
C----Calculate Y axis ranges
C
      ANNHIY = INT((YMAX+DELTY)/DELTY) * DELTY      ! High annotation Y
      ANNLOY = INT(YMIN/DELTY) * DELTY              ! Low annotation Y
      INVALY = INT((ANNHIY-ANNLOY)/DELTY+0.001)     ! No of intervals Y
C
      DYANN  = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
C
C----Scale and offset - Y axis
C
      YSCALE = PLHIGH/(ANNHIY-ANNLOY)
      YOFSET = ANNLOY
C
C
C----Plot routines------------------------------------------------------
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX,-1)
C
C----Annotate axes
C
      ROTATE = 90.0
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHARABS(0.35,3.48, CHTIT, 'THRUST-kgf',ROTATE, -1)
      CALL PLCHARABS(5.4,0.3, CHTIT, 'RPM',0.,-1)
C
C----Operating point
C
      RVEL=KVEL    ! Convert to real
      RPWR=KPWR
      RALT=KALT
C
      IF(KVEL .LT.100) THEN
         VELX = 1.0
      ELSE
         VELX = 1.14
      END IF
C
      IF(KPWR .LT.100) THEN
         PWRX = 3.2
      ELSE
         PWRX = 3.34
      END IF
C
      IF(KALT .LT. 10) THEN
         ALTX = 5.56
      ELSE IF(KALT .LT.100) THEN
         ALTX = 5.7
      ELSE IF(KALT .LT.1000) THEN
         ALTX = 5.84
      ELSE IF(KALT .LT.10000) THEN
         ALTX = 5.98
      ELSE
         ALTX = 6.12
      END IF
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RVEL, 0.,-1)
      CALL PLCHAR(VELX,6.6, CHOP,'KTS',0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RPWR, 0.,-1)
      CALL PLCHAR(PWRX,6.6, CHOP,'HP', 0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RALT, 0.,-1)
      CALL PLCHAR(ALTX,6.6, CHOP,'FT', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot thrust curves------------------------------------------------
C
      DO 50 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NRPM
            PY(I)=TY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)=VERTLB+VERTLD*(NPRO-J)
         ENDDO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NRPM,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 50   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END      ! PTHRPM



C---------------------------------------------------------------------
C       PTHALT - Plotting Thrust vs Altitude from ESPROP
C----------------------------------------------------------------------
C       RELSIZ - Relative screen size of plot window
C       IDEV   - Output device (XWin=1, B&W ps=2, Color ps=4)
C       NAA    - Array size for altitudes (same as NALTA)
C       NPRO   - No of curves (propellers) to be plotted (1-8)
C       NALT   - No of coordinates points (altitudes) per curve
C       PX     - Array of x-axis (altitude) points (NALT)
C       TY     - Array of y-axis (thrust values) (NPRO x NALT)
C       PPROP  - Array of names of propellers (NPRO)
C       KVEL   - Velocity operating point
C       KPWR   - Power operating point
C       KRPM   - RPM operating point
C------------------------------------------------------------------------
C
      SUBROUTINE PTHALT(RELSIZ,IDEV,NAA,NPRO,NALT,PX,TY,
     $                  PPROP,KVEL,KPWR,KRPM)
C
      REAL PX(NAA),PY(NAA),TY(8,NAA)
      CHARACTER PPROP(8)*32
C
      REAL QX(2),QY(2)
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT 
C
      DATA QX/0.4,1.4/
      DATA LMASK1, LMASK2 / -32640, -30584 /
C
      CALL PLIN
C
C----Set up X-axis------------------------------------------------------
C----Min and max altitudes
C
      XMIN=PX(1)
      XMAX=PX(NALT)
      XRANGE=XMAX-XMIN
C
C----Get DELTX
C
      CALL GDELTX(XRANGE,DELTX)
C
C----Calculate X axis ranges
C
      ANNHIX = INT((XMAX+DELTX)/DELTX) * DELTX      ! High annotation X
      ANNLOX = INT(XMIN/DELTX) * DELTX              ! Low annotation X
      INVALX = INT((ANNHIX-ANNLOX)/DELTX+0.001)     ! No of intervals X
C
      DXANN  = PLWIDE/FLOAT(INVALX) ! Distance between X grid/annotations 
C
C----Scale and offset - X axis
C
      XSCALE = PLWIDE/(ANNHIX-ANNLOX)
      XOFSET = ANNLOX
C
C
C----Set up Y-axis------------------------------------------------------
C----Find min and max thrusts
C
      YMAX=0.0
      YMIN=100000.0
C
      DO 40 I=1,NPRO
         DO 30 J=1,NALT
            IF(TY(I,J) .GT. YMAX) YMAX = TY(I,J)
            IF(TY(I,J) .LT. YMIN) YMIN = TY(I,J)
 30      CONTINUE
 40   CONTINUE
C
      YRANGE=YMAX-YMIN
C
C----Get DELTY
C
      CALL GDELTY(YRANGE,DELTY)
C
C----Calculate Y axis ranges
C
      ANNHIY = INT((YMAX+DELTY)/DELTY) * DELTY      ! High annotation Y
      ANNLOY = INT(YMIN/DELTY) * DELTY              ! Low annotation Y
      INVALY = INT((ANNHIY-ANNLOY)/DELTY+0.001)     ! No of intervals Y
C
      DYANN  = PLHIGH/FLOAT(INVALY) ! Distance between Y grid/annotations 
C
C----Scale and offset - Y axis
C
      YSCALE = PLHIGH/(ANNHIY-ANNLOY)
      YOFSET = ANNLOY
C
C
C----Plot routines------------------------------------------------------
C
      CALL PLOPEN(RELSIZ,NUNIT,IDEV)
      CALL NEWORIGIN(XORG,YORG)
C
C----Set up grid
C
      CALL NEWPEN(1)
      CALL NEWCOLORNAME('green')
      CALL PLGRID(0.0,0.0,INVALX,DXANN,INVALY,DYANN,LMASK2)
C
C----Set up axes
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL XAXIS(0.,0.,-PLWIDE,DXANN,ANNLOX,DELTX,CHAX,-1)
      CALL YAXIS(0.,0.,-PLHIGH,DYANN,ANNLOY,DELTY,CHAX,-1)
C
C----Annotate axes
C
      ROTATE = 90.0
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHARABS(0.35,3.48, CHTIT, 'THRUST-kgf',ROTATE, -1)
      CALL PLCHARABS(4.85,0.3,CHTIT,'ALTITUDE-ft',0.0,-1)
C
C----Operating point
C
      RVEL=KVEL    ! Convert to real
      RPWR=KPWR
      RRPM=KRPM
C
      IF(KVEL .LT.100) THEN
         VELX = 1.0
      ELSE
         VELX = 1.14
      END IF
C
      IF(KPWR .LT.100) THEN
         PWRX = 3.2
      ELSE
         PWRX = 3.34
      END IF

      RPMX = 5.98
C
C----Annotate operating point
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('blue')
C
      CALL PLNUMB(0.6, 6.6, CHOP,RVEL, 0.,-1)
      CALL PLCHAR(VELX,6.6, CHOP,'KTS', 0.,-1)
C
      CALL PLNUMB(2.8, 6.6, CHOP,RPWR, 0.,-1)
      CALL PLCHAR(PWRX,6.6, CHOP,'HP',0.,-1)
C
      CALL PLNUMB(5.3, 6.6, CHOP,RRPM, 0.,-1)
      CALL PLCHAR(RPMX,6.6, CHOP,'RPM', 0.,-1)
C
C----Annotate data source
C
      CALL NEWPEN(2)
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(8.0,6.6,CHEP,'Plotted by ESPROP', 0.,-1)
C
      CALL NEWCOLORNAME('black')
      CALL PLCHAR(9.56,1.8,CHXR,'Calculated by XROTOR',-90.,-1)
C
C----Plot thrust curves------------------------------------------------
C
      DO 50 J=1,NPRO
C
         NCOL=J+2
C
         DO I=1,NALT
            PY(I)=TY(J,I)
         ENDDO
C
         DO I=1,2
           QY(I)=VERTLB+VERTLD*(NPRO-J)
         ENDDO
C
         CHLX=QX(2)+0.1
         CHLY=QY(1)-0.05
C
         CALL NEWCOLOR(NCOL)
C
         CALL NEWPEN(3)
         CALL XYLINE(NALT,PX,PY,XOFSET,XSCALE,YOFSET,YSCALE,J)
         CALL XYLINE(2,QX,QY,0.,1.,0.,1.,J)
C
         CALL NEWPEN(2)
         CALL PLCHAR(CHLX,CHLY,CHLEG,PPROP(J),0.,-1)
C
 50   CONTINUE
C
C---Finish
C
      CALL PLFLUSH
      CALL PLEND
C
      RETURN
      END      ! PTHALT




C------------------------------------------------------------------------
C----PLIN - Common constants for plot routines
C
      SUBROUTINE PLIN
C
      COMMON/PLOTIN/CHAX,CHTIT,CHLEG,CHOP,CHXR,CHEP,PLWIDE,PLHIGH,
     $              XORG,YORG,VERTLD,VERTLB,VERTLT,NUNIT    
C
      CHAX = 0.11      ! Character width for axis numbers
      CHTIT= 0.14      ! Character width for axis titles
      CHLEG= 0.11      ! Character width for legend 
      CHOP = 0.14      ! Character width for operating points
      CHXR = 0.09      ! Character width for XROTOR plug
      CHEP = 0.09      ! Character width for ESPROP plug
C
      PLWIDE = 9.5     ! Width of plot region
      PLHIGH = 6.5     ! Height of plot region
C
      XORG   =  0.90   ! Coordinate system X offset
      YORG   =  0.90   ! Coordinate system Y offset
C
      VERTLD = 0.3     ! Vertical space between legends
      VERTLB = 0.25    ! Last legend space from bottom
      VERTLT = 0.5     ! First legend space from top
C
      NUNIT  = 0       ! Postscript to plot.ps
C
      RETURN
      END  ! PLIN


C-------------------------------------------------------------------------
C----GDELTX
C----Common setup of x-axis delta
C
      SUBROUTINE GDELTX(XRANGE,DELTX)
C
      IF      (XRANGE .LE. 16.0) THEN
         DELTX = 1.0
      ELSE IF (XRANGE .LE. 32.0) THEN
         DELTX = 2.0
      ELSE IF (XRANGE .LE. 80.0) THEN
         DELTX = 5.0
      ELSE IF (XRANGE .LE. 160.0) THEN
         DELTX = 10.0
      ELSE IF (XRANGE .LE. 320.0) THEN
         DELTX = 20.0
      ELSE IF (XRANGE .LE. 400.0) THEN
         DELTX = 25.0
      ELSE IF (XRANGE .LE. 800.0) THEN
         DELTX = 50.0
      ELSE IF (XRANGE .LE. 1600.0) THEN
         DELTX = 100.0
      ELSE IF (XRANGE .LE. 3200.0) THEN
         DELTX = 200.0
      ELSE IF (XRANGE .LE. 4000.0) THEN
         DELTX = 250.0
      ELSE IF (XRANGE .LE. 8000.0) THEN
         DELTX = 500.0
      ELSE IF (XRANGE .LE. 16000.0) THEN
         DELTX = 1000.0
      ELSE IF (XRANGE .LE. 32000.0) THEN
         DELTX = 2000.0
      ELSE IF (XRANGE .LE. 40000.0) THEN
         DELTX = 2500.0
      ELSE
         DELTX = 5000.0
      END IF
C
      RETURN
      END ! GDELTX
C
C----------------------------------------------------------------------------
C----GDELTY
C----Common setup of y-axis delta
C
      SUBROUTINE GDELTY(YRANGE,DELTY)
C
      IF      (YRANGE .LE. 13.0) THEN
         DELTY = 1.0
      ELSE IF (YRANGE .LE. 26.0) THEN
         DELTY = 2.0
      ELSE IF (YRANGE .LE. 65.0) THEN
         DELTY = 5.0
      ELSE IF (YRANGE .LE. 130.0) THEN
         DELTY = 10.0
      ELSE IF (YRANGE .LE. 260.0) THEN
         DELTY = 20.0
      ELSE IF (YRANGE .LE. 325.0) THEN
         DELTY = 25.0
      ELSE IF (YRANGE .LE. 650.0) THEN
         DELTY = 50.0
      ELSE IF (YRANGE .LE. 1300.0) THEN
         DELTY = 100.0
      ELSE IF (YRANGE .LE. 2600.0) THEN
         DELTY = 200.0
      ELSE IF (YRANGE .LE. 3250.0) THEN
         DELTY = 250.0
      ELSE IF (YRANGE .LE. 6500.0) THEN
         DELTY = 500.0
      ELSE
         DELTY = 1000.0
      END IF
C
      RETURN
      END ! GDELTY
C
C------------------------------------------------------------------------------
