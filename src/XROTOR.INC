C
C---- ROTOR.INC include file
C
      IMPLICIT REAL (M)
C
C--- IX - max number of radial prop stations 
C--- ICASX - max number of stored cases
C--- NPARX - number of case parameters stored
C--- IWX - dimension of work arrays
      PARAMETER ( IX=100, IXP=IX+1, NPARX=12, ICASX=100, IWX=200)
C
C--- NAX - max number of aerodynamic sections defined
C--- NDX - number of aerodynamic parameter defined for each section
      PARAMETER ( NAX=20, NDX=14 )
C
      PARAMETER ( IQ=IX+5, JX=(IQ*3)/2+1 )
      PARAMETER (PI=3.141592654)
      DIMENSION Q(IQ,IQ)
      LOGICAL CONV, GREEK, TERSE, VRTX, FAST, FREE, DUCT, LSTRUC,
     &        WIND, DEST, DESP, STALL, LDESINI, LOPRINI, 
     &        LROTOR, LVNORM, LPWRVAR, LEGEND
C
      REAL MCRIT
C
      LOGICAL LPLOT,LGRID,LLAND,LCURS,LSLOPE,LPLOFF
      CHARACTER*80 SAVFIL, FNAME
      CHARACTER*32 NAME
C
      COMMON/UNIT_X/ LUREAD,LUWRIT,LUTEMP,LUSAVE
C
      COMMON/LOGI_X/ CONV,GREEK,TERSE,VRTX,FAST,FREE,DUCT,LSTRUC,
     &               LDESINI,LOPRINI,LROTOR,LVNORM,LPWRVAR,
     &               WIND,DEST,DESP,STALL(IX),LEGEND
C
      COMMON/CHAR_X/ SAVFIL,FNAME,NAME
C
      COMMON/DIME_X/ RHO,RMU,VSO,VEL,RAD,GEE,ALT
C
      COMMON/ICON_X/ II,IINF,INCR,NN,NBLDS,IXSPAC, 
     &               NITERD,NITERA
      COMMON/RCON_X/ VERSION,DT
C
      COMMON/IGEOM_X/ IAERO(IX)
C
      COMMON/RGEOM_X/ CH(IX),BETA(IX),BETA0(IX),T(IX),DBETA,
     &                XI(IX),DXI(IX),XI0,XITIP,XINF,
     &                XPITCH, XV(IX), RAKE
C
      COMMON/IVELS_X/ NADD
      COMMON/RVELS_X/ RADD(IX),
     &                UADD(IX), VADD(IX),
     &                UADDR(IX),VADDR(IX),
     &                UBODY(IX),VBODY(IX),URDUCT
C
      COMMON/AERO_X/ CL(IX),CD(IX),CM(IX),
     &               RE(IX),EFFP(IX),GAM(IX),
     &               DTII(IX), DPII(IX),
     &               DTVI(IX), DPVI(IX),
     &               DTWI(IX), DPWI(IX)
C
      COMMON/IFOIL_X/ NAERO
      COMMON/RFOIL_X/ XIAERO(NAX),AERODATA(NDX,NAX)
C
      COMMON/LOAD_X/ PX(IX),PY(IX),PZ(IX),
     &               MX(IX),MY(IX),MZ(IX),
     &               PX_TY(IX),PX_TZ(IX),PZ_TX(IX),PZ_TY(IX),PZ_WZ(IX),
     &               MY_TY(IX),MZ_TY(IX),MZ_TX(IX),MZ_WZ(IX),
     &               TX(IXP),TY(IXP),TZ(IXP),
     &               WX(IXP),WY(IXP),WZ(IXP),
     &               SHRX(IXP),SHRY(IXP),SHRZ(IXP),
     &               MOMX(IXP),MOMY(IXP),MOMZ(IXP),
     &               EIXXB(IX),EIYYB(IX),
     &               EAB(IX),GJB(IX),EKB(IX),
     &               MB(IX),MXXB(IX),
     &               XOCG(IX),XOSC(IX),
     &               RSTB(IX)
C
      COMMON/RCAS_X/ CASPAR(0:NPARX,ICASX)
      COMMON/ICAS_X/ NCASE,KCASE,IWTYP
C
      COMMON/OPER_X/ ADV,ADW,ADWFCTR,
     &               RMS,RLX,EFFINV,
     &               TSPEC,PSPEC,QSPEC,
     &               TTOT,PTOT,QTOT,
     &               TINV,PINV,TWAK,PWAK,TVIS,PVIS,
     &               GRESMX,FRESMX,ARESMX
C
      COMMON/SENS_X/ TI_ADV,PI_ADV,TI_ADW,PI_ADW,
     &               TW_ADV,PW_ADV,TW_ADW,PW_ADW,
     &               TV_ADV,PV_ADV,TV_ADW,PV_ADW,TV_DBE,PV_DBE,
     &               TI_GAM(IX),PI_GAM(IX),
     &               TW_GAM(IX),PW_GAM(IX),
     &               TV_GAM(IX),PV_GAM(IX)
C
      COMMON/TEMP_X/ W0(IWX),W1(IWX),W2(IWX),W3(IWX),W4(IWX),
     &               W5(IWX),W6(IWX),W7(IWX),W8(IWX),W9(IWX),
     &               T0(IWX),T1(IWX),T2(IWX),T3(IWX),T4(IWX),
     &               T5(IWX),T6(IWX),T7(IWX),T8(IWX),T9(IWX)
C
      COMMON/IPLO_X/ IDEV,IDEVRP,IPSLU,NCOLOR,
     &               LPLOT,LGRID,LLAND,LCURS,LSLOPE,LPLOFF
      COMMON/RPLO_X/ SIZE,SCRNFR,PAR,CSIZE,
     &               XWIND,YWIND,
     &               XPAGE,YPAGE,XMARG,YMARG,
     &               XYOFF(2),XYFAC(2)
C
      COMMON/DESI_X/ RADDES,VELDES,ADVDES,RPMDES,R0DES,RWDES,
     &               TDDES,PDDES,
     &               TDES,PDES,CLDES0,CLDES(IX)
C
      COMMON/IPVAR_X/ NPWRVAR
      COMMON/RPVAR_X/ RPMVAR(IX),PWRVAR(IX),XPWRVAR(IX)
C
      COMMON/VIND_X/ VWAK(IX), VW_GAM(IX,IX), VW_ADW(IX), VW_ADV(IX),
     &               VIND(3,IX), VIND_GAM(3,IX,IX), VIND_ADW(3,IX)
C
      COMMON/XWAK_X/ XW0,XWTIP,
     &               XW(IX),  XW_GAM(IX,IX), XW_ADW(IX), XW_ADV(IX),
     &               DXW(IX),DXW_GAM(IX,IX),DXW_ADW(IX),DXW_ADV(IX)
C               
      COMMON/DELT_X/ DGAM(IX), RES(IQ), DADV, DADW, DBET, DEFF, DQ(IQ),
     &               DGAMOLD(IX)
C
ccc   EQUIVALENCE (A_GAMJ(0,1),Q(1,1))
C
C-------------------------------------------------------------------------
C
C   LUREAD       Terminal input  unit number (normally 5)
C   LUWRIT       Terminal output unit number (normally 6)
C   LUTEMP       Disk file unit number
C   LUSAVE       Disk save file output unit number
C       
C   RHO         Fluid density            (dimensioned)
C   RMU         Fluid dynamic viscosity  (dimensioned)
C   VSO         Fluid speed of sound     (dimensioned)
C   VEL         Flight speed             (dimensioned)
C   RAD         Rotor tip radius         (dimensioned)
C   GEE         Earth's acceleration     (dimensioned)
C   ALT         Altitude for fluid properties (km),  999.0 if not defined
C       
C   II          Number of radial stations on blade
C   IINF        Number of radial stations on blade + outer domain
C   INCR        Radial station increment for terminal output
C   NN          Number of perturbation potential harmonic terms
C   NBLDS       Number of blades
C
C   IXSPAC      1 = cosine r/R array stretching
C               2 = sine stretching (less spacing near root)
C   
C   CH(.)       Chord array
C   BETA(.)     Twist angle array
C   BETA0(.)    Static twist angle array (with zero structural loads)
C   T(.)        Dummy radial coordinate array
C   DBETA       Accumulated change in twist angle
C   XI(.)       Radial coordinate array (r/R)
C   DXI(.)      Radial coordinate increment at each station
C   XI0         Blade root radial coordinate value
C   XITIP       Blade tip radial coordinate value  (always = 1)
C   XINF        Outer radial coordinate value where farfield BC is applied
C   XPITCH      x/c location of pitch axis for loads calculations and plots
C   IAERO       Index of inboard aero section for aero characteristics
C       
C   CL(.)       Local lift coefficient array
C   CD(.)       Local drag coefficient array
C   CM(.)       Local blade airfoil Cm
C   GAM(.)      Local circulation array
C   STALL(.)    Local profile stall flag array
C   RE(.)       Local Reynolds number array
C   EFFP(.)     Local profile efficiency array
C   
C-- aero data quantities for each defined radial aerodynamic section
C   NAERO       Number of aerodynamic datasets defined (NAERO>=1)
C   XIAERO      Radial station r/R where aero dataset is defined
C   AERODATA    Aerodynamic definition of the blade section at XIAERO
C               AERODATA( 1,x) = A0 (angle of zero lift)
C               AERODATA( 2,x) = CLMAX (Max CL)
C               AERODATA( 3,x) = CLMIN (Min CL)
C               AERODATA( 4,x) = DCLDA (Incompressible 2-D lift curve slope)
C               AERODATA( 5,x) = DCLDA_STALL (2-D lift curve slope at stall)
C               AERODATA( 6,x) = DCL_STALL (CL increment, onset to full stall)
C               AERODATA( 7,x) = CDMIN (Minimum drag coefficient value)
C               AERODATA( 8,x) = CLDMIN (Lift at minimum drag value)
C               AERODATA( 9,x) = DCDCL2 (Parabolic drag param d(Cd)/dCL^2)
C               AERODATA(10,x) = CMCON (Incompressible 2-D pitching moment)
C               AERODATA(11,x) = REREF (reference Reynold's number)
C               AERODATA(12,x) = REXP (Reynold's number exponent Cd~Re^REXP)
C               AERODATA(13,x) = MCRIT (critical Mach #)
C               AERODATA(14,x) = TOC (thickness/chord)
C
C-- structural quantities below are referred to the following axes:
C-    X-axis points backward along rotation axis
C-    Y-axis points radially outward
C-    Z-axis points sideways at right angles to blade  (Z = X x Y)
C
C   PX PY PZ(.)  load/length in X, Y, Z directions (aero + centrifugal)
C   MX MY MZ(.)  moment/length around X, Y, Z axes (aero + centrifugal)
C
C   PX_TY(.)     sensitivities of loadings to deflections
C   PX_TZ(.)
C   PZ_TX(.)       (A_B  denotes dA/dB)
C   PZ_TY(.)
C   PZ_WZ(.)
C   MY_TY(.)
C   MZ_TY(.)
C   MZ_TX(.)
C   MZ_WZ(.)
C
C   TX TY TZ(.)  deflection angles around X, Y, Z axes
C   WX WY WZ(.)  deflections in X, Y, Z directions
C   SHRX SHRY SHRZ(.)  resultant loads (shear,tension) in X Y Z directions
C   MOMX MOMY MOMZ(.)  resultant moments (b.m., torsion) around X Y Z axes
C
C   EIXXB(.)   bending stiffness in the blade airfoil plane   (dimensioned)
C   EIYYB(.)   bending stiffness out of blade airfoil plane   (dimensioned)
C   EAB(.)     extensional stiffness                          (dimensioned)
C   GJB(.)     torsional stiffness                            (dimensioned)
C   EKB(.)     torsion moment/extension strain  stiffness     (dimensioned)
C   MB(.)      mass/length of blade                           (dimensioned)
C   MXXB(.)    pitch-axis moment of inertia/length of blade   (dimensioned)
C   XOCG(.)    x/c of blade section CG
C   XOSC(.)    x/c of blade section shear center
C   RSTB(.)    radius used for post-processing strain display (dimensioned)
C
C   CASPAR(..) case-parameter array
C          0   case run flags
C          1   advance ratio
C          2   velocity
C          3   tip angle
C          4   altitude
C          5   density
C          6   dynamic viscosity
C          7   speed of sound
C          8   power
C          9   thrust
C         10   torque
C         11   efficiency
C
C   NCASE      current number of saved operating cases
C   KCASE      indicator for independent parameter of case sweep
C            0  = none
C            1  = advance ratio
C            2  = rpm
C            3  = velocity
C            4  = blade angle
C
C   ADV         Advance ratio
C   ADW         Wake advance ratio
C   TPSPEC      Specified thrust, torque, or power
C   RMS, RLX    Rms residual and under-relaxation factor
C   EFFINV      1 / Inviscid efficiency
C   IWTYP       Type of induced velocity model emplyed currently
C                 1 = Graded Momentum,  2 = Potential Formulation
C              
C   TTOT        Rotor inviscid + viscous + nacelle thrust
C   PTOT        Rotor inviscid + viscous + nacelle power
C   QTOT        Rotor inviscid + viscous + nacelle torque  = PTOT*ADV
C   TINV, PINV  Inviscid thrust, power
C   TWAK, PWAK  Inviscid + nacelle thrust, power
C   TVIS, PVIS  Viscous thrust, power
C
C                TTOT  =  TVIS +  TWAK
C                      =  TVIS + (TINV + Tnacelle)
C
C       
C   TI_ADV      Sensitivities of TINV,PINV to advance ratio
C   PI_ADV
C   TI_ADW      Sensitivities of TINV,PINV to wake advance ratio
C   PI_ADW
C   TW_ADV      Sensitivities of TWAK,PWAK to advance ratio
C   PW_ADV
C   TW_ADW      Sensitivities of TWAK,PWAK to wake advance ratio
C   PW_ADW
C   TV_ADV      Sensitivities of TVIS,PVIS to advance ratio
C   PV_ADV
C   TV_ADW      Sensitivities of TVIS,PVIS to wake advance ratio
C   PV_ADW
C   TV_DBE      Sensitivities of TVIS,PVIS to blade angle change
C   PV_DBE
C   TI_GAM(.)   Sensitivity arrays to radial circulation distribution
C   PI_GAM(.)        "
C   TW_GAM(.)        "
C   PW_GAM(.)        "
C   TV_GAM(.)        "
C   PV_GAM(.)        "
C       
C   CONV        T if Converged solution exists
C   GREEK       T if Unrecognized command
C   TERSE       T if Terse output (no radial distributions)
C   FAST        T if Graded Momentum, Potential otherwise
C   FREE        T if Free wake
C   DUCT        T if duct is present
C   LDESINI     T if rotor is to be initialized for design each time
C   LOPRINI     T if rotor is to be initialized for analysis each time
C   LROTOR      T if rotor exists
C   LVNORM      T if flight speed V is used for normalization, wR otherwise
C   WIND        T if windmill-mode plotting is to be used
C
C   DEST        Design-to-thrust option flag
C   DESP        Design-to-power  option flag
C       
C   SAVFIL      Disk output save filename
C   FNAME       Generic filename
C   NAME        Case name
C       
C   W0-9(.)     Temporary work arrays
C   T0-9(.)     Temporary work arrays
C       
C   SIZE        Plot width in inches
C   SCRNFR      Fraction of screen taken up by graphics window
C   PAR         Plot height/width aspect ratio
C   CSIZE       Character size/plot width ratio
C   XWIND,YWIND Dimensions of X-window
C   XPAGE,YPAGE Dimensions of PostScript output page
C   XMARG,YMARG Margins on plot page
C   XYOFF(.)    x,y plot offsets  for current x-y plot, if any
C   XYFAC(.)    x,y scale factors for current x-y plot, if any
C
C   IDEV        Screen window flag
C   IDEVRP      Replotting flag
C   IPSLU       PostScript output-file flag
C   NCOLOR      Number of colors in spectrum
C
C   LPLOT       T if a plot is active
C   LGRID       T if grid is to be overlaid on x,y plots
C   LLAND       T if plots are in Landscape mode
C   LSLOPE      T if slope matching is to be used in cursor-modifications
C
C   RADDES      Design rotor radius             (dimensioned)
C   VELDES      Design speed                    (dimensioned)
C   ADVDES      Design advance ratio            (dimensioned)
C   RPMDES      Design advance ratio            (dimensioned)
C   R0DES       Design root radius              (dimensioned)
C   RWDES       Design disp. body root radius   (dimensioned)
C   TDDES       Design thrust                   (dimensioned)
C   PDDES       Design power                    (dimensioned)
C   TDES        Design thrust
C   PDES        Design power
C   CLDES0      Constant design CL
C   CLDES(.)    Radial design-CL array
C       
C   VTAN(.)     Tangential induced velocity array
C   VWAK(.)     Equivalent-rotor tangential induced velocity array
C   VT_GAM(..)  VTAN-blade bound circulation  sensitivity matrix
C   VT_ADW(.)   VTAN-wake advance ratio sensitivity matrix
C   VW_GAM(..)  VWAK-blade bound circulation  sensitivity matrix
C   VW_ADW(.)   VWAK-wake advance ratio sensitivity matrix
C   VW_ADV(.)   VWAK-advance ratio sensitivity matrix
C   XW0         Root radius of equivalent rotor (= far wake disp. body radius)
C   XWTIP       Tip radius of equivalent rotor
C   XW(.)       Equivalent-rotor radial coordinate array
C   XW_GAM(..)  XW-blade bound circulation  sensitivity matrix
C   XW_ADW(.)   XW-wake advance ratio sensitivity matrix
C   XW_ADV(.)   XW-advance ratio sensitivity matrix
C   DXW(.)      Equivalent-rotor radial coordinate increment array
C   DXW_GAM(..) DXW-blade bound circulation  sensitivity matrix
C   DXW_ADW(.)  DXW-wake advance ratio sensitivity matrix
C   DXW_ADV(.)  DXW-advance ratio sensitivity matrix
C   
C   DGAM(.)     Newton update delta array for bound circulation
C   RES(.)      Newton residual array for bound circulation
C   DADV        Newton update delta for advance ratio
C   DADW        Newton update delta for wake advance ratio
C   DBET        Newton update delta for blade angle
C   DEFF        Newton update delta for 1 / inviscid efficiency
C   DQ(.)       Generic solution vector
C   
C   UBODY(.)    Nacelle perturbation axial  velocity
C   VBODY(.)    Nacelle perturbation radial velocity
C   ABODY(.)    Nacelle cross-sectional area array
C   ZBODY(.)    Nacelle streamwise coordinate
C   NZ          Number of nacelle streamwise stations
C
C
C---------------------------------------------------------------------
C    Counter-Rotation extensions
C    Philip Carter, Esotec Developments, July 2011
C---------------------------------------------------------------------
      CHARACTER*80 FNAMEF, FNAMEA
      CHARACTER*32 CRNAME, NAMECR(2), BNAME, EVERSION
      LOGICAL LRF,LRA,LCRIN,LFWD,LFILER,LCONF,LCONA,LCON,LNCON,LVCLR,
     &   LPITCH,LSRPM,LBA,LBC,LAUTO,LSWIT,LDEF,LROTF,LROTA,LFORWARD,
     &   LESPARA
C
      COMMON/LCR/ LFWD,LFILER,LCONF,LCONA,LCON,LNCON,LVCLR,LFORWARD,
     &            LPITCH,LSRPM,LBA,LBC,LDEF,LROTF,LROTA,LESPARA
C
      COMMON/CHARCR/ FNAMEF, FNAMEA, CRNAME, NAMECR, EVERSION
C
      COMMON/RCR/ UWT,VWT,UWTF,UWTA,VWTF,VWTA,POWERF,POWERA,
     &            RPMF,RPMA,CRCON,CRCLR,CRCLT,BLENDF,VELCR,
     &            CRANGE,CLOC,PLOC,RRAT
C
      COMMON/RCRVEL/ RADDA(IX),UADDA(IX),VADDA(IX),UADDRA(IX),
     &               VADDRA(IX), RADDF(IX),UADDF(IX),VADDF(IX),
     &               UADDRF(IX),VADDRF(IX)
C
      COMMON/ICR/   NADDA,NADDF,ICRITL,NAUTO
C
      COMMON/GEOCR/ RADCR(2),RAKECR(2),XI0CR(2),XW0CR(2),IICR(2),
     &              NBLDCR(2),TCR(2),ECR(2),PCR(2),ADVCR(2),RPMCR(2),
     &              TCRTOT,PCRTOT,ECRTOT,TCRLB,PCRHP,VKNOT
C
      COMMON/STNCR/ XICR(2,IX),CHCR(2,IX),BETACR(2,IX),
     &              BETA0CR(2,IX),UBODYCR(2,IX),CVEL1(2,IX),
     &              CVEL2(2,IX),CVEL3(2,IX),CVEL4(2,IX)
C
C
C
C---------------------------------------------------------------------------
C     ESLOFTX stuff
C     Ported from DFDC 070ES2
C     PJC, Esotec Developments, August 2011
C---------------------------------------------------------------------------
C     NAFX       number of parent airfoils
C     NPX        number of points per parent airfoil
C     NLSX       number of lofting stations (includes 2 possible overshoot)
C     MTI        meters to inches
C
C---- xfoil plot parameters
C     IQXX       following xfoil, could be smaller
C     IBXX       same
C
C---- not many loft parameters
      PARAMETER (NAFX= 12)	  ! max number of parent airfoils
      PARAMETER (NPX = 200)       ! points per airfoil
      PARAMETER (NLSX= 36)        ! loft stations - 2 reserved for osho
      PARAMETER (MTI = 39.370079) ! meters to inches
      PARAMETER (DTRX=PI/180.)    ! degrees to radians
      PARAMETER (NTCX= 10)        ! max t/c locations per airfoil
C
C---- xfoil plot stuff
      PARAMETER (IQXX= 360)
      
C
C-------------------------------------------------------------------------
C---- esloft declarations
C
      DIMENSION XXE(NPX,NAFX),YYE(NPX,NAFX)
      DIMENSION TOCE(NAFX),TLOC(NAFX),ALFZ(NAFX),TETH(NAFX),
     &          CAMLO(NAFX),CAMLOC(NAFX),PAREA(NAFX),PLERAD(NAFX),
     &          PANGTE(NAFX)
      CHARACTER*80 ENAME(NAFX),SNAME,LONAME,ISPARS
      DIMENSION YLOFT(NLSX),CDLOFT(NLSX),BELOFT(NLSX),TCLOFT(NLSX),
     &          THLOFT(NLSX),AZXROT(NLSX),AZLOFT(NLSX),BECORR(NLSX),
     &          BLXX(NPX,NLSX),BLYY(NPX,NLSX), AXLOFT(NLSX),
     &          TRXX(NPX,NLSX),TRYY(NPX,NLSX), AZERO(NLSX)
      LOGICAL   LL2D,LCALC,LBLEN,LTRAN,LROTATE,LROUND,
     &          LTDEF,LSMOD,LCIRC
      DIMENSION RZ1(IX), RZ1S(IX),
     &          RZ2(IX), RZ2S(IX),
     &          RZ3(IX)
      DIMENSION TT1(NLSX),TT1S(NLSX),
     &          TT2(NLSX),TT2S(NLSX),
     &          TT3(NLSX), NTC(NAFX),TCLOC(NAFX,NTCX)
C
C---- xfoil plot declarations
C---- generally modified symbols to stay clear of DFDC/XROTOR
C
      DIMENSION XBB(NPX),YBB(NPX),SBB(NPX),XBBP(NPX),YBBP(NPX),
     &      WXX1(6*IQXX),WXX2(6*IQXX),WXX3(6*IQXX),IXPLOT(NLSX)
      LOGICAL   LGGRIDX, LGTICKX, LGPARMX, LGEOPLX
      CHARACTER*80 NAMEX
      DIMENSION BLENDATA(8,NLSX)
C
C---- esloft
C
      COMMON /DF_ESLOFT/ NAF,XXE,YYE,TOCE,TLOC,ALFZ,ENAME,SNAME,
     &             TETH,NPP,NPP2,PAF,TKHUB,TKTIP,AXHUB,AXTIP,
     &	           OSHUB,OSTIP,NLOFT,NLOFT2,NLOFTX,PLOFT,YLOFT,
     &             CDLOFT,BELOFT,TCLOFT,THLOFT,BECORR,
     &             IHUB,ITIP,ITIPX,AZXROT,AZLOFT,ITTYPE,OUTFAC,
     &             LL2D,LCALC,LBLEN,LTRAN,LONAME,PAXIS,APARA,
     &             BLXX,BLYY,TRXX,TRYY,CAMLO,CAMLOC,
     &             LROTATE,AZTIP,TCUTD,TCUTW,LROUND,NROTOR,
     &             RZ1,RZ1S,RZ2,RZ2S,RZ3,TSFAC,TSLOC,NLOLD,
     &             LTDEF,LSMOD,TT1,TT1S,TT2,TT2S,TT3,NTC,TCLOC,
     &             PAREA,PLERAD,PANGTE,RTCRIT,  AXLOFT,
     &             LCIRC,CIRRAT,CIRRAD,CIRLEN,CIRBET,
     &             CIRCHD,CIRTHK,YBRK,C1BRK,C2BRK,NBRK
C
C---- xfoil plots - mostly modified symbols
C
      COMMON/XF_PLOTS/XBB,YBB,SBB,NBB,XBBP,YBBP,
     &                NAMEX,SBLEX,CHORDBX,AREABX,
     &                RADBLEX,ANGBTEX,THICKBX,CAMBRBX,
     &                EI11BA,EI22BA,APX1BA,APX2BA,
     &                EI11BT,EI22BT,APX1BT,APX2BT,       
     &                XSFX,YSFX,XOFFX,YOFFX, 
     &                XGMINX,XGMAXX,YGMINX,YGMAXX,DXYGX,
     &                XBMINX,XBMAXX,YBMINX,YBMAXX,
     &                LGGRIDX,LGTICKX,LGPARMX,LGEOPLX,
     &                IXTYPE,NXPLOT,IXPLOT,CHGX,GTICKX,NOVER,
     &                BLENDATA,XTHICKBX,XCAMBRBX,TETHX,
     &                PORTFAC,XMIN,YMIN,XMAX,YMAX
C
C--------------------------------------------------------------------------
C
C     XXE	X coordinate of paneled parent airfoils
C     YYE       Y coordinate of paneled parent airfoils
C     TOCE      Max thickness/chord of      "
C     TLOC      Max thickness x/c of        "
C     CAMLO     Max camber of               "
C     CAMLOC    Max camber x/c of           "
C     TETH      Trailing edge t/c           "
C     ALFZ      Zero lift alpha (input)     "
C     PAREA     Section area                "
C     PLERAD    Leading edge radius         "
C     PANGTE    Trailing edge angle         "
C     ENAME     Individual airfoil names (imported)
C     SNAME     Esloft parent airfoil set name
C     NPP       No. of points per paneled airfoil
C     NPP2      No. of points per side
C     PAF       Airfoil panel spacing parameter (del_TE/del_LE)
C
C     TKHUB     Specified blade thickness at root (mm). 0 to set airfoil.
C     TKTIP     Specified blade thickness at tip (mm). 0 to set airfoil.
C     AXHUB     Specified pitch axis at root x/c
C     AXTIP     Specified pitch axis at tip x/c
C     AZTIP     Specified dihedral at tip z (mm)
C     OSHUB     Lofted overshoot at root (beyond design dimensions)
C     OSTIP     Lofted overshoot at tip
C     NLOFT     Specified number of loft stations
C     NLOFT2    Total loft stations with overshoot stations
C     NLOFTX    Total stations including tip for round tip cases
C     PLOFT     Loft station density factor (hub/tip)
C     YLOFT     Radial location of loft stations (m)
C     CDLOFT    Chords at loft stations (m)
C     BELOFT    Beta at loft stations (from DFDC data) (rad)
C     TCLOFT    Thickness/chord at loft stations
C     THLOFT    Thickness at loft stations (m)
C     AXLOFT    Pitch axis at loft stations / chord
C     IHUB      Hub loft station index
C     ITIP      Tip loft station index (for loft calculations)
C     ITIPX     Geometric tip loft station (for round tips, including TIPCUT)
C     AZXROT    A0 at lofting stations - interpolated from AERO data (deg)
C     AZLOFT    A0 at lofting stations - interpolated from ESLOFT data (deg)
C     BECORR    Beta at lofting stations corrected using A0 data (rad)
C     ITTYPE    Thickness distribution type:
C               1  linear t/c
C               2  parabolic t/c
C               3  splined t/c
C               4  linear t
C               5  parabolic t
C               6  splined t
C     PAXIS     Parabola axis location (units of Rtip-Rhub inbd of hub)
C     APARA     Calculated parabolic coefficient A ( t ~ SQRT(Rad/A) )
C     OUTFAC    Output factor for unit conversions
C     LL2D      True to export 2D points files (false is 3D)
C     LCALC     True if current loft input has been calculated
C     LBLEN	True if current blended sections are calculated
C     LTRAN	True if current transformed sections are calculated
C     LROTATE   True if left hand rotation selected for output
C     BLXX      X coordinates of blended sections
C     BLYY      Y coordinates of blended sections
C     TRXX      X coordinates of transformed sections
C     TRYY      Y coordinates of transformed sections
C     IXTYPE    Type of xfoil plot
C               1  Parent airfoils
C	        2  Blended airfoils
C               3  Transformed airfoils
C     NXPLOT    Number of airfoils to be plotted
C     IXPLOT    Array of airfoil indices to be plotted
C
C     LGEOPLX   true if there is a current xfoil plot
C     NOVER     number of plot overlays
C     TCUTD     default tipcut/bladelength for round tips
C     TCUTW     working tipcut/bladelength for round tips
C     LROUND    true if round tip, false if square tip
C     TSFAC     station density factor for round tips
C     TSLOC     blade region/bladelength for tip station treatment
C     LTDEF     True if t and t/c data exist for spline modes
C     LSMOD     True if spline needs user input during processing
C     NLOLD     Number of loft stations of last splined t or t/c
C     NTC       Number of t/c matches per parent airfoil
C     TCLOC     Radial locations of t/c matches per airfoil
C     RTCRIT    Round tip criterion - tip_chord/radius
C
C-----------------------------------------------------------------------
C    End Esotec extensions
C-----------------------------------------------------------------------




