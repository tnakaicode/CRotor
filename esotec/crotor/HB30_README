
XROTOR restart files HB30f.xrotor and HB30a.xrotor facilitate 
evaluation of CROTOR for ducted cases. These were designed following 
the procedure given in CRotor_doc, with the forward rotor as produced 
by the MIL design routine and the aft rotor adjusted in MODI.

- Load XROTOR and go into CROTOR (CROT).
- Import HB30f.xrotor as forward rotor (LFWD)
- Import HB30a.xrotor as aft rotor (LAFT)
- Go to INPU and enter the following parameters:
  Total Power 10,000 W 
  Rpm 4400 
  Velocity 15 m/s

The POWE (or RPM) command will now converge the CR system. 

To observe some curious numerics, after completing the above:
- Clear slipstreams with VCLR.
- Design a forward rotor with DFWD, <ret> at the prompt to use
  current rotor parameters.
- Notice that several columns of output data line up.
- Execute DFWD several more times while observing the output data.
  You will observe that effi becomes constant across the disk. 

This behaviour occurs only in ducted cases with low tip Mach numbers
and is clearly related to the MIL design condition for ducted rotors, 
as mentioned by Hal Youngren near the end of version_notes.txt. 

PJC
31 July 11
