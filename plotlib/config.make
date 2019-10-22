
#=========================================#
# Makefile options for Xplot11 library    #
#   Set up or select a set of compile     #
#   options for your system               # 
#=========================================#
# Compile options for Ubuntu-gfortran-gcc #
#=========================================#

# Set library name 
PLTLIB = libPlt.a

# Some fortrans need trailing underscores in C interface symbols (see Xwin.c)
# This should work for most of the "unix" fortran compilers
DEFINE = -DUNDERSCORE

FC = gfortran
CC = gcc

FFLAGS  = -O2
CFLAGS  = -O2 $(DEFINE)
AR = ar r
RANLIB = ranlib 
LINKLIB = -lX11 

