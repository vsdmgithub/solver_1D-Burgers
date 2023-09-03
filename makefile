# MAKEFILE FOR BURGERS

# DEFINE VARIABLES
# ---------------------------start-----

# COMPILER
cc=gfortran

# COMPILER LOCATION
cc_lc= -I/home/sugan/fftw/include

# LIBRARY LOCATION
lb_fftw=-L/home/sugan/fftw/lib -lfftw3 -lm

# PROGRAM
prog=burgers.f90

# MODULES
mod=FFT_mod.f90\
	timer_mod.f90\
	constants.f90\
	global_variables.f90\
	system_parameters.f90\
	initial_condition.f90\
	solver.f90\
	output.f90\
	main_run.f90

# OBJECTS
obj=FFT_mod.o\
	timer_mod.o\
	constants.o\
	global_variables.o\
	system_parameters.o\
	initial_condition.o\
	solver.o\
	output.o\
	main_run.o\

# EXECUTABLE
run=./ex

# CLEAN COMMANDS
mkclean=make clean
mkcl=make cl
#----------------------------end-------



# MAKEFILE
# ---------------------------start-----
ex:$(ob)
#$(mkclean)
	$(cc) $(cc_lc) -c $(mod) $(lb_fftw)
	$(cc) $(cc_lc) -c $(prog)
	$(cc) $(cc_lc) $(prog) $(obj) $(lb_fftw) -o ex
	$(mkcl)
	$(run)

#----------------------------end-------

# CLEANING
# ---------------------------start-----
clean:
	rm ex
	clear
cl:
	rm *.mod
	rm *.o
#----------------------------end-------
