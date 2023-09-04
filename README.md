# Description

This solver is for the one-dimensional Burgers equation,
$$
\partial_t u + u\partial_x u = \nu \partial_{xx}^2 u
$$
in a periodic domain $x\in [0,2\pi]$. This is a pseudo-spectral code, that uses Runga-Kutta $4^{th}$ order method to advance each time step, de-aliased with two-third rule at every step.

## Simulation details
The system details can be edited in the file: [simulation_parameters](simulation_parameters.dat)
```
N - Resolution of the simulation
1024
dt- Time step of the simulation
0.00002
T - Total time of the simulation
5.0
S - No of saves to be made
500
```
The viscosity $\nu$ and initial energy ($E_0= \frac{1}{2}\displaystyle \int_0^{2\pi}dx |u(x,0)|^2$) can be edited in the [system_parameters](system_parameters.f90). Please make sure that the time step $dt$ is sufficiently smaller than the $\texttt{`time\_grid'}$ and $\texttt{`time\_visc'}$ defined there. 

### Initial Condition:
The form of velocity at $t=0$ can be edited in the [initial_condition](initial_condition.f90).

## Run
To run the solver (after choosing the simulation parameters) via the [makefile](makefile), enter:
>$ make

The simulation running time and net energy will be displayed in the terminal:
```
HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
          PROGRAM STARTED ON :Mon Sep  4 22:02:02 2023      
-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
 TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
    S  I  M  U  L  A  T  I  O  N        S  T  A  R  T  S 
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
----------------------------------------
  |   TIME    |    ENERGY     | 
----------------------------------------
  |   0.0000  |   1.00000000  | 
  |   0.1000  |   0.82360937  | 
  |   0.2000  |   0.68067571  | 
  |   0.3000  |   0.56161278  | 
  |   0.4000  |   0.46544134  | 
  |   0.5000  |   0.39082639  | 
  |   0.6000  |   0.33275580  | 
  |   0.7000  |   0.28649900  | 
  |   0.8000  |   0.24870288  | 
  |   0.9000  |   0.21714240  | 
  |   1.0000  |   0.19036267  | 
 -----------------------------------------------------------
 IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
    S  I  M  U  L  A  T  I  O  N        E  N  D  S 
 TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
            PROGRAM ENDED ON :Mon Sep  4 22:03:15 2023      
TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      TOTAL RUN TIME (HOURS) :              0.0081
 HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH

```
## Outputs
The simulation prints the output at definite times, chosen by the user in the file [simulation_parameters](simulation_parameters.dat).  The following outputs are saved,
1. Real velocity  $\{x,u(x)\} \Rightarrow$ **velocity_t_0.0000.dat**
2. Spectral velocity $\{k,|\hat{u}_k|^2\}\Rightarrow$ **spectrum_t_0.0000.dat**

Also the total energy _vs_ time $\{t,E(t)\}$, for every time-step, is written in **energy_vs_time.dat**

### Author:
**Sugan Durai Murugan** \
Research Scholar, \
International Centre for Theoretical Sciences,\
Tata Institute of Fundamental Research,\
Bangalore, India.
