! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! CODE BY:
! --------   |         |   ---------        /\        |\      |
! |          |         |  |                /  \       | \     |
! |          |         |  |               /    \      |  \    |
! --------   |         |  |   ------|    /------\     |   \   |
!         |  |         |  |         |   /        \    |    \  |
!         |  |         |  |         |  /          \   |     \ |
! ---------   ----------  ----------  /            \  |      \|
! --------------------------------------------------------------------------------------------------------------------------------------------                                                                           
! XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
! #########################
! MODULE: constants
! LAST MODIFIED: 10 November 2020
! #########################
! TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
! CONSTANTS FOR BURGERS EQUATION
! IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
MODULE constants
! INFO - START  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! ------------
! All the user defined constants (that can't be changed) are declared here. And refered in other modules. 
! -------------
! INFO - END <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

    ! HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
	IMPLICIT  NONE

    ! _________________________
    ! CONSTANTS
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    DOUBLE COMPLEX,PARAMETER::i=DCMPLX(0.0D0,1.0D0),c0=DCMPLX(0.0D0,0.0D0)
	DOUBLE PRECISION,PARAMETER::two_pi=DATAN(1.0D0)*8.0D0
	DOUBLE PRECISION,PARAMETER::zero=0.0D0
    DOUBLE PRECISION,PARAMETER::hf=0.5D0,one=1.0D0,two=2.0D0,thr=3.0D0,six=6.0D0
    DOUBLE PRECISION,PARAMETER::tol_double=0.00000000000000001
    ! _________________________
    ! FORMATS 
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    CHARACTER(LEN=*),PARAMETER::f_i2='(I2)',f_i4='(I4)',f_i6='(I6)'
    CHARACTER(LEN=*),PARAMETER::f_i8='(I8)',f_i16='(I16)'
    CHARACTER(LEN=*),PARAMETER::f_d8p4='(F8.4)',f_d12p6='(F12.6)',f_d16p8='(F16.8)',f_d5p2='(F5.2)'
    CHARACTER(LEN=*),PARAMETER::f_d12p2='(F12.2)',f_d32p17='(F32.17)',f_d12p8='(F12.8)'
    CHARACTER(LEN=*),PARAMETER::f_e5p2='(ES6.2)',f_e10p4='(ES12.4)'
    CHARACTER(LEN=*),PARAMETER::f_c32p17='(F32.17,F32.17)'
    ! _________________________
    ! USER DEFINED DATA-TYPE
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE constants
