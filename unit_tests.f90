program unit_tests
!
! Purpose of the program:
!   This program performs unit tests for the expression evaluator based on
!   the test cases provided in issue #12. It validates that the evaluator
!   produces the correct results for 5 specific mathematical expressions.
!
! Authors:
!   Ivomar B. Soares - ivomarbsoares@gmail.com
!   Wilton P. Silva - wiltonps@uol.com.br!   
!   Claude - AI Assistant
!
! Test Cases:
!   1. Complex expression: 5.481916
!   2. Linear expression: 0.9508000
!   3. Hyperbolic/exponential expression: 20.69617
!   4. Inverse trig/hyperbolic expression: 1.559742
!   5. Combined expression: 1.557368
!

use interpreter
implicit none

! Variables used by the evaluator
character(len = 10), dimension(12) :: variables
real(realkind), dimension(12) :: variablesvalues

! String variable that will store the function
character(len = 275) :: func

! String variable for status
character(len = 5) :: statusflag

! Test variables
real(realkind) :: result, expected
real(realkind), parameter :: tolerance = 1.0e-6
integer :: test_count = 0
integer :: passed_count = 0

! Initialize variable names
variables(1) = 'x'
variables(2) = 'y'
variables(3) = 'z'
variables(4) = 'x1'
variables(5) = 'x2'
variables(6) = 'a'
variables(7) = 'b'
variables(8) = 'c'
variables(9) = 'd'
variables(10) = 'e'
variables(11) = 'f'
variables(12) = 'g'

write(*,*) '==============================================='
write(*,*) '        Expression Evaluator Unit Tests       '
write(*,*) '==============================================='
write(*,*)

!==================================================
! Test 1: Complex expression
!==================================================
write(*,*) 'Test 1: Complex Expression'
write(*,*) '=========================='

! Set variables for test 1
variablesvalues(1) = 0.175  ! x
variablesvalues(2) = 0.110  ! y
variablesvalues(3) = 0.900  ! z
variablesvalues(4) = 0.508  ! x1
variablesvalues(5) = 30.000 ! x2
variablesvalues(6) = 0.900  ! a
variablesvalues(7) = 0.100  ! b
variablesvalues(8) = 0.110  ! c
variablesvalues(9) = 0.120  ! d
variablesvalues(10) = 0.130 ! e
variablesvalues(11) = 0.140 ! f
variablesvalues(12) = 0.000 ! g

func = '(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z)*2/(x+y+z+x*y+x*z+y*z+x/y+x/z+y/z+x*cos(x)+y*sin(y)+z*tan(z))*3+sqrt(x*y*z+x+y+z)*log10(sqrt(x*2+y*2+z*2)+x+y+z))'
expected = 5.481916

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 2: Linear expression
!==================================================
write(*,*) 'Test 2: Linear Expression'
write(*,*) '======================='

! Set variables for test 2
variablesvalues(6) = 0.900  ! a
variablesvalues(7) = 0.100  ! b
variablesvalues(4) = 0.508  ! x1

func = 'a+b*x1'
expected = 0.9508000

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 3: Hyperbolic/exponential expression
!==================================================
write(*,*) 'Test 3: Hyperbolic/Exponential Expression'
write(*,*) '======================================='

! Set variables for test 3
variablesvalues(1) = 0.175  ! x
variablesvalues(2) = 0.110  ! y
variablesvalues(3) = 0.900  ! z
variablesvalues(4) = 0.508  ! x1
variablesvalues(5) = 30.000 ! x2
variablesvalues(6) = 0.900  ! a
variablesvalues(8) = 0.110  ! c
variablesvalues(9) = 0.120  ! d
variablesvalues(11) = 0.140 ! f

func = 'cosh(log(abs(y*z+x**2+x1**x2)))+a*d*(exp(c*f)+154.3)'
expected = 20.696168449581393

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 4: Inverse trig/hyperbolic expression
!==================================================
write(*,*) 'Test 4: Inverse Trigonometric/Hyperbolic Expression'
write(*,*) '================================================='

! Set variables for test 4
variablesvalues(1) = 0.175  ! x
variablesvalues(2) = 0.110  ! y
variablesvalues(3) = 0.900  ! z
variablesvalues(6) = 0.900  ! a
variablesvalues(8) = 0.110  ! c
variablesvalues(10) = 0.130 ! e
variablesvalues(11) = 0.140 ! f

func = 'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))'
expected = 1.559742

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 5: Combined expression
!==================================================
write(*,*) 'Test 5: Combined Expression'
write(*,*) '========================='

! Same variables as test 4
func = 'atan(sinh(log(abs(exp(z/x)*sqrt(y+a**c+f*e)))))*cos(log(abs(sqrt(y+a**c+f*e))))'
expected = 1.557368

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test Summary
!==================================================
write(*,*)
write(*,*) '==============================================='
write(*,*) '              Test Summary                     '
write(*,*) '==============================================='
write(*,*) 'Total tests:', test_count
write(*,*) 'Passed:', passed_count
write(*,*) 'Failed:', test_count - passed_count

if (passed_count == test_count) then
    write(*,*) 'All tests PASSED!'
    call exit(0)
else
    write(*,*) 'Some tests FAILED!'
    call exit(1)
end if

contains

subroutine test_expression(expression, var_names, var_values, expected_result, tol, test_num, pass_num)
    character(len=*), intent(inout) :: expression
    character(len=10), dimension(:), intent(inout) :: var_names
    real(realkind), dimension(:), intent(in) :: var_values
    real(realkind), intent(in) :: expected_result
    real(realkind), intent(in) :: tol
    integer, intent(inout) :: test_num
    integer, intent(inout) :: pass_num

    character(len=5) :: status
    real(realkind) :: actual_result
    real(realkind) :: error

    test_num = test_num + 1

    write(*,*) 'Expression:', trim(expression)
    write(*,*) 'Expected result:', expected_result

    ! Initialize and evaluate expression
    call init(expression, var_names, status)

    if (status == 'ok') then
        actual_result = evaluate(var_values)
        error = abs(actual_result - expected_result)

        write(*,*) 'Actual result:', actual_result
        write(*,*) 'Error:', error

        if (error <= tol) then
            write(*,*) 'Status: PASSED'
            pass_num = pass_num + 1
        else
            write(*,*) 'Status: FAILED (tolerance exceeded)'
        end if

        ! Clean up
        call destroyfunc()
    else
        write(*,*) 'Status: FAILED (evaluation error)'
    end if

    write(*,*)
end subroutine test_expression

end program unit_tests
