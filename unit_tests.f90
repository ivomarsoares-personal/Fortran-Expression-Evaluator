program unit_tests
!
! Purpose of the program:
!   This program performs unit tests for the expression evaluator.
!   It validates that the evaluator produces the correct results for various
!   mathematical expressions including arithmetic operations, trigonometric,
!   hyperbolic, logarithmic, and other mathematical functions.
!
! Authors:
!   Ivomar B. Soares - ivomarbsoares@gmail.com
!   Wilton P. Silva - wiltonps@uol.com.br
!   Claude - AI Assistant
!
! Test Cases:
!   1. Complex expression: 5.481916
!   2. Linear expression: 0.9508000
!   3. Hyperbolic/exponential expression: 20.69617
!   4. Inverse trig/hyperbolic expression: 1.559742
!   5. Combined expression: 1.557368
!   6. Operator precedence and nested operations
!   7. Deeply nested function calls
!   8. Mixed trigonometric and hyperbolic functions
!   9. Rounding and floor functions
!   10. Complex division and exponentiation
!   11. Logarithmic expressions with multiple bases
!   12. Inverse trigonometric with complex argument
!   13. Parentheses nesting and order of operations
!   14. Absolute value with negative expressions
!   15. Complex expression with all operation types
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
! Test 6: Operator precedence and nested operations
!==================================================
write(*,*) 'Test 6: Operator Precedence and Nested Operations'
write(*,*) '==============================================='

! Set variables for test 6
variablesvalues(1) = 2.0    ! x
variablesvalues(2) = 3.0    ! y
variablesvalues(3) = 4.0    ! z
variablesvalues(6) = 5.0    ! a

func = 'x+y*z^2-a/x'
expected = 2.0 + 3.0 * (4.0**2) - 5.0/2.0

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 7: Deeply nested function calls
!==================================================
write(*,*) 'Test 7: Deeply Nested Function Calls'
write(*,*) '=================================='

! Set variables for test 7
variablesvalues(1) = 0.5    ! x
variablesvalues(2) = 0.25   ! y

func = 'sqrt(abs(log10(exp(sin(x)+cos(y)))))'
expected = sqrt(abs(log10(exp(sin(0.5_realkind)+cos(0.25_realkind)))))

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 8: Mixed trigonometric and hyperbolic functions
!==================================================
write(*,*) 'Test 8: Mixed Trigonometric and Hyperbolic Functions'
write(*,*) '=================================================='

! Set variables for test 8
variablesvalues(1) = 0.3    ! x
variablesvalues(2) = 0.6    ! y
variablesvalues(3) = 0.9    ! z

func = 'tanh(x)*cos(y)+sinh(z)/cosh(x)-tan(y*0.5)'
expected = tanh(0.3_realkind)*cos(0.6_realkind) + sinh(0.9_realkind)/cosh(0.3_realkind) - tan(0.6_realkind*0.5_realkind)

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 9: Rounding and floor functions
!==================================================
write(*,*) 'Test 9: Rounding and Floor Functions'
write(*,*) '=================================='

! Set variables for test 9
variablesvalues(1) = 3.7    ! x
variablesvalues(2) = 2.3    ! y
variablesvalues(3) = 5.5    ! z

func = 'floor(x)+nint(y)*anint(z)-aint(x+y)'
expected = floor(3.7_realkind) + nint(2.3_realkind)*anint(5.5_realkind) - aint(3.7_realkind+2.3_realkind)

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 10: Complex division and exponentiation
!==================================================
write(*,*) 'Test 10: Complex Division and Exponentiation'
write(*,*) '=========================================='

! Set variables for test 10
variablesvalues(1) = 2.5    ! x
variablesvalues(2) = 1.5    ! y
variablesvalues(3) = 0.5    ! z

func = '(x^y)/(y^z)+sqrt(x*y)/exp(z)'
expected = (2.5_realkind**1.5_realkind)/(1.5_realkind**0.5_realkind) + sqrt(2.5_realkind*1.5_realkind)/exp(0.5_realkind)

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 11: Logarithmic expressions with multiple bases
!==================================================
write(*,*) 'Test 11: Logarithmic Expressions with Multiple Bases'
write(*,*) '=================================================='

! Set variables for test 11
variablesvalues(1) = 10.0   ! x
variablesvalues(2) = 2.718  ! y (approximately e)

func = 'log10(x)*log(y)+exp(log(x/y))'
expected = log10(10.0_realkind)*log(2.718_realkind) + exp(log(10.0_realkind/2.718_realkind))

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 12: Inverse trigonometric with complex argument
!==================================================
write(*,*) 'Test 12: Inverse Trigonometric with Complex Argument'
write(*,*) '=================================================='

! Set variables for test 12
variablesvalues(1) = 0.4    ! x
variablesvalues(2) = 0.3    ! y
variablesvalues(3) = 0.5    ! z

func = 'asin(x*y)+acos(z)-atan(x/z)*2.0'
expected = asin(0.4_realkind*0.3_realkind) + acos(0.5_realkind) - atan(0.4_realkind/0.5_realkind)*2.0_realkind

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 13: Parentheses nesting and order of operations
!==================================================
write(*,*) 'Test 13: Parentheses Nesting and Order of Operations'
write(*,*) '=================================================='

! Set variables for test 13
variablesvalues(1) = 1.0    ! x
variablesvalues(2) = 2.0    ! y
variablesvalues(3) = 3.0    ! z
variablesvalues(6) = 4.0    ! a

func = '((x+y)*(z-a))/(x*y+z*a)+sqrt((x+y+z+a)/a)'
expected = ((1.0_realkind+2.0_realkind)*(3.0_realkind-4.0_realkind))/(1.0_realkind*2.0_realkind+3.0_realkind*4.0_realkind) + sqrt((1.0_realkind+2.0_realkind+3.0_realkind+4.0_realkind)/4.0_realkind)

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 14: Absolute value with negative expressions
!==================================================
write(*,*) 'Test 14: Absolute Value with Negative Expressions'
write(*,*) '==============================================='

! Set variables for test 14
variablesvalues(1) = -2.5   ! x
variablesvalues(2) = 3.5    ! y
variablesvalues(3) = -1.5   ! z

func = 'abs(x*z)+abs(y+x)-abs(z-y)'
expected = abs(-2.5_realkind*(-1.5_realkind)) + abs(3.5_realkind+(-2.5_realkind)) - abs(-1.5_realkind-3.5_realkind)

call test_expression(func, variables, variablesvalues, expected, tolerance, test_count, passed_count)

!==================================================
! Test 15: Complex expression with all operation types
!==================================================
write(*,*) 'Test 15: Complex Expression with All Operation Types'
write(*,*) '=================================================='

! Set variables for test 15
variablesvalues(1) = 1.2    ! x
variablesvalues(2) = 2.3    ! y
variablesvalues(3) = 0.7    ! z

func = 'exp(x)/sqrt(y)+log(abs(x+y))*cos(z)-sin(x^2)+tanh(z/x)'
expected = exp(1.2_realkind)/sqrt(2.3_realkind) + log(abs(1.2_realkind+2.3_realkind))*cos(0.7_realkind) - sin(1.2_realkind**2) + tanh(0.7_realkind/1.2_realkind)

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
