program calculator
    implicit none
    
    ! --- Variables ---
    integer :: choice
    real(8) :: num1, num2, result
    logical :: running
    real(8) :: a, b, c, disc, x1, x2
    
    running = .true.
    
    print *, "========================================================"
    print *, "               CALCULATOR (FORTRAN)"
    print *, "       [With Input Validation & Extra Features]"
    print *, "========================================================"
    
    do while (running)
        call print_menu()
        
        call get_integer_input("Your choice: ", choice)
        
        if (choice < 1 .or. choice > 23) then
            print *, ">>> Error: Invalid selection."
            cycle
        end if
        
        select case (choice)
            ! --- BASIC ARITHMETIC ---
            case (1) ! Addition
                call get_real_input("Enter first number: ", num1)
                call get_real_input("Enter second number: ", num2)
                result = num1 + num2
                print *, "Result: ", result
                
            case (2) ! Subtraction
                call get_real_input("Enter first number: ", num1)
                call get_real_input("Enter second number: ", num2)
                result = num1 - num2
                print *, "Result: ", result
                
            case (3) ! Multiplication
                call get_real_input("Enter first number: ", num1)
                call get_real_input("Enter second number: ", num2)
                result = num1 * num2
                print *, "Result: ", result
                
            case (4) ! Division
                call get_real_input("Enter numerator: ", num1)
                call get_real_input("Enter denominator: ", num2)
                if (abs(num2) < 1.0d-9) then ! Small number check
                    print *, ">>> Error: Division by zero!"
                else
                    result = num1 / num2
                    print *, "Result: ", result
                end if

            ! --- ADVANCED MATH ---
            case (5) ! Power
                call get_real_input("Enter base: ", num1)
                call get_real_input("Enter exponent: ", num2)
                result = num1 ** num2
                print *, "Result: ", result
                
            case (6) ! Square Root
                call get_real_input("Enter number: ", num1)
                if (num1 < 0.0) then
                    print *, ">>> Error: Negative input for square root."
                else
                    result = sqrt(num1)
                    print *, "Result: ", result
                end if

            case (7) ! Cube Root (Cubic Root)
                call get_real_input("Enter number: ", num1)
                ! Works for negative numbers correctly: sign(x) * abs(x)**(1/3)
                result = sign(abs(num1)**(1.0d0/3.0d0), num1)
                print *, "Result: ", result

            case (8) ! Factorial
                call get_real_input("Enter integer number: ", num1)
                if (num1 < 0 .or. mod(num1, 1.0d0) /= 0) then
                    print *, ">>> Error: Factorial only for positive integers."
                else
                    result = factorial(int(num1))
                    print *, "Result: ", result
                end if

            case (9) ! Percentage
                call get_real_input("Enter total value: ", num1)
                call get_real_input("Enter percentage (%): ", num2)
                result = (num1 * num2) / 100.0d0
                print *, "Result: ", result, "%"

            case (10) ! Absolute Value
                call get_real_input("Enter number: ", num1)
                result = abs(num1)
                print *, "Result: ", result

            ! --- LOGARITHMS ---
            case (11) ! Natural Log
                call get_real_input("Enter number: ", num1)
                if (num1 <= 0) then
                    print *, ">>> Error: Log undefined for zero or negative."
                else
                    result = log(num1)
                    print *, "Ln(x): ", result
                end if

            case (12) ! Log10
                call get_real_input("Enter number: ", num1)
                if (num1 <= 0) then
                    print *, ">>> Error: Log undefined for zero or negative."
                else
                    result = log10(num1)
                    print *, "Log10(x): ", result
                end if

            ! --- TRIGONOMETRY (Input in Degrees) ---
            case (13) ! Sine
                call get_real_input("Enter angle (Degrees): ", num1)
                result = sin(deg_to_rad(num1))
                print *, "Sin: ", result
                
            case (14) ! Cosine
                call get_real_input("Enter angle (Degrees): ", num1)
                result = cos(deg_to_rad(num1))
                print *, "Cos: ", result
                
            case (15) ! Tangent
                call get_real_input("Enter angle (Degrees): ", num1)
                if (abs(mod(num1, 180.0d0)) == 90.0d0) then
                    print *, ">>> Error: Tan undefined at 90 degrees."
                else
                    result = tan(deg_to_rad(num1))
                    print *, "Tan: ", result
                end if
                
            case (16) ! Cotangent
                call get_real_input("Enter angle (Degrees): ", num1)
                if (mod(num1, 180.0d0) == 0.0d0) then
                    print *, ">>> Error: Cot undefined at 0/180 degrees."
                else
                    result = 1.0d0 / tan(deg_to_rad(num1))
                    print *, "Cot: ", result
                end if
            
            ! --- INVERSE TRIGONOMETRY (Output in Degrees) ---
            case (17) ! ArcSin
                call get_real_input("Enter value (-1 to 1): ", num1)
                if (num1 < -1.0d0 .or. num1 > 1.0d0) then
                    print *, ">>> Error: Domain error (must be -1 to 1)."
                else
                    result = rad_to_deg(asin(num1))
                    print *, "ArcSin (Degrees): ", result
                end if

            case (18) ! ArcCos
                call get_real_input("Enter value (-1 to 1): ", num1)
                if (num1 < -1.0d0 .or. num1 > 1.0d0) then
                    print *, ">>> Error: Domain error (must be -1 to 1)."
                else
                    result = rad_to_deg(acos(num1))
                    print *, "ArcCos (Degrees): ", result
                end if

            case (19) ! ArcTan
                call get_real_input("Enter value: ", num1)
                result = rad_to_deg(atan(num1))
                print *, "ArcTan (Degrees): ", result

            ! --- TOOLS & EQUATIONS ---
            case (20) ! Quadratic Equation
                print *, "Solving ax^2 + bx + c = 0"
                call get_real_input("Enter a: ", a)
                call get_real_input("Enter b: ", b)
                call get_real_input("Enter c: ", c)
                
                if (a == 0.0d0) then
                    print *, "Not a quadratic equation (a=0)."
                else
                    disc = b**2 - 4.0d0*a*c
                    if (disc > 0) then
                        x1 = (-b + sqrt(disc)) / (2.0d0*a)
                        x2 = (-b - sqrt(disc)) / (2.0d0*a)
                        print *, "Two real roots:"
                        print *, "x1 = ", x1
                        print *, "x2 = ", x2
                    else if (disc == 0) then
                        x1 = -b / (2.0d0*a)
                        print *, "One real root: x = ", x1
                    else
                        print *, "Complex roots (no real solution)."
                        
                    end if
                end if

            case (21) ! Deg to Rad
                call get_real_input("Enter Degrees: ", num1)
                print *, "Radians: ", deg_to_rad(num1)

            case (22) ! Rad to Deg
                call get_real_input("Enter Radians: ", num1)
                print *, "Degrees: ", rad_to_deg(num1)

            case (23) ! Exit
                running = .false.
                print *, "Exiting. Have a nice day!"
        end select
        
        print *, "--------------------------------------------------------"
    end do

contains

    ! --- INPUT VALIDATION LOGIC (The "Catcher") ---
    
    ! Safely reads a REAL number. Loops until valid.
    subroutine get_real_input(prompt, val)
        character(*), intent(in) :: prompt
        real(8), intent(out) :: val
        character(len=100) :: buffer
        integer :: ios
        
        do
            print *, prompt
            read(*, '(a)', iostat=ios) buffer ! Read as string first
            if (ios /= 0) then
                print *, ">>> Invalid input. Please enter a NUMBER."
                cycle
            end if
            
            ! Convert string to number
            read(buffer, *, iostat=ios) val
            if (ios /= 0) then
                print *, ">>> Invalid format. Try again."
                cycle
            end if
            
            exit ! Success
        end do
    end subroutine get_real_input

    ! Safely reads an INTEGER number.
    subroutine get_integer_input(prompt, val)
        character(*), intent(in) :: prompt
        integer, intent(out) :: val
        character(len=100) :: buffer
        integer :: ios
        
        do
            print *, prompt
            read(*, '(a)', iostat=ios) buffer
            if (ios /= 0) then
                print *, ">>> Invalid input. Please enter an INTEGER."
                cycle
            end if
            
            read(buffer, *, iostat=ios) val
            if (ios /= 0) then
                print *, ">>> Invalid format. Try again."
                cycle
            end if
            
            exit
        end do
    end subroutine get_integer_input

    ! --- MATH HELPERS ---

    subroutine print_menu()
        print *, ""
        print *, "AVAILABLE OPERATIONS:"
        print *, " [1] Add    [2] Subtract  [3] Multiply  [4] Divide"
        print *, " [5] Power (x^y)   [6] Square Root   [7] Cube Root"
        print *, " [8] Factorial     [9] Percentage    [10] Abs Value"
        print *, " --- LOGARITHMS ---"
        print *, " [11] Natural Log (ln)  [12] Base-10 Log"
        print *, " --- TRIGONOMETRY (Degrees) ---"
        print *, " [13] Sin   [14] Cos   [15] Tan   [16] Cot"
        print *, " [17] ArcSin [18] ArcCos [19] ArcTan"
        print *, " --- ADVANCED TOOLS ---"
        print *, " [20] Solve Quadratic Equation (ax^2+bx+c=0)"
        print *, " [21] Deg -> Rad  [22] Rad -> Deg"
        print *, " [23] EXIT"
    end subroutine print_menu

    function deg_to_rad(degrees) result(radians)
        real(8), intent(in) :: degrees
        real(8) :: radians
        real(8), parameter :: pi = 3.14159265358979323846d0
        radians = degrees * (pi / 180.0d0)
    end function deg_to_rad

    function rad_to_deg(radians) result(degrees)
        real(8), intent(in) :: radians
        real(8) :: degrees
        real(8), parameter :: pi = 3.14159265358979323846d0
        degrees = radians * (180.0d0 / pi)
    end function rad_to_deg

    ! Recursive factorial function
    recursive function factorial(n) result(res)
        integer, intent(in) :: n
        real(8) :: res
        if (n <= 0) then
            res = 1
        else
            res = n * factorial(n - 1)
        end if
    end function factorial

end program calculator
