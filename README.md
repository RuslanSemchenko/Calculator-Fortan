# Calculator (Fortran)

A robust command-line scientific calculator built with **Fortran 90/95**. It supports basic arithmetic, exponentiation, and advanced trigonometric functions with high precision.

## Features
* **Basic Math:** Addition, Subtraction, Multiplication, Division.
* **Advanced Math:** Square Root and Exponentiation ($x^y$).
* **Trigonometry:** Sine, Cosine, Tangent, and Cotangent (Inputs in Degrees).
* **Conversion:** Quick Degrees $\leftrightarrow$ Radians unit converter.
* **Precision:** Uses 64-bit floating-point numbers (`real(8)`).

## Mathematical Implementation
Trigonometric functions in Fortran's standard library (`sin`, `cos`, etc.) require input in radians. This calculator automatically handles the conversion using:
$$rad = deg \times \frac{\pi}{180}$$
