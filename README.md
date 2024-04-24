# Fortran
Repository with fortran programs

# Variables declaration
- I, J, K, L, M, N - Integers variables
- others letters - real variables

# Arithmetic
- exponentiation: A**b
- multiplication and division: A*B/C
- addition and subtration: A + B - C

# Arrays
dimension name(100)
name(index)

dimension variable(x,...) - arrays dimension definition
data variable/value/ - variables initialization

# Program control
## if
```
if (I .gt. 0) then
  code
else if (I .eq. 3) then
  code
else
  code
end if
```
## do
```
do I=0, 10, [2] then
  code
end do
```
## do while
```
do [label] while (I .le. 10)
  code
end do
```
## Files
### Read File
```
open(10, file='file.txt')
read (10, *) x, y, z
```
### Write File
```
open(10, file='file.txt')
write (10, *) x, y, z
```

# YouTube Tutorials

[Fortran Tuto](https://www.youtube.com/watch?v=X1x0fgn1tMo&list=PLvkU6i2iQ2fprrVmmkNP_V36mh0BMnS5L)
[Fortran 77 Language Reference](https://docs.oracle.com/cd/E19957-01/805-4939/index.html)
