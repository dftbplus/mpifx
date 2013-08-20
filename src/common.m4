dnl
dnl Undefining some M4 builtins to avoid conflicts with Fortran code
dnl invoke them via the builtin() command if needed.
dnl
undefine(`len')dnl
undefine(`index')dnl
undefine(`shift')dnl


dnl Sets a variable ($1) to the value of an optional argument ($2)
dnl if present or to a default value ($3) otherwise.
dnl
define(`_handle_inoptflag',`dnl
if (present($2)) then
  $1 = $2
else
  $1 = $3
end if
')


dnl Sets an optional output argument ($1) if present to a certain value ($2).
dnl
define(`_handle_outoptflag', `dnl
if (present($1)) then
  $1 = $2
end if
')
