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

dnl Set DEBUG to 1, unless DEBUG is unspecified or explicitely set to 0.
dnl
define(`DEBUG', ifdef(`DEBUG', ifelse(DEBUG, 0, 0, 1), 0))

dnl Indicates debug code.
dnl $1 Code. It is only inserted, if DEBUG is defined as 1.
dnl
define(`_debug', ifelse(DEBUG, 1, $1, `'))

dnl Removing directory part of a file
dnl
define(`basename', `patsubst($1,`.*/',`')')

dnl Assertion
dnl $1  Condition to check (only inserted if in debug mode).
dnl
define(`_assert', _debug(`dnl
if (.not. ($1)) then
  call assertfailed("`basename(__file__)'", `__line__')
end if'))
