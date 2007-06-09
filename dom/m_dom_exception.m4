include(`foreach.m4')`'dnl
define(`TOHW_m_dom_throw_error',`dnl
dnl 1 is numerical code
dnl 2 is list of things to deallocate
call throw_exception($1, "m4f_thisfunc", ex)
if (present(ex)) then
  if (is_in_error(ex)) then
ifelse($2, `', `', 
     m4_foreach(`x', `$2', `
if (associated(x)) deallocate(x)
'))`'dnl
     return
  endif
endif
')`'dnl
dnl
define(`TOHW_function', `define(`m4f_thisfunc', `$1')`'function $1`'dnl
(m4_foreach(`x', `$2', `x, ')ex)ifelse($3, `', `', `result($3)') 
    type(DOMException), intent(inout), optional :: ex`'dnl
')`'dnl
dnl
define(`TOHW_subroutine', `define(`m4f_thisfunc', `$1')`'subroutine $1`'dnl
(m4_foreach(`x', `$2', `x, ')ex)
    type(DOMException), intent(inout), optional :: ex`'dnl
')`'dnl
dnl
