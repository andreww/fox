include(`foreach.m4')`'dnl
define(`TOHW_m_dom_throw_error',`dnl
dnl 1 is numerical code
dnl 2 is message, usually name of subroutine.
dnl 3 ... is things to deallocate
call throw_exception($1, $2, ex)
if (present(ex)) then
  if (is_in_error(ex)) then
ifelse($3, `', `', 
     m4_foreach(`x', `$3', `
if (associated(x)) deallocate(x)
'))
     return
  endif
endif
')`'dnl
