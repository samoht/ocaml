/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include "unixsupport.h"

CAMLprim value unix_unlink(value path)
{
  CAMLparam1(path);
  char * p;
  int ret;
  caml_unix_check_path(path, "unlink");
  p = caml_strdup(String_val(path));
  caml_enter_blocking_section();
  ret = unlink(p);
  caml_leave_blocking_section();
  caml_stat_free(p);
  if (ret == -1) uerror("unlink", path);
  CAMLreturn(Val_unit);
}
