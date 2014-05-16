#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

//TODO: invoke llvmconfig in build system
#define __STDC_FORMAT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <llvm-c/Target.h>
#include <llvm-c/ExecutionEngine.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>

#include <stdio.h>

CAMLprim value eval_method_float(void *p) {
  double (*f)() = p;
  return caml_copy_double(f());
}

CAMLprim value eval_method_int(void *p) {
  int (*f)() = p;
  return Val_int(f());
}

CAMLprim value eval_method_bool(void *p) {
  int (*f)() = p;
  return Val_bool(f());
}

CAMLprim value eval_method_obj(void *p) {
  value (*f)() = p;
  return f();
}

/* external eval__i_i : method_ptr -> int -> int = "eval__i_i" */
CAMLprim value eval__i_i(void *p, value i) {
  value (*f)(int) = p;
  return Val_int(f(Int_val(i)));
}

