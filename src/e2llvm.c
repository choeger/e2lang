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

void op_tnp_number_add(int, int, double*, double*, double*);
void op_tnp_number_mult(int, int, double*, double*, double*);
void op_tnp_number_pow(int, int, double*, double*, int);
void op_tnp_number_write_constant(int, int, double*, double);
void op_tnp_number_write_variable(int, int, double*, double, int);

CAMLprim value ints(int i1, int i2) {
    printf("i1: %d\ti2: %d\n", Int_val(i1), Int_val(i2));
    return 0;
}

CAMLprim value d_const(value params, value order, double* arr, value c) {
    op_tnp_number_write_constant(Int_val(params), Int_val(order), arr, Double_val(c));
    return 0;
}

CAMLprim value d_add(value p, value o, double* dst, double* src1, double* src2) {
    op_tnp_number_add(Int_val(p), Int_val(o), dst, src1, src2);
    return 0;
}

CAMLprim value d_mul(value p, value o, double* dst, double* src1, double* src2) {
    printf("src1[0]=%f src1[1]=%f\nsrc2[0]=%f src2[1]=%f\n", src1[0], src1[1], src2[0], src2[1]);
    op_tnp_number_mult(Int_val(p), Int_val(o), dst, src1, src2);
    printf("dst[0]=%f dst[1]=%f\n", dst[0], dst[1]);
    return 0;
}

CAMLprim value d_pow(value p, value o, double* dst, double* src, int n) {
    op_tnp_number_pow(Int_val(p), Int_val(o), dst, src, Int_val(n));
    return 0;
}

CAMLprim value d_var(value p, value o, double* dst, value val, int n) {
    op_tnp_number_write_variable(Int_val(p), Int_val(o), dst, Double_val(val), Int_val(n));
    return 0;
}

CAMLprim value eval_method_float(void *p) {
  double (*f)() = p;
  return caml_copy_double(f());
}

CAMLprim value eval_method_int(void *p) {
  int (*f)() = p;
  return Val_int(f());
}

CAMLprim value eval_method_bool(void *p) {
  char (*f)() = p;
  return Val_bool(f());
}

CAMLprim value eval_method_obj(void *p) {
  int (*f)() = p;
  return f();
}

/* external eval__i_i : method_ptr -> int -> int = "eval__i_i" */
CAMLprim value eval__i_i(void *p, value i) {
  long long arg = i;
  value (*f)(long long) = p;
  return Val_int(f(Int_val(arg)));
}

CAMLprim value eval__i_b(void *p, value i) {
    value (*f)(int) = p;
    return Val_bool(f(Int_val(i)));
}
