/*s: uint64p.c */
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/config.h>
/*x: uint64p.c */
value uint64_compare(value v1, value v2)
{
    uint64_t i1 = Int64_val(v1);
    uint64_t i2 = Int64_val(v2);
    return i1 == i2 ? Val_int(0) : i1 < i2 ? Val_int(-1) : Val_int(1);
}
/*x: uint64p.c */
value uint64_float64(value f)
{
    union { double d; int64_t i; } buffer;
    buffer.d = Double_val(f);
    return caml_copy_int64(buffer.i);
}

value uint64_float32(value f)
{
    union { float f; unsigned n; } buffer;
    int64_t i;
    buffer.f = (float) Double_val(f);
    i = (int64_t) buffer.n;
    return caml_copy_int64(i);
}

/*x: uint64p.c */
value uint64_i2f(value i)
{
    union { double d; int64_t i; } buffer;
    buffer.i = Int64_val(i);
    return caml_copy_double(buffer.d);
}
/*x: uint64p.c */
value uint64_i2i(value i)
{
    union { int32_t i[2]; int64_t j; } buffer;

    buffer.i[0] = Int_val(i);
    buffer.i[1] = 0;

    return caml_copy_int64(buffer.j);
}
/*x: uint64p.c */
value uint64_add(value v1, value v2)  /* ML */
{ return caml_copy_int64((uint64_t)Int64_val(v1) + (uint64_t)Int64_val(v2)); }

value uint64_sub(value v1, value v2)  /* ML */
{ return caml_copy_int64((uint64_t)Int64_val(v1) - (uint64_t)Int64_val(v2)); }

value uint64_mul(value v1, value v2)  /* ML */
{ return caml_copy_int64((uint64_t)Int64_val(v1) * (uint64_t)Int64_val(v2)); }
/*x: uint64p.c */
value uint64_div(value v1, value v2)  /* ML */
{
    int64_t divisor = Int64_val(v2);
    if (divisor == 0) caml_raise_zero_divide();
    return caml_copy_int64((uint64_t)Int64_val(v1) / (uint64_t)divisor);
}

value uint64_mod(value v1, value v2)  /* ML */
{
    int64_t divisor = Int64_val(v2);
    if (divisor == 0) caml_raise_zero_divide();
    return caml_copy_int64((uint64_t)Int64_val(v1) % (uint64_t)divisor);
}
/*x: uint64p.c */
static int parse_digit(const char * p)
{
    int c = *p;
    if (c >= '0' && c <= '9')
        return c - '0';
    else if (c >= 'A' && c <= 'F')
        return c - 'A' + 10;
    else if (c >= 'a' && c <= 'f')
        return c - 'a' + 10;
    else
        return -1;
}
/*x: uint64p.c */
static const char* parse_base (const char *p, int *base)
{
    *base = 10;
    if (*p == '0') {
        switch (p[1]) {
        case 'x': case 'X':
            *base = 16; return p + 2;
        case 'o': case 'O':
            *base =  8; return p + 2; 
        case 'b': case 'B':
            *base =  2; return p + 2; 
        default:
            *base =  8; return p;
        }
    } else {
        return p;
    }
}
/*x: uint64p.c */
value uint64_of_string(value s)          /* ML */
{
    uint64_t max_uint64 = ~(uint64_t)0;
    const char * p;
    uint64_t res, threshold;
    int base, d;

    p = parse_base (String_val(s), &base);
    threshold = max_uint64 / (uint64_t) base;
    for (res = 0; /*nothing*/; p++) {
        d = parse_digit(p);
        if (d < 0 || d >= base) break;
        /* Detect overflow in multiplication base * res */
        if (threshold < res) caml_failwith("overflow");
        res = res * (uint64_t)base + (uint64_t)d;
        /* Detect overflow in addition (base * res) + d */
        if (res < (uint64_t)d) caml_failwith("overflow");
    }
    if ((base == 10 || (base == 8 && res == 0)) && (*p == 'u' || *p == 'U')) p++;
    if (p != String_val(s) + caml_string_length(s)) caml_failwith("syntax");
    return caml_copy_int64(res);
}
/*e: uint64p.c */
