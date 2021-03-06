/* L3 Library for 15411 Compiler Design */

/* fpt alias for float */
typedef int fpt;

fpt fadd(fpt x, fpt y);
fpt fsub(fpt x, fpt y);
fpt fmul(fpt x, fpt y);
fpt fdiv(fpt x, fpt y);
bool fless(fpt x, fpt y);

fpt itof(int n);
int ftoi(fpt x);

void print_fpt(fpt x);
void print_int(int n);
void print_hex(int n);
