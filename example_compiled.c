#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "setjmp.h"
#include "sys/resource.h" /* for hard stack limit */

#define STACK_SIZE 32768
#define MULTI_LINE_STRING(a) #a

typedef struct {
  void* apply_func;
  void* func;
  void* forward;
  int tenured;
  char* str;
  int num_freevars;
  void* bound_args[];
} clos;

typedef struct {
  void* func;
  int num_args;
  clos* args[];
} call;

typedef enum { POOL_A, POOL_B } cur_pool_enum;

static cur_pool_enum cur_pool;
static char* mem_pool_a;
static char* mem_pool_b;
static char* mem_pool_a_end;
static char* mem_pool_b_end;
static char* cur_mem_ptr;
static char* cur_mem_pool_end;
static char* cur_stack;
static char* stack_end;
static int stack_grows_up;

void cleanup() {
  free(mem_pool_a);
  free(mem_pool_b);
}
void halt_func(clos* halt, clos* val) {
  printf("%s\n", val->str);
  cleanup();
  exit(0);
}
clos halt_impl = {&halt_func, /* apply func */
                  NULL, /* func */
                  NULL, /* forward pointer */
                     0, /* tenured */
                  "halt func", /* func string */
                     0, /* num_freevars */
                  NULL}; // no bound args
static clos* halt = &halt_impl;

/* major GC not yet implemented */
void switch_pools() {
  printf("switching pools\n");
  if(cur_pool == POOL_A) { 
    cur_mem_ptr = mem_pool_b;
    cur_mem_pool_end = mem_pool_b_end;
  } else { 
    cur_mem_ptr = mem_pool_a;
    cur_mem_pool_end = mem_pool_a_end;
  }
}
int check_stack() {
  if(stack_grows_up) {
    return cur_stack >= stack_end;
  } else { 
    return cur_stack <= stack_end;
  }
}
void perform_call(call* call);
void toplevel();
static call* saved_cont_call;
jmp_buf empty_stack_state;
static int mem_needed;
void trampoline(call* cl) {
 /* save stack pointer and registers (including program counter) */
  if(setjmp(empty_stack_state)) {
    /* if got here by longjump, load saved call (to continue after GC) */
    cl = saved_cont_call; 
    if(STACK_SIZE < mem_needed) { 
      printf("Stack size too small!\n");
      cleanup();
      exit(0);
    }
  }

  cur_stack = ((char*)alloca(1));
  char* last_stack = cur_stack;
  cur_stack = ((char*)alloca(1));
  stack_grows_up = (cur_stack > last_stack);
  if (stack_grows_up) {
    stack_end = last_stack + STACK_SIZE;
  } else {
    stack_end = last_stack - STACK_SIZE;
  }
  perform_call(cl);
}
void entry_point() {
  call* toplevel_call = alloca(sizeof(call));
  toplevel_call->func = &toplevel;
  toplevel_call->num_args = 0;
  trampoline(toplevel_call);
}

char* pool_alloc(int num_bytes) {
  char* new_ptr = (cur_mem_ptr+num_bytes);
  if (new_ptr >= cur_mem_pool_end) {
    printf("Out of memory!\n");
    cleanup();
    exit(0);
  } else { 
    char* ret_ptr = cur_mem_ptr;
    cur_mem_ptr = new_ptr;
    return ret_ptr;
  }
}
static unsigned long bytes_copied;

clos* copy(clos* cls) {
 /* if forward pointer is unset, copy closure,
    set forward address to copied closure's address
    return return forward address  */
  if(cls->forward == NULL) {
    /* if tenured, just return pointer */
    /* it's safe from the incoming stack crunch */
    if(cls->tenured == 1) { return cls; }
    int num_freevars = cls->num_freevars;
    int freevars_size = (sizeof(clos*)*num_freevars);
    int clos_size = sizeof(clos)+freevars_size+sizeof(char*);

    bytes_copied += clos_size;

    /* allocate memory for closure in tenured generation */
    clos* new_cls = (clos*)(pool_alloc(clos_size));

    /* set forward pointer for further copy calls that reach this closure */
    cls->forward = new_cls;
    new_cls->tenured = 1;
    /* copy closure fields */
    new_cls->apply_func = cls->apply_func;
    new_cls->func = cls->func;
    new_cls->num_freevars = cls->num_freevars;
    /* copy string pointer (i think we can assume it's allocated statically) */
    new_cls->str = cls->str;
    new_cls->forward = NULL;
    /* copy bound arguments */
    for(int i = 0; i < num_freevars; i++) {
      new_cls->bound_args[i] = copy(cls->bound_args[i]);
    }
  }
  return cls->forward;
}
void minor_gc() {
  bytes_copied = 0;
  int num_args = saved_cont_call->num_args;
  for(int i = 0; i < num_args; i++) {
    saved_cont_call->args[i] = copy(saved_cont_call->args[i]);
  }
  printf("bytes_copied: %lu\n", bytes_copied);
  longjmp(empty_stack_state, 1);
}

/* typedefs */
typedef void (*fp_typedef834)(clos*, clos*, clos*);
typedef void (*fp_typedef831)(clos*, clos*);
typedef void (*fp_typedef828)();

/* apply function definitions */
void apply_func2870(clos* cls, clos* arg869) {
  return ((fp_typedef831)(cls->func))(arg869,((clos*)(cls->bound_args[0])));
}

void apply_func3867(clos* cls, clos* arg866) {
  return ((fp_typedef834)(cls->func))(arg866,((clos*)(cls->bound_args[0])),((clos*)(cls->bound_args[1])));
}

void apply_func2842(clos* cls, clos* arg840,clos* arg841) {
  return ((fp_typedef831)(cls->func))(arg840,arg841);
}

void apply_func3838(clos* cls, clos* arg836,clos* arg837) {
  return ((fp_typedef834)(cls->func))(arg836,arg837,((clos*)(cls->bound_args[0])));
}


/* forward declarations */
void func863(clos*, clos*, clos*) __attribute__ ((noreturn));
void func859(clos*, clos*, clos*) __attribute__ ((noreturn));
void func857(clos*, clos*) __attribute__ ((noreturn));
void func853(clos*, clos*, clos*) __attribute__ ((noreturn));
void func851(clos*, clos*) __attribute__ ((noreturn));
void func849(clos*, clos*) __attribute__ ((noreturn));
void func845(clos*, clos*, clos*) __attribute__ ((noreturn));
void func843(clos*, clos*) __attribute__ ((noreturn));
void func832(clos*, clos*, clos*) __attribute__ ((noreturn));
void func829(clos*, clos*) __attribute__ ((noreturn));

/* lifted function definitions */
void toplevel() {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*0)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->num_args = 0;
    saved_cont_call->func = toplevel;
    minor_gc();  }
  int size848 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos844 = alloca(size848);
  clos844->num_freevars = 0;
  clos844->func = &func843;
  clos844->apply_func = &apply_func2842;
  clos844->forward = NULL;
  clos844->tenured = 0;
  clos844->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (Y X))));
  int size868 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos850 = alloca(size868);
  clos850->num_freevars = 1;
  clos850->func = &func849;
  clos850->apply_func = &apply_func2870;
  clos850->forward = NULL;
  clos850->tenured = 0;
  clos850->str = NULL;
  clos850->bound_args[0] = (void*)halt;
  int size839 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos830 = alloca(size839);
  clos830->num_freevars = 0;
  clos830->func = &func829;
  clos830->apply_func = &apply_func2842;
  clos830->forward = NULL;
  clos830->tenured = 0;
  clos830->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (X Y))));
  ((fp_typedef834)(clos830->apply_func))(clos830,clos844,clos850);
}

void func849(clos* rv818, clos* halt) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*2)+(sizeof(char)*0)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = rv818;
    saved_cont_call->args[1] = halt;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func849;
    minor_gc();  }
  int size862 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos858 = alloca(size862);
  clos858->num_freevars = 0;
  clos858->func = &func857;
  clos858->apply_func = &apply_func2842;
  clos858->forward = NULL;
  clos858->tenured = 0;
  clos858->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (Y X))));
  int size865 = (sizeof(clos)+(sizeof(clos*)*2)+(sizeof(char*)));
  clos* clos864 = alloca(size865);
  clos864->num_freevars = 2;
  clos864->func = &func863;
  clos864->apply_func = &apply_func3867;
  clos864->forward = NULL;
  clos864->tenured = 0;
  clos864->str = NULL;
  clos864->bound_args[0] = (void*)halt;
  clos864->bound_args[1] = (void*)rv818;
  int size856 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos852 = alloca(size856);
  clos852->num_freevars = 0;
  clos852->func = &func851;
  clos852->apply_func = &apply_func2842;
  clos852->forward = NULL;
  clos852->tenured = 0;
  clos852->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (X Y))));
  ((fp_typedef834)(clos852->apply_func))(clos852,clos858,clos864);
}

void func863(clos* rv819, clos* halt, clos* rv818) {
  ((fp_typedef834)(rv818->apply_func))(rv818,rv819,halt);
}

void func857(clos* X, clos* k822) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k822;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func857;
    minor_gc();  }
  int size861 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos860 = alloca(size861);
  clos860->num_freevars = 1;
  clos860->func = &func859;
  clos860->apply_func = &apply_func3838;
  clos860->forward = NULL;
  clos860->tenured = 0;
  clos860->str = MULTI_LINE_STRING((LAMBDA (Y) (Y X)));
  clos860->bound_args[0] = (void*)X;
  ((fp_typedef831)(k822->apply_func))(k822,clos860);
}

void func859(clos* Y, clos* k823, clos* X) {
  ((fp_typedef834)(Y->apply_func))(Y,X,k823);
}

void func851(clos* X, clos* k820) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k820;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func851;
    minor_gc();  }
  int size855 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos854 = alloca(size855);
  clos854->num_freevars = 1;
  clos854->func = &func853;
  clos854->apply_func = &apply_func3838;
  clos854->forward = NULL;
  clos854->tenured = 0;
  clos854->str = MULTI_LINE_STRING((LAMBDA (Y) (X Y)));
  clos854->bound_args[0] = (void*)X;
  ((fp_typedef831)(k820->apply_func))(k820,clos854);
}

void func853(clos* Y, clos* k821, clos* X) {
  ((fp_typedef834)(X->apply_func))(X,Y,k821);
}

void func843(clos* X, clos* k826) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k826;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func843;
    minor_gc();  }
  int size847 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos846 = alloca(size847);
  clos846->num_freevars = 1;
  clos846->func = &func845;
  clos846->apply_func = &apply_func3838;
  clos846->forward = NULL;
  clos846->tenured = 0;
  clos846->str = MULTI_LINE_STRING((LAMBDA (Y) (Y X)));
  clos846->bound_args[0] = (void*)X;
  ((fp_typedef831)(k826->apply_func))(k826,clos846);
}

void func845(clos* Y, clos* k827, clos* X) {
  ((fp_typedef834)(Y->apply_func))(Y,X,k827);
}

void func829(clos* X, clos* k824) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k824;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func829;
    minor_gc();  }
  int size835 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos833 = alloca(size835);
  clos833->num_freevars = 1;
  clos833->func = &func832;
  clos833->apply_func = &apply_func3838;
  clos833->forward = NULL;
  clos833->tenured = 0;
  clos833->str = MULTI_LINE_STRING((LAMBDA (Y) (X Y)));
  clos833->bound_args[0] = (void*)X;
  ((fp_typedef831)(k824->apply_func))(k824,clos833);
}

void func832(clos* Y, clos* k825, clos* X) {
  ((fp_typedef834)(X->apply_func))(X,Y,k825);
}

void perform_call(call* cl) {
  switch(cl->num_args) {
  case 0:
    ((fp_typedef828)cl->func)();
    break;
  case 2:
    ((fp_typedef831)cl->func)(cl->args[0],cl->args[1]);
    break;
  case 3:
    ((fp_typedef834)cl->func)(cl->args[0],cl->args[1],cl->args[2]);
    break;
  }
}

int main() { 
  struct rlimit rl;
  rl.rlim_cur = STACK_SIZE*2;
  setrlimit(RLIMIT_STACK, &rl);
  mem_pool_a = malloc(1024*1024*8);
  mem_pool_b = malloc(1024*1024*1024);
  mem_pool_a_end = (mem_pool_a+(1024*1024*8));
  mem_pool_b_end = (mem_pool_b+(1024*1024*8));
  cur_mem_ptr = mem_pool_a;
  cur_mem_pool_end = mem_pool_a_end;
  saved_cont_call = malloc(sizeof(call)+(sizeof(void*)*3));
  entry_point();
  return 0;
}" 
"#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "setjmp.h"
#include "sys/resource.h" /* for hard stack limit */

#define STACK_SIZE 32768
#define MULTI_LINE_STRING(a) #a

typedef struct {
  void* apply_func;
  void* func;
  void* forward;
  int tenured;
  char* str;
  int num_freevars;
  void* bound_args[];
} clos;

typedef struct {
  void* func;
  int num_args;
  clos* args[];
} call;

typedef enum { POOL_A, POOL_B } cur_pool_enum;

static cur_pool_enum cur_pool;
static char* mem_pool_a;
static char* mem_pool_b;
static char* mem_pool_a_end;
static char* mem_pool_b_end;
static char* cur_mem_ptr;
static char* cur_mem_pool_end;
static char* cur_stack;
static char* stack_end;
static int stack_grows_up;

void cleanup() {
  free(mem_pool_a);
  free(mem_pool_b);
}
void halt_func(clos* halt, clos* val) {
  printf("%s\n", val->str);
  cleanup();
  exit(0);
}
clos halt_impl = {&halt_func, /* apply func */
                  NULL, /* func */
                  NULL, /* forward pointer */
                     0, /* tenured */
                  "halt func", /* func string */
                     0, /* num_freevars */
                  NULL}; // no bound args
static clos* halt = &halt_impl;

/* major GC not yet implemented */
void switch_pools() {
  printf("switching pools\n");
  if(cur_pool == POOL_A) { 
    cur_mem_ptr = mem_pool_b;
    cur_mem_pool_end = mem_pool_b_end;
  } else { 
    cur_mem_ptr = mem_pool_a;
    cur_mem_pool_end = mem_pool_a_end;
  }
}
int check_stack() {
  if(stack_grows_up) {
    return cur_stack >= stack_end;
  } else { 
    return cur_stack <= stack_end;
  }
}
void perform_call(call* call);
void toplevel();
static call* saved_cont_call;
jmp_buf empty_stack_state;
static int mem_needed;
void trampoline(call* cl) {
 /* save stack pointer and registers (including program counter) */
  if(setjmp(empty_stack_state)) {
    /* if got here by longjump, load saved call (to continue after GC) */
    cl = saved_cont_call; 
    if(STACK_SIZE < mem_needed) { 
      printf("Stack size too small!\n");
      cleanup();
      exit(0);
    }
  }

  cur_stack = ((char*)alloca(1));
  char* last_stack = cur_stack;
  cur_stack = ((char*)alloca(1));
  stack_grows_up = (cur_stack > last_stack);
  if (stack_grows_up) {
    stack_end = last_stack + STACK_SIZE;
  } else {
    stack_end = last_stack - STACK_SIZE;
  }
  perform_call(cl);
}
void entry_point() {
  call* toplevel_call = alloca(sizeof(call));
  toplevel_call->func = &toplevel;
  toplevel_call->num_args = 0;
  trampoline(toplevel_call);
}

char* pool_alloc(int num_bytes) {
  char* new_ptr = (cur_mem_ptr+num_bytes);
  if (new_ptr >= cur_mem_pool_end) {
    printf("Out of memory!\n");
    cleanup();
    exit(0);
  } else { 
    char* ret_ptr = cur_mem_ptr;
    cur_mem_ptr = new_ptr;
    return ret_ptr;
  }
}
static unsigned long bytes_copied;

clos* copy(clos* cls) {
 /* if forward pointer is unset, copy closure,
    set forward address to copied closure's address
    return return forward address  */
  if(cls->forward == NULL) {
    /* if tenured, just return pointer */
    /* it's safe from the incoming stack crunch */
    if(cls->tenured == 1) { return cls; }
    int num_freevars = cls->num_freevars;
    int freevars_size = (sizeof(clos*)*num_freevars);
    int clos_size = sizeof(clos)+freevars_size+sizeof(char*);

    bytes_copied += clos_size;

    /* allocate memory for closure in tenured generation */
    clos* new_cls = (clos*)(pool_alloc(clos_size));

    /* set forward pointer for further copy calls that reach this closure */
    cls->forward = new_cls;
    new_cls->tenured = 1;
    /* copy closure fields */
    new_cls->apply_func = cls->apply_func;
    new_cls->func = cls->func;
    new_cls->num_freevars = cls->num_freevars;
    /* copy string pointer (i think we can assume it's allocated statically) */
    new_cls->str = cls->str;
    new_cls->forward = NULL;
    /* copy bound arguments */
    for(int i = 0; i < num_freevars; i++) {
      new_cls->bound_args[i] = copy(cls->bound_args[i]);
    }
  }
  return cls->forward;
}
void minor_gc() {
  bytes_copied = 0;
  int num_args = saved_cont_call->num_args;
  for(int i = 0; i < num_args; i++) {
    saved_cont_call->args[i] = copy(saved_cont_call->args[i]);
  }
  printf("bytes_copied: %lu\n", bytes_copied);
  longjmp(empty_stack_state, 1);
}

/* typedefs */
typedef void (*fp_typedef834)(clos*, clos*, clos*);
typedef void (*fp_typedef831)(clos*, clos*);
typedef void (*fp_typedef828)();

/* apply function definitions */
void apply_func2870(clos* cls, clos* arg869) {
  return ((fp_typedef831)(cls->func))(arg869,((clos*)(cls->bound_args[0])));
}

void apply_func3867(clos* cls, clos* arg866) {
  return ((fp_typedef834)(cls->func))(arg866,((clos*)(cls->bound_args[0])),((clos*)(cls->bound_args[1])));
}

void apply_func2842(clos* cls, clos* arg840,clos* arg841) {
  return ((fp_typedef831)(cls->func))(arg840,arg841);
}

void apply_func3838(clos* cls, clos* arg836,clos* arg837) {
  return ((fp_typedef834)(cls->func))(arg836,arg837,((clos*)(cls->bound_args[0])));
}


/* forward declarations */
void func863(clos*, clos*, clos*) __attribute__ ((noreturn));
void func859(clos*, clos*, clos*) __attribute__ ((noreturn));
void func857(clos*, clos*) __attribute__ ((noreturn));
void func853(clos*, clos*, clos*) __attribute__ ((noreturn));
void func851(clos*, clos*) __attribute__ ((noreturn));
void func849(clos*, clos*) __attribute__ ((noreturn));
void func845(clos*, clos*, clos*) __attribute__ ((noreturn));
void func843(clos*, clos*) __attribute__ ((noreturn));
void func832(clos*, clos*, clos*) __attribute__ ((noreturn));
void func829(clos*, clos*) __attribute__ ((noreturn));

/* lifted function definitions */
void toplevel() {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*0)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->num_args = 0;
    saved_cont_call->func = toplevel;
    minor_gc();  }
  int size848 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos844 = alloca(size848);
  clos844->num_freevars = 0;
  clos844->func = &func843;
  clos844->apply_func = &apply_func2842;
  clos844->forward = NULL;
  clos844->tenured = 0;
  clos844->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (Y X))));
  int size868 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos850 = alloca(size868);
  clos850->num_freevars = 1;
  clos850->func = &func849;
  clos850->apply_func = &apply_func2870;
  clos850->forward = NULL;
  clos850->tenured = 0;
  clos850->str = NULL;
  clos850->bound_args[0] = (void*)halt;
  int size839 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos830 = alloca(size839);
  clos830->num_freevars = 0;
  clos830->func = &func829;
  clos830->apply_func = &apply_func2842;
  clos830->forward = NULL;
  clos830->tenured = 0;
  clos830->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (X Y))));
  ((fp_typedef834)(clos830->apply_func))(clos830,clos844,clos850);
}

void func849(clos* rv818, clos* halt) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31))+(sizeof(clos)+(sizeof(clos*)*2)+(sizeof(char)*0)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = rv818;
    saved_cont_call->args[1] = halt;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func849;
    minor_gc();  }
  int size862 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos858 = alloca(size862);
  clos858->num_freevars = 0;
  clos858->func = &func857;
  clos858->apply_func = &apply_func2842;
  clos858->forward = NULL;
  clos858->tenured = 0;
  clos858->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (Y X))));
  int size865 = (sizeof(clos)+(sizeof(clos*)*2)+(sizeof(char*)));
  clos* clos864 = alloca(size865);
  clos864->num_freevars = 2;
  clos864->func = &func863;
  clos864->apply_func = &apply_func3867;
  clos864->forward = NULL;
  clos864->tenured = 0;
  clos864->str = NULL;
  clos864->bound_args[0] = (void*)halt;
  clos864->bound_args[1] = (void*)rv818;
  int size856 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos852 = alloca(size856);
  clos852->num_freevars = 0;
  clos852->func = &func851;
  clos852->apply_func = &apply_func2842;
  clos852->forward = NULL;
  clos852->tenured = 0;
  clos852->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (X Y))));
  ((fp_typedef834)(clos852->apply_func))(clos852,clos858,clos864);
}

void func863(clos* rv819, clos* halt, clos* rv818) {
  ((fp_typedef834)(rv818->apply_func))(rv818,rv819,halt);
}

void func857(clos* X, clos* k822) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k822;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func857;
    minor_gc();  }
  int size861 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos860 = alloca(size861);
  clos860->num_freevars = 1;
  clos860->func = &func859;
  clos860->apply_func = &apply_func3838;
  clos860->forward = NULL;
  clos860->tenured = 0;
  clos860->str = MULTI_LINE_STRING((LAMBDA (Y) (Y X)));
  clos860->bound_args[0] = (void*)X;
  ((fp_typedef831)(k822->apply_func))(k822,clos860);
}

void func859(clos* Y, clos* k823, clos* X) {
  ((fp_typedef834)(Y->apply_func))(Y,X,k823);
}

void func851(clos* X, clos* k820) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k820;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func851;
    minor_gc();  }
  int size855 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos854 = alloca(size855);
  clos854->num_freevars = 1;
  clos854->func = &func853;
  clos854->apply_func = &apply_func3838;
  clos854->forward = NULL;
  clos854->tenured = 0;
  clos854->str = MULTI_LINE_STRING((LAMBDA (Y) (X Y)));
  clos854->bound_args[0] = (void*)X;
  ((fp_typedef831)(k820->apply_func))(k820,clos854);
}

void func853(clos* Y, clos* k821, clos* X) {
  ((fp_typedef834)(X->apply_func))(X,Y,k821);
}

void func843(clos* X, clos* k826) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k826;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func843;
    minor_gc();  }
  int size847 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos846 = alloca(size847);
  clos846->num_freevars = 1;
  clos846->func = &func845;
  clos846->apply_func = &apply_func3838;
  clos846->forward = NULL;
  clos846->tenured = 0;
  clos846->str = MULTI_LINE_STRING((LAMBDA (Y) (Y X)));
  clos846->bound_args[0] = (void*)X;
  ((fp_typedef831)(k826->apply_func))(k826,clos846);
}

void func845(clos* Y, clos* k827, clos* X) {
  ((fp_typedef834)(Y->apply_func))(Y,X,k827);
}

void func829(clos* X, clos* k824) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf("Soft stack size limit reached, GCing\n");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k824;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func829;
    minor_gc();  }
  int size835 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos833 = alloca(size835);
  clos833->num_freevars = 1;
  clos833->func = &func832;
  clos833->apply_func = &apply_func3838;
  clos833->forward = NULL;
  clos833->tenured = 0;
  clos833->str = MULTI_LINE_STRING((LAMBDA (Y) (X Y)));
  clos833->bound_args[0] = (void*)X;
  ((fp_typedef831)(k824->apply_func))(k824,clos833);
}

void func832(clos* Y, clos* k825, clos* X) {
  ((fp_typedef834)(X->apply_func))(X,Y,k825);
}

void perform_call(call* cl) {
  switch(cl->num_args) {
  case 0:
    ((fp_typedef828)cl->func)();
    break;
  case 2:
    ((fp_typedef831)cl->func)(cl->args[0],cl->args[1]);
    break;
  case 3:
    ((fp_typedef834)cl->func)(cl->args[0],cl->args[1],cl->args[2]);
    break;
  }
}

int main() { 
  struct rlimit rl;
  rl.rlim_cur = STACK_SIZE*2;
  setrlimit(RLIMIT_STACK, &rl);
  mem_pool_a = malloc(1024*1024*8);
  mem_pool_b = malloc(1024*1024*1024);
  mem_pool_a_end = (mem_pool_a+(1024*1024*8));
  mem_pool_b_end = (mem_pool_b+(1024*1024*8));
  cur_mem_ptr = mem_pool_a;
  cur_mem_pool_end = mem_pool_a_end;
  saved_cont_call = malloc(sizeof(call)+(sizeof(void*)*3));
  entry_point();
  return 0;
}
