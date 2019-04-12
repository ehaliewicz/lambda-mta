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
typedef void (*fp_typedef756)(clos*, clos*, clos*);
typedef void (*fp_typedef753)(clos*, clos*);
typedef void (*fp_typedef750)();

/* apply function definitions */
void apply_func2764(clos* cls, clos* arg762,clos* arg763) {
  return ((fp_typedef753)(cls->func))(arg762,arg763);
}

void apply_func3760(clos* cls, clos* arg758,clos* arg759) {
  return ((fp_typedef756)(cls->func))(arg758,arg759,((clos*)(cls->bound_args[0])));
}


/* forward declarations */
void func754(clos*, clos*, clos*) __attribute__ ((noreturn));
void func751(clos*, clos*) __attribute__ ((noreturn));

/* lifted function definitions */
void toplevel() {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char)*31)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf(\"Soft stack size limit reached, GCing\\n\");
    saved_cont_call->num_args = 0;
    saved_cont_call->func = toplevel;
    minor_gc();  }
  int size761 = (sizeof(clos)+(sizeof(clos*)*0)+(sizeof(char*)));
  clos* clos752 = alloca(size761);
  clos752->num_freevars = 0;
  clos752->func = &func751;
  clos752->apply_func = &apply_func2764;
  clos752->forward = NULL;
  clos752->tenured = 0;
  clos752->str = MULTI_LINE_STRING((LAMBDA (X) (LAMBDA (Y) (X Y))));
  ((fp_typedef753)(halt->apply_func))(halt,clos752);
}

void func751(clos* X, clos* k748) {
  /* check if allocation for this function fits on the stack */
  mem_needed = ((sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char)*18)))*2;
  if (stack_grows_up) {
    cur_stack+= mem_needed;
  } else { 
    cur_stack-= mem_needed;
  }
  if(check_stack()) {
    /* GC + save call */
    printf(\"Soft stack size limit reached, GCing\\n\");
    saved_cont_call->args[0] = X;
    saved_cont_call->args[1] = k748;
    saved_cont_call->num_args = 2;
    saved_cont_call->func = func751;
    minor_gc();  }
  int size757 = (sizeof(clos)+(sizeof(clos*)*1)+(sizeof(char*)));
  clos* clos755 = alloca(size757);
  clos755->num_freevars = 1;
  clos755->func = &func754;
  clos755->apply_func = &apply_func3760;
  clos755->forward = NULL;
  clos755->tenured = 0;
  clos755->str = MULTI_LINE_STRING((LAMBDA (Y) (X Y)));
  clos755->bound_args[0] = (void*)X;
  ((fp_typedef753)(k748->apply_func))(k748,clos755);
}

void func754(clos* Y, clos* k749, clos* X) {
  ((fp_typedef756)(X->apply_func))(X,Y,k749);
}

void perform_call(call* cl) {
  switch(cl->num_args) {
  case 0:
    ((fp_typedef750)cl->func)();
    break;
  case 2:
    ((fp_typedef753)cl->func)(cl->args[0],cl->args[1]);
    break;
  case 3:
    ((fp_typedef756)cl->func)(cl->args[0],cl->args[1],cl->args[2]);
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
