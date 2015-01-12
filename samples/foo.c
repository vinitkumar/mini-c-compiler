/*******************************************************************
 * foo.c
 * Atul Varma - 5/24/2004
 * CS Independent Study
 * $Id: foo.c,v 1.1 2004/05/27 16:25:14 varmaa Exp $
 *
 * This is a simple C file that should be compiled by my mini-C
 * compiler.
 *******************************************************************
*/

/* Prototypes for some standard C library functions (the code
   calls these directly). */
extern int printf(char *str, ...);
extern char *malloc(int size);
extern int free(char *ptr);

/* Test of extern variable.  How many times we've called
   a printf() function. */
extern int stuff_count;

/* Increments this global variable. */
extern int increment_stuff_count();

/* Test of global variable.  How many times we've called
   the fib() function. */
int fib_count;

/* fibonacci function: Test of basic branching and recursion. */
static int fib(int i)
{
  fib_count += 1;
  if (i == 1) {
    return 1;
  } else {
    if (i == 0) {
      return 0;
    } else {
      return fib(i-1) + fib(i-2);
    }
  }
}

/* Just a wrapper to easily show the results of a
   call to fib(). */
static int show_fib(int i)
{
  printf("fib(%d) is %d.\n", i, fib(i));
  return 0;
}

/* Test of pointer indirection and char type. */
static int set_a(char *c)
{
  *c = 'a';
  return 0;
}

/* Test of string literals and returning char *'s. */
static char *get_literal()
{
  return "blah\n";
}

/* Main program that runs the tests. */
int main(int argc, char **argv) {
  char c;
  int i;

  c = 'h';

  /* Test of multiple assignment. */
  fib_count = stuff_count = 0;

  /* Test of command-line argument passing, pointer
     indirection/array indexing, for looping. */
  printf("My executable name is %s.\n", *argv);
  for (i = 0; i < argc; i += 1) {
    printf("  argv[%d] is: %s    "
           "argv[%d][0] is: %c\n", i, argv[i], i, argv[i][0]);
    increment_stuff_count();
  }

  /* Test of while looping with break/continue. */
  i = 0;
  while (1) {
    show_fib(i);
    i += 1;
    if (i > 5)
      break;
    else
      continue;
  }
  stuff_count = stuff_count * 2;

  printf("fib_count is %d.\n", fib_count);
  printf("stuff_count is %d.\n", stuff_count);

  printf("before set_a(&c), c == '%c'\n", c);

  /* Test of address-of (&) operator. */
  set_a(&c);

  {
    /* Test of char-int and int-char type coercion. */
    int a;
    char b;
    int c;

    /* Note that in two's complement arithmetic, this is
       a 32-bit int consisting of all 1's.

       (This is also a test of the '-' unary operator.) */
    a = -1;

    /* The following line will raise a warning from the
       compiler, because a signed 32-bit int is being truncated
       to an unsigned 8-bit char. */
    b = a;

    c = b;

    printf("  a = %d\n", a);
    printf("  b = %d\n", b);
    printf("  c = %d\n", c);
  }

  /* Note now that the scope of c is in the function's main
     scope, not the scope of the above compound statement.
     This test makes sure that the address and contents
     of c did not change during the execution of the
     compound statement. */
  printf("after set_a(&c), c == '%c'\n", c);

  printf("get_literal() = %s\n", get_literal());

  /* Pointer indexing via array example. */
  printf("get_literal()[3] = %c\n", get_literal()[3]);

  {
    /* Test of building a string using assignment via array indexing
       of a char pointer.  The buffer is dynamically allocated. */
    char *c;

    c = malloc(30);
    c[0] = 'h';
    c[1] = 'i';
    c[2] = 0;
    printf("array-built string is: %s\n", c);
    free(c);
  }
  return 0;
}
