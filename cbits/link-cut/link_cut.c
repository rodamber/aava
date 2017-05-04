#include <stdlib.h>
#include <stdio.h>

/* MinUnit */
int tests_run = 0;

#ifdef DEBUG

#define mu_assert(message, test) do { if (!(test)) return message; } while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; \
    if (message) return message; } while (0)

#else

#define mu_assert(message,test)
#define mu_run_test

#endif

int main_link_cut() {
  return 0;
}

/* int main() { return main_link_cut(); } */
