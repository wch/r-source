/* Need to include non-public Defn.h to get
   the SA_NOSAVE constant for calling R_CleanUp.
   This is not part of the published API!
   It is here only to illustrate a different approach,
   not a recommended one.
 */
#include "../src/include/R.h"
#include "../src/include/Startup.h"

int
main(int argc, char *argv[])
{
  printf("%d\n", SA_NOSAVE);
}
