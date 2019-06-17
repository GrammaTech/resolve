#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  int a = atoi(argv[1]);;
  int b = atoi(argv[2]);

  for(int i = b; i<a; i--){
    printf("%d\n", i);
  }

  return 0;
}
