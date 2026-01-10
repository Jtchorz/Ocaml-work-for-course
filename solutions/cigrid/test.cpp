  #include <stdio.h>

  extern int printf(char* format);

  int main() {
    int i = 0;
    int** p = new int*[2];
    int* first = p[0];
    int* last = p[1];

    p[0] = new int[3];
    p[1] = new int[4];
    first = p[0];
    last = p[1];

    for (i = 0; i < 3; i++) {
      first[i] = i;
    }

    for (i = 0; i < 4; i++) {
      last[i] = first[2] + i;
    }

    printf("First: %d %d %d\n", first[0], first[1], first[2]);
    printf("Last: %d %d %d %d\n", last[0], last[1], last[2], last[3]);

    delete[] first;
    delete[] last;
    delete[] p;
  
    return 0;
  }