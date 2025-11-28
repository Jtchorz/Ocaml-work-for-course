int main() {
  int x = 1;
  int y = 3;
  int z = 7;
  
  // 3
  int a = x | y;
  // 1
  int b = x & y;
  // 7
  int c = (x & y) | z;
  // 4 * 6 | 5 = 29
  int d = (x << 2) * (3 << 1) | (z+3) >> 1;

  if (a != 3 && b == 1 && c == 7 && d == 29) {
    return 0;
  } else {
    return 1;
  }
}