extern int islower(int c);
extern int tolower(int c);
extern int putchar(int c);

int main() {
  int c = 'A';
  if (islower(c) == 0) {
    c = tolower(c);
  }
  return c;
}
