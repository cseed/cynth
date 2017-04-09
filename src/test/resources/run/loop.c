// expected: 32'd10
int f() {
  int i = 0;
  int s = 0;
  while (i < 5) {
    s += i;
    ++i;
  }
  return s;
}
