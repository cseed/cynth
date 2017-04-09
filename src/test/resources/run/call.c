// expected: 32'd7
int g(int x) {
  return x + 2;
}

int f() {
  return g(3) + 2;
}
