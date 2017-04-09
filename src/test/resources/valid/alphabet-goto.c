void write_uart(unsigned char c);

void write_alphabet() {
  unsigned char c;
  
 L:
  c = 97;
  if (c <= 122) {
    write_uart(c);
    c = c + 1;
    goto L;
  }
  write_uart(13);
  write_uart(10);
  return;
}
