void write_uart(uint8 c);

void write_alphabet() {
  uint8 c;
  
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
