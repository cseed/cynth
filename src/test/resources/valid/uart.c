void uart_write(char c);

void write_alphabet() {
  char c = 'a';
  
  while (c <= 'z') {
    uart_write(c);
    ++c;
  }
  uart_write('\r');
  uart_write('\n');
}
