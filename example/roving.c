__nonstd void sleep(int ms);
__nonstd void write_leds(unsigned __int4 c);

void f()
{
  unsigned __int4 c = 1;
  unsigned __int1 dir = 1;
  
  while (1)
    {
      if (dir && c == 8)
	dir = 0;
      else if (!dir && c == 1)
	dir = 1;

      if (dir)
	c <<= 1;
      else
	c >>= 1;

      write_leds(c);
      
      sleep(1000); // 1s
    }
}
