#include <stdio.h>

int test(int eax, int edx, int esi, int ebx, int edi)
{
  eax = (edx - esi) / 2;
  ebx = eax + esi;
  if (edi < 
}

int test_init(int edi)
{
  return test(99, 14, 0, 99, edi);
}


int main(int argc, char* argv[])
{
  
}
