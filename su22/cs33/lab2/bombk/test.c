#include <stdio.h>

int test(int eax, int edx, int esi, int ebx, int edi)
{
  eax = (edx - esi) / 2;
  ebx = eax + esi;
  if (edi < ebx)
  {
    edx = ebx - 1;
    eax = test(eax, edx, esi, ebx, edi) + ebx;
    return eax;
  }
  else
  {
    if (edi == ebx)
      return ebx;
    else
    {
      esi = ebx + 1;
      eax = test(eax, edx, esi, ebx, edi) + ebx;
      return eax;
    }
  }
}

int test_init(int edi)
{
  return test(99, 14, 0, 99, edi);
}


int main(int argc, char* argv[])
{
  for (int i = 0; i < 15; i++)
    printf("test(%d) returns %d\n", i, test_init(i));
  return 0;
}
