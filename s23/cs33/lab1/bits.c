/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 * Arthur Kim
 * UID: 004919548
 * classart@cs33.seas.ucla.edu
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }


NOTES:
  1. Our checker requires that you do NOT define a variable after 
     a statement that does not define a variable.

     For example, this is NOT allowed:

     int illegal_function_for_this_lab(int x, int y) {
      // this statement doesn't define a variable
      x = x + y + 1;
      
      // The checker for this lab does NOT allow the following statement,
      // because this variable definition comes after a statement 
      // that doesn't define a variable
      int z;

      return 0;
     }
     
  2. VERY IMPORTANT: Use the dlc (data lab checker) compiler (described in the handout)
     to check the legality of your solutions.
  3. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  4. Use the btest to check your functions for correctness.
  5. The maximum number of ops for each function is given in the
     header comment for each function. 

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the btest to verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
//1
/*
 * isTmax - returns 1 if x is the maximum, two's complement number,
 *     and 0 otherwise 
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmax(int x) {
  /* x + (x + 1) = -1 if x = TMAX or -1
   * Note that ~(-1) = 0
   * Make sure x isn't -1 */
  return !!(x + 1) & !~(x + x + 1);
}
//2
/* 
 * evenBits - return word with all even-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int evenBits(void) {
  /* 0b01010101 = 85; bits are zero-indexed */
  return 85 + (85 << 8) + (85 << 16) + (85 << 24);
}
//3
/* 
 * isEqual - return 1 if x == y, and 0 otherwise 
 *   Examples: isEqual(5,5) = 1, isEqual(4,5) = 0
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int isEqual(int x, int y) {
  /* x - y = x + -y = x + ~y + 1
   * if x == y, x + -y = 0 */
  return !(x + ~y + 1);
}
//4
/* 
 * fitsBits - return 1 if x can be represented as an 
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n) {
  /* After shift = x >> (n - 1), positive numbers should
   * only have zeroes left, or negative numbers should
   * only have ones left and shift + 1 = 0 */
  int shift;
  shift = x >> (n + ~0);
  return !shift | !(shift + 1);
}
//5
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  /* If x is not 0, return y; if x is 0, return z */
  int offset, mask;
  /* If x isn't 0, we wanna return y, and an offset masked
   * to 0
   * If x is 0, we wanna return y + (offset & mask) */
  offset = z + ~y + 1;
  mask = (!x) << 31 >> 31;
  return y + (offset & mask);
}
//6
/* 
 * isGreater - if x > y  then return 1, else return 0 
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
  /* If x > y, x - y - 1 is guaranteed to be 0 or positive.
   * If x <= y, x - y - 1 is guaranteed to be negative.
   * If x and y are positive, x - y is safe. Do that.
   * If x > 0 and y < 0, return 1 (base1)
   * If x < 0 and y > 0, return 0 (base2)
   * If x and y are negative, add TMAX to each and then compute
   * (base3). */
  int xsign, ysign, cmp_x, cmp_y, offset, diff, mask, base1, base2, base3;
  xsign = (x >> 31) + 1;
  ysign = (y >> 31) + 1;
  base1 = xsign & !ysign;
  base2 = !xsign & ysign;
  base3 = !xsign & !ysign;
  offset = (base3 << 31 >> 31) & (1 << 31);
  cmp_x = x + offset;
  cmp_y = y + offset;
  diff = cmp_x + ~cmp_y;
  mask = diff >> 31;
  return !base2 & (!mask | base1);
}
//7
/*
 * multFiveEighths - multiplies by 5/8 rounding toward 0.
 *   Should exactly duplicate effect of C expression (x*5/8),
 *   including overflow behavior.
 *   Examples: multFiveEighths(77) = 48
 *             multFiveEighths(-22) = -13
 *             multFiveEighths(1073741824) = 13421728 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
int multFiveEighths(int x) {
  int prod, bias;
  prod = x + x + x + x + x;
  bias = (prod >> 31) & 7;
  return (prod + bias) >> 3;
}
//8
/* 
 * logicalNeg - implement the ! operator, using all of 
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int logicalNeg(int x) {
  /* Note that:
   *
   * Negative numbers always have a sign bit of 1, meaning they
   * should always return 0.
   * 
   * For all positive numbers:
   *   x ^ TMAX < TMAX
   * For x = 0:
   *   x ^ TMAX == TMAX
   * Let xor be x ^ TMAX:
   *   xor + 1 only switches signs with x if x was 0 or TMIN, but
   *   we catch TMIN with the check for negative numbers. */
  int signbit, tmax, xor, signafter, signcheck;
  signbit = (x >> 31) + 1; /* 0 if negative, 1 if positive */
  tmax = ~(1 << 31);
  xor = x ^ tmax;
  signafter = ((xor + 1) >> 31) + 1;
  signcheck = signafter + ~signbit + 1; /* 1 if sign switch */
  return signbit & signcheck;
}
//9
/* 
 * twosComp2SignMag - Convert from two's complement to sign-magnitude 
 *   where the MSB is the sign bit
 *   You can assume that x > TMin
 *   Example: twosComp2SignMag(-5) = 0x80000005.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 4
 */
int twosComp2SignMag(int x) {
  /* Use an offset and a mask based on x's initial sign, and add
   * offset = 2 * (TMIN - x) if negative? Then add the sign bit. */
  int diff, offset, mask;
  diff = (1 << 31) + ~x + 1;
  offset = diff + diff;
  mask = (x >> 31);
  return x + (offset & mask) + (mask << 31);
}
//10
/*
 * isPower2 - returns 1 if x is a power of 2, and 0 otherwise
 *   Examples: isPower2(5) = 0, isPower2(8) = 1, isPower2(0) = 0
 *   Note that no negative number is a power of 2.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int isPower2(int x) {
  /* From Piazza: Note that the intersection of a power of 2 and
   * its negative gives you the original number; this isn't the
   * case for non-powers of 2. Then, just make sure it isn't
   * TMIN (x & ~n == 0 if x != n)
   *
   * 8: 0b 0000 1000
   * -8: 0b 1111 1000
   *
   * 5: 0b 0000 0101
   * -5: 0b 1111 1011

   * Make sure both sides of the & give 1
   * Find the intersection of x and its negative; if x minus the
   * intersection is 0, numbers are the same; just make sure it
   * isn't TMIN */
  int intersect;
  intersect = x & (~x + 1);
  return !(x + ~intersect + 1) & !!(x & ~(1 << 31));
}
