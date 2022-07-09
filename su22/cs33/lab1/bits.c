/* 
 * CS:APP Data Lab 
 * 
 * Arthur Kim
 * UID: 004919548
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
  int sum = x + x + 2;
  return !sum;
}
/* 
 * evenBits - return word with all even-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int evenBits(void) {
  // 0b01010101 = 85; add bitwise shifted numbers to create 32-bit
  // 010101...
  return 85 + (85 << 8) + (85 << 16) + (85 << 24);
}
//2
/* 
 * isEqual - return 1 if x == y, and 0 otherwise 
 *   Examples: isEqual(5,5) = 1, isEqual(4,5) = 0
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int isEqual(int x, int y) {
  // if x == y, then ~x + y equals -1 (0b111...111), so add one and
  // use !
  return !(~x + y + 1);
}
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
  // When checking if a positive x fits, ~x must fit as well
  // For x >= 0, take ~|x|; for x < 0, take ~(|x| - 1) to get x back
  // Check if ~x for x >= 0 or x for x < 0 fits in n bits
  
  // Algorithm to find absolute value of x
  // y = 0 for x >= 0, y = -1 for x < 0
  int y = x >> 31;
  // add 0 to x if x >= 0 or -1 if x < 0
  // if x < 0, (x - 1) XOR -1 = |x|
  // if x >= 0, x XOR 0 = |x|
  int z = x + y;
  int abs_x = z ^ y;
  int neg = ~(abs_x + y);

  // Save operators by creating a constant
  int m1 = ~0;
  // Minimum in n bytes must equal -1 << (n - 1)
  int min = m1 << (n + m1);
  // If neg does fit within min, min & neg should equal min
  int cmp = min & neg;
  return !(~cmp + min + 1);
}
//3
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  // !0 = 1, so !(!0) - 1 = -1
  int m1 = ~0;
  int n = !x + m1;
  // note that any number n & -1 = n
  // if n == -1, return statement evals to (-1 & y) | (0 & z) which
  // simplifies to y | 0 and then y
  // if n == 0, return statement evals to (0 & y) | (-1 & z) which
  // simplifies to 0 | z and then z
  return (n & y) | (~n & z);
}
/* 
 * isGreater - if x > y  then return 1, else return 0 
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
  // check signs, equals -1 if conditions met
  // use notation from conditional() to create flags equal to -1 if
  // true, 0 else
  // this setup avoids edge cases of overflow
  int yx_neg = (x & y) >> 31;
  int y_neg_x_pos = (~x & y) >> 31;
  // y_pos_x_neg leads to y - x always at least 0, so this condition
  // is always 0
  int yx_pos = (~x & ~y) >> 31;
  

  // if x > y, y - x < 0
  // if x <= y, y - x >= 0

  // if y_neg_x_pos, y - x < 0 so return 1
  int ret1 = (y_neg_x_pos & 1);

  // if yx_neg or yx_pos, overflow/underflows shouldn't occur
  // if y - x < 0, signbit == -1, else signbit == 0
  // !!0 = 0, !!(-1) = !0 = 1
  int neg_x = ~x + 1;
  int diff = y + neg_x;
  int signbit = diff >> 31;
  int isNeg = !!signbit;
  int ret2 = (yx_neg | yx_pos) & isNeg;

  // Note that y_pos_x_neg guarantees y - x >= 0; if y - x >= 0, ret1
  // and ret2 both evaluate to 0, so we return 0
  return ret1 | ret2;
}
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
  int isNeg = x >> 31;
  int sum = x << 2 + x;

  // if left side of | is executed, isNeg guaranteed to be -1 so no
  // need to recalculate -1
  return (isNeg & ((sum + (1 << 3) + isNeg) >> 3)) | (~isNeg & sum >> 3);
}
//4
/* 
 * logicalNeg - implement the ! operator, using all of 
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int logicalNeg(int x) {
  // sign checks
  int sign_pos_x = x >> 31;
  int sign_neg_x = (~x + 1) >> 31;

  // if (sign_pos_x NOR sign_neg_x) return 1
  return (~(sign_pos_x | sign_neg_x) & 1);
}
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
  // if x > TMin, no need to worry about TMin = ~TMin + 1
  // return condition: if x < 0, get unsigned |x| and add sign bit
  int isNeg = x >> 31;
  int conv = ~x + 1 + (1 << 31);
  return (isNeg & conv) | (~isNeg & x);
}
/*
 * isPower2 - returns 1 if x is a power of 2, and 0 otherwise
 *   Examples: isPower2(5) = 0, isPower2(8) = 1, isPower2(0) = 0
 *   Note that no negative number is a power of 2.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int isPower2(int x) {
  // Naive approach but I need the points I guess
  // Make sure leftmost bit isn't 1
  // if x >> 31 == -1, isPos == 0
  // if x >> 31 == 0, isPos == 1
  int isPos = !(~(x >> 31) + 1);

  // int b1 = 0; // dummy
  int chk1 = 0; // dummy  

  // Use !!(bx & x) to check each individual bit
  // I eventually ended up automating this text
  int b2 = 1 << 30;
  int chk2 = chk1 + !!(b2 & x);
  int b3 = b2 >> 1;
  int chk3 = chk2 + !!(b3 & x);
  int b4 = b3 >> 1;
  int chk4 = chk3 + !!(b4 & x);
  int b5 = b4 >> 1;
  int chk5 = chk4 + !!(b5 & x);
  int b6 = b5 >> 1;
  int chk6 = chk5 + !!(b6 & x);
  int b7 = b6 >> 1;
  int chk7 = chk6 + !!(b7 & x);
  int b8 = b7 >> 1;
  int chk8 = chk7 + !!(b8 & x);
  int b9 = b8 >> 1;
  int chk9 = chk8 + !!(b9 & x);
  int b10 = b9 >> 1;
  int chk10 = chk9 + !!(b10 & x);
  int b11 = b10 >> 1;
  int chk11 = chk10 + !!(b11 & x);
  int b12 = b11 >> 1;
  int chk12 = chk11 + !!(b12 & x);
  int b13 = b12 >> 1;
  int chk13 = chk12 + !!(b13 & x);
  int b14 = b13 >> 1;
  int chk14 = chk13 + !!(b14 & x);
  int b15 = b14 >> 1;
  int chk15 = chk14 + !!(b15 & x);
  int b16 = b15 >> 1;
  int chk16 = chk15 + !!(b16 & x);
  int b17 = b16 >> 1;
  int chk17 = chk16 + !!(b17 & x);
  int b18 = b17 >> 1;
  int chk18 = chk17 + !!(b18 & x);
  int b19 = b18 >> 1;
  int chk19 = chk18 + !!(b19 & x);
  int b20 = b19 >> 1;
  int chk20 = chk19 + !!(b20 & x);
  int b21 = b20 >> 1;
  int chk21 = chk20 + !!(b21 & x);
  int b22 = b21 >> 1;
  int chk22 = chk21 + !!(b22 & x);
  int b23 = b22 >> 1;
  int chk23 = chk22 + !!(b23 & x);
  int b24 = b23 >> 1;
  int chk24 = chk23 + !!(b24 & x);
  int b25 = b24 >> 1;
  int chk25 = chk24 + !!(b25 & x);
  int b26 = b25 >> 1;
  int chk26 = chk25 + !!(b26 & x);
  int b27 = b26 >> 1;
  int chk27 = chk26 + !!(b27 & x);
  int b28 = b27 >> 1;
  int chk28 = chk27 + !!(b28 & x);
  int b29 = b28 >> 1;
  int chk29 = chk28 + !!(b29 & x);
  int b30 = b29 >> 1;
  int chk30 = chk29 + !!(b30 & x);
  int b31 = b30 >> 1;
  int chk31 = chk30 + !!(b31 & x);
  int b32 = b31 >> 1;
  int chk32 = chk31 + !!(b32 & x);

  // should return 1 if chk32 == 1 like it should
  return (isPos & !(chk32 + ~0));
}
