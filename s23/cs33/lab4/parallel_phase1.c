#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

#include "utils.h"

//This code is buggy! Find the bug and speed it up.
void parallel_avg_pixel(long img[DIM_ROW][DIM_COL][DIM_RGB], long *avgs) {
    int row, col, pixel;
    long count = 0;

    #pragma omp parallel
    for (pixel = 0; pixel < DIM_RGB; pixel++) {
        for (col = 0; col < DIM_COL; col++) {
            for (row = 0; row < DIM_ROW; row++){
                avgs[pixel] += img[row][col][pixel];
                count++;
            }
        }
    }

    count /= 3;

    for (pixel = 0; pixel < DIM_RGB; pixel++) {
        avgs[pixel] /= count;
    }
}