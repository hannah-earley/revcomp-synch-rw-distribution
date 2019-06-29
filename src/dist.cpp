// #include <fstream>
#include <iostream>
// #include <random>
// #include <vector>
// #define _USE_MATH_DEFINES
#include <cmath>
// #include <chrono>
#include <cstdlib>
#include <unistd.h>
// #include <omp.h>
// #include <exception>
// #include <cctype>
// #include <cstring>
// #include <string>
// #include <limits>
// #include "pcg/pcg_random.hpp"
// #include "walk.hpp"

size_t triangle(size_t n) {
    return (n * (n + 1)) >> 1;
}

template<typename T>
class TriMatrix {
    size_t row0;
    size_t rows;
    size_t size;
    T* storage;
    T* start_;
    T* end_;

public:
    TriMatrix(size_t rows, size_t row0) :
        row0(row0),
        rows(rows),
        size(triangle(rows)),
        storage(new T[size]),
        start_(storage + triangle(row0)),
        end_(storage + size)
    {}

    TriMatrix(size_t rows) :
        TriMatrix(rows, 0)
    {}

    size_t index(size_t row) {
        return triangle(row);
    }

    size_t index(size_t row, size_t col) {
        return triangle(row) + col;
    }

    T* operator[](const size_t row) {
        return storage + index(row);
    }

    T* start() {
        return start_;
    }

    T* end() {
        return end_;
    }

    void fill(const T value) {
        T* pos = start();
        while (pos < end())
            *pos++ = value;
    }

    void zero() {
        fill(0);
    }

    void print() {
        print(row0,rows);
    }

    void print(size_t row, size_t rowf) {
        for (; row < rowf; row++) {
            for (size_t col = 0; col <= row; col++) {
                std::cout << (*this)[row][col] << ",";
            }
            std::cout << "\n";
        }
    }

    void print_log() {
        for (size_t row = row0; row < rows; row++) {
            std::cout << row << " 0 0 [";
            for (size_t col = 0; col <= row; col++) {
                std::cout << (*this)[row][col];
                if (col < row) {
                    std::cout << ",";
                }
            }
            std::cout << "]\n";
        }

    }

    void clip_pos() {
        T* pos = start();
        while (pos < end()) {
            T val = *pos;
            if (val < 0)
                *pos = 0;
            pos++;
        }
    }

    T sum() {
        T ret = 0;
        T* pos = start();
        while (pos < end())
            ret += *pos++;
        return ret;
    }

    void normalise() {
        T factor = 1 / sum();
        T* pos = start();
        while (pos < end())
            *pos++ *= factor;
    }
};

struct Dist2D {
    double bias;
    unsigned int width;
    unsigned int dist;
    unsigned int slack;

    TriMatrix<double> mat1;
    TriMatrix<double> mat2;

    Dist2D(double bias, unsigned int width, unsigned int dist, unsigned int slack) :
        bias(bias), width(width), dist(dist), slack(slack),
        mat1(rows()+1,row0()), mat2(rows()+1,row0())
    {
        // fill_decay_1();
        fill_decay_2();
        // mat1.fill(1);
        mat1.normalise();
        mat2.zero();
    }

    unsigned int rows() {
        return dist + slack;
    }

    unsigned int row0() {
        return width - 1;
    }

    double p() {
        return 0.5 * (1+bias);
    }

    double q() {
        return 0.5 * (1-bias);
    }

    void evolve() {
        mat2.zero();

        double p2 = p() * 0.5;
        double q2 = q() * 0.5;

        unsigned int row = row0() + 1;
        unsigned int rows_ = rows();

        double* old_rowc = mat1[row];
        double* new_rowp = mat2[row - 1];
        double* new_rowc = mat2[row];
        double* new_rown = mat2[row + 1];

        while (row < rows_) {
            new_rowp[0]     += p2 * old_rowc[0];
            new_rowc[0]     += p2 * old_rowc[0];
            new_rowc[row]   += p2 * old_rowc[row];
            new_rowp[row-1] += p2 * old_rowc[row];

            for (size_t col = 1; col <= row-1; col++) {
                new_rowp[col-1] += p2 * old_rowc[col];
                new_rowp[col]   += p2 * old_rowc[col];
            }
            for (size_t col = 0; col <= row; col++) {
                new_rown[col]   += q2 * old_rowc[col];
                new_rown[col+1] += q2 * old_rowc[col];
            }

            row++;
            new_rowp = new_rowc;
            new_rowc = new_rown;
            new_rown = mat2[row + 1];
            old_rowc = mat1[row];
        }

        double sum0 = 0;
        row = row0();
        new_rowc = mat2[row];
        for (size_t col = 0; col <= row; col++) {
            sum0 += new_rowc[col];
            new_rowc[col] = 0;
        }

        row = dist;
        new_rowc = mat2[row];
        sum0 /= row + 1;
        for (size_t col = 0; col <= row; col++) {
            new_rowc[col] += sum0;
        }

        // row = rows_;
        // new_rowc = mat2[row];
        // for (size_t col = 0; col <= row; col++) {
        //     new_rowc[col] = 0;
        // }

        mat2.clip_pos();
        mat2.normalise();
        std::swap(mat1, mat2);
    }

    double diff() {
        double ret = 0;
        double *start1 = mat1.start(), *end1 = mat1.end();
        double *start2 = mat2.start();
        while (start1 < end1) {
            double diff = (*start1++) - (*start2++);
            ret += diff * diff;
        }
        return ret;
    }

    double estimate_mfpt() {
        double p1 = p();
        double p2 = p1 * 0.5;

        double j = 0;

        unsigned int row = row0() + 1;
        double* rowc = mat1[row];

        j += p2 * rowc[0];
        for (size_t col = 1; col <= row-1; col++)
            j += p1 * rowc[col];
        j += p2 * rowc[row];

        return 1./j;
    }

    void fill_decay_1() {
        double t = q() / p();
        double v = 1;

        for (size_t row = row0(); row < rows(); row++) {
            for (size_t col = 0; col <= row; col++)
                mat1[row][col] = v;
            v *= t;
        }
    }

    void fill_decay_2() {
        double t = pow(q() / p(), 0.5);
        double v = 1;

        for (size_t row = row0(); row < rows(); row++) {
            for (size_t col = 0; col <= row; col++)
                mat1[row][col] = v;
            v *= t;
        }
    }
};

int main(int argc, char *argv[]) {
    Dist2D dist(0.01, 10, 100, 2000);
    double target_diff = 1e-20;

    while (true) {
        for (size_t j = 0; j < 1000; j++)
            dist.evolve();

        std::cerr << dist.diff() << "\t" << dist.estimate_mfpt() << std::endl;
    }

    // dist.mat1.print(0,10);
    // for (size_t i = 0; i < 1000; i++)
    //     dist.evolve();
    // dist.mat1.print(0,10);
    for (size_t i = 0; i < 1000; i++) {
        for (size_t j = 0; j < 1000; j++) {
            dist.evolve();
        }
        if (dist.diff() < target_diff)
            break;
        std::cerr << "." << dist.diff() << std::flush;
    }
    std::cerr << dist.diff() << std::endl;
    std::cerr << dist.estimate_mfpt() << std::endl;
    // dist.mat1.print(0,10);
    // dist.mat1.print(60,65);

    return 0;
    dist.mat1.print_log();

    for(size_t r=0;r<=dist.rows();r++) {
        double sum = 0;
        for (size_t c=0;c<=r;c++) {
            sum += dist.mat1[r][c];
        }
        std::cerr << sum << ",";
    }
    std::cerr<<std::endl;

    // dist.mat1.print(dist.rows()-1,dist.rows()+1);

    // std::cout << mat1[3][4];
    // mat1[3][4] += 7;

    return 0;
}