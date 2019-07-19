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

bool VERBOSE_MODE = false;

size_t triangle(size_t n) {
    return (n * (n + 1)) >> 1;
}

unsigned stou(std::string const& str, size_t* idx=0, int base=10);
unsigned stou(std::string const& str, size_t* idx, int base) {
    unsigned long result = std::stoul(str, idx, base);
    if (result > std::numeric_limits<unsigned>::max()) {
        throw std::out_of_range("stou");
    }
    return result;
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

void help(int argc, char *argv[]) {
    char progn_def[] = "./dist";
    char *progn = progn_def;
    if (argc > 0) progn = argv[0];

    fprintf(stderr, "Usage: %s [options]\n", progn);
    // fprintf(stderr, "    -2           Compute 2D walk MFPT\n");
    fprintf(stderr, "    -h           Print this help message\n");
    // fprintf(stderr, "    -v           Verbose/debug mode\n");
    fprintf(stderr, "    -b bias      Biased walk, bias \\in [-1,1]\n");
    fprintf(stderr, "    -d distance  Starting point, [nat]\n");
    fprintf(stderr, "    -w width     Constriction width (2D only), [nat]\n");
    fprintf(stderr, "    -x rows      Excess row count, [nat]\n");
    fprintf(stderr, "    -s window    Sample time window, [nat]\n");
}

int main(int argc, char *argv[]) {
    std::cout.precision(17);
    std::cerr.precision(17);

    double bias = 1;
    unsigned int width = 1;
    unsigned int distance = 1;
    unsigned int slack = 250;
    unsigned int every = 1000;
    // Dist2D dist;

    int c;
    while ((c = getopt(argc, argv, "hb:d:w:x:s:")) != -1) switch(c) {
        case 'b':
            bias = std::stod(optarg);
            if (bias < -1 || bias > 1) {
                std::cerr << "Bias outside range..\n\n";
                goto help;
            }
            break;
        case 'd':
            distance = stou(optarg);
            if (distance > 2147483647) { 
                std::cerr << "Distance outside range..\n\n";
                goto help;
            }
            break;
        case 'w':
            width = stou(optarg);
            break;
        case 'x':
            slack = std::stoul(optarg);
            break;
        case 's':
            every = std::stoul(optarg);
            break;

        // case 'v':
        //     VERBOSE_MODE = true;
        //     break;
        case 'h':
        case '?':
        default:
            goto help;
    }

    // run simulation
    {
        Dist2D dist(bias, width, distance, slack);
        while (true) {
            for (size_t j = 0; j < every; j++)
                dist.evolve();
    
            std::cerr << dist.diff() << "\t" << dist.estimate_mfpt() << std::endl;
        }
    }

    return 0;

help:
    help(argc, argv);
    return 1;
}