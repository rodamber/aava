#include <cmath>
#include <cstdlib>
#include <ctime>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

using namespace std;

string filename(int size) {
  stringstream ss;
  ss << "data/lc-" << size << ".in";
  return ss.str();
}

void link(ostream &out, int from, int to) {
  out << "L" << from << " " << to << "\n";
}

void cut(ostream &out, int from, int to) {
  out << "C" << from << " " << to << "\n";
}

void connected(ostream &out, int from, int to) {
  out << "Q" << from << " " << to << "\n";
}

void init(ostream &out, int size) {
  out << size << "\n";
}

void finish(ostream &out) {
  out << "X\n";
}

void linear_tree(ostream &out, int size) {
  init(out, size);

  for (int i = 1; i <= size - 1; i++) {
    for (int j = 2; j <= size; j++) {
      link(out, i, j);
    }
  }

  cut(out, size/2, 1 + size/2);
}

void test_case(ostream &out, int size, int num_ops) {
  linear_tree(out, size);

  for (int i = 0; i < num_ops; i++) {
    int from = 1 + random() % size;
    int to = 1 + random() % size;

    switch (random() % 3) {
    case 0:
      link(out, from, to);
      break;
    case 1:
      cut(out, from, to);
      break;
    case 2:
      connected(out, from, to);
      break;
    }
  }

  finish(out);
}

int main() {
  srand(time(NULL));

  int _1_KiB = 1024;
  int _1_MiB = 1048576;

  for (int i = _1_MiB; i <= 8 * _1_MiB; i += 56 * _1_KiB) {
    ofstream out(filename(i), ofstream::out);
    test_case(out, i, _1_MiB);
    out.close();
  }

  return 0;
}


