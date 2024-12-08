#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <string>
#include <sstream>
#include <cassert>
#include <regex>
#include <set>
#include <map>
#include <ranges>
#include <numeric>

#include <boost/algorithm/string.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>

using namespace std;


typedef int Idx;
Idx NY = 0, NX = 0, NXY = 0;

struct Point {
  int y = -1;
  int x = -1;

  Point& operator+=(const Point& p2) {
    y += p2.y;
    x += p2.x;;
    return *this;
  }

  Point& operator-=(const Point& p2) {
    y -= p2.y;
    x -= p2.x;;
    return *this;
  }

  Idx toIdx() {
    return y*NX + x;
  }

  bool isInside() {
    return 0 <= this->y && this->y < NY && 0 <= this->x && this->x < NX;
  }
};
Point ToPoint(int idx) {
  return Point{idx / NX, idx % NX};
}

std::ostream& operator<<(std::ostream& outs, const Point& p) {
    return outs << "(" << p.y << ", " << p.x << ")";
}

struct Matrix {
  int nx = 0, ny = 0;
  vector<string> m;

  void readFile(const string& fname) {
    ifstream fin(fname);
    string line;
    getline(fin, line);
    while (line!="") {
      if (nx == 0) {
        nx = line.size();
      }
      assert(nx == line.size());
      m.push_back(line);
      ny++;

      getline(fin, line);
    }
    NY = this->ny;
    NX = this->nx;
    NXY = NY * NX;
  }

  char val(Point coor) {
    if (coor.isInside()) {
      return m[coor.y][coor.x];
    }
    return '?';
  }

  char val(Idx idx) {
    assert(0 <= idx && idx < NXY);
    return m[idx / NX][idx % NX];
  }

  void set(Point coor, char c) {
    assert(coor.isInside());
    m[coor.y][coor.x] = c;
  }

  Point doFind(char c) {
    for (int y: boost::irange(0, ny)) {
      for (int x: boost::irange(0, nx)) {
        if (m[y][x] == c) {
          return Point{y, x};
        }
      }
    }
    assert(false); // doFind didn't Find
    // return Point{-1, -1};
  }
};

// P1 = 249
// P2 = 905
int main() {
  Matrix m;
  m.readFile("input.txt");

  map<char, vector<Idx>> antennas;

  for (Idx idx = 0; idx < NXY; idx++) {
    char c = m.val(idx);
    if (c != '.') {
      antennas[c].push_back(idx);
    }
  }

  set<Idx> antinodes;
  set<Idx> antinodes2;

  for (auto antennaList: antennas) {
    vector<Idx>& vec = antennaList.second;
    for (int i = 0; i < vec.size(); i++) {
      Point a = ToPoint(vec[i]);
      for (int j = 0; j < vec.size(); j++) {
        if (i != j) {
          // p1
          Point b = ToPoint(vec[j]);
          Point antinode = b;
          antinode += b;
          antinode -= a;
          if (antinode.isInside()) {
            antinodes.insert(antinode.toIdx());
          }

          // p2
          Point diff = b;
          diff -= a;
          int commonDivisor = std::gcd(diff.y, diff.x);
          Point step{diff.y / commonDivisor, diff.x / commonDivisor};
          antinode = a;
          while (antinode.isInside()) {
            antinodes2.insert(antinode.toIdx());
            antinode += step;
          }
        }
      }
    }
  }

  cout << "P1 = " << antinodes.size() << "\n";
  cout << "P2 = " << antinodes2.size() << "\n";
  return 0;
}