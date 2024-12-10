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

  vector<Point> neighbours4() {
    Point dirs[]{{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
    vector<Point> ret;
    for (Point d: dirs) {
      Point b = *this;
      b += d;
      if (b.isInside()) {
        ret.push_back(b);
      }
    }
    return ret;
  }
};
Point ToPoint(int idx) {
  return Point{idx / NX, idx % NX};
}
vector<Idx> Neighbours4(Idx idx) {
  vector<Idx> ret;
  for (Point p: ToPoint(idx).neighbours4()) {
    ret.push_back(p.toIdx());
  }
  return ret;
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


// P1 = 811
// P2 = 1794
int main() {
  Matrix m;
  m.readFile("input.txt");

  int p1 = 0;
  long p2 = 0;
  for (Idx idx0 = 0; idx0 < NXY; idx0++) {
    if (m.val(idx0) == '0') {
      map<Idx, long> level;
      level[idx0] = 1;
      map<Idx, long> nextLevel;
      for (char nextChar = '1'; nextChar <= '9'; nextChar++) {
        for (auto x: level) {
          long count = x.second;
          for (Idx neigh: Neighbours4(x.first)) {
            if (nextChar == m.val(neigh)) {
              nextLevel[neigh] += count;
            }
          }
        }
        level.swap(nextLevel);
        nextLevel.clear();
      }
      p1 += level.size();
      for (auto x: level) {
        p2 += x.second;
      }
    }
  }
  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";
  return 0;
}
