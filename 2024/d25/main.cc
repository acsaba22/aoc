#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <unordered_set>
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

  // TODO find out how to make spaceship operator work in vscode
  // to not show error.
  // auto operator<=>(const Point&) const = default;


  Point operator+(const Point& p2) const {
      return Point{y + p2.y, x + p2.x};
  }

  Point operator-(const Point& p2) const {
      return Point{y - p2.y, x - p2.x};
  }

  bool operator!=(const Point& p2) const {
      return x != p2.x || y != p2.y;
  }

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

  vector<Point> allNeighbours4();

  vector<Point> neighbours4() {
    vector<Point> ret;
    for (Point b: allNeighbours4()) {
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

Point Directions4[]{{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
vector<Point> Point::allNeighbours4() {
  vector<Point> ret;
  for (Point d: Directions4) {
    Point b = *this;
    b += d;
    ret.push_back(b);
  }
  return ret;
}


struct Matrix {
  int nx = 0, ny = 0;
  vector<string> m;

  void init(int ny, int nx, char c) {
    this->ny = ny;
    this->nx = nx;
    string s(nx, c);
    for (int i = 0; i < ny; i++) {
      m.push_back(s);
    }
  }
  void readFile(const string& fname) {
    ifstream fin(fname);
    readFile(fin);
  }

  void readFile(ifstream& fin) {
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

std::ostream& operator<<(std::ostream& outs, const Matrix& m) {
  for (auto line: m.m) {
    outs << line << "\n";
  }
  return outs;
}


///////// STARTS HERE

vector<int> changeHeights(Matrix &m, int (*transform)(int)) {
  vector<int> ret;
  char firstChar = m.m[0][0];
  for (int x = 0; x < m.nx; x++) {
    int y = 0;
    for (; m.m[y][x] == firstChar; y++);
    ret.push_back(transform(y));
  }
  return ret;
}

int main() {
  ifstream fin("input.txt");

  vector<vector<int>> locks;
  vector<vector<int>> keys;

  for (bool hasMore = true; hasMore;) {
    Matrix m;
    m.readFile(fin);
    cout << m << endl;

    hasMore = m.ny != 0;
    if (hasMore) {
      assert(m.ny == 7 && m.nx == 5);
      if (m.m[0][0] == '#') {
        vector<int> lock = changeHeights(m, [](int y) { return y-1; });
        locks.push_back(lock);
      } else {
        assert(m.m[0][0] == '.');
        vector<int> key = changeHeights(m, [](int y) { return 6-y; });
        keys.push_back(key);
      }
    }
  }

  int p1 = 0;

  for (auto& lock: locks) {
    for (auto& key: keys) {
      bool fit = true;
      for (int k = 0 ; k < 5; k++) {
        if (5 < lock[k] + key[k]) {
          fit = false;
        }
      }
      if (fit) {
        p1++;
      }
    }
  }

  cout << "P1: " << p1 << endl;

  return 0;
}
