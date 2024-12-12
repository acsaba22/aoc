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

  Point operator+(const Point& p2) const {
      return Point{y + p2.y, x + p2.x};
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

  vector<Point> allNeighbours4() {
    Point dirs[]{{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
    vector<Point> ret;
    for (Point d: dirs) {
      Point b = *this;
      b += d;
      ret.push_back(b);
    }
    return ret;
  }

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


// P1 = 1371306
// P2 = 805880
int main() {
  Matrix m;
  m.readFile("input.txt");

  long p1 = 0;
  long p2 = 0;

  unordered_set<Idx> seen;
  for (Idx idx0 = 0; idx0 < NXY; idx0++) {
    if (!seen.count(idx0)) {
      char regionChar = m.val(idx0);

      deque<Idx> bfs;
      unordered_set<Idx> region;

      seen.insert(idx0);
      region.insert(idx0);
      bfs.push_back(idx0);
      while (!bfs.empty()) {
        Idx i = bfs[0];
        bfs.pop_front();
        for (Idx neigh: Neighbours4(i)) {
          if (m.val(neigh) == regionChar && region.count(neigh) == 0) {
            seen.insert(neigh);
            region.insert(neigh);
            bfs.push_back(neigh);
          }
        }
      }

      long area = region.size();
      long perimeter = 0;
      for (Idx i: region) {
        for (Point neigh: ToPoint(i).allNeighbours4()) {
          if (m.val(neigh) != regionChar) {
            perimeter++;
          }
        }
      }
      p1 += area*perimeter;

      long cornerCount = 0;
      Point dirOutside{-1, 0};
      Point dirNeigh0{0, -1};
      auto rotate90 = [](Point &p) {
        int tmp = p.x;
        p.x = -p.y;
        p.y = tmp;
      };
      for (Idx idx: region) {
        Point p = ToPoint(idx);
        for (int facingDir = 0; facingDir < 4; facingDir++) {
          assert(m.val(p) == regionChar);
          if (m.val(p + dirOutside) != regionChar) {
            bool middle = (
              m.val(p + dirNeigh0) == regionChar && m.val(p + dirNeigh0 + dirOutside) != regionChar
            );
            if (!middle) {
              cornerCount++;
            }
          }
          rotate90(dirOutside);
          rotate90(dirNeigh0);
        }
      }
      p2 += area*cornerCount;
    }
  }

  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";
  return 0;
}
