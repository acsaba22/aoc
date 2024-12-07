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

#include <boost/algorithm/string.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>

using namespace std;

struct Point {
  int y = -1;
  int x = -1;

  Point& operator+=(const Point& p2) {
    y += p2.y;
    x += p2.x;;
    return *this;
  }
};

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
  }

  char val(Point coor) {
    if (0 <= coor.y && coor.y < ny
        && 0 <= coor.x && coor.x < nx) {
      return m[coor.y][coor.x];
    }
    return '?';
  }

  void set(Point coor, char c) {
    assert(0 <= coor.y && coor.y < ny
        && 0 <= coor.x && coor.x < nx);
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


struct SimResult {
  int xcount;
  bool finished;
};
SimResult RunSim(Matrix& m, int cutoff) {

  Point pos = m.doFind('^');

  m.set(pos, 'X');
  SimResult ret{1, true}; // 1 because start point ^


  int dirN = 4;
  Point dirs[]{{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
  int dir = 0;
  bool inside = true;

  int count = 0;
  while (inside) {
    count++;
    if (cutoff < count) {
      ret.finished = false;
      return ret;
    }
    Point nextPos = pos;
    nextPos += dirs[dir];
    char nextChar = m.val(nextPos);
    switch (m.val(nextPos))
    {
    case '?':
      inside = false;
      break;
    case '.':
      ret.xcount++;
    case 'X':
      pos = nextPos;
      m.set(pos, 'X');
      break;
    case '#':
      dir++;
      dir %= dirN;
      break;
    default:
      cout << int(m.val(nextPos)) << " " << nextPos << endl;
      assert(false); // whatchar?
    }
  }

  return ret;
}

// P1 = 4982
// P2 = 1663
int main() {
  Matrix m;
  m.readFile("input.txt");
  Matrix mOrig = m;

  int cutoff = m.ny * m.nx * 4;
  SimResult p1 = RunSim(m, cutoff);

  int p2 = 0;
  for (int y: boost::irange(0, m.ny)) {
    cout << y << endl;
    for (int x: boost::irange(0, m.nx)) {
      Point p{y, x};
      if (mOrig.val(p) == '.') {
        m = mOrig;
        m.set(p, '#');
        if (!RunSim(m, cutoff).finished) {
          p2++;
        }
      }
    }
  }

  cout << "P1 = " << p1.xcount << endl;
  cout << "P2 = " << p2 << "\n";
  return 0;
}