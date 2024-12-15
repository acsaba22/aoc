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

// we need a namespace because we have two points (one with x,y and one with y,x)
namespace Matrix {

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

  Matrix(int ny, int nx, char fillchar) : nx(nx), ny(ny) {
    for (int y: boost::irange(0, ny)) {
      m.push_back(string(nx, fillchar));
    }
  }
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

std::ostream& operator<<(std::ostream& outs, const Matrix& m) {
  for (auto line: m.m) {
    outs << line << "\n";
  }
  return outs;
}

}; // namespace Matrix

struct Point {
  long x, y;

  Point operator+(const Point& p2) const {
      return Point{x + p2.x, y + p2.y};
  }

  Point operator-(const Point& p2) const {
      return Point{x - p2.x, y - p2.y};
  }

  Point operator*(long n) const {
      return Point{x * n, y * n};
  }

  bool operator<(const Point& p2) const {
      return x < p2.x && y < p2.y;
  }

  bool operator==(const Point& p2) const {
      return x == p2.x && y == p2.y;
  }

  Point(const long& sx, const long& sy): x(sx), y(sy) {}
  Point(const string& sx, const string& sy): x(stoi(sx)), y(stoi(sy)) {}
};

std::ostream& operator<<(std::ostream& outs, const Point& p) {
    return outs << "(" << p.x << ", " << p.y << ")";
}


// P1 = 229069152
// P2 = 7383
int main() {
  // ifstream fin("example.txt");
  // long nx = 11, ny = 7;

  ifstream fin("input.txt");
  long nx = 101, ny = 103;

  Matrix::NX = nx;
  Matrix::NY = ny;
  Matrix::NXY = nx * ny;
  long nx2 = nx / 2, ny2 = ny / 2;

  string line;
  getline(fin, line);

  regex re(R"(p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+))");

  struct Robot {
    Point p;
    Point v;
  };

  auto parse = [&re](const string& s) {
    smatch m;
    assert(regex_match(s, m, re));
    Robot ret{{m[1], m[2]}, {m[3], m[4]}};
    return ret;
  };

  auto step = [nx, ny](Robot& r, long count) {
    r.p = r.p + r.v * count;
    r.p.x = ((r.p.x % nx) + nx) % nx;
    r.p.y = ((r.p.y % ny) + ny) % ny;
  };

  long p1 = 0;
  long p2 = 0;

  vector<Robot> robots;
  long q[2][2] = {};
  while (line != "") {
    Robot r = parse(line);
    robots.push_back(r);
    step(r, 100);
    int q0 = -1;
    if (r.p.x < nx2) {
      q0 = 0;
    }
    if (nx2 < r.p.x) {
      q0 = 1;
    }
    long q1 = -1;
    if (r.p.y < ny2) {
      q1 = 0;
    }
    if (ny2 < r.p.y) {
      q1 = 1;
    }
    if (0 <= q0 && 0 <= q1) {
      q[q0][q1] += 1;
    }
    getline(fin, line);
  }
  p1 = q[0][0] * q[0][1] * q[1][0] * q[1][1];

  ofstream fout("output.txt");
  int maxStep = 10000;
  for (int stepCount = 0 ; stepCount < maxStep; stepCount++) {
    int visibleRobots = 0;

    Matrix::Matrix m(ny, nx, ' ');
    for (Robot& r: robots) {
      Matrix::Point mp;
      mp.y = r.p.y;
      mp.x = r.p.x;
      char cnow = m.val(mp);
      if (cnow == ' ') {
        m.set(mp, '1');
        visibleRobots++;
      } else {
        m.set(mp, cnow+1);
      }
      step(r, 1);
    }

    // if (visibleRobots < 470) {
    // if ((stepCount - 10)%101 == 0) {
    // if ((stepCount - 70)%103 == 0) {
    if ((stepCount - 10)%101 == 0 && (stepCount - 70)%103 == 0) {
      cout << "Interesting step " << stepCount << " (" << visibleRobots << ")" << endl;
      fout << "Step " << stepCount << ":\n";
      fout << m << "\n";
      p2 = stepCount;
    }
  }

  cout << "P1 = " << p1 << "\n";

  cout << "P2 = " << p2 << "\n";

  return 0;
}
