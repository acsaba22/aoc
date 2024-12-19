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


// P1 = 260
// P2 = 24,48
int main() {
  // ifstream fin("example.txt");
  // NY = 7;
  // NX = 7;
  // int K = 12;

  ifstream fin("input.txt");
  NY = 71;
  NX = 71;
  int K = 1024;

  NXY = NX * NY;

  string line;
  vector<Point> corrupted;
  char comma;
  Point p;
  while (fin >> p.x >> comma >> p.y) {
    assert(comma == ',');
    corrupted.push_back(p);
    // cout << p << endl;
  }
  assert(K <= corrupted.size());

  auto solve = [&corrupted](int k) {
    Matrix m;
    m.init(NY, NX, '.');

    for (int i = 0; i < k; i++) {
      m.set(corrupted[i], '#');
    }

    unordered_map<Idx, int> sol;
    struct State {
      Point p;
      int steps;
    };

    deque<State> bfs = {{{0, 0}, 0}};

    while (!bfs.empty()) {
      // cout << m << endl;
      State state = bfs[0];
      bfs.pop_front();
      m.set(state.p, 'O');
      sol[state.p.toIdx()] = state.steps;

      for (auto n: state.p.neighbours4()) {
        if (m.val(n) == '.') {
          bfs.push_back({n, state.steps + 1});
          m.set(n, 'T');
        }
      }
    }
    return sol[NXY-1];
  };

  long p1 = solve(K);

  cout << "P1 = " << p1 << endl;

  int low = K, high = corrupted.size();
  while (low < high) {
    int middle = (low+high+1)/2;
    int sol = solve(middle);
    if (sol == 0) {
      high = middle - 1;
    } else {
      low = middle;
    }
  }
  Point p2 = corrupted[low];
    cout << "P2 = " << p2.x << "," << p2.y << endl;

  return 0;
}
