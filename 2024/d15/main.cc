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


// P1 = 1526673
// P2 = 1535509
int main() {
  Matrix m;
  ifstream fin("input.txt");
  m.readFile(fin);

  Matrix m2;
  m2.nx = m.nx * 2;
  m2.ny = m.ny;
  for (auto lineOrig: m.m) {
    string line2;
    for (char c: lineOrig) {
      switch (c)
      {
      case '#':
        line2 += "##";
        break;
      case 'O':
        line2 += "[]";
        break;
      case '.':
        line2 += "..";
        break;
      case '@':
        line2 += "@.";
        break;
      default:
        assert(false);
        break;
      }
    }
    m2.m.push_back(line2);
  }

  string program;
  string line;
  getline(fin, line);
  while (line!="") {
    program += line;
    getline(fin, line);
  }

  long p1 = 0;
  long p2 = 0;

  { // P1
    Point robot;
    for (Idx i = 0; i < NXY; i++) {
      if (m.val(i) == '@') {
        robot = ToPoint(i);
      }
    }
    assert(robot.isInside());

    map<char, Point> dirs{
      {'^', {-1, 0}},
      {'>', {0, 1}},
      {'v', {1, 0}},
      {'<', {0, -1}},
    };
    for (char task: program) {
      auto dirIt = dirs.find(task);
      assert(dirIt != dirs.end());
      Point dir = dirIt->second;

      Point nextRobot = robot + dir;
      Point freeSpace;
      if (m.val(nextRobot) == '#') {
        continue;
      }
      freeSpace = nextRobot;
      while (m.val(freeSpace) == 'O') {
        freeSpace += dir;
      }
      if (m.val(freeSpace) != '.') {
        continue;
      }
      m.set(freeSpace, m.val(nextRobot));
      m.set(nextRobot, '@');
      m.set(robot, '.');
      robot = nextRobot;
    }

    // cout << m << endl;

    for (Idx i = 0; i < NXY; i++) {
      if (m.val(i) == 'O') {
        Point p = ToPoint(i);
        p1 += 100 * p.y + p.x;
      }
    }
  }

  { // P2
    m = m2;
    NX = m2.nx;
    NY = m2.ny;
    NXY = NX * NY;

    Point robot;
    for (Idx i = 0; i < NXY; i++) {
      if (m.val(i) == '@') {
        robot = ToPoint(i);
      }
    }
    assert(robot.isInside());

    map<char, Point> dirs{
      {'^', {-1, 0}},
      {'>', {0, 1}},
      {'v', {1, 0}},
      {'<', {0, -1}},
    };
    for (char task: program) {
      // cout << m << endl;
      // cout << "task: " << task << endl;
      auto dirIt = dirs.find(task);
      assert(dirIt != dirs.end());
      Point dir = dirIt->second;

      if (task == '>' || task == '<') {
        Point nextRobot = robot + dir;
        Point freeSpace;
        if (m.val(nextRobot) == '#') {
          continue;
        }
        freeSpace = nextRobot;
        while (m.val(freeSpace) == '[' || m.val(freeSpace) == ']') {
          freeSpace += dir;
        }
        if (m.val(freeSpace) != '.') {
          continue;
        }
        while (freeSpace != robot) {
          m.set(freeSpace, m.val(freeSpace - dir));
          freeSpace -= dir;
        }
        m.set(robot, '.');
        robot = nextRobot;
      } else if (task == '^' || task == 'v') {
        bool moveOK = true;
        vector<vector<Point>> move;
        move.push_back(vector<Point>{robot});
        for (int level = 0; level < move.size() && moveOK; level++) {
          vector<Point> nextLevel;
          map<int, bool> seenX;
          for (Point p: move[level]) {
            Point nextP = p + dir;
            char nextC = m.val(nextP);
            if (nextC == '#') {
              moveOK = false;
              break;
            } else if (nextC == '.') {
            } else if (nextC == '[' or nextC == ']') {
              Point nextP2 = nextP;
              if (nextC == '[') {
                nextP2.x++;
              }
              if (nextC == ']') {
                nextP2.x--;
              }
              for (Point pp: {nextP, nextP2}) {
                if (!seenX[pp.x]) {
                  nextLevel.push_back(pp);
                  seenX[pp.x] = true;
                }
              }
            }
          }
          if (0 < nextLevel.size()) {
            move.push_back(nextLevel);
          }
        }
        if (moveOK) {
          for (int level = move.size() - 1; 0 <= level; level--) {
            for (Point p: move[level]) {
              m.set(p+dir, m.val(p));
              m.set(p, '.');
            }
          }
          robot += dir;
        }
      } else {
        assert(false);
      }
    }

    // cout << m << endl;

    for (Idx i = 0; i < NXY; i++) {
      if (m.val(i) == '[') {
        Point p = ToPoint(i);
        p2 += 100 * p.y + p.x;
      }
    }
  }

  cout << "P1 = " << p1 << "\n";
  cout << "P2 = " << p2 << "\n";
  return 0;
}
