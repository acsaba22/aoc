#include <iostream>
#include <fstream>
#include <vector>
#include <regex>
#include <cassert>

#include <boost/algorithm/string.hpp>
#include <boost/range/algorithm.hpp>

using namespace std;


struct Computer {
  long a, b, c;
  vector<int> program;
  int ip = 0;
  vector<int> output;

  void readFile(const string& fname) {
    ifstream fin(fname);
    string line;
    regex re(R"(Register (\w): (\d+))");

    smatch m;
    getline(fin, line);
    assert(regex_match(line, m, re));
    assert(m[1] == "A");
    a = stol(m[2]);

    getline(fin, line);
    assert(regex_match(line, m, re));
    assert(m[1] == "B");
    b = stol(m[2]);

    getline(fin, line);
    assert(regex_match(line, m, re));
    assert(m[1] == "C");
    c = stol(m[2]);

    getline(fin, line);
    assert(line == "");

    getline(fin, line);
    assert(regex_match(line, m, regex(R"(Program: (.*))")));

    vector<string> strs;
    boost::split(strs, string(m[1]), boost::is_any_of(","));
    for (auto s: strs) {
      program.push_back(stoi(s));
    }
  }

  long combo(int operand) {
    if (operand <= 3) {
      return operand;
    }
    switch (operand) {
    case 4:
      return a;
    case 5:
      return b;
    case 6:
      return c;
    default:
      assert(false);
    }

  }
};

map<int, void (*)(Computer&, int operand)> instructions {
  // adv
  {0, [](Computer &c, int operand) {
    long d = c.combo(operand);
    c.a >>= d;
    c.ip += 2;
  }},
  // bxl
  {1, [](Computer &c, int operand) {
    c.b ^= operand;
    c.ip += 2;
  }},
  // bst
  {2, [](Computer &c, int operand) {
    long d = c.combo(operand);
    c.b = d & 7;
    c.ip += 2;
  }},
  // jnz
  {3, [](Computer &c, int operand) {
    if (c.a != 0) {
      c.ip = operand;
    } else {
      c.ip += 2;
    }
  }},
  // bxc
  {4, [](Computer &c, int operand) {
    c.b ^= c.c;
    c.ip += 2;
  }},
  // out
  {5, [](Computer &c, int operand) {
    long d = c.combo(operand);
    c.output.push_back(d&7);
    c.ip += 2;
  }},
  // bdv
  {6, [](Computer &c, int operand) {
    long d = c.combo(operand);
    c.b = c.a >> d;
    c.ip += 2;
  }},
  // cdv
  {7, [](Computer &c, int operand) {
    long d = c.combo(operand);
    c.c = c.a >> d;
    c.ip += 2;
  }},
};

// P1: 1,5,3,0,2,5,2,5,3
// P2: 108107566389757
int main() {
  Computer c;
  c.readFile("input.txt");

  while (c.ip < c.program.size()) {
    assert(c.ip + 1 < c.program.size());
    int instruction = c.program[c.ip];
    int operand = c.program[c.ip+1];
    instructions[instruction](c, operand);
  }
  cout << "P1: " ;
  for (int i = 0; i < c.output.size(); i++) {
    if (i != 0) {
      cout << ",";
    }
    cout << c.output[i];
  }
  cout << endl;

  long p2 = -1;
  // for (long a = 0; p2 < 0; a++) {
  // for (long a = 498173; p2 < 0; a += 524288) {
  for (long a = 3239549437; p2 < 0; a += 8589934592) {
    c.output.clear();
    c.a = a;
    c.b = 0;
    c.c = 0;
    c.ip = 0;

    bool ok = true;

    while (ok && c.ip < c.program.size()) {
      assert(c.ip + 1 < c.program.size());
      int instruction = c.program[c.ip];
      int operand = c.program[c.ip+1];
      instructions[instruction](c, operand);
      if (instruction == 5) { // out
        int outS = c.output.size();
        if (c.program.size() < outS ||
            c.program[outS-1] != c.output[outS-1]) {
          ok = false;
        } else {
          if (12 <= outS) {
            cout << "ok output: " << a << " [" << outS << "]" << endl;
          }
        }
      }
    }
    if (c.program.size() != c.output.size()) {
      ok = false;
    }
    if (ok) {
      p2 = a;
    }
  }
  cout << "P2: " << p2;
  return 0;
}
