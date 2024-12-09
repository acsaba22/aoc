#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <unordered_map>
#include <string>
#include <sstream>
#include <cassert>
#include <map>
#include <set>

using namespace std;

long p1(const string &line) {
  vector<long> blocks;

  long id = 0;
  bool file = true;
  for (char c: line) {
    int len(c-'0');
    long x = -1;
    if (file) {
      x = id;
      id++;
    }
    for (int i = 0 ; i < len; i++) {
      blocks.push_back(x);
    }
    file = !file;
  }

  long front = 0, back = blocks.size() - 1;
  while (front < back) {
    if (blocks[front] != -1) {
      front++;
    } else if (blocks[back] == -1) {
      back--;
    } else {
      blocks[front] = blocks[back];
      blocks[back] = -1;
    }
  }

  long ret = 0;

  for (long i = 0; 0 <= blocks[i]; i++) {
    ret += i * blocks[i];
  }
  return ret;
}


struct File{
  long id = -1;
  long location = -1;
  int size = -1;
};

long p2(const string& line) {
  vector<File> files;
  map<int, set<long>> freeLocationsBySize;

  long id = 0;
  long location = 0;
  bool file = true;
  for (char c: line) {
    int len(c-'0');
    if (file) {
      files.push_back(File{id, location, len});
      id++;
    } else {
      freeLocationsBySize[len].insert(location);
    }
    file = !file;
    location += len;
  }

  long maxLocation = location;

  for (auto file = files.rbegin(); file != files.rend(); file++) {
    set<long> *freeSet;
    set<long>::iterator freeIt;
    int freeSize = -1;
    long smallestPos = maxLocation;

    for (int size = file->size; size <= 9; size++) {
      set<long>& s = freeLocationsBySize[size];
      auto begin = s.begin();
      if (begin != s.end() && *begin < smallestPos) {
        freeSet = &s;
        freeIt = begin;
        freeSize = size;
        smallestPos = *begin;
      }
    }
    if (smallestPos < file->location) {
      file->location = smallestPos;
      freeSet->erase(freeIt);

      int remaining = freeSize - file->size;
      if (0 <= remaining) {
        freeLocationsBySize[remaining].insert(smallestPos + file->size);
      }
    }
  }

  long ret = 0;
  for (auto f: files) {
    long loc = f.location;
    for (int i = 0; i < f.size; i++) {
      ret += (loc + i) * f.id;
    }
  }

  return ret;
}

// P1 = 6421128769094
// P2 = 6448168620520
int main() {
  ifstream fin("input.txt");
  string line;
  std::getline(fin, line);

  cout << "P1 = " << p1(line) << "\n";
  cout << "P2 = " << p2(line) << "\n";

  return 0;
}
