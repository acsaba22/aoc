import * as util from '../util.js'

class Table {
  n: number
  m: number
  nm: number
  v: number[]

  constructor(lines: number[][]) {
    this.n = lines.length
    this.m = lines[0].length
    this.nm = this.n * this.m
    this.v = ([] as number[]).concat(...lines)
  }

  neighs4(k: number): number[] {
    return this._neighs(k, [[-1, 0], [1, 0], [0, 1], [0, -1]])
  }
  neighs8(k: number): number[] {
    return this._neighs(k,
      [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]])
  }

  _neighs(k: number, ds: number[][]): number[] {
    let y = Math.floor(k / this.m)
    let x = k % this.m
    let ret = []
    for (let [dy, dx] of ds) {
      let ny = y + dy
      let nx = x + dx
      if (0 <= ny && ny < this.n && 0 <= nx && nx < this.m) {
        ret.push(ny * this.m + nx)
      }
    }
    return ret
  }

  print() {
    for (let y of Array(this.n).keys()) {
      util.log(this.v.slice(y * this.m, (y + 1) * this.m).map(x => String(x)).join(''))
    }
  }
}

export async function main() {
  let s: string = await util.loadFile('/src/d11/input.txt')
  let lines: number[][] = s.trim().split('\n').map(x => x.trim().split('').map(y => Number(y)))
  let t = new Table(lines)

  let p1 = 0
  let p2 = 0
  for (let round = 0; p2 == 0; round++) {
    let flashed = Array(t.nm).fill(false)
    t.v = t.v.map(x => x + 1)
    let flashedNow = false;
    let roundFlashNum = 0
    do {
      flashedNow = false
      for (let [k, x] of t.v.entries()) {
        if (!flashed[k] && 10 <= x) {
          flashedNow = true
          roundFlashNum++
          if (round < 100) {
            p1++
          }
          flashed[k] = true
          for (let neigh of t.neighs8(k)) {
            t.v[neigh]++
          }
        }
      }
    } while (flashedNow)
    t.v = t.v.map(x => 10 <= x ? 0 : x)
    if (roundFlashNum == t.nm) {
      p2 = round + 1
    }
  }
  util.log('P1: ', p1) // 1613
  util.log('P2: ', p2) // 510
}

