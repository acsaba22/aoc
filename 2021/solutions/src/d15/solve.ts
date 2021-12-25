import * as util from '../util.js'

class Table {
  n: number
  m: number
  nm: number
  v: number[]

  constructor(n: number, m: number, defaultValue: number) {
    this.n = n
    this.m = m
    this.nm = this.n * this.m
    this.v = Array(this.nm).fill(defaultValue)
  }

  static FromLines(lines: number[][]): Table {
    let n = lines.length
    let m = lines[0].length
    let ret = new Table(n, m, 0)
    ret.v = ([] as number[]).concat(...lines)
    return ret
  }

  neighs4(k: number): number[] {
    return this._neighs(k, [[-1, 0], [1, 0], [0, 1], [0, -1]])
  }
  neighs8(k: number): number[] {
    return this._neighs(k,
      [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]])
  }

  idx(y: number, x: number): number {
    return y * this.m + x
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

  draw() {
    this.v.forEach((vv, k) => {
      let y = Math.floor(k / this.m)
      let x = k % this.m
      let color = `${vv}0`
      if (9 < vv) {
        color = "FF"
      }
      util.env.rect(y, x, this.n, this.m, color)
    })
  }
}

function solve(t: Table): number {
  let visited = Array(t.nm).fill(0)

  let queue = (Array(t.nm * 10).fill(null) as (number[] | null)[])
  queue[0] = Array(1).fill(0)

  let minDistance = 0
  while (true) {
    let q = queue[minDistance]
    if (q != null && 0 < q.length) {
      let closestV = minDistance
      let closestId = q.pop()!
      if (visited[closestId] == 1) {
        continue
      }
      if (closestId == t.nm - 1) {
        return closestV
      }
      visited[closestId] = 1
      for (let neigh of t.neighs4(closestId)) {
        let nextV = closestV + t.v[neigh]
        let q2 = queue[nextV] || []
        q2.push(neigh)
        queue[nextV] = q2
      }
    }
    else {
      minDistance++
    }
  }
}

export async function main() {
  let s: string = await util.loadFile('/src/d15/input.txt')
  let lines: number[][] = s.trim().split('\n').map(x => x.trim().split('').map(y => Number(y)))
  let t = Table.FromLines(lines)

  util.log('P1 ', solve(t)) // 40

  const K = 5

  let div: (a: number, b: number) => number = (a, b) => (a - a % b) / b
  let t2 = new Table(t.n * 5, t.m * 5, 0)
  for (let y = 0; y < t.n * K; y++) {
    for (let x = 0; x < t.m * K; x++) {
      let y0 = y % t.n
      let ym = div(y, t.n)
      let x0 = x % t.m
      let xm = div(x, t.m)
      t2.v[t2.idx(y, x)] = (t.v[t.idx(y0, x0)] + ym + xm - 1) % 9 + 1
    }
  }

  util.log('P2 ', solve(t2)) // 3063
}

