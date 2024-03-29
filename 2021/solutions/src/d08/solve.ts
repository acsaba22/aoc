import * as util from '../util.js'

type digit = string[]

function contains(d1: digit, d2: digit): boolean {
  for (let d of d2) {
    if (!d1.includes(d)) {
      return false
    }
  }
  return true
}

function digits(s: string): digit[] {
  return s.split(' ').map((x) => x.split('').sort())
}

export async function main() {
  let s: string = await util.loadFile('/src/d08/input.txt')
  let p1 = 0
  let p2 = 0
  for (let line of s.trim().split('\n')) {
    let [a, b] = line.split(' | ')
    let train = digits(a)
    let question = digits(b)
    let count1478 = question.reduce(
      (counts, x) => [2, 3, 4, 7].includes(x.length) ? counts + 1 : counts,
      0)
    p1 += count1478

    function findAndRemove(criteria: (d: digit) => boolean): digit {
      for (let [i, d] of train.entries()) {
        if (criteria(d)) {
          train.splice(i, 1)
          return d
        }
      }
      util.assert(false, 'criteria not found')
      return []
    }

    let dmap: digit[] = Array(10).fill(null)
    dmap[1] = findAndRemove(x => x.length == 2)
    dmap[7] = findAndRemove(x => x.length == 3)
    dmap[4] = findAndRemove(x => x.length == 4)
    dmap[8] = findAndRemove(x => x.length == 7)
    dmap[6] = findAndRemove(x => x.length == 6 && !contains(x, dmap[1]))
    dmap[5] = findAndRemove(x => x.length == 5 && contains(dmap[6], x))
    dmap[3] = findAndRemove(x => x.length == 5 && contains(x, dmap[1]))
    dmap[2] = findAndRemove(x => x.length == 5)
    dmap[9] = findAndRemove(x => x.length == 6 && contains(x, dmap[5]))
    dmap[0] = findAndRemove(x => x.length == 6)

    let val: Record<string, number> = {}
    dmap.forEach((v, i) => val[v.join('')] = i)

    var qdigits = question.map(x=>x.join(''))
    p2 += val[qdigits[0]] * 1000 + val[qdigits[1]] * 100 + val[qdigits[2]] * 10 + val[qdigits[3]]
  }
  util.log('P1: ', p1) // 488
  util.log('P2: ', p2) // 1040429
}
