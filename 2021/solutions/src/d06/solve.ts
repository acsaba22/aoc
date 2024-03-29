import * as util from '../util.js'

function p12(s:string, rounds:number): number {
  let buckets = Array(9).fill(0)
  s.split(',').forEach(x => buckets[Number(x)]++)
  for (let day = 0; day < rounds; day++) {
    let hatch = buckets.shift()
    buckets[6]+=hatch
    buckets.push(hatch)
  }
  return buckets.reduce((a,b) => a+b)
}

export async function main() {
  let s: string = await util.loadFile('/src/d06/input.txt')
  // P1:  352151
  // P2:  1601616884019
  util.log('P1: ', p12(s, 80))
  util.log('P2: ', p12(s,256))
}
