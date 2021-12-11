import * as util from '../util.js'


export async function main() {
  let file: string = await util.loadFile('/src/d10/input.txt')
  let lines: string[] = file.trim().split('\n')

  const opener = "([{<"
  const closer = ")]}>"
  const pointsP1 = [3, 57, 1197, 25137]
  const pointsP2: Record<string, number> = { ')': 1, ']': 2, '}': 3, '>': 4 }


  let p1 = 0
  let p2s: number[] = []

  for (let line of lines) {
    let pos = 0
    let reason = ""
    let finalClosers = ""

    try {
      function parse(neededClosers: string) {
        let c = line[pos]
        let openCode = opener.indexOf(c)
        if (openCode < 0) {
          throw c
        }
        let currentCloser = closer[openCode]
        neededClosers = currentCloser + neededClosers
        pos++
        while (true) {
          if (line.length <= pos) {
            finalClosers = neededClosers
            throw 'unfinished'
          }
          if (line[pos] == currentCloser) {
            pos++
            return
          } else if (opener.includes(line[pos])) {
            parse(neededClosers)
          } else {
            throw line[pos]
          }
        }
      }
      while (pos < line.length) {
        parse("")
      }
    } catch (e) {
      reason = e as string
    }

    if (reason == '') {
      throw "OK"
    } else if (closer.includes(reason)) {
      p1 += pointsP1[closer.indexOf(reason)]
    } else {
      util.assert(reason == 'unfinished')
      p2s.push(finalClosers.split('').reduce((s, b) => 5 * s + pointsP2[b], 0))
    }
  }
  util.log('P1: ', p1) // 296535
  p2s.sort((a, b) => a - b)
  util.log('P2: ', p2s[Math.floor(p2s.length / 2)]) // 4245130838
}
