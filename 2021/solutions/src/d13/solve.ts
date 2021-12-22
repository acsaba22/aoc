import * as util from '../util.js'

type Point = [number, number];

export async function main() {
  let s: string = await util.loadFile('/src/d13/input.txt')
  let [dotsS, foldsS] = s.trim().split('\n\n')
  let points: Point[] = dotsS.split('\n').map(l => {
    let [x, y] = l.trim().split(',').map(y => Number(y))
    return [x, y]
  })
  util.env.showCanvas()

  let maxX = 0
  let maxY = 0
  function draw() {
    maxX = 0
    maxY = 0
    points.forEach(([x, y]) => {
      maxX = Math.max(x + 1, maxX)
      maxY = Math.max(y + 1, maxY)
    })
    const maxWidth = 800
    const maxHeight = 500
    let ratio = Math.min(maxHeight / maxY, maxWidth / maxX)
    util.env.setCanvasSize(Math.floor(maxY * ratio), Math.floor(maxX * ratio))
    util.env.rect(0, 0, 1, 1, 'FF')
    points.forEach(([x, y]) => {
      util.env.rect(y, x, maxY, maxX, '00')
    })
  }

  draw()
  for (let [round, foldStr] of foldsS.split('\n').entries()) {
    let fold = foldStr.match(/fold along ([xy])=(\d+)/)!
    let foldPlane = fold[1]
    let foldLine = Number(fold[2])
    await util.env.pause(1000)
    if (foldPlane == 'y') {
      for (let x of Array(maxX).keys()) {
        util.env.rect(foldLine, x, maxY, maxX, '#0022AA')
      }
    } else {
      for (let y of Array(maxY).keys()) {
        util.env.rect(y, foldLine, maxY, maxX, '#0022AA')
      }
    }

    points = points.map(([x, y]) => {
      if (foldPlane == 'y') {
        [x, y] = [y, x]
      }
      if (foldLine <= x) {
        x = 2 * foldLine - x
      }
      if (foldPlane == 'y') {
        [x, y] = [y, x]
      }
      return [x, y]
    })
    if (round == 0) {
      let ss = new Set(points.map(x => x.join('-')))
      let aa = [...ss]
      aa.sort()
      util.log('P1: ', ss.size)  // P1 785
    }
    await util.env.pause(1000)
    draw()
  }
  util.log('P2:  read on canvas')
}

