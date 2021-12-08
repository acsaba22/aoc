import * as util from '../util.js';
let N = 0;
function ToCoord(x, y) {
    return y * N + x;
}
function ToCoordS(sx, sy) {
    return ToCoord(Number(sx), Number(sy));
}
function Y(c) {
    return Math.floor(c / N);
}
function X(c) {
    return c % N;
}
class Ocean {
    constructor() {
        this.floor = Array(N * N).fill(0);
    }
    drawLine(p0, p1) {
        if (p1 < p0) {
            [p0, p1] = [p1, p0];
        }
        let y0 = Y(p0);
        let x0 = X(p0);
        let y1 = Y(p1);
        let x1 = X(p1);
        let dy = y1 - y0;
        let dx = x1 - x0;
        util.assert(dy == 0 || dx == 0 || Math.abs(dy / dx) == 1);
        let step = N * Math.sign(dy) + 1 * Math.sign(dx);
        let c = p0 - step;
        do {
            c += step;
            this.floor[c]++;
        } while (c != p1);
    }
    print() {
        for (let i = 0; i < N; i++) {
            util.log(this.floor.slice(i * N, (i + 1) * N).map(x => x ? String(x) : '.').join(''));
        }
    }
}
function maxNum(s) {
    return Math.max(...(s.match(/(\d+)/g)).map(x => Number(x)));
}
function p12(s, diagonalToo) {
    let ocean = new Ocean();
    let re = /(\d+),(\d+) -> (\d+),(\d+)/g;
    let match;
    while (match = re.exec(s)) {
        let [_, a, b, c, d] = match;
        if (diagonalToo || a == c || b == d) {
            ocean.drawLine(ToCoordS(a, b), ToCoordS(c, d));
        }
    }
    // ocean.print()
    return ocean.floor.map(x => 1 < x ? 1 : 0).reduce((a, b) => a + b);
}
export async function main() {
    let s = await util.loadFile('/src/d05/input.txt');
    N = maxNum(s) + 1;
    util.log('P1: ', p12(s, false)); // 7142
    util.log('P2: ', p12(s, true)); // 20012
}
main();
