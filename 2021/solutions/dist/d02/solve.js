import * as util from '../util.js';
function p1(lines) {
    let x = 0;
    let y = 0;
    const move = {
        'forward': (n) => x += n,
        'down': (n) => y += n,
        'up': (n) => y -= n,
    };
    for (const l of lines) {
        const [d, n] = l.split(' ');
        move[d](Number(n));
    }
    util.log('P1: ', x * y); // 1882980
}
function p2(lines) {
    let x = 0;
    let y = 0;
    let aim = 0;
    let move = new Map([
        ['forward', (n) => {
                x += n;
                y += n * aim;
            }],
        ['down', (n) => aim += n],
        ['up', (n) => aim -= n],
    ]);
    for (let l of lines) {
        let [d, n] = l.split(' ');
        move.get(d)(Number(n));
    }
    util.log('P2: ', x * y); // 1882980
}
export async function main() {
    let s = await util.loadFile('/src/d02/input.txt');
    let lines = s.trim().split('\n');
    p1(lines);
    p2(lines);
}
