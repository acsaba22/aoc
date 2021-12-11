import * as util from '../util.js';
export async function main() {
    let s = await util.loadFile('/src/d09/input.txt');
    let t = s.trim().split('\n').map(x => x.trim().split('').map(y => Number(y)));
    let N = t.length;
    let M = t[0].length;
    let NM = N * M;
    let tflat = [];
    tflat = tflat.concat(...t);
    function neighs(k) {
        let y = Math.floor(k / M);
        let x = k % M;
        let ret = [];
        for (let [ny, nx] of [[y - 1, x], [y + 1, x], [y, x - 1], [y, x + 1]]) {
            if (0 <= ny && ny < N && 0 <= nx && nx < M) {
                ret.push(ny * M + nx);
            }
        }
        return ret;
    }
    let p1 = 0;
    for (let k of Array(NM).keys()) {
        let smallest = true;
        for (let neigh of neighs(k)) {
            if (tflat[neigh] <= tflat[k]) {
                smallest = false;
            }
        }
        if (smallest) {
            p1 += 1 + tflat[k];
        }
    }
    util.log('P1: ', p1); // 580
    let seen = Array(NM).fill(false);
    function bfs(k) {
        let count = 1;
        for (let neigh of neighs(k)) {
            if (tflat[neigh] != 9 && !seen[neigh]) {
                seen[neigh] = true;
                count += bfs(neigh);
            }
        }
        return count;
    }
    let sizes = [];
    for (let o of Array(NM).keys()) {
        if (tflat[o] != 9 && !seen[o]) {
            seen[o] = true;
            sizes.push(bfs(o));
        }
    }
    sizes.sort((a, b) => b - a);
    util.log('P2: ', sizes[0] * sizes[1] * sizes[2]); // 856716
}
