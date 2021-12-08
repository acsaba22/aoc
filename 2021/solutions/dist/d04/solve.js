import * as util from '../util.js';
const N = 5;
const N2 = N * N;
const NI = [0, 1, 2, 3, 4];
class Card {
    constructor(v) {
        util.assert(v.length == N2);
        this.v = v;
        this.marked = Array(N2).fill(false);
    }
    mark(num) {
        for (let [i, v] of this.v.entries()) {
            if (v == num) {
                this.marked[i] = true;
            }
        }
        for (let i of NI) {
            let ok1 = true;
            let ok2 = true;
            for (let j of NI) {
                if (!this.marked[i * N + j]) {
                    ok1 = false;
                }
                if (!this.marked[i + j * N]) {
                    ok2 = false;
                }
            }
            if (ok1 || ok2) {
                return true;
            }
        }
        return false;
    }
    unmarkedSum() {
        let ret = 0;
        for (let [i, b] of this.marked.entries()) {
            if (!b) {
                ret += this.v[i];
            }
        }
        return ret;
    }
}
function takeCard(seq) {
    return [new Card(seq.slice(0, N2)), seq.slice(N2)];
}
export async function main() {
    let s = await loadFile('/src/d04/input.txt');
    let lines = s.split('\n');
    let nums = lines[0].split(',').map(s => Number(s));
    let seq = lines.slice(2).join(' ').match(/[^ ]+/g).map(s => Number(s));
    util.assert(seq.length % (N2) == 0);
    let cards = [];
    while (0 < seq.length) {
        let [card, seq2] = takeCard(seq);
        cards.push(card);
        seq = seq2;
    }
    let first = true;
    for (let num of nums) {
        for (let i = 0; i < cards.length;) {
            let card = cards[i];
            let win = card.mark(num);
            if (win) {
                if (first || cards.length == 1) {
                    log((first ? 'P1: ' : 'P2: '), card.unmarkedSum() * num);
                    first = false;
                }
                cards.splice(i, 1);
            }
            else {
                i++;
            }
        }
    }
    // P1:  50008
    // P2:  17408
}
