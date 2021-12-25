import * as util from '../util.js';
function add(state, key, n) {
    state[key] = (state[key] || 0) + n;
}
export async function main() {
    let file = await util.loadFile('/src/d14/input.txt');
    let [seq, ruleLines] = file.trim().split('\n\n');
    seq = seq.trim();
    let rules = {};
    for (let rule of ruleLines.split('\n')) {
        let [from, _, to] = rule.trim().split(' ');
        rules[from] = to;
    }
    let state = {};
    let prev = '';
    for (let c of seq) {
        if (prev != '') {
            add(state, prev + c, 1);
        }
        prev = c;
    }
    function solution() {
        let counts = {};
        add(counts, seq[0], 1);
        add(counts, seq[seq.length - 1], 1);
        for (let [pair, v] of Object.entries(state)) {
            add(counts, pair[0], v);
            add(counts, pair[1], v);
        }
        return Math.max(...Object.values(counts)) / 2 - Math.min(...Object.values(counts)) / 2;
    }
    for (let i of Array(40).keys()) {
        let nextState = {};
        for (let [pair, v] of Object.entries(state)) {
            let middle = rules[pair];
            add(nextState, pair[0] + middle, v);
            add(nextState, middle + pair[1], v);
        }
        state = Object.assign({}, nextState);
        if (i + 1 == 10) {
            util.log('P1: ', solution()); // 2112
        }
        if (i + 1 == 40) {
            util.log('P2: ', solution()); // 3243771149914
        }
    }
}
