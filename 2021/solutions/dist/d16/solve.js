import * as util from '../util.js';
function add(state, key, n) {
    state[key] = (state[key] || 0) + n;
}
export async function main() {
    let file = await util.loadFile('/src/d16/example.txt');
    let lines = file.trim();
}
