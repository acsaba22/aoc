const toBits = (s) => [...s].map(Number);
const bitsToNum = (bs) => bs.reduce((a, b) => a * 2 + b);
function p1(lines) {
    const n = lines.length;
    const bitNum = lines[0].length;
    const counts = Array(bitNum).fill(0);
    lines.forEach(l => {
        l.forEach((v, i) => { counts[i] += v; });
    });
    let res = bitsToNum(counts.map(v => Number(n - v <= v)));
    log('P1: ', res * ((1 << bitNum) - res - 1));
}
function rating(lines, criteria) {
    const bitNum = lines[0].length;
    for (let bitI = 0; bitI < bitNum && 1 < lines.length; bitI++) {
        let column = lines.map(bs => bs[bitI]);
        let sum = column.reduce((a, b) => a + b);
        let keep = Number(criteria(sum, lines.length));
        lines = lines.filter(v => v[bitI] == keep);
    }
    return bitsToNum(lines[0]);
}
function p2(lines) {
    let oxygen = rating(lines, (count, len) => len / 2 <= count);
    let co2 = rating(lines, (count, len) => count < len / 2);
    log('P2: ', oxygen * co2);
}
export async function main() {
    let s = await loadFile('/src/d03/input.txt');
    let bits = s.trim().split('\n').map(toBits);
    p1(bits); // 3148794
    p2(bits); // 2795310
}
