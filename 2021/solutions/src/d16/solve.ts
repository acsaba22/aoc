import * as util from '../util.js'

type State = Record<string, number>
type Rules = Record<string, string>

function add(state: State, key: string, n: number) {
  state[key] = (state[key] || 0) + n
}

export async function main() {
  let file: string = await util.loadFile('/src/d16/example.txt')
  let lines = file.trim()
}