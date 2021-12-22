import * as util from './util.js'
import React from "react";
import ReactDOM from "react-dom"

util.env.log = function (...args: any[]) {
  console.log(...args)
  const s = args.map(x => x.toString()).join(' ')
  globalRootDiv.addLog(s)
}

util.env.clear = function () {
  console.log('================')
  globalRootDiv.clear()
}

util.env.loadFile = function (fname: string): Promise<string> {
  return fetch(fname)
    .then(response => {
      if (response.ok)
        return response.text();
      if (response.status == 404) {
        console.error("Input not found", `please put the input into <code>${fname}</code> in the project folder`);
      }
      else {
        console.error("Loading input failed", `"${response.statusText}", look in the console for the response object ;)`);
      }
      console.error(response);
      throw response;
    });
}

util.env.pause = function (ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms*pauseMultiplier));
}


let globalRootDiv: RootDiv;


interface State {
  logs: string[]
}

// globalRootDiv.setState({})
class RootDiv extends React.Component {
  state: State
  constructor(props: any) {
    super(props)
    globalRootDiv = this

    this.state = { logs: [] }
  }

  addLog(s: string) {
    this.setState({ logs: this.state.logs.concat([s]) })
  }

  clear() {
    this.setState({logs: []})
  }

  render() {
    let rows = this.state.logs.map((s, i) => <p key={i}>{s}</p>)
    return <div>{rows}</div>
  }

}

util.env.showCanvas = function () {
  let canvas: HTMLCanvasElement = document.querySelector("canvas")!
  canvas.style.display = 'block'
  let context = canvas.getContext("2d")!
  let w = canvas.width
  let h = canvas.height
  util.env.setCanvasSize = (n, m) => {
    h = n
    w = m
    canvas.height = h
    canvas.width = w
  }
  util.env.rect = (y,x,n,m,color) => {
    if (color.startsWith('#')) {
      context.fillStyle = color
    } else {
      context.fillStyle = `#${color}${color}${color}`
    }
    context.fillRect(x/m*w, y/n*h, w/m, h/n)
  }
}

let pauseMultiplier = 1

async function run() {
  ReactDOM.render(
    <RootDiv />,
    document.getElementById('root')
  )

  const urlParams = new URLSearchParams(window.location.search);

  const p = urlParams.get('pauseMultiplier')
  if (p != null) {
    pauseMultiplier = Number(p)
  }

  const d = urlParams.get('d')
  if (d == null || !/^\d{2}$/.test(d)) {
    throw new Error('Specify ?d=NN')
  }

  const path = `./d${d}/solve.js`
  const solve = await import(path);
  solve.main()
}

run().catch(e => util.log('Failed: ', e))

