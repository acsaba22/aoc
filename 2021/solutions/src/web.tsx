import * as util from './util.js'
import React from "react";
import ReactDOM from "react-dom"

util.env.log = function (...args: any[]) {
  console.log(...args)
  const s = args.map(x => x.toString()).join(' ')
  globalRootDiv.addLog(s)
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

  render() {
    let rows = this.state.logs.map((s, i) => <p key={i}>{s}</p>)
    return <div>{rows}</div>
  }

}

ReactDOM.render(
  <RootDiv />,
  document.getElementById('root')
)
