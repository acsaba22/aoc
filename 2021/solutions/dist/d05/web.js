import React from "react";
import ReactDOM from "react-dom";
import { main } from "./solve.js";
const gThis = globalThis;
gThis.log = function (...args) {
    console.log(...args);
    const s = args.map(x => x.toString()).join(' ');
    globalRootDiv.addLog(s);
};
gThis.loadFile = function (fname) {
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
};
let globalRootDiv;
// globalRootDiv.setState({})
class RootDiv extends React.Component {
    constructor(props) {
        super(props);
        globalRootDiv = this;
        this.state = { logs: [] };
    }
    addLog(s) {
        this.setState({ logs: this.state.logs.concat([s]) });
    }
    render() {
        let rows = this.state.logs.map((s, i) => React.createElement("p", { key: i }, s));
        return React.createElement("div", null, rows);
    }
}
ReactDOM.render(React.createElement(RootDiv, null), document.getElementById('root'));
main();
