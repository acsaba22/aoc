import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';


function Square(props) {
  return (
    <button className="square" onClick={props.onClick}>
      {props.value}
    </button>
  );
}

class Board extends React.Component {
  renderSquare(i) {
    return (
      <Square
        value={this.props.squares[i]}
        onClick={() => { this.props.onClick(i) }}
      />);
  }
  render() {
    return (
      <div>
        <div className="board-row">
          {this.renderSquare(0)}
          {this.renderSquare(1)}
          {this.renderSquare(2)}
        </div>
        <div className="board-row">
          {this.renderSquare(3)}
          {this.renderSquare(4)}
          {this.renderSquare(5)}
        </div>
        <div className="board-row">
          {this.renderSquare(6)}
          {this.renderSquare(7)}
          {this.renderSquare(8)}
        </div>
      </div>
    );
  }
}

class Game extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      history: [{
        squares: Array(9).fill(null),
        xIsNext: true,
      },
      ],
      stepNumber: 0,
    }
  }

  next() {
    return this.current().xIsNext ? 'X' : 'O';
  }

  current() {
    const h = this.state.history
    return h[this.state.stepNumber]
  }

  handleClick(i) {
    const c = this.current()
    const xIsNext = c.xIsNext
    let squares = c.squares.slice();
    if (this.winner() || squares[i]) return;
    squares[i] = this.next();
    let step = this.state.stepNumber
    this.setState({
      history: this.state.history.slice(0, step+1).concat([{squares, xIsNext: !xIsNext}]),
      stepNumber: step + 1,
    })
    console.log(this.state)
  }

  same(a,b,c) {
    const s = this.current().squares
    if (s[a] === s[b] && s[a] === s[c]) return s[a];
    return null
  }
  winner() {
    return (
      this.same(0,1,2) ||
      this.same(3,4,5) ||
      this.same(6,7,8) ||
      this.same(0,3,6) ||
      this.same(1,4,7) ||
      this.same(2,5,8) ||
      this.same(0,4,8) ||
      this.same(2,4,6)
    )
  }

  goBack(i) {
    let s = this.state
    this.setState({
      history: s.history,
      stepNumber: i
    })
  }

  render() {
    const w = this.winner()
    let status = 'Next player: ' + this.next();
    if (w) {
      status = 'Winner: ' + w;
    }

    let moves = this.state.history.map((_, move) => {
      return (
        <li key={move} onClick={() => {this.goBack(move)}}>Go to move {move!==0?"#"+move: "start"}</li>
      );
    })
    return (
      <div className="game">
        <div className="game-board">
          <Board squares={this.current().squares} onClick={(i) => this.handleClick(i)}/>
        </div>
        <div className="game-info">
          <div>{ status }</div>
          <ol>{moves}</ol>
        </div>
      </div>
    );
  }
}

// ========================================

ReactDOM.render(
  <Game />,
  document.getElementById('root')
);
