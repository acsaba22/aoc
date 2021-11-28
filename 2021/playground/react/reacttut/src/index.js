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
  constructor(props) {
    super(props)
    this.state = {
      squares: Array(9).fill(null),
      xIsNext: true,
    }
  }
  renderSquare(i) {
    return (
      <Square
        value={this.state.squares[i]}
        onClick={() => { this.handleClick(i) }}
      />);
  }
  next() {
    return this.state.xIsNext ? 'X' : 'O';
  }
  handleClick(i) {
    const xIsNext = this.state.xIsNext
    const squares = this.state.squares.slice();
    if (this.winner() || squares[i]) return;
    squares[i] = this.next();
    this.setState({squares, xIsNext: !xIsNext})
  }
  same(a,b,c) {
    const s = this.state.squares
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

  render() {
    const w = this.winner()
    let status = 'Next player: ' + this.next();
    if (w) {
      status = 'Winner: ' + w;
    }

    return (
      <div>
        <div className="status">{status}</div>
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
  render() {
    return (
      <div className="game">
        <div className="game-board">
          <Board />
        </div>
        <div className="game-info">
          <div>{/* status */}</div>
          <ol>{/* TODO */}</ol>
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
