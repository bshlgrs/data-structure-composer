import React, { Component } from 'react';
import './App.css';

class App extends Component {
  fetch() {
    fetch('/api').then((response) => {
      if (response.status !== 200) {
        console.log('Looks like there was a problem. Status Code: ' +
          response.status);
        return;
      }

      // Examine the text in the response
      response.json().then(function(data) {
        console.log(data);
      });
    });
  }
  render() {
    return (
      <div className="App">
        <div className="App-header">
          <h2>Data structure chooser</h2>
        </div>
        <p className="App-body">
          Choose the methods you want WOW
        </p>

        <button onClick={() => this.fetch()}>Fetch!</button>

        <table>
          <tbody>
            <tr>
              <th>Methods</th>
              <th>Hash map</th>
              <th>BST + OST</th>
              <th>OST</th>
            </tr>
            <tr>
              <td>insertByIndex!</td>
              <td>log(n)</td>
              <td>log(n)</td>
              <td>log(n)</td>
            </tr>
            <tr>
              <td>deleteMinimum</td>
              <td>log(n)</td>
              <td>log(n)</td>
              <td>log(n)</td>
            </tr>
            <tr>
              <td>insertAtFront!</td>
              <td>log(n)</td>
              <td>log(n)</td>
              <td>log(n)</td>
            </tr>
            <tr>
              <td>updateNode!</td>
              <td>log(n)</td>
              <td>log(n)</td>
              <td>log(n)</td>
            </tr>
          </tbody>
        </table>
      </div>
    );
  }
}

export default App;
