import React, { Component } from 'react';
import './App.css';

class App extends Component {
  constructor() {
    super();
    this.state = {
      selectedMethods: ['insertLast!', 'deleteLast!', 'getLast', 'getMinimum'],
      optimalDataStructures: null,
      searching: false
    }
  }

  fetch() {
    var body = JSON.stringify({
      adt_methods: this.state.selectedMethods.map((methodName) => {
        var parameters = this.props.decls[methodName].parameters;
        if (parameters.length) {
          return methodName + "[" + parameters.map((p) => p.name) + "]";
        } else {
          return methodName;
        }
      })
    });

    this.setState({ searching: true });

    fetch('/api/search', { headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json, charset=UTF-8'
    }, method: "POST", body: body }).then((response) => {
      if (response.status !== 200) {
        console.log('Looks like there was a problem. Status Code: ' +
          response.status);
        return;
      }

      // Examine the text in the response
      response.json().then((result) => {
        this.setState({ optimalDataStructures: result, searching: false });
      });
    });
  }

  changeSelectedMethod(e, idx) {
    var newSelectedMethods = [...this.state.selectedMethods];
    newSelectedMethods[idx] = e.target.value;
    this.setState({ selectedMethods: newSelectedMethods });
  }

  addSelectedMethod(e, idx) {
    this.setState({ selectedMethods: [...this.state.selectedMethods, ""]});
  }

  removeSelectedMethod(idx) {
    var newSelectedMethods = [...this.state.selectedMethods];
    newSelectedMethods.splice(idx, 1);
    this.setState({ selectedMethods: newSelectedMethods});
  }

  render() {
    var optimalDataStructures = this.state.optimalDataStructures;
    var previousSearchMethods = optimalDataStructures && optimalDataStructures[0] && Object.keys(optimalDataStructures[0].results).sort();

    return (
      <div className="App">
        <div className="App-header">
          <h2>Data structure chooser</h2>
        </div>
        <p className="App-body">
          Choose the methods you want
        </p>

        {this.state.selectedMethods.map((x, idx) => <div key={idx}>
          <select value={x} onChange={(e) => this.changeSelectedMethod(e, idx)}>
            <option />
            {Object.keys(this.props.decls).sort().map((decl, idx) => <option value={decl} key={idx}>{decl}</option>)}
          </select>
          <button onClick={() => this.removeSelectedMethod(idx)}>remove</button>
        </div>)}

        <button onClick={() => this.addSelectedMethod()}>add method</button>

        <button onClick={() => this.fetch()}>{this.state.searching ? "Searching..." : "Search!"}</button>


      {optimalDataStructures &&
        <table>
          <tbody>
            <tr>
              <th />
              {previousSearchMethods.map((m, idx) => <th key={idx}>{m}</th>)}
            </tr>
            {optimalDataStructures.map((ds, idx) => <tr key={idx}>
              <td>{ds.choices.join(", ")}</td>
              {previousSearchMethods.map((m, idx) => <td key={idx}>{ds.results[m].as_string_for_json}</td>)}
            </tr>)}
          </tbody>
        </table>}
      </div>
    );
  }
}

export default App;
