import React, { Component } from 'react';
import marked from "marked";
import 'whatwg-fetch';

const PublicMethods = [
  "getFirst",
  "getByIndex",
  "getNext",
  "getLast",
  "insertLast!",
  "insertAtIndex!",
  "insertFirst!",
  "insertNextToNode!",
  "insertAnywhere!",
  "deleteAtIndex!",
  "deleteFirst!",
  "deleteLast!",
  "deleteBetweenNodes!",
  "deleteFirstNodeWithValue!",
  "updateFirstNode!",
  "updateLastNode!",
  "updateByIndex!",
  "getKthBy",
  "getFirstBy",
  "getLastBy",
  "countBetweenBy",
  "getMaximum",
  "getMinimum",
  "deleteFirstBy!",
  "deleteLastBy!",
  "deleteMinimum!",
  "deleteMaximum!",
  "getFirstNodeWithValue",
  "getNearest",
  "getUniformlyRandomlyChosenElement",
  "count",
  "contains",
  "reduce",
  "countOfEquivalenceClass",
  "mostNumerousEquivalenceClass",
  "getMode",
  "twoSidedIndexRangeQuery",
  "oneSidedIndexRangeQuery",
  "rangeMinimumQuery",
  "twoSidedValueRangeQuery",
  "oneSidedValueRangeQuery",
  "getSum"
];

class App extends Component {

  constructor() {
    super();

    let methods = location.hash.substring(1).split(",");

    this.state = {
      selectedMethods: methods[0].length ? methods : ['insertLast!', 'deleteLast!', 'getLast', 'getMinimum'],
      optimalDataStructures: null,
      searching: false,
      bottomPanel: null
    }
  }

  searchWithAjax() {
    let body = JSON.stringify({
      adt_methods: this.state.selectedMethods.map((methodName) => {
        let parameters = this.props.decls[methodName].parameters;
        if (parameters.length) {
          return methodName + "[" + parameters.map((p) => "_") + "]";
        } else {
          return methodName;
        }
      })
    });

    this.setState({
      searching: true,
      optimalDataStructures: null,
      bottomPanel: null
    });

    location.hash = this.state.selectedMethods.join(",")

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
        this.setState({ optimalDataStructures: result.sort(orderDataStructuresByOverallTime), searching: false });
      });
    });
  }

  fetch() {
    console.log(new Date());
    let res = this.props.chooser.choose(this.state.selectedMethods);
    console.log(new Date());
    debugger;
  }

  changeSelectedMethod(e, idx) {
    let newSelectedMethods = [...this.state.selectedMethods];
    newSelectedMethods[idx] = e.target.value;
    this.setState({ selectedMethods: newSelectedMethods });
  }

  randomizeSelectedMethods() {
    function getRandomMethod () {
      return PublicMethods[Math.floor(Math.random()*PublicMethods.length)];
    }

    let obj = {};

    for (let i = 0; i < 4; i++) {
      obj[getRandomMethod()] = true;
    }

    this.setState({ selectedMethods: Object.keys(obj) });
  }

  addSelectedMethod(e, idx) {
    this.setState({ selectedMethods: [...this.state.selectedMethods, ""]});
  }

  removeSelectedMethod(idx) {
    let newSelectedMethods = [...this.state.selectedMethods];
    newSelectedMethods.splice(idx, 1);
    this.setState({ selectedMethods: newSelectedMethods});
  }

  dsChoiceLink(structure) {
    return <a
      className="choice"
      onClick={() => this.setState({ bottomPanel: ["ds", structure.name] })}>
      {structure.name}
    </a>
  }

  dsChoiceList(ds) {
    return ds.structures.map((structure, idx) =>
      <span key={idx}>{this.dsChoiceLink(structure)}</span>
    ).reduce((acc, elem) => (acc === null ? [elem] : [...acc, ', ', elem]), null)
  }

  renderCombo () {
    let choice = this.state.bottomPanel[1];
    let frontendResult = choice.frontend_result.value;

    return <div>
      <h2>Composite data structure: {this.dsChoiceList(choice)}</h2>

      <h3>Read methods</h3>
      <ul>
        {frontendResult.read_methods.map((method, idx) => this.renderMethod(method, idx))}
      </ul>

      <h3>Write methods</h3>
      <ul>
        {frontendResult.write_methods.map((method, idx) => <li key={idx}>
          {method.method_name.name}
          <ul>
            {Object.keys(method.impls).map((dsName, idx) => <li key={idx}>
              {dsName}: <ul>{this.renderMethod(method.impls[dsName])}</ul>
            </li>)}
          </ul>
        </li>)}
      </ul>
    </div>;
  }

  renderMethod(method, idx) {
    return <li key={idx}>
      {method.template.impl_string}
      {method.template.free_impl_source.ds &&
        <span> (from {this.dsChoiceLink(this.props.dataStructures[method.template.free_impl_source.ds])})</span>}
      <ul>
        {method.materials.map((material, idx2) => this.renderMethod(material, idx2))}
      </ul>
    </li>
  }

  render () {
    let optimalDataStructures = this.state.optimalDataStructures;
    let previousSearchMethods = optimalDataStructures && optimalDataStructures[0] && Object.keys(optimalDataStructures[0].results).sort();
    let bottomPanel = this.state.bottomPanel;

    return (
      <div>
        {this.state.selectedMethods.map((x, idx) => <div key={idx}>
          <select value={x} onChange={(e) => this.changeSelectedMethod(e, idx)}>
            <option />
            {Object.keys(this.props.decls).sort().map((decl, idx) => <option value={decl} key={idx}>{decl}</option>)}
          </select>
          <button onClick={() => this.removeSelectedMethod(idx)}>remove</button>
        </div>)}

        <button onClick={() => this.randomizeSelectedMethods()}>randomize!</button>
        <button onClick={() => this.addSelectedMethod()}>add method</button>

        <button onClick={() => this.fetch()}>{this.state.searching ? "Searching..." : "Search!"}</button>

        {optimalDataStructures &&
          (optimalDataStructures.length ?
            <table>
              <tbody>
                <tr>
                  <th />
                  {previousSearchMethods.map((m, idx) => <th key={idx}>{m}</th>)}
                </tr>
                {optimalDataStructures.map((ds, idx) => <tr key={idx}>
                  <td>
                    {this.dsChoiceList(ds)}

                    <span style={{paddingLeft: "5px"}}><button onClick={() => this.setState({ bottomPanel: ["combo", ds]})}>show details</button></span>
                  </td>
                  {previousSearchMethods.map((m, idx) => {
                    let time = ds.result_times[m].to_short_string;
                    return <td key={idx} className={"time-"+time.replace(/[()*]/g, "")}>{time}</td>
                  })}
                </tr>)}
              </tbody>
            </table> :
            <div>
              <p>There were no results found for that ADT!</p>
              <p>This is basically just an error in this app; there shouldn't actually be ADTs which can't be implemented.</p>
            </div>
        )}

        {bottomPanel &&
          <div className="description">
            <hr />
            {bottomPanel[0] === "ds" &&
              <div>
                <h2>{bottomPanel[1]}</h2>
                <pre>{this.props.dataStructureTexts[bottomPanel[1]][0]}</pre>
                <div
                  dangerouslySetInnerHTML={
                    { __html: marked(this.props.dataStructureTexts[bottomPanel[1]][1]) }} />
              </div>
            }
            {bottomPanel[0] === "combo" &&
              this.renderCombo()
            }
          </div>
        }
      </div>
    );
  }
}

function orderByBigO(l, r) {
  if (l.power_of_n !== r.power_of_n) {
    return l.power_of_n < r.power_of_n ? -1 : 1;
  } else {
    return r.power_of_log_n - l.power_of_log_n;
  }
}

function orderDataStructuresByOverallTime(l, r) {
  return orderByBigO(l.overall_time_for_adt, r.overall_time_for_adt);
}

export default App;
