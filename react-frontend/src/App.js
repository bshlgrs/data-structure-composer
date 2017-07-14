import React, { Component } from 'react';

import ChallengeApp from './ChallengeApp'
import SearchApp from './SearchApp'

class App extends Component {
  constructor () {
    super();

    this.state = {
      challengeMode: location.hash === "#challenge"
    };
  }

  render () {
    if (this.state.challengeMode) {
      return <ChallengeApp />;
    } else {
      return <SearchApp methods={location.hash.substring(1).split(",")} {...this.props} />;
    }
  }
}


export default App;

